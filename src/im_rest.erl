%%% im_rest.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018-2019 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This library module implements utility functions
%%% 	for REST servers in the {@link //im. im} application.
%%%
-module(im_rest).
-copyright('Copyright (c) 2018-2019 SigScale Global Inc.').

-export([date/1, iso8601/1, etag/1]).
-export([parse_query/1, range/1]).
-export([lifecycle_status/1]).
-export([related/1, related_party/1, related_category/1]).

-include("im.hrl").

% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(EPOCH, 62167219200).

%%----------------------------------------------------------------------
%%  The im_rest public API
%%----------------------------------------------------------------------

-spec date(DateTimeFormat) -> Result
	when
		DateTimeFormat	:: pos_integer() | tuple(),
		Result			:: calendar:datetime() | non_neg_integer().
%% @doc Convert iso8610 to date and time or
%%		date and time to timeStamp.
date(MilliSeconds) when is_integer(MilliSeconds) ->
	Seconds = ?EPOCH + (MilliSeconds div 1000),
	calendar:gregorian_seconds_to_datetime(Seconds);
date(DateTime) when is_tuple(DateTime) ->
	Seconds = calendar:datetime_to_gregorian_seconds(DateTime) - ?EPOCH,
	Seconds * 1000.

-spec iso8601(MilliSeconds) -> Result
	when
		MilliSeconds	:: pos_integer() | string(),
		Result			:: string() | pos_integer().
%% @doc Convert iso8610 to ISO 8601 format date and time.
iso8601(MilliSeconds) when is_integer(MilliSeconds) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = date(MilliSeconds),
	DateFormat = "~4.10.0b-~2.10.0b-~2.10.0b",
	TimeFormat = "T~2.10.0b:~2.10.0b:~2.10.0b.~3.10.0b",
	Chars = io_lib:fwrite(DateFormat ++ TimeFormat,
			[Year, Month, Day, Hour, Minute, Second, MilliSeconds rem 1000]),
	lists:flatten(Chars);
iso8601(ISODateTime) when is_list(ISODateTime) ->
	case string:rchr(ISODateTime, $T) of
		0 ->
			iso8601(ISODateTime, []);
		N ->
			iso8601(lists:sublist(ISODateTime, N - 1),
				lists:sublist(ISODateTime,  N + 1, length(ISODateTime)))
	end.
%% @hidden
iso8601(Date, Time) when is_list(Date), is_list(Time) ->
	D = iso8601_date(string:tokens(Date, ",-"), []),
	{H, Mi, S, Ms} = iso8601_time(string:tokens(Time, ":."), []),
	date({D, {H, Mi, S}}) + Ms.
%% @hidden
iso8601_date([[Y1, Y2, Y3, Y4] | T], _Acc) ->
	Y = list_to_integer([Y1, Y2, Y3, Y4]),
	iso8601_date(T, Y);
iso8601_date([[M1, M2] | T], Y) when is_integer(Y) ->
	M = list_to_integer([M1, M2]),
	iso8601_date(T, {Y, M});
iso8601_date([[D1, D2] | T], {Y, M}) ->
	D = list_to_integer([D1, D2]),
	iso8601_date(T, {Y, M, D});
iso8601_date([], {Y, M}) ->
	{Y, M, 1};
iso8601_date([], {Y, M, D}) ->
	{Y, M, D}.
%% @hidden
iso8601_time([H1 | T], []) ->
	H = list_to_integer(H1),
	iso8601_time(T, H);
iso8601_time([M1 | T], H) when is_integer(H) ->
	Mi = list_to_integer(M1),
	iso8601_time(T, {H, Mi});
iso8601_time([S1 | T], {H, Mi}) ->
	S = list_to_integer(S1),
	iso8601_time(T, {H, Mi, S});
iso8601_time([], {H, Mi}) ->
	{H, Mi, 0, 0};
iso8601_time([Ms1 | T], {H, Mi, S}) ->
	Ms = list_to_integer(Ms1),
	iso8601_time(T, {H, Mi, S, Ms});
iso8601_time([], {H, Mi, S}) ->
	{H, Mi, S, 0};
iso8601_time([], {H, Mi, S, Ms}) ->
	{H, Mi, S, Ms};
iso8601_time([], []) ->
	{0,0,0,0}.

-spec etag(Etag) -> Etag
	when
		Etag :: string() | {TS, N},
		TS :: pos_integer(),
		N :: pos_integer().
%% @doc Map unique timestamp and HTTP ETag.
etag({TS, N} = _Etag) when is_integer(TS), is_integer(N)->
	integer_to_list(TS) ++ "-" ++ integer_to_list(N);
etag(Etag) when is_list(Etag) ->
	[TS, N] = string:tokens(Etag, "-"),
	{list_to_integer(TS), list_to_integer(N)}.

-spec parse_query(Query) -> Result
	when
		Query :: string(),
		Result :: [{Key, Value}],
		Key :: string(),
		Value :: string().
%% @doc Parse the query portion of a URI.
%% @throws {error, 400}
parse_query("?" ++ Query) ->
	parse_query(Query);
parse_query(Query) when is_list(Query) ->
	parse_query(string:tokens(Query, "&"), []).
%% @hidden
parse_query([H | T], Acc) ->
	parse_query(T, parse_query1(H, string:chr(H, $=), Acc));
parse_query([], Acc) ->
	lists:reverse(Acc).
%% @hidden
parse_query1(_Field, 0, _Acc) ->
	throw({error, 400});
parse_query1(Field, N, Acc) ->
	Key = lists:sublist(Field, N - 1),
	Value = lists:sublist(Field, N + 1, length(Field)),
	[{Key, Value} | Acc].

-spec range(Range) -> Result
	when
		Range :: RHS | {Start, End},
		RHS :: string(),
		Result :: {ok, {Start, End}} | {ok, RHS} | {error, 400},
		Start :: pos_integer(),
		End :: pos_integer().
%% @doc Parse or create a `Range' request header.
%% 	`RHS' should be the right hand side of an
%% 	RFC7233 `Range:' header conforming to TMF630
%% 	(e.g. "items=1-100").
%% @private
range(Range) when is_list(Range) ->
	try
		["items", S, E] = string:tokens(Range, "= -"),
		{ok, {list_to_integer(S), list_to_integer(E)}}
	catch
		_:_ ->
			{error, 400}
	end;
range({Start, End}) when is_integer(Start), is_integer(End) ->
	{ok, "items=" ++ integer_to_list(Start) ++ "-" ++ integer_to_list(End)}.

-spec lifecycle_status(LifeCycleStatus) -> LifeCycleStatus
	when
		LifeCycleStatus :: string() | in_study | in_design | in_test
				| rejected | active | launched | retired | obsolete.
%% @doc CODEC for Resource Catalog life cycle status.
lifecycle_status(in_study = _Status) ->
	"In Study";
lifecycle_status(in_design) ->
	"In Design";
lifecycle_status(in_test) ->
	"In Test";
lifecycle_status(rejected) ->
	"Rejected";
lifecycle_status(active) ->
	"Active";
lifecycle_status(launched) ->
	"Launched";
lifecycle_status(retired) ->
	"Retired";
lifecycle_status(obsolete) ->
	"Obsolete";
lifecycle_status("In Study") ->
	in_study;
lifecycle_status("In Design") ->
	in_design;
lifecycle_status("In Test") ->
	in_test;
lifecycle_status("Rejected") ->
	rejected;
lifecycle_status("Active") ->
	active;
lifecycle_status("Launched") ->
	launched;
lifecycle_status("Retired") ->
	retired;
lifecycle_status("Obsolete") ->
	obsolete.

-spec related(RelatedRef) -> RelatedRef
	when
		RelatedRef :: [related()] | [map()]
				| related() | map().
%% @doc CODEC for simple relationships.
related(#related{} = RelatedRef) ->
	related(record_info(fields, related), RelatedRef, #{});
related(#{} = RelatedRef) ->
	related(record_info(fields, related), RelatedRef, #related{});
related([#related{} | _] = List) ->
	Fields = record_info(fields, related),
	[related(Fields, R, #{}) || R <- List];
related([#{} | _] = List) ->
	Fields = record_info(fields, related),
	[related(Fields, R, #related{}) || R <- List].
%% @hidden
related([id | T], #related{id = Id} = R, Acc) ->
	related(T, R, Acc#{"id" => Id});
related([id | T], #{"id" := Id} = M, Acc) ->
	related(T, M, Acc#related{id = Id});
related([href | T], #related{href = Href} = R, Acc) ->
	related(T, R, Acc#{"href" => Href});
related([href | T], #{"href" := Href} = M, Acc) ->
	related(T, M, Acc#related{href = Href});
related([name | T], #related{name = Name} = R, Acc) ->
	related(T, R, Acc#{"name" => Name});
related([name | T], #{"name" := Name} = M, Acc) ->
	related(T, M, Acc#related{name = Name});
related([version | T], #related{version = Version} = R, Acc)
		when is_list(Version) ->
	related(T, R, Acc#{"version" => Version});
related([version | T], #{"version" := Version} = M, Acc) ->
	related(T, M, Acc#related{version = Version});
related([_ | T], R, Acc) ->
	related(T, R, Acc);
related([], _, Acc) ->
	Acc.

-spec related_party(RelatedPartyRef) -> RelatedPartyRef
	when
		RelatedPartyRef :: [related_party()] | [map()]
				| related_party() | map().
%% @doc CODEC for `RelatedPartyRef'.
related_party(#related_party{} = RelatedPartyRef) ->
	related_party(record_info(fields, related_party), RelatedPartyRef, #{});
related_party(#{} = RelatedPartyRef) ->
	related_party(record_info(fields, related_party), RelatedPartyRef, #related_party{});
related_party([#related_party{} | _] = List) ->
	Fields = record_info(fields, related_party),
	[related_party(Fields, RP, #{}) || RP <- List];
related_party([#{} | _] = List) ->
	Fields = record_info(fields, related_party),
	[related_party(Fields, RP, #related_party{}) || RP <- List].
%% @hidden
related_party([id | T], #related_party{id = Id} = R, Acc) ->
	related_party(T, R, Acc#{"id" => Id});
related_party([id | T], #{"id" := Id} = M, Acc) ->
	related_party(T, M, Acc#related_party{id = Id});
related_party([href | T], #related_party{href = Href} = R, Acc) ->
	related_party(T, R, Acc#{"href" => Href});
related_party([href | T], #{"href" := Href} = M, Acc) ->
	related_party(T, M, Acc#related_party{href = Href});
related_party([name | T], #related_party{name = Name} = R, Acc) ->
	related_party(T, R, Acc#{"name" => Name});
related_party([name | T], #{"name" := Name} = M, Acc) ->
	related_party(T, M, Acc#related_party{name = Name});
related_party([role | T], #related_party{role = Role} = R, Acc)
		when is_list(Role) ->
	related_party(T, R, Acc#{"role" => Role});
related_party([role | T], #{"role" := Role} = M, Acc) ->
	related_party(T, M, Acc#related_party{role = Role});
related_party([start_date | T], #related_party{start_date = StartDate} = R, Acc)
		when is_integer(StartDate) ->
	ValidFor = #{"startDateTime" => im_rest:iso8601(StartDate)},
	related_party(T, R, Acc#{"validFor" => ValidFor});
related_party([start_date | T],
		#{"validFor" := #{"startDateTime" := Start}} = M, Acc) ->
	related_party(T, M, Acc#related_party{start_date = im_rest:iso8601(Start)});
related_party([end_date | T], #related_party{end_date = End} = R,
		#{validFor := ValidFor} = Acc) when is_integer(End) ->
	NewValidFor = ValidFor#{"endDateTime" => im_rest:iso8601(End)},
	related_party(T, R, Acc#{"validFor" := NewValidFor});
related_party([end_date | T], #related_party{end_date = End} = R, Acc)
		when is_integer(End) ->
	ValidFor = #{"endDateTime" => im_rest:iso8601(End)},
	related_party(T, R, Acc#{"validFor" := ValidFor});
related_party([end_date | T],
		#{"validFor" := #{"endDateTime" := End}} = M, Acc) ->
	related_party(T, M, Acc#related_party{end_date = im_rest:iso8601(End)});
related_party([_ | T], R, Acc) ->
	related_party(T, R, Acc);
related_party([], _, Acc) ->
	Acc.

-spec related_category(CategoryRef) -> CategoryRef
	when
		CategoryRef :: #related{} | map().
%% @doc CODEC for `CategoryRef'.
related_category(#related{} = CategoryRef) ->
	related_category(record_info(fields, related), CategoryRef, #{});
related_category(#{} = CategoryRef) ->
	related_category(record_info(fields, related), CategoryRef, #related{});
related_category([#related{} | _] = List) ->
	Fields = record_info(fields, related),
	[related_category(Fields, R, #{}) || R <- List];
related_category([#{} | _] = List) ->
	Fields = record_info(fields, related),
	[related_category(Fields, R, #related{}) || R <- List].
%% @hidden
related_category([id | T], #related{id = Id} = R, Acc) ->
	related_category(T, R, Acc#{"id" => Id});
related_category([id | T], #{"id" := Id} = M, Acc) ->
	related_category(T, M, Acc#related{id = Id});
related_category([href | T], #related{href = Href} = R, Acc) ->
	related_category(T, R, Acc#{"href" => Href});
related_category([href | T], #{"href" := Href} = M, Acc) ->
	related_category(T, M, Acc#related{href = Href});
related_category([name | T], #related{name = Name} = R, Acc) ->
	related_category(T, R, Acc#{"name" => Name});
related_category([name | T], #{"name" := Name} = M, Acc) ->
	related_category(T, M, Acc#related{name = Name});
related_category([version | T], #related{version = Version} = R, Acc) ->
	related_category(T, R, Acc#{"version" => Version});
related_category([version | T], #{"version" := Version} = M, Acc) ->
	related_category(T, M, Acc#related{version = Version});
related_category([_ | T], R, Acc) ->
	related_category(T, R, Acc);
related_category([], _, Acc) ->
	Acc.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

