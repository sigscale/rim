%%% im_rest.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018-2020 SigScale Global Inc.
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
-copyright('Copyright (c) 2018-2020 SigScale Global Inc.').

-export([date/1, iso8601/1, geoaxis/1, etag/1]).
-export([parse_query/1, range/1, pointer/1, patch/2]).
-export([lifecycle_status/1]).
-export([party_ref/1, category_ref/1, candidate_ref/1,
		specification_ref/1, target_schema_ref/1, constraint_ref/1]).
-export([format_problem/2]).

-include("im.hrl").

% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(EPOCH, 62167219200).

%%----------------------------------------------------------------------
%%  The im_rest public API
%%----------------------------------------------------------------------

-spec date(DateTime) -> DateTime
	when
		DateTime	:: pos_integer() | calendar:datetime().
%% @doc Convert between `DateTime' and number of milliseconds since the Unix epoch.
%%		date and time to timeStamp.
date(DateTime) when is_integer(DateTime) ->
	Seconds = ?EPOCH + (DateTime div 1000),
	calendar:gregorian_seconds_to_datetime(Seconds);
date(DateTime) when is_tuple(DateTime) ->
	Seconds = calendar:datetime_to_gregorian_seconds(DateTime) - ?EPOCH,
	Seconds * 1000.

-spec pointer(Path) -> Pointer
   when
      Path :: string(),
      Pointer :: [string()].
%% @doc Decode JSON Pointer.
%%    Apply the decoding rules of <a href="http://tools.ietf.org/html/rfc6901">RFC6901</a>. 
%%    `Path' is a JSON string as used in the `"path"' member of a
%%    JSON Patch ((<a href="http://tools.ietf.org/html/rfc6902">RFC6902</a>)
%%    operation. `Pointer' is a list of member name strings in a path.
pointer(Pointer) ->
   pointer(Pointer, [], []).
%% @hidden
pointer([$/ | T], [], Acc) ->
   pointer(T, [], Acc);
pointer([$/ | T], Acc1, Acc2) ->
   pointer(T, [], [lists:reverse(Acc1) | Acc2]);
pointer([$- | T], Acc1, Acc2) ->
   pointer1(T, Acc1, Acc2);
pointer([H | T], Acc1, Acc2) ->
   pointer(T, [H | Acc1], Acc2);
pointer([], Acc1, Acc2) ->
   lists:reverse([lists:reverse(Acc1) | Acc2]).
%% @hidden
pointer1([$1 | T], Acc1, Acc2) ->
   pointer(T, [$/ | Acc1], Acc2);
pointer1([$0 | T], Acc1, Acc2) ->
   pointer(T, [$- | Acc1], Acc2);
pointer1(T, Acc1, Acc2) ->
   pointer(T, [$- | Acc1], Acc2).

-spec patch(Patch, Resource) -> Result
   when
      Patch :: [map()],
      Resource :: map(),
		Result :: map().
%% @doc Apply a JSON `Patch' (<a href="http://tools.ietf.org/html/rfc6902">RFC6902</a>).
%%    Modifies the `Resource' by applying the operations listed in `Patch'.
%%    `Operation' may be `"add"', `"remove"', or `"replace"'.
%%
patch([#{"op" := "add", "path" := Pointer,
		"value" := Value} | T] = _Patch, Resource) ->
	[Path] = pointer(Pointer),
   patch(T, Resource#{Path => Value});
patch([#{"op" := "replace", "path" := Pointer,
		"value" := Value} | T], Resource) ->
	[Path] = pointer(Pointer),
   patch(T, Resource#{Path := Value});
patch([#{"op" := "remove", "path" := Pointer} | T], Resource) ->
	[Path] = pointer(Pointer),
   patch(T, maps:remove(Path, Resource));
patch([], Resource) ->
   Resource.

-spec iso8601(DateTime) -> DateTime
	when
		DateTime :: pos_integer() | string().
%% @doc Convert between ISO 8601 and Unix epoch milliseconds.
%% 	Parsing is not strict to allow prefix matching.
iso8601(DateTime) when is_integer(DateTime) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = date(DateTime),
	DateFormat = "~4.10.0b-~2.10.0b-~2.10.0b",
	TimeFormat = "T~2.10.0b:~2.10.0b:~2.10.0b.~3.10.0bZ",
	Chars = io_lib:fwrite(DateFormat ++ TimeFormat,
			[Year, Month, Day, Hour, Minute, Second, DateTime rem 1000]),
	lists:flatten(Chars);
iso8601([Y1, Y2, Y3, Y4 | T])
		when Y1 >= $0, Y1 =< $9, Y2 >= $0, Y2 =< $9,
		Y3 >= $0, Y3 =< $9, Y4 >= $0, Y4 =< $9 ->
	iso8601month(list_to_integer([Y1, Y2, Y3, Y4]), T).
%% @hidden
iso8601month(Year, []) ->
	DateTime = {{Year, 1, 1}, {0, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601month(Year, [$-]) ->
	iso8601month(Year, []);
iso8601month(Year, [$-, $0]) ->
	iso8601month(Year, [$-, $0, $1]);
iso8601month(Year, [$-, $1]) ->
	iso8601month(Year, [$-, $1, $0]);
iso8601month(Year, [$-, M1, M2 | T])
		when M1 >= $0, M1 =< $1, M2 >= $0, M2 =< $9 ->
	iso8601day(Year, list_to_integer([M1, M2]), T).
%% @hidden
iso8601day(Year, Month, []) ->
	DateTime = {{Year, Month, 1}, {0, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601day(Year, Month, [$-]) ->
	iso8601day(Year, Month, []);
iso8601day(Year, Month, [$-, $0]) ->
	iso8601day(Year, Month, [$-, $1, $0]);
iso8601day(Year, Month, [$-, D1])
		when D1 >= $1, D1 =< $3 ->
	iso8601day(Year, Month, [$-, D1, $0]);
iso8601day(Year, Month, [$-, D1, D2 | T])
		when D1 >= $0, D1 =< $3, D2 >= $0, D2 =< $9 ->
	Day = list_to_integer([D1, D2]),
	iso8601hour({Year, Month, Day}, T).
%% @hidden
iso8601hour(Date, []) ->
	DateTime = {Date, {0, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601hour(Date, [$T]) ->
	iso8601hour(Date, []);
iso8601hour(Date, [$T, H1])
		when H1 >= $0, H1 =< $2 ->
	iso8601hour(Date, [$T, H1, $0]);
iso8601hour(Date, [$T, H1, H2 | T])
		when H1 >= $0, H1 =< $2, H2 >= $0, H2 =< $9 ->
	Hour = list_to_integer([H1, H2]),
	iso8601minute(Date, Hour, T).
%% @hidden
iso8601minute(Date, Hour, []) ->
	DateTime = {Date, {Hour, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601minute(Date, Hour, [$:]) ->
	iso8601minute(Date, Hour, []);
iso8601minute(Date, Hour, [$:, M1])
		when M1 >= $0, M1 =< $5 ->
	iso8601minute(Date, Hour, [$:, M1, $0]);
iso8601minute(Date, Hour, [$:, M1, M2 | T])
		when M1 >= $0, M1 =< $5, M2 >= $0, M2 =< $9 ->
	Minute = list_to_integer([M1, M2]),
	iso8601second(Date, Hour, Minute, T);
iso8601minute(Date, Hour, _) ->
	DateTime = {Date, {Hour, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000.
%% @hidden
iso8601second(Date, Hour, Minute, []) ->
	DateTime = {Date, {Hour, Minute, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601second(Date, Hour, Minute, [$:]) ->
	iso8601second(Date, Hour, Minute, []);
iso8601second(Date, Hour, Minute, [$:, S1])
		when S1 >= $0, S1 =< $5 ->
	iso8601second(Date, Hour, Minute, [$:, S1, $0]);
iso8601second(Date, Hour, Minute, [$:, S1, S2 | T])
		when S1 >= $0, S1 =< $5, S2 >= $0, S2 =< $9 ->
	Second = list_to_integer([S1, S2]),
	DateTime = {Date, {Hour, Minute, Second}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	EpocMilliseconds = (GS - ?EPOCH) * 1000,
	iso8601millisecond(EpocMilliseconds, T);
iso8601second(Date, Hour, Minute, _) ->
	DateTime = {Date, {Hour, Minute, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000.
%% @hidden
iso8601millisecond(EpocMilliseconds, []) ->
	EpocMilliseconds;
iso8601millisecond(EpocMilliseconds, [$.]) ->
	EpocMilliseconds;
iso8601millisecond(EpocMilliseconds, [$., N1, N2, N3 | _])
		when N1 >= $0, N1 =< $9, N2 >= $0, N2 =< $9,
		N3 >= $0, N3 =< $9 ->
	EpocMilliseconds + list_to_integer([N1, N2, N3]);
iso8601millisecond(EpocMilliseconds, [$., N1, N2 | _])
		when N1 >= $0, N1 =< $9, N2 >= $0, N2 =< $9 ->
	EpocMilliseconds + list_to_integer([N1, N2]) * 10;
iso8601millisecond(EpocMilliseconds, [$., N | _])
		when N >= $0, N =< $9 ->
	EpocMilliseconds + list_to_integer([N]) * 100;
iso8601millisecond(EpocMilliseconds, _) ->
	EpocMilliseconds.

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

-spec party_ref(RelatedPartyRef) -> RelatedPartyRef
	when
		RelatedPartyRef :: [party_ref()] | [map()]
				| party_ref() | map().
%% @doc CODEC for `RelatedPartyRef'.
party_ref(#party_ref{} = RelatedPartyRef) ->
	party_ref(record_info(fields, party_ref), RelatedPartyRef, #{});
party_ref(#{} = RelatedPartyRef) ->
	party_ref(record_info(fields, party_ref), RelatedPartyRef, #party_ref{});
party_ref([#party_ref{} | _] = List) ->
	Fields = record_info(fields, party_ref),
	[party_ref(Fields, RP, #{}) || RP <- List];
party_ref([#{} | _] = List) ->
	Fields = record_info(fields, party_ref),
	[party_ref(Fields, RP, #party_ref{}) || RP <- List].
%% @hidden
party_ref([id | T], #party_ref{id = Id} = R, Acc)
		when is_list(Id) ->
	party_ref(T, R, Acc#{"id" => Id});
party_ref([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	party_ref(T, M, Acc#party_ref{id = Id});
party_ref([href | T], #party_ref{href = Href} = R, Acc)
		when is_list(Href) ->
	party_ref(T, R, Acc#{"href" => Href});
party_ref([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	party_ref(T, M, Acc#party_ref{href = Href});
party_ref([name | T], #party_ref{name = Name} = R, Acc)
		when is_list(Name) ->
	party_ref(T, R, Acc#{"name" => Name});
party_ref([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	party_ref(T, M, Acc#party_ref{name = Name});
party_ref([class_type | T], #party_ref{class_type = Type} = R, Acc)
		when is_list(Type) ->
	party_ref(T, R, Acc#{"@type" => Type});
party_ref([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	party_ref(T, M, Acc#party_ref{class_type = Type});
party_ref([base_type | T], #party_ref{base_type = Type} = R, Acc)
		when is_list(Type) ->
	party_ref(T, R, Acc#{"@type" => Type});
party_ref([base_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	party_ref(T, M, Acc#party_ref{base_type = Type});
party_ref([schema | T], #party_ref{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	party_ref(T, R, Acc#{"@schemaLocation" => Schema});
party_ref([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	party_ref(T, M, Acc#party_ref{schema = Schema});
party_ref([role | T], #party_ref{role = Role} = R, Acc)
		when is_list(Role) ->
	party_ref(T, R, Acc#{"role" => Role});
party_ref([role | T], #{"role" := Role} = M, Acc)
		when is_list(Role) ->
	party_ref(T, M, Acc#party_ref{role = Role});
party_ref([ref_type | T], #party_ref{ref_type = Type} = R, Acc)
		when is_list(Type) ->
	party_ref(T, R, Acc#{"@referredType" => Type});
party_ref([ref_type | T], #{"@referredType" := Type} = M, Acc)
		when is_list(Type) ->
	party_ref(T, M, Acc#party_ref{ref_type = Type});
party_ref([_ | T], R, Acc) ->
	party_ref(T, R, Acc);
party_ref([], _, Acc) ->
	Acc.

-spec category_ref(CategoryRef) -> CategoryRef
	when
		CategoryRef :: [category_ref()] | [map()]
				| category_ref() | map().
%% @doc CODEC for `CategoryRef'.
category_ref(#category_ref{} = CategoryRef) ->
	category_ref(record_info(fields, category_ref), CategoryRef, #{});
category_ref(#{} = CategoryRef) ->
	category_ref(record_info(fields, category_ref), CategoryRef, #category_ref{});
category_ref([#category_ref{} | _] = List) ->
	Fields = record_info(fields, category_ref),
	[category_ref(Fields, R, #{}) || R <- List];
category_ref([#{} | _] = List) ->
	Fields = record_info(fields, category_ref),
	[category_ref(Fields, R, #category_ref{}) || R <- List].
%% @hidden
category_ref([id | T], #category_ref{id = Id} = R, Acc)
		when is_list(Id) ->
	category_ref(T, R, Acc#{"id" => Id});
category_ref([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	category_ref(T, M, Acc#category_ref{id = Id});
category_ref([href | T], #category_ref{href = Href} = R, Acc)
		when is_list(Href) ->
	category_ref(T, R, Acc#{"href" => Href});
category_ref([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	category_ref(T, M, Acc#category_ref{href = Href});
category_ref([name | T], #category_ref{name = Name} = R, Acc)
		when is_list(Name) ->
	category_ref(T, R, Acc#{"name" => Name});
category_ref([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	category_ref(T, M, Acc#category_ref{name = Name});
category_ref([class_type | T], #category_ref{class_type = Type} = R, Acc)
		when is_list(Type) ->
	category_ref(T, R, Acc#{"@type" => Type});
category_ref([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	category_ref(T, M, Acc#category_ref{class_type = Type});
category_ref([base_type | T], #category_ref{base_type = Type} = R, Acc)
		when is_list(Type) ->
	category_ref(T, R, Acc#{"@baseType" => Type});
category_ref([base_type | T], #{"@baseTtype" := Type} = M, Acc)
		when is_list(Type) ->
	category_ref(T, M, Acc#category_ref{base_type = Type});
category_ref([schema | T], #category_ref{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	category_ref(T, R, Acc#{"@schemaLocation" => Schema});
category_ref([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	category_ref(T, M, Acc#category_ref{schema = Schema});
category_ref([version | T], #category_ref{version = Version} = R, Acc)
		when is_list(Version) ->
	category_ref(T, R, Acc#{"version" => Version});
category_ref([version | T], #{"version" := Version} = M, Acc)
		when is_list(Version) ->
	category_ref(T, M, Acc#category_ref{version = Version});
category_ref([ref_type | T], #category_ref{ref_type = Type} = R, Acc)
		when is_list(Type) ->
	category_ref(T, R, Acc#{"@referredType" => Type});
category_ref([ref_type | T], #{"@referredTtype" := Type} = M, Acc)
		when is_list(Type) ->
	category_ref(T, M, Acc#category_ref{ref_type = Type});
category_ref([_ | T], R, Acc) ->
	category_ref(T, R, Acc);
category_ref([], _, Acc) ->
	Acc.

-spec candidate_ref(CandidateRef) -> CandidateRef
	when
		CandidateRef :: [candidate_ref()] | [map()]
				| candidate_ref() | map().
%% @doc CODEC for `CandidateRef'.
candidate_ref(#candidate_ref{} = CandidateRef) ->
	candidate_ref(record_info(fields, candidate_ref), CandidateRef, #{});
candidate_ref(#{} = CandidateRef) ->
	candidate_ref(record_info(fields, candidate_ref), CandidateRef, #candidate_ref{});
candidate_ref([#candidate_ref{} | _] = List) ->
	Fields = record_info(fields, candidate_ref),
	[candidate_ref(Fields, R, #{}) || R <- List];
candidate_ref([#{} | _] = List) ->
	Fields = record_info(fields, candidate_ref),
	[candidate_ref(Fields, R, #candidate_ref{}) || R <- List].
%% @hidden
candidate_ref([id | T], #candidate_ref{id = Id} = R, Acc)
		when is_list(Id) ->
	candidate_ref(T, R, Acc#{"id" => Id});
candidate_ref([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	candidate_ref(T, M, Acc#candidate_ref{id = Id});
candidate_ref([href | T], #candidate_ref{href = Href} = R, Acc)
		when is_list(Href) ->
	candidate_ref(T, R, Acc#{"href" => Href});
candidate_ref([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	candidate_ref(T, M, Acc#candidate_ref{href = Href});
candidate_ref([name | T], #candidate_ref{name = Name} = R, Acc)
		when is_list(Name) ->
	candidate_ref(T, R, Acc#{"name" => Name});
candidate_ref([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	candidate_ref(T, M, Acc#candidate_ref{name = Name});
candidate_ref([class_type | T], #candidate_ref{class_type = Type} = R, Acc)
		when is_list(Type) ->
	candidate_ref(T, R, Acc#{"@type" => Type});
candidate_ref([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	candidate_ref(T, M, Acc#candidate_ref{class_type = Type});
candidate_ref([base_type | T], #candidate_ref{base_type = Type} = R, Acc)
		when is_list(Type) ->
	candidate_ref(T, R, Acc#{"@baseType" => Type});
candidate_ref([base_type | T], #{"@baseTtype" := Type} = M, Acc)
		when is_list(Type) ->
	candidate_ref(T, M, Acc#candidate_ref{base_type = Type});
candidate_ref([schema | T], #candidate_ref{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	candidate_ref(T, R, Acc#{"@schemaLocation" => Schema});
candidate_ref([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	candidate_ref(T, M, Acc#candidate_ref{schema = Schema});
candidate_ref([version | T], #candidate_ref{version = Version} = R, Acc)
		when is_list(Version) ->
	candidate_ref(T, R, Acc#{"version" => Version});
candidate_ref([version | T], #{"version" := Version} = M, Acc)
		when is_list(Version) ->
	candidate_ref(T, M, Acc#candidate_ref{version = Version});
candidate_ref([ref_type | T], #candidate_ref{ref_type = Type} = R, Acc)
		when is_list(Type) ->
	candidate_ref(T, R, Acc#{"@referredType" => Type});
candidate_ref([ref_type | T], #{"@referredTtype" := Type} = M, Acc)
		when is_list(Type) ->
	candidate_ref(T, M, Acc#candidate_ref{ref_type = Type});
candidate_ref([_ | T], R, Acc) ->
	candidate_ref(T, R, Acc);
candidate_ref([], _, Acc) ->
	Acc.

-spec specification_ref(ResourceSpecificationRef) -> ResourceSpecificationRef
	when
		ResourceSpecificationRef :: [specification_ref()] | [map()]
				| specification_ref() | map().
%% @doc CODEC for `ResourceSpecificationRef'.
specification_ref(#specification_ref{} = ResourceSpecificationRef) ->
	specification_ref(record_info(fields, specification_ref),
			ResourceSpecificationRef, #{});
specification_ref(#{} = ResourceSpecificationRef) ->
	specification_ref(record_info(fields, specification_ref),
			ResourceSpecificationRef, #specification_ref{});
specification_ref([#specification_ref{} | _] = List) ->
	Fields = record_info(fields, specification_ref),
	[specification_ref(Fields, R, #{}) || R <- List];
specification_ref([#{} | _] = List) ->
	Fields = record_info(fields, specification_ref),
	[specification_ref(Fields, R, #specification_ref{}) || R <- List].
%% @hidden
specification_ref([id | T], #specification_ref{id = Id} = R, Acc)
		when is_list(Id) ->
	specification_ref(T, R, Acc#{"id" => Id});
specification_ref([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	specification_ref(T, M, Acc#specification_ref{id = Id});
specification_ref([href | T], #specification_ref{href = Href} = R, Acc)
		when is_list(Href) ->
	specification_ref(T, R, Acc#{"href" => Href});
specification_ref([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	specification_ref(T, M, Acc#specification_ref{href = Href});
specification_ref([name | T], #specification_ref{name = Name} = R, Acc)
		when is_list(Name) ->
	specification_ref(T, R, Acc#{"name" => Name});
specification_ref([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	specification_ref(T, M, Acc#specification_ref{name = Name});
specification_ref([class_type | T], #specification_ref{class_type = Type} = R, Acc)
		when is_list(Type) ->
	specification_ref(T, R, Acc#{"@type" => Type});
specification_ref([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	specification_ref(T, M, Acc#specification_ref{class_type = Type});
specification_ref([base_type | T], #specification_ref{base_type = Type} = R, Acc)
		when is_list(Type) ->
	specification_ref(T, R, Acc#{"@baseType" => Type});
specification_ref([base_type | T], #{"@baseType" := Type} = M, Acc)
		when is_list(Type) ->
	specification_ref(T, M, Acc#specification_ref{base_type = Type});
specification_ref([schema | T], #specification_ref{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	specification_ref(T, R, Acc#{"@schemaLocation" => Schema});
specification_ref([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	specification_ref(T, M, Acc#specification_ref{schema = Schema});
specification_ref([version | T], #specification_ref{version = Version} = R, Acc)
		when is_list(Version) ->
	specification_ref(T, R, Acc#{"version" => Version});
specification_ref([version | T], #{"version" := Version} = M, Acc)
		when is_list(Version) ->
	specification_ref(T, M, Acc#specification_ref{version = Version});
specification_ref([ref_type | T], #specification_ref{ref_type = Type} = R, Acc)
		when is_list(Type) ->
	specification_ref(T, R, Acc#{"@referredType" => Type});
specification_ref([ref_type | T], #{"@referredType" := Type} = M, Acc)
		when is_list(Type) ->
	specification_ref(T, M, Acc#specification_ref{ref_type = Type});
specification_ref([_ | T], R, Acc) ->
	specification_ref(T, R, Acc);
specification_ref([], _, Acc) ->
	Acc.

-spec target_schema_ref(TargetSchemaRef) -> TargetSchemaRef
	when
		TargetSchemaRef :: [target_schema_ref()] | [map()]
				| target_schema_ref() | map().
%% @doc CODEC for `TargetSchemaRef'.
target_schema_ref(#target_schema_ref{} = TargetSchemaRef) ->
	target_schema_ref(record_info(fields, target_schema_ref),
			TargetSchemaRef, #{});
target_schema_ref(#{} = TargetSchemaRef) ->
	target_schema_ref(record_info(fields, target_schema_ref),
			TargetSchemaRef, #target_schema_ref{});
target_schema_ref([#target_schema_ref{} | _] = List) ->
	Fields = record_info(fields, target_schema_ref),
	[target_schema_ref(Fields, R, #{}) || R <- List];
target_schema_ref([#{} | _] = List) ->
	Fields = record_info(fields, target_schema_ref),
	[target_schema_ref(Fields, R, #target_schema_ref{}) || R <- List].
%% @hidden
target_schema_ref([class_type | T], #target_schema_ref{class_type = ClassType} = R, Acc)
		when is_list(ClassType) ->
	target_schema_ref(T, R, Acc#{"@type" => ClassType});
target_schema_ref([class_type | T], #{"@type" := ClassType} = M, Acc)
		when is_list(ClassType) ->
	target_schema_ref(T, M, Acc#target_schema_ref{class_type = ClassType});
target_schema_ref([base_type | T], #target_schema_ref{base_type = ClassType} = R, Acc)
		when is_list(ClassType) ->
	target_schema_ref(T, R, Acc#{"@baseType" => ClassType});
target_schema_ref([base_type | T], #{"@baseType" := ClassType} = M, Acc)
		when is_list(ClassType) ->
	target_schema_ref(T, M, Acc#target_schema_ref{base_type = ClassType});
target_schema_ref([schema | T], #target_schema_ref{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	target_schema_ref(T, R, Acc#{"@schemaLocation" => Schema});
target_schema_ref([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	target_schema_ref(T, M, Acc#target_schema_ref{schema = Schema});
target_schema_ref([_ | T], R, Acc) ->
	target_schema_ref(T, R, Acc);
target_schema_ref([], _, Acc) ->
	Acc.

-spec constraint_ref(ConstraintRef) -> ConstraintRef
	when
		ConstraintRef :: [constraint_ref()] | [map()]
				| constraint_ref() | map().
%% @doc CODEC for `ConstraintRef'.
constraint_ref(#constraint_ref{} = ConstraintRef) ->
	constraint_ref(record_info(fields, constraint_ref), ConstraintRef, #{});
constraint_ref(#{} = ConstraintRef) ->
	constraint_ref(record_info(fields, constraint_ref), ConstraintRef, #constraint_ref{});
constraint_ref([#constraint_ref{} | _] = List) ->
	Fields = record_info(fields, constraint_ref),
	[constraint_ref(Fields, R, #{}) || R <- List];
constraint_ref([#{} | _] = List) ->
	Fields = record_info(fields, constraint_ref),
	[constraint_ref(Fields, R, #constraint_ref{}) || R <- List].
%% @hidden
constraint_ref([id | T], #constraint_ref{id = Id} = R, Acc)
		when is_list(Id) ->
	constraint_ref(T, R, Acc#{"id" => Id});
constraint_ref([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	constraint_ref(T, M, Acc#constraint_ref{id = Id});
constraint_ref([href | T], #constraint_ref{href = Href} = R, Acc)
		when is_list(Href) ->
	constraint_ref(T, R, Acc#{"href" => Href});
constraint_ref([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	constraint_ref(T, M, Acc#constraint_ref{href = Href});
constraint_ref([name | T], #constraint_ref{name = Name} = R, Acc)
		when is_list(Name) ->
	constraint_ref(T, R, Acc#{"name" => Name});
constraint_ref([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	constraint_ref(T, M, Acc#constraint_ref{name = Name});
constraint_ref([class_type | T], #constraint_ref{class_type = Type} = R, Acc)
		when is_list(Type) ->
	constraint_ref(T, R, Acc#{"@type" => Type});
constraint_ref([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	constraint_ref(T, M, Acc#constraint_ref{class_type = Type});
constraint_ref([base_type | T], #constraint_ref{base_type = Type} = R, Acc)
		when is_list(Type) ->
	constraint_ref(T, R, Acc#{"@baseType" => Type});
constraint_ref([base_type | T], #{"@baseTtype" := Type} = M, Acc)
		when is_list(Type) ->
	constraint_ref(T, M, Acc#constraint_ref{base_type = Type});
constraint_ref([schema | T], #constraint_ref{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	constraint_ref(T, R, Acc#{"@schemaLocation" => Schema});
constraint_ref([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	constraint_ref(T, M, Acc#constraint_ref{schema = Schema});
constraint_ref([version | T], #constraint_ref{version = Version} = R, Acc)
		when is_list(Version) ->
	constraint_ref(T, R, Acc#{"version" => Version});
constraint_ref([version | T], #{"version" := Version} = M, Acc)
		when is_list(Version) ->
	constraint_ref(T, M, Acc#constraint_ref{version = Version});
constraint_ref([ref_type | T], #constraint_ref{ref_type = Type} = R, Acc)
		when is_list(Type) ->
	constraint_ref(T, R, Acc#{"@referredType" => Type});
constraint_ref([ref_type | T], #{"@referredTtype" := Type} = M, Acc)
		when is_list(Type) ->
	constraint_ref(T, M, Acc#constraint_ref{ref_type = Type});
constraint_ref([_ | T], R, Acc) ->
	constraint_ref(T, R, Acc);
constraint_ref([], _, Acc) ->
	Acc.

-spec geoaxis(Axis) -> Axis
	when
		Axis :: string() | non_neg_integer().
%% @doc CODEC for latitude/longitude axis value.
%%
%% Internally an integer value is used to represent an axis.
%% Externally a string representation of a decimal number, with
%% up to four decimal places, allows a precision of 11.1 meters.
%%
geoaxis(Axis) when Axis rem 10000 =:= 0 ->
	integer_to_list(Axis div 10000);
geoaxis(Axis) when Axis < 0, Axis > -10000, Axis rem 1000 =:= 0 ->
	lists:flatten(io_lib:fwrite("-0.~1.10.0b", [abs(Axis) div 1000]));
geoaxis(Axis) when Axis < 0, Axis > -10000, Axis rem 100 =:= 0 ->
	lists:flatten(io_lib:fwrite("-0.~2.10.0b", [abs(Axis)div 100]));
geoaxis(Axis) when Axis < 0, Axis > -10000, Axis rem 10 =:= 0 ->
	lists:flatten(io_lib:fwrite("-0.~3.10.0b", [abs(Axis) div 10]));
geoaxis(Axis) when Axis < 0, Axis > -10000 ->
	lists:flatten(io_lib:fwrite("-0.~4.10.0b", [abs(Axis)]));
geoaxis(Axis) when Axis rem 1000 =:= 0  ->
	lists:flatten(io_lib:fwrite("~b.~b", [Axis div 10000, (abs(Axis) rem 10000) div 1000]));
geoaxis(Axis) when Axis rem 100 =:= 0  ->
	lists:flatten(io_lib:fwrite("~b.~2.10.0b", [Axis div 10000, (abs(Axis) rem 10000) div 100]));
geoaxis(Axis) when Axis rem 10 =:= 0  ->
	lists:flatten(io_lib:fwrite("~b.~3.10.0b", [Axis div 10000, (abs(Axis) rem 10000) div 10]));
geoaxis(Axis) when is_integer(Axis) ->
	lists:flatten(io_lib:fwrite("~b.~4.10.0b", [Axis div 10000, abs(Axis) rem 10000]));
geoaxis(Axis) when is_list(Axis) ->
	case string:tokens(Axis, ".") of
		[[$- | Int], Dec] when length(Dec) =:= 4 ->
			-((list_to_integer(Int) * 10000) + list_to_integer(Dec));
		[Int, Dec] when length(Dec) =:= 4 ->
			(list_to_integer(Int) * 10000) + list_to_integer(Dec);
		[[$- | Int], Dec] when length(Dec) =:= 3 ->
			-((list_to_integer(Int) * 10000) + (list_to_integer(Dec) * 10));
		[Int, Dec] when length(Dec) =:= 3 ->
			(list_to_integer(Int) * 10000) + (list_to_integer(Dec) * 10);
		[[$- | Int], Dec] when length(Dec) =:= 2 ->
			-((list_to_integer(Int) * 10000) + (list_to_integer(Dec) * 100));
		[Int, Dec] when length(Dec) =:= 2 ->
			(list_to_integer(Int) * 10000) + (list_to_integer(Dec) * 100);
		[[$- | Int], Dec] when length(Dec) =:= 1 ->
			-((list_to_integer(Int) * 10000) + (list_to_integer(Dec) * 1000));
		[Int, Dec] when length(Dec) =:= 1 ->
			(list_to_integer(Int) * 10000) + (list_to_integer(Dec) * 1000);
		[Int] ->
			list_to_integer(Int) * 10000
	end.

-type uri() :: string().
-type problem() :: #{type := uri(), title := string(),
		code := string(), cause => string(), detail => string(),
		invalidParams => [#{param := string(), reason => string()}],
		status => 200..599}.
-spec format_problem(Problem, Headers) -> Result
	when
		Problem :: problem(),
		Headers :: [tuple()],
		Result :: {ContentType, Body},
		ContentType :: string(),
		Body :: string().
%% @doc Format a problem report in an accepted content type.
%%
%% 	`Problem' MUST contain `type', `title', and `code'.
%% 	RFC7807 specifies `type' as a URI reference to
%% 	human-readable documentation for the problem type.
%% 	Use `title' for a short summary of the problem type.
%% 	TMF630 mandates `code' to provide an application
%% 	related code which may be included in an API
%% 	specification. 3GPP SBI adds `cause' and `invalidParams'.
%%
%% 	The result shall be formatted in one of the following
%% 	media types, in priority order:
%%
%%		ContentType :: "application/problem+json"
%%				| "application/json" | "text/html"
%% @private
format_problem(Problem, Headers) ->
	case lists:keyfind("accept", 1, Headers) of
		{_, Accept} ->
			format_problem1(Problem, string:tokens(Accept, ", "));
		false ->
			[]
	end.
%% @hidden
format_problem1(Problem, Accepted) ->
	F = fun(AcceptedType) ->
			lists:prefix("application/problem+json", AcceptedType)
	end,
	case lists:any(F, Accepted) of
		true ->
			Type = ["\t\"type\": \"", maps:get(type, Problem), "\",\n"],
			Title = ["\t\"title\": \"", maps:get(title, Problem), "\""],
			Detail = case maps:find(detail, Problem) of
				{ok, Value1} ->
					[",\n\t\"detail\": \"", Value1, "\",\n"];
				error ->
					[]
			end,
			Cause = case maps:find(cause, Problem) of
				{ok, Value2} ->
					[",\n\t\"cause\": \"", Value2, "\""];
				error ->
					[]
			end,
			InvalidParams = case maps:find(invalidParams, Problem) of
				{ok, Value3} ->
					Fold = fun(#{param := P, reason := R}, Acc) ->
								Comma = case length(Acc) of
									0 ->
										[];
									_ ->
										",\n"
								end,
								Param = ["\t\t{\n\t\t\t\"param\": \"", P, "\",\n"],
								Reason = ["\t\t\t\"reason\": \"", R, "\"\n\t\t}"],
								Acc ++ [Comma, Param, Reason];
							(#{param := P}, Acc) ->
								Comma = case length(Acc) of
									0 ->
										[];
									_ ->
										",\n"
								end,
								Param = ["\t\t{\n\t\t\t\"param\": \"", P, "\"\n\t\t}"],
								Acc ++ [Comma, Param]
					end,
					[",\n\t\"invalidParams\": [\n",
							lists:foldl(Fold, [], Value3), "\n\t]"];
				error ->
					[]
			end,
			Status = case maps:find(status, Problem) of
				{ok, Value4} ->
					[",\n\t\"status\": ", integer_to_list(Value4)];
				error ->
					[]
			end,
			Code = case maps:get(code, Problem) of
				C1 when length(C1) > 0 ->
					[",\n\t\"code\": \"", C1, "\"\n"];
				_C1 ->
					[]
			end,
			{"application/problem+json",
					[${, $\n, Type, Title, Detail, Cause,
					InvalidParams, Status, Code, $\n, $}]};
		false ->
			format_problem2(Problem, Accepted)
	end.
%% @hidden
format_problem2(Problem, Accepted) ->
	F = fun(AcceptedType) ->
			lists:prefix("application/json", AcceptedType)
	end,
	case lists:any(F, Accepted) of
		true ->
			Class = "\t\"@type\": \"Error\"\n",
			Type = ["\t\"referenceError\": \"", maps:get(type, Problem), "\",\n"],
			Code = ["\t\"code\": \"", maps:get(code, Problem), "\",\n"],
			Reason = ["\t\"reason\": \"", maps:get(title, Problem), "\""],
			Message = case maps:find(detail, Problem) of
				{ok, Value1} ->
					[",\n\t\"message\": \"", Value1, "\",\n"];
				error ->
					[]
			end,
			Status = case maps:find(status, Problem) of
				{ok, Value2} ->
					[",\n\t\"status\": \"", integer_to_list(Value2), "\""];
				error ->
					[]
			end,
			Body = [${, $\n, Class, Type, Code, Reason, Message, Status, $\n, $}],
			{"application/json", Body};
		false ->
			format_problem3(Problem)
	end.
%% @hidden
format_problem3(Problem) ->
	H1 = "\n\t\t\t<h1>SigScale IM REST API</h1>\n",
	Paragraph  = "\t\t\t<p>Oops! Something went wrong.</p>\n",
	Header = ["\t\t<header>\n", H1, Paragraph, "\t\t</header>\n"],
	ProblemType = maps:get(type, Problem),
	Link = ["<a href=\"", ProblemType, "\">", ProblemType, "</a>"],
	Type = ["\t\t\t\<dt>Problem Type</dt>\n\t\t\t<dd>",
			Link, "</dd>\n"],
	Title = ["\t\t\t<dt>Title</dt>\n\t\t\t<dd>",
			maps:get(title, Problem), "</dd>\n"],
	Detail = case maps:find(detail, Problem) of
		{ok, Value1} ->
			["\t\t\t<dt>Detail</dt>\n\t\t\t<dd>",
					Value1, "</dd>\n"];
		error ->
			[]
	end,
	Cause = case maps:find(cause, Problem) of
		{ok, Value2} ->
			["\t\t\t<dt>Cause</dt>\n\t\t\t<dd>",
					Value2, "</dd>\n"];
		error ->
			[]
	end,
	InvalidParams = case maps:find(invalidParams, Problem) of
		{ok, Value3} ->
			F = fun(#{param := P, reason := R}, Acc) ->
						Acc ++ ["\t\t\t\t\t<tr>\n",
						"\t\t\t\t\t\t<td>", P, "</td>\n",
						"\t\t\t\t\t\t<td>", R, "</td>\n",
						"\t\t\t\t\t</tr>\n"];
					(#{param := P}, Acc) ->
						Acc ++ ["\t\t\t\t\t<tr>\n",
						"\t\t\t\t\t\t<td>", P, "</td>\n",
						"\t\t\t\t\t</tr>\n"]
			end,
			["\t\t\t<dt>Invalid Parameters</dt>\n\t\t\t<dd>\n",
					"\t\t\t\t<table>\n",
					"\t\t\t\t\t<tr>\n",
					"\t\t\t\t\t\t<th>Parameter</th>\n",
					"\t\t\t\t\t\t<th>Reason</th>\n",
					"\t\t\t\t\t</tr>\n",
					lists:foldl(F, [], Value3),
					"\t\t\t</dd>\n"];
		error ->
			[]
	end,
	Status = case maps:find(status, Problem) of
		{ok, Value4} ->
			["\t\t\t<dt>Status</dt>\n\t\t\t<dd>",
					integer_to_list(Value4), "</dd>\n"];
		error ->
			[]
	end,
	Code = case maps:get(code, Problem) of
		C1 when length(C1) > 0 ->
			["\t\t\t<dt>Code<dt>\n\t\t\t<dd>", C1, "</dd>\n"];
		_C1 ->
			[]
	end,
	Definitions = ["\t\t<dl>\n", Type, Title, Detail, Cause,
			InvalidParams, Status, Code, "\t\t</dl>\n"],
	Body = ["\t<body>\n", Header, Definitions, "\t</body>\n"],
	Head = "\t<head>\n\t\t<title>Error</title>\n\t</head>\n",
	HTML = ["<!DOCTYPE html>\n<html lang=\"en\">\n", Head, Body, "</html>"],
	{"text/html", HTML}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

