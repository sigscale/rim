%%% im_rest_res_specification.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2019 SigScale Global Inc.
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
%%% @doc This library module implements resource handling functions
%%% 	for a REST server in the {@link //im. im} application.
%%%
%%% 	Handle `ResourceSpecification' collection.
%%%
-module(im_rest_res_specification).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([get_specifications/2, get_specification/2, post_specification/1,
		delete_specification/1]).
-export([specification/1]).
 
-include("im.hrl").

-define(PathCatalog, "/resourceCatalogManagement/v3/").

%%----------------------------------------------------------------------
%%  The im public API
%%----------------------------------------------------------------------

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/json-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec get_specifications(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Handle `GET' request on `ResourceSpecification' collection.
get_specifications(Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_specifications(NewQuery, Filters, Headers);
		false ->
			get_specifications(Query, [], Headers)
	end.
%% @hidden
get_specifications(Query, Filters, Headers) ->
	case {lists:keyfind("if-match", 1, Headers),
			lists:keyfind("if-range", 1, Headers),
			lists:keyfind("range", 1, Headers)} of
		{{"if-match", Etag}, false, {"range", Range}} ->
			case global:whereis_name(Etag) of
				undefined ->
					{error, 412};
				PageServer ->
					case im_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_page(PageServer, Etag, Query, Filters, Start, End)
					end
			end;
		{{"if-match", Etag}, false, false} ->
			case global:whereis_name(Etag) of
				undefined ->
					{error, 412};
				PageServer ->
					query_page(PageServer, Etag, Query, Filters, undefined, undefined)
			end;
		{false, {"if-range", Etag}, {"range", Range}} ->
			case global:whereis_name(Etag) of
				undefined ->
					case im_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_start(Query, Filters, Start, End)
					end;
				PageServer ->
					case im_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_page(PageServer, Etag, Query, Filters, Start, End)
					end
			end;
		{{"if-match", _}, {"if-range", _}, _} ->
			{error, 400};
		{_, {"if-range", _}, false} ->
			{error, 400};
		{false, false, {"range", Range}} ->
			case im_rest:range(Range) of
				{error, _} ->
					{error, 400};
				{ok, {Start, End}} ->
					query_start(Query, Filters, Start, End)
			end;
		{false, false, false} ->
			query_start(Query, Filters, undefined, undefined)
	end.

-spec get_specification(Id, Query) -> Result
	when
		Id :: string(),
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Handle `GET' request on a `ResourceSpecification' resource.
get_specification(Id, Query) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, L}, NewQuery} ->
			get_specification(Id, NewQuery, string:tokens(L, ","));
		false ->
			get_specification(Id, Query, [])
	end.
%% @hidden
get_specification(Id, [] = _Query, _Filters) ->
	case im:get_specification(Id) of
		{ok, #specification{last_modified = LastModified} = Specification} ->
			Headers = [{content_type, "application/json"},
					{etag, im_rest:etag(LastModified)}],
			Body = zj:encode(specification(Specification)),
			{ok, Headers, Body};
		{error, _Reason} ->
			{error, 404}
	end;
get_specification(_, _, _) ->
	{error, 400}.

-spec post_specification(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Handle `POST' request on `ResourceSpecification' collection.
post_specification(RequestBody) ->
	try
		Specification = specification(zj:decode(RequestBody)),
		case im:add_specification(Specification) of
			{ok, #specification{id = Id, last_modified = LM} = NewSpecification} ->
				Body = zj:encode(specification(NewSpecification)),
				Location = ?PathCatalog ++ "specification/" ++ Id,
				Headers = [{location, Location}, {etag, im_rest:etag(LM)}],
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec delete_specification(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Handle `DELETE' request on a `ResourceSpecification' resource.
delete_specification(Id) ->
	case im:delete_specification(Id) of
		ok ->
			{ok, [], []};
		{error, _Reason} ->
			{error, 400}
	end.

-spec specification(ResourceSpecification) -> ResourceSpecification
	when
		ResourceSpecification :: #specification{} | map().
%% @doc CODEC for `ResourceSpecification'.
specification(#specification{} = ResourceSpecification) ->
	specification(record_info(fields, specification), ResourceSpecification, #{});
specification(#{} = ResourceSpecification) ->
	specification(record_info(fields, specification), ResourceSpecification, #specification{}).
%% @hidden
specification([id | T], #specification{id = Id} = R, Acc) ->
	specification(T, R, Acc#{"id" => Id});
specification([id | T], #{"id" := Id} = M, Acc) ->
	specification(T, M, Acc#specification{id = Id});
specification([href | T], #specification{href = Href} = R, Acc) ->
	specification(T, R, Acc#{"href" => Href});
specification([href | T], #{"href" := Href} = M, Acc) ->
	specification(T, M, Acc#specification{href = Href});
specification([name | T], #specification{name = Name} = R, Acc) ->
	specification(T, R, Acc#{"name" => Name});
specification([name | T], #{"name" := Name} = M, Acc) ->
	specification(T, M, Acc#specification{name = Name});
specification([description| T],
		#specification{description = Description} = R, Acc) ->
	specification(T, R, Acc#{"description" => Description});
specification([description| T], #{"description" := Description} = M, Acc) ->
	specification(T, M, Acc#specification{description = Description});
specification([version | T], #specification{version = Version} = R, Acc) ->
	specification(T, R, Acc#{"version" => Version});
specification([version | T], #{"version" := Version} = M, Acc) ->
	specification(T, M, Acc#specification{version = Version});
specification([start_date | T], #specification{start_date = StartDate} = R, Acc)
		when is_integer(StartDate) ->
	ValidFor = #{"startDateTime" => im_rest:iso8601(StartDate)},
	specification(T, R, Acc#{"validFor" => ValidFor});
specification([start_date | T],
		#{"validFor" := #{"startDateTime" := Start}} = M, Acc) ->
	specification(T, M, Acc#specification{start_date = im_rest:iso8601(Start)});
specification([end_date | T], #specification{end_date = End} = R,
		#{validFor := ValidFor} = Acc) when is_integer(End) ->
	NewValidFor = ValidFor#{"endDateTime" => im_rest:iso8601(End)},
	specification(T, R, Acc#{"validFor" := NewValidFor});
specification([end_date | T], #specification{end_date = End} = R, Acc)
		when is_integer(End) ->
	ValidFor = #{"endDateTime" => im_rest:iso8601(End)},
	specification(T, R, Acc#{"validFor" := ValidFor});
specification([end_date | T],
		#{"validFor" := #{"endDateTime" := End}} = M, Acc) ->
	specification(T, M, Acc#specification{end_date = im_rest:iso8601(End)});
specification([last_modified | T], #specification{last_modified = LM} = R, Acc) ->
	specification(T, R, Acc#{"lastUpdate" => im_rest:iso8601(LM)});
specification([last_modified | T], #{"lastUpdate" := LM} = M, Acc) ->
	specification(T, M, Acc#specification{last_modified = im_rest:iso8601(LM)});
specification([status | T], #specification{status = Status} = R, Acc)
		when Status /= undefined ->
	specification(T, R, Acc#{"lifecycleStatus" => im_rest:lifecycle_status(Status)});
specification([status | T], #{"lifecycleStatus" := Status} = M, Acc) ->
	specification(T, M, Acc#specification{status = im_rest:lifecycle_status(Status)});
specification([bundle | T], #specification{bundle = Bundle} = R, Acc) ->
	specification(T, R, Acc#{"isBundle" => Bundle});
specification([bundle | T], #{"isBundle" := Bundle} = M, Acc) ->
	specification(T, M, Acc#specification{bundle = Bundle});
specification([related_party | T], #specification{related_party = PartyRefs} = R, Acc) ->
	specification(T, R, Acc#{"relatedParty" => im_rest:related_party(PartyRefs)});
specification([related_party | T], #{"relatedParty" := PartyRefs} = M, Acc) ->
	specification(T, M, Acc#specification{category = im_rest:related_party(PartyRefs)});
%% @todo resourceSpecCharacteristic and resourceSpecRelationship
specification([_ | T], R, Acc) ->
	specification(T, R, Acc);
specification([], _, Acc) ->
	Acc.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
query_start(Query, Filters, RangeStart, RangeEnd) ->
	try
		case lists:keyfind("filter", 1, Query) of
			{_, String} ->
				{ok, Tokens, _} = im_rest_query_scanner:string(String),
				case im_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex, [{"id", like, [Id]}]}]}]} ->
						{{like, Id}, '_'}
				end;
			false ->
				{'_', '_'}
		end
	of
		{MatchId, MatchLocale} ->
			MFA = [im, query_specification, [MatchId, MatchLocale]],
			case supervisor:start_child(im_rest_pagination_sup, [MFA]) of
				{ok, PageServer, Etag} ->
					query_page(PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
				{error, _Reason} ->
					{error, 500}
			end
	catch
		_ ->
			{error, 400}
	end.

%% @hidden
query_page(PageServer, Etag, _Query, _Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}, infinity) of
		{error, Status} ->
			{error, Status};
		{Events, ContentRange} ->
			Specifications = lists:map(fun specification/1, Events),
			Body = zj:encode(Specifications),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange}],
			{ok, Headers, Body}
	end.

