%%% im_rest_res_candidate.erl
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
%%% 	Handle `ResourceCandidate' collection.
%%%
-module(im_rest_res_candidate).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([get_candidates/2, get_candidate/2, post_candidate/1,
		delete_candidate/1]).
-export([candidate/1]).
 
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

-spec get_candidates(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Handle `GET' request on `ResourceCandidate' collection.
get_candidates(Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_candidates(NewQuery, Filters, Headers);
		false ->
			get_candidates(Query, [], Headers)
	end.
%% @hidden
get_candidates(Query, Filters, Headers) ->
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

-spec get_candidate(Id, Query) -> Result
	when
		Id :: string(),
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Handle `GET' request on a `ResourceCandidate' resource.
get_candidate(Id, Query) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, L}, NewQuery} ->
			get_candidate(Id, NewQuery, string:tokens(L, ","));
		false ->
			get_candidate(Id, Query, [])
	end.
%% @hidden
get_candidate(Id, [] = _Query, _Filters) ->
	case im:get_candidate(Id) of
		{ok, #candidate{last_modified = LastModified} = Candidate} ->
			Headers = [{content_type, "application/json"},
					{etag, im_rest:etag(LastModified)}],
			Body = zj:encode(candidate(Candidate)),
			{ok, Headers, Body};
		{error, _Reason} ->
			{error, 404}
	end;
get_candidate(_, _, _) ->
	{error, 400}.

-spec post_candidate(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Handle `POST' request on `ResourceCandidate' collection.
post_candidate(RequestBody) ->
	try
		{ok, CandidateMap} = zj:decode(RequestBody),
		case im:add_candidate(candidate(CandidateMap)) of
			{ok, #candidate{href = Href, last_modified = LM} = Candidate} ->
				Body = zj:encode(candidate(Candidate)),
				Headers = [{location, Href}, {etag, im_rest:etag(LM)}],
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec delete_candidate(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Handle `DELETE' request on a `ResourceCandidate' resource.
delete_candidate(Id) ->
	case im:del_candidate(Id) of
		ok ->
			{ok, [], []};
		{error, _Reason} ->
			{error, 400}
	end.

-spec candidate(ResourceCandidate) -> ResourceCandidate
	when
		ResourceCandidate :: candidate() | map().
%% @doc CODEC for `ResourceCandidate'.
candidate(#candidate{} = ResourceCandidate) ->
	candidate(record_info(fields, candidate), ResourceCandidate, #{});
candidate(#{} = ResourceCandidate) ->
	candidate(record_info(fields, candidate), ResourceCandidate, #candidate{}).
%% @hidden
candidate([id | T], #candidate{id = Id} = R, Acc)
		when is_list(Id) ->
	candidate(T, R, Acc#{"id" => Id});
candidate([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	candidate(T, M, Acc#candidate{id = Id});
candidate([href | T], #candidate{href = Href} = R, Acc)
		when is_list(Href) ->
	candidate(T, R, Acc#{"href" => Href});
candidate([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	candidate(T, M, Acc#candidate{href = Href});
candidate([name | T], #candidate{name = Name} = R, Acc)
		when is_list(Name) ->
	candidate(T, R, Acc#{"name" => Name});
candidate([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	candidate(T, M, Acc#candidate{name = Name});
candidate([description| T],
		#candidate{description = Description} = R, Acc)
		when is_list(Description) ->
	candidate(T, R, Acc#{"description" => Description});
candidate([description| T], #{"description" := Description} = M, Acc)
		when is_list(Description) ->
	candidate(T, M, Acc#candidate{description = Description});
candidate([class_type | T], #candidate{class_type = Type} = R, Acc)
		when is_list(Type) ->
	candidate(T, R, Acc#{"@type" => Type});
candidate([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	candidate(T, M, Acc#candidate{class_type = Type});
candidate([base_type | T], #candidate{base_type = Type} = R, Acc)
		when is_list(Type) ->
	candidate(T, R, Acc#{"@baseType" => Type});
candidate([base_type | T], #{"@baseType" := Type} = M, Acc)
		when is_list(Type) ->
	candidate(T, M, Acc#candidate{base_type = Type});
candidate([schema | T], #candidate{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	candidate(T, R, Acc#{"@schemaLocation" => Schema});
candidate([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	candidate(T, M, Acc#candidate{schema = Schema});
candidate([version | T], #candidate{version = Version} = R, Acc)
		when is_list(Version) ->
	candidate(T, R, Acc#{"version" => Version});
candidate([version | T], #{"version" := Version} = M, Acc)
		when is_list(Version) ->
	candidate(T, M, Acc#candidate{version = Version});
candidate([start_date | T], #candidate{start_date = StartDate} = R, Acc)
		when is_integer(StartDate) ->
	ValidFor = #{"startDateTime" => im_rest:iso8601(StartDate)},
	candidate(T, R, Acc#{"validFor" => ValidFor});
candidate([start_date | T],
		#{"validFor" := #{"startDateTime" := Start}} = M, Acc)
		when is_list(Start) ->
	candidate(T, M, Acc#candidate{start_date = im_rest:iso8601(Start)});
candidate([end_date | T], #candidate{end_date = End} = R,
		#{"validFor" := ValidFor} = Acc) when is_integer(End) ->
	NewValidFor = ValidFor#{"endDateTime" => im_rest:iso8601(End)},
	candidate(T, R, Acc#{"validFor" := NewValidFor});
candidate([end_date | T], #candidate{end_date = End} = R, Acc)
		when is_integer(End) ->
	ValidFor = #{"endDateTime" => im_rest:iso8601(End)},
	candidate(T, R, Acc#{"validFor" := ValidFor});
candidate([end_date | T],
		#{"validFor" := #{"endDateTime" := End}} = M, Acc)
		when is_list(End) ->
	candidate(T, M, Acc#candidate{end_date = im_rest:iso8601(End)});
candidate([last_modified | T], #candidate{last_modified = {TS, _}} = R, Acc)
		when is_integer(TS) ->
	candidate(T, R, Acc#{"lastUpdate" => im_rest:iso8601(TS)});
candidate([last_modified | T], #{"lastUpdate" := DateTime} = M, Acc)
		when is_list(DateTime) ->
	LM = {im_rest:iso8601(DateTime), erlang:unique_integer([positive])},
	candidate(T, M, Acc#candidate{last_modified = LM});
candidate([status | T], #candidate{status = Status} = R, Acc)
		when Status /= undefined ->
	candidate(T, R, Acc#{"lifecycleStatus" => im_rest:lifecycle_status(Status)});
candidate([status | T], #{"lifecycleStatus" := Status} = M, Acc)
		when is_list(Status) ->
	candidate(T, M, Acc#candidate{status = im_rest:lifecycle_status(Status)});
candidate([category | T], #candidate{category = CatRefs} = R, Acc)
		when is_list(CatRefs), length(CatRefs) > 0 ->
	candidate(T, R, Acc#{"category" => im_rest:category_ref(CatRefs)});
candidate([category | T], #{"category" := CatRefs} = M, Acc)
		when is_list(CatRefs), length(CatRefs) > 0 ->
	candidate(T, M, Acc#candidate{category = im_rest:category_ref(CatRefs)});
candidate([specification | T], #candidate{specification = Spec} = R, Acc)
		when is_record(Spec, specification_ref) ->
	candidate(T, R, Acc#{"resourceSpecification" => im_rest:specification_ref(Spec)});
candidate([specification | T], #{"resourceSpecification" := Spec} = M, Acc)
		when is_map(Spec) ->
	candidate(T, M, Acc#candidate{specification = im_rest:specification_ref(Spec)});
candidate([_ | T], R, Acc) ->
	candidate(T, R, Acc);
candidate([], _, Acc) ->
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
						{#catalog{id = Id ++ '_', _ = '_'}, []};
					{ok, [{array, [{complex, [{"id", exact, [Id]}]}]}]} ->
						{#catalog{id = Id, _ = '_'}, []}
				end;
			false ->
				{'_', []}
		end
	of
		{MatchHead, MatchConditions} ->
			MFA = [im, query_candidate, [MatchHead, MatchConditions]],
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
			Candidates = lists:map(fun candidate/1, Events),
			Body = zj:encode(Candidates),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange}],
			{ok, Headers, Body}
	end.

