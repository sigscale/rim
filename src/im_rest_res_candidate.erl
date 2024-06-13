%%% im_rest_res_candidate.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2020 - 2024 SigScale Global Inc.
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
-copyright('Copyright (c) 2020 - 2024 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([get_candidates/3, get_candidate/2, post_candidate/1,
		delete_candidate/1, patch_candidate/4]).
-export([candidate/1]).

-include("im.hrl").
-define(MILLISECOND, milli_seconds).

-define(PathCandidate, "/resourceCatalogManagement/v4/resourceCandidate/").

%%----------------------------------------------------------------------
%%  The im public API
%%----------------------------------------------------------------------

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/json-patch+json",
	"application/merge-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations available.
content_types_provided() ->
	["application/json", "application/problem+json"].

-spec get_candidates(Method, Query, Headers) -> Result
	when
		Method :: string(), % "GET" | "HEAD"
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET|HEAD /resourceCatalogManagement/v4/resourceCandidate'
%% 	requests.
get_candidates(Method, Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_candidates(Method, NewQuery, Filters, Headers);
		false ->
			get_candidates(Method, Query, [], Headers)
	end.
%% @hidden
get_candidates(Method, Query, Filters, Headers) ->
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
							query_start(Method, Query, Filters, Start, End)
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
		{false, false, {"range", "items=1-" ++ _ = Range}} ->
			case im_rest:range(Range) of
				{error, _} ->
					{error, 400};
				{ok, {Start, End}} ->
					query_start(Method, Query, Filters, Start, End)
			end;
		{false, false, {"range", _Range}} ->
			{error, 416};
		{false, false, false} ->
			query_start(Method, Query, Filters, undefined, undefined)
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
			ets:update_counter(metrics, resourceCandidateRead, 1,
					{resourceCandidateRead, 0}),
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
				Headers = [{content_type, "application/json"},
						{location, Href}, {etag, im_rest:etag(LM)}],
				ets:update_counter(metrics, resourceCandidateCreate, 1,
						{resourceCandidateCreate, 0}),
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec patch_candidate(Id, Etag, ContentType, ReqBody) -> Result
	when
		Id :: string(),
		Etag :: undefined | string(),
		ContentType :: string(),
		ReqBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()} .
%% @doc Update a existing `resourceCandidate'.
%%
%% 	Respond to `PATCH /resourceCatalogManagement/v4/resourceCandidate/{Id}' request.
%%
patch_candidate(Id, Etag, "application/merge-patch+json", ReqBody) ->
	try
		case Etag of
			undefined ->
				{undefined, zj:decode(ReqBody)};
			Etag ->
				{im_rest:etag(Etag) , zj:decode(ReqBody)}
		end
	of
		{EtagT, {ok, Patch}} ->
			F = fun() ->
					case mnesia:read(candidate, Id, write) of
						[#candidate{last_modified = LM}]
								when EtagT /= undefined, LM /= EtagT ->
							mnesia:abort(412);
						[#candidate{} = Candidate] ->
							TS = erlang:system_time(?MILLISECOND),
							N = erlang:unique_integer([positive]),
							LM = {TS, N},
							Candidate1 = Candidate#candidate{last_modified = LM},
							case catch im:merge(Candidate1, candidate(Patch)) of
								#candidate{} = Candidate2 ->
									mnesia:write(candidate, Candidate2, write),
									Candidate2;
								_ ->
									mnesia:abort(400)
							end;
						[] ->
							mnesia:abort(404)
					end
			end,
			case mnesia:transaction(F) of
				{atomic, #candidate{last_modified = LM1} = NewCandidate} ->
					Body = zj:encode(candidate(NewCandidate)),
					Headers = [{content_type, "application/json"},
							{location, ?PathCandidate ++ Id},
							{etag, im_rest:etag(LM1)}],
					ets:update_counter(metrics, resourceCandidateChange, 1,
							{resourceCandidateChange, 0}),
					{ok, Headers, Body};
				{aborted, Status} when is_integer(Status) ->
					{error, Status};
				{aborted, _Reason} ->
					{error, 500}
			end;
		_ ->
			{error, 400}
	catch
		_:_ ->
			{error, 400}
	end;
patch_candidate(_, _, "application/json", _) ->
	{error, 415}.

-spec delete_candidate(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Handle `DELETE' request on a `ResourceCandidate' resource.
delete_candidate(Id) ->
	case im:del_candidate(Id) of
		ok ->
			ets:update_counter(metrics, resourceCandidateDelete, 1,
					{resourceCandidateDelete, 0}),
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
		when Status /= undefined->
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
match([{Key, Value} | T], Acc) ->
	match(T, [{exact, Key, Value} | Acc]);
match([], Acc) ->
	Acc.

%% @hidden
query_start(Method, Query, Filters, RangeStart, RangeEnd) ->
	try
		CountOnly = case Method of
			"GET" ->
				false;
			"HEAD" ->
				true
		end,
		FilterArgs = case lists:keyfind("filter", 1, Query) of
			{_, String} ->
				{ok, Tokens, _} = im_rest_query_scanner:string(String),
				{ok, Filter} = im_rest_query_parser:parse(Tokens),
				parse_filter(Filter);
			false when length(Query) > 0 ->
				Rest = match(Query, []),
				Rest1 = [{array,[{complex, Rest}]}],
				parse_filter(Rest1);
			false ->
				'_'
		end,
		Sort = case lists:keyfind("sort", 1, Query) of
			{_, StringS} ->
				sorts(StringS);
			false ->
				[]
		end,
		MFA = [im, query, [candidate, Sort, FilterArgs, CountOnly]],
		case supervisor:start_child(im_rest_pagination_sup, [MFA]) of
			{ok, PageServer, Etag} ->
				query_page(PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
			{error, _Reason} ->
				{error, 500}
		end
	catch
		_:_ ->
			{error, 400}
	end.

%% @hidden
query_page(PageServer, Etag, _Query, _Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}, infinity) of
		{error, Status} ->
			{error, Status};
		{undefined, ContentRange} ->
			Headers = [{content_type, "application/json"},
				{etag, Etag}, {accept_ranges, "items"},
				{content_range, ContentRange}],
			{ok, Headers, []};
		{Events, ContentRange} ->
			Candidates = lists:map(fun candidate/1, Events),
			Body = zj:encode(Candidates),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange}],
			ets:update_counter(metrics, resourceCandidateRead, 1,
					{resourceCandidateRead, 0}),
			{ok, Headers, Body}
	end.

-spec parse_filter(Query) -> Result
	when
		Query :: term(),
		Result :: ets:match_spec().
%% @doc Create `[MatchHead, MatchConditions]' from `Query'.
%% 	MatchHead = ets:match_pattern()
%%		MatchConditions = [tuple()]
%% @private
parse_filter(Query) ->
	parse_filter(Query, #candidate{_ = '_'}, []).
%% @hidden
parse_filter([{array, [{complex, {all, Filters}}]}], MatchHead, MatchConditions) ->
	parse_filter(Filters, all, MatchHead, MatchConditions);
parse_filter([{array, [{complex, {any, Filters}}]}], MatchHead, MatchConditions) ->
	parse_filter(Filters, any, MatchHead, MatchConditions);
parse_filter([{array, [{complex, [{in, Filter}]}]}], MatchHead, MatchConditions) ->
	parse_filter(Filter, all, MatchHead, MatchConditions);
parse_filter([{array, [{complex, Filter}]}], MatchHead, MatchConditions) ->
	parse_filter(Filter, all, MatchHead, MatchConditions);
parse_filter([], MatchHead, MatchConditions) ->
	[{MatchHead, MatchConditions, ['$_']}].

%% @hidden
parse_filter([{like, "id", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#candidate{id = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#candidate{id = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$1', Like} | MatchConditions],
			{MatchHead#candidate{id = '$1'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "id", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$1', Cond, []),
	NewMatchHead = MatchHead#candidate{id = '$1'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "id", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#candidate{id = Name}, MatchConditions);
parse_filter([{exact, "id", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$1', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#candidate{id = '$1'}, NewMatchConditions);
parse_filter([{notexact, "id", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#candidate{id = '$1'},
	NewMatchConditions = [{'/=', '$1', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "id", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$1', []),
	NewMatchHead = MatchHead#candidate{id = '$1'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "href", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#candidate{href = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#candidate{href = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$2', Like} | MatchConditions],
			{MatchHead#candidate{href = '$2'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "href", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$2', Cond, []),
	NewMatchHead = MatchHead#candidate{href = '$2'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "href", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#candidate{href = Name}, MatchConditions);
parse_filter([{exact, "href", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$2', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#candidate{href = '$2'}, NewMatchConditions);
parse_filter([{notexact, "href", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#candidate{href = '$2'},
	NewMatchConditions = [{'/=', '$2', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "href", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$2', []),
	NewMatchHead = MatchHead#candidate{href = '$2'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "name", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#candidate{name = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#candidate{name = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$3', Like} | MatchConditions],
			{MatchHead#candidate{name = '$3'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "name", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$3', Cond, []),
	NewMatchHead = MatchHead#candidate{name = '$3'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "name", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#candidate{name = Name}, MatchConditions);
parse_filter([{exact, "name", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$3', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#candidate{name = '$3'}, NewMatchConditions);
parse_filter([{notexact, "name", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#candidate{name = '$3'},
	NewMatchConditions = [{'/=', '$3', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "name", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$3', []),
	NewMatchHead = MatchHead#candidate{name = '$3'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "description", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#candidate{description = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#candidate{description = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$4', Like} | MatchConditions],
			{MatchHead#candidate{description = '$4'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "description", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$4', Cond, []),
	NewMatchHead = MatchHead#candidate{description = '$4'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "description", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#candidate{description = Name}, MatchConditions);
parse_filter([{exact, "description", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$4', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#candidate{description = '$4'}, NewMatchConditions);
parse_filter([{notexact, "description", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#candidate{description = '$4'},
	NewMatchConditions = [{'/=', '$4', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "description", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$4', []),
	NewMatchHead = MatchHead#candidate{description = '$4'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "@type", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#candidate{class_type = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#candidate{class_type = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$5', Like} | MatchConditions],
			{MatchHead#candidate{class_type = '$5'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "@type", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$5', Cond, []),
	NewMatchHead = MatchHead#candidate{class_type = '$5'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "@type", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#candidate{class_type = Name}, MatchConditions);
parse_filter([{exact, "@type", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$5', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#candidate{class_type = '$5'}, NewMatchConditions);
parse_filter([{notexact, "@type", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#candidate{class_type = '$5'},
	NewMatchConditions = [{'/=', '$5', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "@type", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$5', []),
	NewMatchHead = MatchHead#candidate{class_type = '$5'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Study"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#candidate{status = in_study}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Design"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#candidate{status = in_design}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Test"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#candidate{status = in_test}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Rejected"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#candidate{status = rejected}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Active"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#candidate{status = active}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Launched"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#candidate{status = launched}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Retired"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#candidate{status = retired}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Obsolete"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#candidate{status = obsolete}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Study"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', in_study} | MatchConditions],
	parse_filter(T, any, MatchHead#candidate{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Design"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', in_design} | MatchConditions],
	parse_filter(T, any, MatchHead#candidate{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Test"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', in_test} | MatchConditions],
	parse_filter(T, any, MatchHead#candidate{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Rejected"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', rejected} | MatchConditions],
	parse_filter(T, any, MatchHead#candidate{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Active"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', active} | MatchConditions],
	parse_filter(T, any, MatchHead#candidate{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Launched"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', launched} | MatchConditions],
	parse_filter(T, any, MatchHead#candidate{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Retired"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', retired} | MatchConditions],
	parse_filter(T, any, MatchHead#candidate{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Obsolete"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', obsolete} | MatchConditions],
	parse_filter(T, any, MatchHead#candidate{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "In Study"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', in_study} | MatchConditions],
	parse_filter(T, Cond, MatchHead#candidate{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "In Design"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', in_design} | MatchConditions],
	parse_filter(T, Cond, MatchHead#candidate{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "In Test"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', in_test} | MatchConditions],
	parse_filter(T, Cond, MatchHead#candidate{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Rejected"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', rejected} | MatchConditions],
	parse_filter(T, Cond, MatchHead#candidate{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Active"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', active} | MatchConditions],
	parse_filter(T, Cond, MatchHead#candidate{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Launched"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', launched} | MatchConditions],
	parse_filter(T, Cond, MatchHead#candidate{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Retired"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', retired} | MatchConditions],
	parse_filter(T, Cond, MatchHead#candidate{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Obsolete"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', obsolete} | MatchConditions],
	parse_filter(T, Cond, MatchHead#candidate{status = '$12'}, NewMatchConditions);
parse_filter([{in, "lifecycleStatus", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$12', []),
	NewMatchHead = MatchHead#candidate{status = '$12'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([], all, MatchHead, MatchConditions) ->
	[{MatchHead, MatchConditions, ['$_']}];
parse_filter([], any, MatchHead, MatchConditions) ->
	NewMatchConditions =  list_to_tuple(['or' | MatchConditions]),
	[{MatchHead, [NewMatchConditions], ['$_']}].

%% @hidden
sorts(Query) ->
	sorts(string:tokens(Query, [$,]), []).
%% @hidden
sorts(["id" | T], Acc) ->
	sorts(T, [#candidate.id | Acc]);
sorts(["-id" | T], Acc) ->
	sorts(T, [-#candidate.id | Acc]);
sorts(["href" | T], Acc) ->
	sorts(T, [#candidate.href | Acc]);
sorts(["-href" | T], Acc) ->
	sorts(T, [-#candidate.href | Acc]);
sorts(["name" | T], Acc) ->
	sorts(T, [#candidate.name | Acc]);
sorts(["-name" | T], Acc) ->
	sorts(T, [-#candidate.name | Acc]);
sorts(["description" | T], Acc) ->
	sorts(T, [#candidate.description | Acc]);
sorts(["-description" | T], Acc) ->
	sorts(T, [-#candidate.description | Acc]);
sorts(["@type" | T], Acc) ->
	sorts(T, [#candidate.class_type | Acc]);
sorts(["-@type" | T], Acc) ->
	sorts(T, [-#candidate.class_type | Acc]);
sorts(["@baseType" | T], Acc) ->
	sorts(T, [#candidate.base_type | Acc]);
sorts(["-@baseType" | T], Acc) ->
	sorts(T, [-#candidate.base_type | Acc]);
sorts(["@schemaLocation" | T], Acc) ->
	sorts(T, [#candidate.schema | Acc]);
sorts(["-@schemaLocation" | T], Acc) ->
	sorts(T, [-#candidate.schema | Acc]);
sorts(["lifecycleStatus" | T], Acc) ->
	sorts(T, [#candidate.status | Acc]);
sorts(["-lifecycleStatus" | T], Acc) ->
	sorts(T, [-#candidate.status | Acc]);
sorts(["version" | T], Acc) ->
	sorts(T, [#candidate.version | Acc]);
sorts(["-version" | T], Acc) ->
	sorts(T, [-#candidate.version | Acc]);
sorts(["lastUpdate" | T], Acc) ->
	sorts(T, [#candidate.last_modified | Acc]);
sorts(["-lastUpdate" | T], Acc) ->
	sorts(T, [-#candidate.last_modified | Acc]);
sorts([], Acc) ->
	lists:reverse(Acc).

%% @hidden
like([H |T], Var, Cond, Acc) ->
	case lists:last(H) of
		$% ->
			Prefix = lists:droplast(H),
			NewMatchConditions = match_prefix(Prefix, Var),
			like(T, Var, Cond, [NewMatchConditions | Acc]);
		_ ->
			like(T, Var, Cond, [{'==', Var, H} | Acc])
	end;
like([], _, _, Acc) when length(Acc) > 1 ->
	[list_to_tuple(['or' | Acc])];
like([], _, _, Acc) ->
	Acc.

%% @hidden
match_prefix([A] = Start, Var) ->
	{'and', {'>=', Var, Start}, {'<', Var, [A + 1]}};
match_prefix(Start, Var) ->
	{A, [B]} = lists:split(length(Start) - 1, Start),
	{'and', {'>=', Var, Start}, {'<', Var, A ++ [B + 1]}}.

%% @hidden
in(["In Study" | T], '$12' = Var, Acc) ->
	in(T, Var, [{'==', Var, in_study} | Acc]);
in(["In Design" | T], '$12' = Var, Acc) ->
	in(T, Var, [{'==', Var, in_design} | Acc]);
in(["In Test" | T], '$12' = Var, Acc) ->
	in(T, Var, [{'==', Var, in_test} | Acc]);
in(["Rejected" | T], '$12' = Var, Acc) ->
	in(T, Var, [{'==', Var, rejected} | Acc]);
in(["Active" | T], '$12' = Var, Acc) ->
	in(T, Var, [{'==', Var, active} | Acc]);
in(["Launched" | T], '$12' = Var, Acc) ->
	in(T, Var, [{'==', Var, launched} | Acc]);
in(["Retired" | T], '$12' = Var, Acc) ->
	in(T, Var, [{'==', Var, retired} | Acc]);
in([H |T], Var, Acc) ->
	in(T, Var, [{'==', Var, H} | Acc]);
in([], _, Acc) when length(Acc) > 1 ->
	[list_to_tuple(['or' | Acc])];
in([], _, Acc) ->
	Acc.
