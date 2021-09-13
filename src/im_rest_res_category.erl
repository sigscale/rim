%%% im_rest_res_category.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018 - 2021 SigScale Global Inc.
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
%%% 	Handle `ResourceCategory' collection.
%%%
-module(im_rest_res_category).
-copyright('Copyright (c) 2018 - 2021 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([get_categories/3, get_category/2, post_category/1, delete_category/1,
			patch_category/4]).
-export([category/1]).

-include("im.hrl").
-define(MILLISECOND, milli_seconds).
  
-define(PathCategory, "/resourceCatalogManagement/v4/resourceCategory/").

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

-spec get_categories(Method, Query, Headers) -> Result
	when
		Method :: string(), % "GET" | "HEAD"
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET|HEAD /resourceCatalogManagement/v4/resourceCategory'
%% 	requests.
get_categories(Method, Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_categories(Method, NewQuery, Filters, Headers);
		false ->
			get_categories(Method, Query, [], Headers)
	end.
%% @hidden
get_categories(Method, Query, Filters, Headers) ->
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

-spec get_category(Id, Query) -> Result
	when
		Id :: string(),
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Handle `GET' request on a `ResourceCategory' resource.
get_category(Id, Query) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, L}, NewQuery} ->
			get_category(Id, NewQuery, string:tokens(L, ","));
		false ->
			get_category(Id, Query, [])
	end.
%% @hidden
get_category(Id, [] = _Query, _Filters) ->
	case im:get_category(Id) of
		{ok, #category{last_modified = LastModified} = Category} ->
			Headers = [{content_type, "application/json"},
					{etag, im_rest:etag(LastModified)}],
			Body = zj:encode(category(Category)),
			{ok, Headers, Body};
		{error, _Reason} ->
			{error, 404}
	end;
get_category(_, _, _) ->
	{error, 400}.

-spec patch_category(Id, Etag, ContentType, ReqBody) -> Result
	when
		Id :: string(),
		Etag :: undefined | string(),
		ContentType :: string(),
		ReqBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()} .
%% @doc Update a existing `category'.
%%
%% 	Respond to `PATCH /resourceCatalogManagement/v4/resourceCategory/{Id}' request.
%%
patch_category(Id, Etag, "application/merge-patch+json", ReqBody) ->
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
					case mnesia:read(category, Id, write) of
						[#category{last_modified = LM}]
								when EtagT /= undefined, LM /= EtagT ->
							mnesia:abort(412);
						[#category{} = Category] ->
							TS = erlang:system_time(?MILLISECOND),
							N = erlang:unique_integer([positive]),
							LM = {TS, N},
							Category1 = Category#category{last_modified = LM},
							case catch im:merge(Category1, category(Patch)) of
								#category{} = Category2 ->
									mnesia:write(category, Category2, write),
									Category2;
								_ ->
									mnesia:abort(400)
							end;
						[] ->
							mnesia:abort(404)
					end
			end,
			case mnesia:transaction(F) of
				{atomic, #category{last_modified = LM1} = NewCategory} ->
					Body = zj:encode(category(NewCategory)),
					Headers = [{content_type, "application/json"},
							{location, ?PathCategory ++ Id},
							{etag, im_rest:etag(LM1)}],
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
patch_category(_, _, "application/json", _) ->
	{error, 415}.

-spec post_category(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Handle `POST' request on `ResourceCategory' collection.
post_category(RequestBody) ->
	try
		{ok, CategoryMap} = zj:decode(RequestBody),
		case im:add_category(category(CategoryMap)) of
			{ok, #category{href = Href, last_modified = LM} = NewCategory} ->
				Body = zj:encode(category(NewCategory)),
				Headers = [{content_type, "application/json"},
						{location, Href}, {etag, im_rest:etag(LM)}],
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec delete_category(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Handle `DELETE' request on a `ResourceCategory' resource.
delete_category(Id) ->
	case im:del_category(Id) of
		ok ->
			{ok, [], []};
		{error, _Reason} ->
			{error, 400}
	end.

-spec category(Category) -> Category
	when
		Category :: category() | map().
%% @doc CODEC for `ResourceCategory'.
category(#category{} = Category) ->
	category(record_info(fields, category), Category, #{});
category(#{} = Category) ->
	category(record_info(fields, category), Category, #category{}).
%% @hidden
category([id | T], #category{id = Id} = R, Acc)
		when is_list(Id) ->
	category(T, R, Acc#{"id" => Id});
category([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	category(T, M, Acc#category{id = Id});
category([href | T], #category{href = Href} = R, Acc)
		when is_list(Href) ->
	category(T, R, Acc#{"href" => Href});
category([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	category(T, M, Acc#category{href = Href});
category([name | T], #category{name = Name} = R, Acc)
		when is_list(Name) ->
	category(T, R, Acc#{"name" => Name});
category([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	category(T, M, Acc#category{name = Name});
category([description| T],
		#category{description = Description} = R, Acc)
		when is_list(Description) ->
	category(T, R, Acc#{"description" => Description});
category([description| T], #{"description" := Description} = M, Acc)
		when is_list(Description) ->
	category(T, M, Acc#category{description = Description});
category([class_type | T], #category{class_type = Type} = R, Acc)
		when is_list(Type) ->
	category(T, R, Acc#{"@type" => Type});
category([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	category(T, M, Acc#category{class_type = Type});
category([base_type | T], #category{base_type = Type} = R, Acc)
		when is_list(Type) ->
	category(T, R, Acc#{"@baseType" => Type});
category([base_type | T], #{"@baseType" := Type} = M, Acc)
		when is_list(Type) ->
	category(T, M, Acc#category{base_type = Type});
category([schema | T], #category{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	category(T, R, Acc#{"@schemaLocation" => Schema});
category([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	category(T, M, Acc#category{schema = Schema});
category([version | T], #category{version = Version} = R, Acc)
		when is_list(Version) ->
	category(T, R, Acc#{"version" => Version});
category([version | T], #{"version" := Version} = M, Acc)
		when is_list(Version) ->
	category(T, M, Acc#category{version = Version});
category([start_date | T], #category{start_date = StartDate} = R, Acc)
		when is_integer(StartDate) ->
	ValidFor = #{"startDateTime" => im_rest:iso8601(StartDate)},
	category(T, R, Acc#{"validFor" => ValidFor});
category([start_date | T],
		#{"validFor" := #{"startDateTime" := Start}} = M, Acc)
		when is_list(Start) ->
	category(T, M, Acc#category{start_date = im_rest:iso8601(Start)});
category([end_date | T], #category{end_date = End} = R,
		#{"validFor" := ValidFor} = Acc) when is_integer(End) ->
	NewValidFor = ValidFor#{"endDateTime" => im_rest:iso8601(End)},
	category(T, R, Acc#{"validFor" := NewValidFor});
category([end_date | T], #category{end_date = End} = R, Acc)
		when is_integer(End) ->
	ValidFor = #{"endDateTime" => im_rest:iso8601(End)},
	category(T, R, Acc#{"validFor" := ValidFor});
category([end_date | T],
		#{"validFor" := #{"endDateTime" := End}} = M, Acc)
		when is_list(End) ->
	category(T, M, Acc#category{end_date = im_rest:iso8601(End)});
category([last_modified | T], #category{last_modified = {TS, _}} = R, Acc)
		when is_integer(TS) ->
	category(T, R, Acc#{"lastUpdate" => im_rest:iso8601(TS)});
category([last_modified | T], #{"lastUpdate" := DateTime} = M, Acc)
		when is_list(DateTime) ->
	LM = {im_rest:iso8601(DateTime), erlang:unique_integer([positive])},
	category(T, M, Acc#category{last_modified = LM});
category([status | T], #category{status = Status} = R, Acc)
		when Status /= undefined ->
	category(T, R, Acc#{"lifecycleStatus" => im_rest:lifecycle_status(Status)});
category([status | T], #{"lifecycleStatus" := Status} = M, Acc)
		when is_list(Status) ->
	category(T, M, Acc#category{status = im_rest:lifecycle_status(Status)});
category([parent | T], #category{parent = Parent} = R, Acc)
		when is_list(Parent) ->
	category(T, R, Acc#{"parentId" => Parent});
category([parent | T], #{"parentId" := Parent} = M, Acc)
		when is_list(Parent) ->
	category(T, M, Acc#category{parent = Parent});
category([root | T], #category{root = IsRoot} = R, Acc)
		when is_boolean(IsRoot) ->
	category(T, R, Acc#{"isRoot" => IsRoot});
category([root | T], #{"isRoot" := IsRoot} = M, Acc)
		when is_boolean(IsRoot) ->
	category(T, M, Acc#category{root = IsRoot});
category([party | T], #category{party = RP} = R, Acc)
		when is_list(RP), length(RP) > 0 ->
	category(T, R, Acc#{"relatedParty" => im_rest:party_ref(RP)});
category([party | T], #{"relatedParty" := RP} = M, Acc)
		when is_list(RP), length(RP) > 0 ->
	category(T, M, Acc#category{party = im_rest:party_ref(RP)});
category([category | T], #category{category = CatRefs} = R, Acc)
		when is_list(CatRefs), length(CatRefs) > 0 ->
	category(T, R, Acc#{"category" => im_rest:category_ref(CatRefs)});
category([category | T], #{"category" := CatRefs} = M, Acc)
		when is_list(CatRefs), length(CatRefs) > 0 ->
	category(T, M, Acc#category{category = im_rest:category_ref(CatRefs)});
category([candidate | T], #category{candidate = CanRefs} = R, Acc)
		when is_list(CanRefs), length(CanRefs) > 0 ->
	category(T, R, Acc#{"resourceCandidate" => im_rest:candidate_ref(CanRefs)});
category([candidate | T], #{"resourceCandidate" := CanRefs} = M, Acc)
		when is_list(CanRefs), length(CanRefs) > 0 ->
	category(T, M, Acc#category{candidate = im_rest:candidate_ref(CanRefs)});
category([_ | T], R, Acc) ->
	category(T, R, Acc);
category([], _, Acc) ->
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
			{_, StringF} ->
				{ok, Tokens, _} = im_rest_query_scanner:string(StringF),
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
		MFA = [im, query, [category, Sort, FilterArgs, CountOnly]],
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
			JsonObj = lists:map(fun category/1, Events),
			Body = zj:encode(JsonObj),
			Headers = [{content_type, "application/json"},
				{etag, Etag}, {accept_ranges, "items"},
				{content_range, ContentRange}],
			{ok, Headers, Body}
	end.

%% @hidden
sorts(Query) ->
	sorts(string:tokens(Query, [$,]), []).
%% @hidden
sorts(["id" | T], Acc) ->
	sorts(T, [#category.id | Acc]);
sorts(["-id" | T], Acc) ->
	sorts(T, [-#category.id | Acc]);
sorts(["href" | T], Acc) ->
	sorts(T, [#category.href | Acc]);
sorts(["-href" | T], Acc) ->
	sorts(T, [-#category.href | Acc]);
sorts(["name" | T], Acc) ->
	sorts(T, [#category.name | Acc]);
sorts(["-name" | T], Acc) ->
	sorts(T, [-#category.name | Acc]);
sorts(["description" | T], Acc) ->
	sorts(T, [#category.description | Acc]);
sorts(["-description" | T], Acc) ->
	sorts(T, [-#category.description | Acc]);
sorts(["@type" | T], Acc) ->
	sorts(T, [#category.class_type | Acc]);
sorts(["-@type" | T], Acc) ->
	sorts(T, [-#category.class_type | Acc]);
sorts(["@baseType" | T], Acc) ->
	sorts(T, [#category.base_type | Acc]);
sorts(["-@baseType" | T], Acc) ->
	sorts(T, [-#category.base_type | Acc]);
sorts(["@schemaLocation" | T], Acc) ->
	sorts(T, [#category.schema | Acc]);
sorts(["-@schemaLocation" | T], Acc) ->
	sorts(T, [-#category.schema | Acc]);
sorts(["lifecycleStatus" | T], Acc) ->
	sorts(T, [#category.status | Acc]);
sorts(["-lifecycleStatus" | T], Acc) ->
	sorts(T, [-#category.status | Acc]);
sorts(["version" | T], Acc) ->
	sorts(T, [#category.version | Acc]);
sorts(["-version" | T], Acc) ->
	sorts(T, [-#category.version | Acc]);
sorts(["lastUpdate" | T], Acc) ->
	sorts(T, [#category.last_modified | Acc]);
sorts(["-lastUpdate" | T], Acc) ->
	sorts(T, [-#category.last_modified | Acc]);
sorts(["parentId" | T], Acc) ->
	sorts(T, [#category.parent| Acc]);
sorts(["-parentId" | T], Acc) ->
	sorts(T, [-#category.parent| Acc]);
sorts(["isRoot" | T], Acc) ->
	sorts(T, [#category.root| Acc]);
sorts(["-isRoot" | T], Acc) ->
	sorts(T, [-#category.root| Acc]);
sorts([], Acc) ->
	lists:reverse(Acc).

-spec parse_filter(Query) -> Result
	when
		Query :: term(),
		Result :: ets:match_spec().
%% @doc Create `[MatchHead, MatchConditions]' from `Query'.
%% 	MatchHead = ets:match_pattern()
%%		MatchConditions = [tuple()]
%% @private
parse_filter(Query) ->
	parse_filter(Query, #category{_ = '_'}, []).
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
			{MatchHead#category{id = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#category{id = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$1', Like} | MatchConditions],
			{MatchHead#category{id = '$1'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "id", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$1', Cond, []),
	NewMatchHead = MatchHead#category{id = '$1'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "id", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#category{id = Name}, MatchConditions);
parse_filter([{exact, "id", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$1', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#category{id = '$1'}, NewMatchConditions);
parse_filter([{notexact, "id", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#category{id = '$1'},
	NewMatchConditions = [{'/=', '$1', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "id", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$1', []),
	NewMatchHead = MatchHead#category{id = '$1'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "href", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#category{href = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#category{href = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$2', Like} | MatchConditions],
			{MatchHead#category{href = '$2'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "href", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$2', Cond, []),
	NewMatchHead = MatchHead#category{href = '$2'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "href", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#category{href = Name}, MatchConditions);
parse_filter([{exact, "href", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$2', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#category{href = '$2'}, NewMatchConditions);
parse_filter([{notexact, "href", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#category{href = '$2'},
	NewMatchConditions = [{'/=', '$2', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "href", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$2', []),
	NewMatchHead = MatchHead#category{href = '$2'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "name", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#category{name = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#category{name = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$3', Like} | MatchConditions],
			{MatchHead#category{name = '$3'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "name", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$3', Cond, []),
	NewMatchHead = MatchHead#category{name = '$3'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "name", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#category{name = Name}, MatchConditions);
parse_filter([{exact, "name", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$3', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#category{name = '$3'}, NewMatchConditions);
parse_filter([{notexact, "name", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#category{name = '$3'},
	NewMatchConditions = [{'/=', '$3', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "name", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$3', []),
	NewMatchHead = MatchHead#category{name = '$3'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "description", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#category{description = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#category{description = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$4', Like} | MatchConditions],
			{MatchHead#category{description = '$4'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "description", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$4', Cond, []),
	NewMatchHead = MatchHead#category{description = '$4'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "description", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#category{description = Name}, MatchConditions);
parse_filter([{exact, "description", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$4', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#category{description = '$4'}, NewMatchConditions);
parse_filter([{notexact, "description", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#category{description = '$4'},
	NewMatchConditions = [{'/=', '$4', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "description", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$4', []),
	NewMatchHead = MatchHead#category{description = '$4'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "@type", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#category{class_type = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#category{class_type = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$5', Like} | MatchConditions],
			{MatchHead#category{class_type = '$5'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "@type", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$5', Cond, []),
	NewMatchHead = MatchHead#category{class_type = '$5'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "@type", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#category{class_type = Name}, MatchConditions);
parse_filter([{exact, "@type", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$5', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#category{class_type = '$5'}, NewMatchConditions);
parse_filter([{notexact, "@type", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#category{class_type = '$5'},
	NewMatchConditions = [{'/=', '$5', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "@type", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$5', []),
	NewMatchHead = MatchHead#category{class_type = '$5'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Study"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#category{status = in_study}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Design"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#category{status = in_design}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Test"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#category{status = in_test}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Rejected"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#category{status = rejected}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Active"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#category{status = active}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Launched"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#category{status = launched}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Retired"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#category{status = retired}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Obsolete"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#category{status = obsolete}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Study"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', in_study} | MatchConditions],
	parse_filter(T, any, MatchHead#category{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Design"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', in_design} | MatchConditions],
	parse_filter(T, any, MatchHead#category{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Test"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', in_test} | MatchConditions],
	parse_filter(T, any, MatchHead#category{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Rejected"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', rejected} | MatchConditions],
	parse_filter(T, any, MatchHead#category{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Active"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', active} | MatchConditions],
	parse_filter(T, any, MatchHead#category{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Launched"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', launched} | MatchConditions],
	parse_filter(T, any, MatchHead#category{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Retired"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', retired} | MatchConditions],
	parse_filter(T, any, MatchHead#category{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Obsolete"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', obsolete} | MatchConditions],
	parse_filter(T, any, MatchHead#category{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "In Study"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', in_study} | MatchConditions],
	parse_filter(T, Cond, MatchHead#category{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "In Design"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', in_design} | MatchConditions],
	parse_filter(T, Cond, MatchHead#category{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "In Test"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', in_test} | MatchConditions],
	parse_filter(T, Cond, MatchHead#category{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Rejected"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', rejected} | MatchConditions],
	parse_filter(T, Cond, MatchHead#category{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Active"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', active} | MatchConditions],
	parse_filter(T, Cond, MatchHead#category{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Launched"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', launched} | MatchConditions],
	parse_filter(T, Cond, MatchHead#category{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Retired"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', retired} | MatchConditions],
	parse_filter(T, Cond, MatchHead#category{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Obsolete"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', obsolete} | MatchConditions],
	parse_filter(T, Cond, MatchHead#category{status = '$12'}, NewMatchConditions);
parse_filter([{in, "lifecycleStatus", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$12', []),
	NewMatchHead = MatchHead#category{status = '$12'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "parentId", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#category{parent = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#category{parent = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$13', Like} | MatchConditions],
			{MatchHead#category{parent = '$13'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "parentId", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$13', Cond, []),
	NewMatchHead = MatchHead#category{parent = '$13'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "parentId", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#category{parent = Name}, MatchConditions);
parse_filter([{exact, "parentId", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$13', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#category{parent = '$13'}, NewMatchConditions);
parse_filter([{notexact, "parentId", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#category{parent = '$13'},
	NewMatchConditions = [{'/=', '$13', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "parentId", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$13', []),
	NewMatchHead = MatchHead#category{parent = '$13'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "isRoot", "true"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#category{root = true}, MatchConditions);
parse_filter([{exact, "isRoot", "false"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#category{root = false}, MatchConditions);
parse_filter([{exact, "isRoot", "true"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$14', true} | MatchConditions],
	parse_filter(T, any, MatchHead#category{root = '$14'}, NewMatchConditions);
parse_filter([{exact, "isRoot", "false"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$14', false} | MatchConditions],
	parse_filter(T, any, MatchHead#category{root = '$14'}, NewMatchConditions);
parse_filter([], all, MatchHead, MatchConditions) ->
	[{MatchHead, MatchConditions, ['$_']}];
parse_filter([], any, MatchHead, MatchConditions) ->
	NewMatchConditions =  list_to_tuple(['or' | MatchConditions]),
	[{MatchHead, [NewMatchConditions], ['$_']}].

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

