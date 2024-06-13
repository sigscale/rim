%%% im_rest_res_catalog.erl
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
%%% 	Handle `ResourceCatalog' collection.
%%%
-module(im_rest_res_catalog).
-copyright('Copyright (c) 2020 - 2024 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([get_catalogs/3, get_catalog/2, post_catalog/1, delete_catalog/1,
			patch_catalog/4]).
-export([catalog/1]).

-include("im.hrl").
-define(MILLISECOND, milli_seconds).

-define(PathCatalog, "/resourceCatalogManagement/v4/resourceCatalog/").

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

-spec get_catalogs(Method, Query, Headers) -> Result
	when
		Method :: string(), % "GET" | "HEAD"
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET|HEAD /resourceCatalogManagement/v4/resourceCatalog'
%% 	requests.
get_catalogs(Method, Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_catalogs(Method, NewQuery, Filters, Headers);
		false ->
			get_catalogs(Method, Query, [], Headers)
	end.
%% @hidden
get_catalogs(Method, Query, Filters, Headers) ->
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

-spec get_catalog(Id, Query) -> Result
	when
		Id :: string(),
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Handle `GET' request on a `ResourceCatalog' resource.
get_catalog(Id, Query) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, L}, NewQuery} ->
			get_catalog(Id, NewQuery, string:tokens(L, ","));
		false ->
			get_catalog(Id, Query, [])
	end.
%% @hidden
get_catalog(Id, [] = _Query, _Filters) ->
	case im:get_catalog(Id) of
		{ok, #catalog{last_modified = LastModified} = Catalog} ->
			Headers = [{content_type, "application/json"},
					{etag, im_rest:etag(LastModified)}],
			Body = zj:encode(catalog(Catalog)),
			ets:update_counter(metrics, resourceCatalogRead, 1,
					{resourceCatalogRead, 0}),
			{ok, Headers, Body};
		{error, _Reason} ->
			{error, 404}
	end;
get_catalog(_, _, _) ->
	{error, 400}.

-spec post_catalog(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Handle `POST' request on `ResourceCatalog' collection.
post_catalog(RequestBody) ->
	try
		{ok, CatalogMap} = zj:decode(RequestBody),
		case im:add_catalog(catalog(CatalogMap)) of
			{ok, #catalog{href = Href, last_modified = LM} = Catalog} ->
				Body = zj:encode(catalog(Catalog)),
				Headers = [{content_type, "application/json"},
						{location, Href}, {etag, im_rest:etag(LM)}],
				ets:update_counter(metrics, resourceCatalogCreate, 1,
						{resourceCatalogCreate, 0}),
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec patch_catalog(Id, Etag, ContentType, ReqBody) -> Result
	when
		Id :: string(),
		Etag :: undefined | string(),
		ContentType :: string(),
		ReqBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()} .
%% @doc Update a existing `catalog'.
%%
%% 	Respond to `PATCH /resourceCatalogManagement/v4/resourceCatalog/{Id}' request.
%%
patch_catalog(Id, Etag, "application/merge-patch+json", ReqBody) ->
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
					case mnesia:read(catalog, Id, write) of
						[#catalog{last_modified = LM}]
								when EtagT /= undefined, LM /= EtagT ->
							mnesia:abort(412);
						[#catalog{} = Catalog] ->
							TS = erlang:system_time(?MILLISECOND),
							N = erlang:unique_integer([positive]),
							LM = {TS, N},
							Catalog1 = Catalog#catalog{last_modified = LM},
							case catch im:merge(Catalog1, catalog(Patch)) of
								#catalog{} = Catalog2 ->
									mnesia:write(catalog, Catalog2, write),
									Catalog2;
								_ ->
									mnesia:abort(400)
							end;
						[] ->
							mnesia:abort(404)
					end
			end,
			case mnesia:transaction(F) of
				{atomic, #catalog{last_modified = LM1} = NewCatalog} ->
					Body = zj:encode(catalog(NewCatalog)),
					Headers = [{content_type, "application/json"},
							{location, ?PathCatalog ++ Id},
							{etag, im_rest:etag(LM1)}],
					ets:update_counter(metrics, resourceCatalogChange, 1,
							{resourceCatalogChange, 0}),
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
patch_catalog(_, _, "application/json", _) ->
	{error, 415}.

-spec delete_catalog(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Handle `DELETE' request on a `ResourceCatalog' resource.
delete_catalog(Id) ->
	case im:del_catalog(Id) of
		ok ->
			ets:update_counter(metrics, resourceCatalogDelete, 1,
					{resourceCatalogDelete, 0}),
			{ok, [], []};
		{error, _Reason} ->
			{error, 400}
	end.

-spec catalog(Catalog) -> Catalog
	when
		Catalog :: catalog() | map().
%% @doc CODEC for `ResourceCatalog'.
catalog(#catalog{} = Catalog) ->
	catalog(record_info(fields, catalog), Catalog, #{});
catalog(#{} = Catalog) ->
	catalog(record_info(fields, catalog), Catalog, #catalog{}).
%% @hidden
catalog([id | T], #catalog{id = Id} = R, Acc)
		when is_list(Id) ->
	catalog(T, R, Acc#{"id" => Id});
catalog([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	catalog(T, M, Acc#catalog{id = Id});
catalog([href | T], #catalog{href = Href} = R, Acc)
		when is_list(Href) ->
	catalog(T, R, Acc#{"href" => Href});
catalog([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	catalog(T, M, Acc#catalog{href = Href});
catalog([name | T], #catalog{name = Name} = R, Acc)
		when is_list(Name) ->
	catalog(T, R, Acc#{"name" => Name});
catalog([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	catalog(T, M, Acc#catalog{name = Name});
catalog([description| T],
		#catalog{description = Description} = R, Acc)
		when is_list(Description) ->
	catalog(T, R, Acc#{"description" => Description});
catalog([description| T], #{"description" := Description} = M, Acc)
		when is_list(Description) ->
	catalog(T, M, Acc#catalog{description = Description});
catalog([class_type | T], #catalog{class_type = Type} = R, Acc)
		when is_list(Type) ->
	catalog(T, R, Acc#{"@type" => Type});
catalog([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	catalog(T, M, Acc#catalog{class_type = Type});
catalog([base_type | T], #catalog{base_type = Type} = R, Acc)
		when is_list(Type) ->
	catalog(T, R, Acc#{"@baseType" => Type});
catalog([base_type | T], #{"@baseType" := Type} = M, Acc)
		when is_list(Type) ->
	catalog(T, M, Acc#catalog{base_type = Type});
catalog([schema | T], #catalog{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	catalog(T, R, Acc#{"@schemaLocation" => Schema});
catalog([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	catalog(T, M, Acc#catalog{schema = Schema});
catalog([version | T], #catalog{version = Version} = R, Acc)
		when is_list(Version) ->
	catalog(T, R, Acc#{"version" => Version});
catalog([version | T], #{"version" := Version} = M, Acc)
		when is_list(Version) ->
	catalog(T, M, Acc#catalog{version = Version});
catalog([start_date | T], #catalog{start_date = StartDate} = R, Acc)
		when is_integer(StartDate) ->
	ValidFor = #{"startDateTime" => im_rest:iso8601(StartDate)},
	catalog(T, R, Acc#{"validFor" => ValidFor});
catalog([start_date | T],
		#{"validFor" := #{"startDateTime" := Start}} = M, Acc)
		when is_list(Start) ->
	catalog(T, M, Acc#catalog{start_date = im_rest:iso8601(Start)});
catalog([end_date | T], #catalog{end_date = End} = R,
		#{"validFor" := ValidFor} = Acc) when is_integer(End) ->
	NewValidFor = ValidFor#{"endDateTime" => im_rest:iso8601(End)},
	catalog(T, R, Acc#{"validFor" := NewValidFor});
catalog([end_date | T], #catalog{end_date = End} = R, Acc)
		when is_integer(End) ->
	ValidFor = #{"endDateTime" => im_rest:iso8601(End)},
	catalog(T, R, Acc#{"validFor" := ValidFor});
catalog([end_date | T],
		#{"validFor" := #{"endDateTime" := End}} = M, Acc)
		when is_list(End) ->
	catalog(T, M, Acc#catalog{end_date = im_rest:iso8601(End)});
catalog([last_modified | T], #catalog{last_modified = {TS, _}} = R, Acc)
		when is_integer(TS) ->
	catalog(T, R, Acc#{"lastUpdate" => im_rest:iso8601(TS)});
catalog([last_modified | T], #{"lastUpdate" := DateTime} = M, Acc)
		when is_list(DateTime) ->
	LM = {im_rest:iso8601(DateTime), erlang:unique_integer([positive])},
	catalog(T, M, Acc#catalog{last_modified = LM});
catalog([status | T], #catalog{status = Status} = R, Acc)
		when Status /= undefined ->
	catalog(T, R, Acc#{"lifecycleStatus" => im_rest:lifecycle_status(Status)});
catalog([status | T], #{"lifecycleStatus" := Status} = M, Acc)
		when is_list(Status) ->
	catalog(T, M, Acc#catalog{status = im_rest:lifecycle_status(Status)});
catalog([party | T], #catalog{party = RP} = R, Acc)
		when is_list(RP), length(RP) > 0 ->
	catalog(T, R, Acc#{"relatedParty" => im_rest:party_ref(RP)});
catalog([party | T], #{"relatedParty" := RP} = M, Acc)
		when is_list(RP) ->
	catalog(T, M, Acc#catalog{party = im_rest:party_ref(RP)});
catalog([category | T], #catalog{category = Category} = R, Acc)
		when is_list(Category), length(Category) > 0 ->
	catalog(T, R, Acc#{"category" => im_rest:category_ref(Category)});
catalog([category | T], #{"category" := Category} = M, Acc)
		when is_list(Category) ->
	catalog(T, M, Acc#catalog{category = im_rest:category_ref(Category)});
catalog([_ | T], R, Acc) ->
	catalog(T, R, Acc);
catalog([], _, Acc) ->
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
		MFA = [im, query, [catalog, Sort, FilterArgs, CountOnly]],
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
			Catalogs = lists:map(fun catalog/1, Events),
			Body = zj:encode(Catalogs),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange}],
			ets:update_counter(metrics, resourceCatalogRead, 1,
					{resourceCatalogRead, 0}),
			{ok, Headers, Body}
	end.

%% @hidden
sorts(Query) ->
	sorts(string:tokens(Query, [$,]), []).
%% @hidden
sorts(["id" | T], Acc) ->
	sorts(T, [#catalog.id | Acc]);
sorts(["-id" | T], Acc) ->
	sorts(T, [-#catalog.id | Acc]);
sorts(["href" | T], Acc) ->
	sorts(T, [#catalog.href | Acc]);
sorts(["-href" | T], Acc) ->
	sorts(T, [-#catalog.href | Acc]);
sorts(["name" | T], Acc) ->
	sorts(T, [#catalog.name | Acc]);
sorts(["-name" | T], Acc) ->
	sorts(T, [-#catalog.name | Acc]);
sorts(["description" | T], Acc) ->
	sorts(T, [#catalog.description | Acc]);
sorts(["-description" | T], Acc) ->
	sorts(T, [-#catalog.description | Acc]);
sorts(["@type" | T], Acc) ->
	sorts(T, [#catalog.class_type | Acc]);
sorts(["-@type" | T], Acc) ->
	sorts(T, [-#catalog.class_type | Acc]);
sorts(["@baseType" | T], Acc) ->
	sorts(T, [#catalog.base_type | Acc]);
sorts(["-@baseType" | T], Acc) ->
	sorts(T, [-#catalog.base_type | Acc]);
sorts(["@schemaLocation" | T], Acc) ->
	sorts(T, [#catalog.schema | Acc]);
sorts(["-@schemaLocation" | T], Acc) ->
	sorts(T, [-#catalog.schema | Acc]);
sorts(["lifecycleStatus" | T], Acc) ->
	sorts(T, [#catalog.status | Acc]);
sorts(["-lifecycleStatus" | T], Acc) ->
	sorts(T, [-#catalog.status | Acc]);
sorts(["version" | T], Acc) ->
	sorts(T, [#catalog.version | Acc]);
sorts(["-version" | T], Acc) ->
	sorts(T, [-#catalog.version | Acc]);
sorts(["lastUpdate" | T], Acc) ->
	sorts(T, [#catalog.last_modified | Acc]);
sorts(["-lastUpdate" | T], Acc) ->
	sorts(T, [-#catalog.last_modified | Acc]);
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
	parse_filter(Query, #catalog{_ = '_'}, []).
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
			{MatchHead#catalog{id = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#catalog{id = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$1', Like} | MatchConditions],
			{MatchHead#catalog{id = '$1'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "id", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$1', Cond, []),
	NewMatchHead = MatchHead#catalog{id = '$1'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "id", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#catalog{id = Name}, MatchConditions);
parse_filter([{exact, "id", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$1', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#catalog{id = '$1'}, NewMatchConditions);
parse_filter([{notexact, "id", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#catalog{id = '$1'},
	NewMatchConditions = [{'/=', '$1', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "id", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$1', []),
	NewMatchHead = MatchHead#catalog{id = '$1'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "href", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#catalog{href = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#catalog{href = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$2', Like} | MatchConditions],
			{MatchHead#catalog{href = '$2'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "href", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$2', Cond, []),
	NewMatchHead = MatchHead#catalog{href = '$2'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "href", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#catalog{href = Name}, MatchConditions);
parse_filter([{exact, "href", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$2', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#catalog{href = '$2'}, NewMatchConditions);
parse_filter([{notexact, "href", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#catalog{href = '$2'},
	NewMatchConditions = [{'/=', '$2', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "href", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$2', []),
	NewMatchHead = MatchHead#catalog{href = '$2'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "name", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#catalog{name = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#catalog{name = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$3', Like} | MatchConditions],
			{MatchHead#catalog{name = '$3'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "name", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$3', Cond, []),
	NewMatchHead = MatchHead#catalog{name = '$3'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "name", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#catalog{name = Name}, MatchConditions);
parse_filter([{exact, "name", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$3', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#catalog{name = '$3'}, NewMatchConditions);
parse_filter([{notexact, "name", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#catalog{name = '$3'},
	NewMatchConditions = [{'/=', '$3', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "name", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$3', []),
	NewMatchHead = MatchHead#catalog{name = '$3'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "description", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#catalog{description = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#catalog{description = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$4', Like} | MatchConditions],
			{MatchHead#catalog{description = '$4'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "description", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$4', Cond, []),
	NewMatchHead = MatchHead#catalog{description = '$4'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "description", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#catalog{description = Name}, MatchConditions);
parse_filter([{exact, "description", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$4', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#catalog{description = '$4'}, NewMatchConditions);
parse_filter([{notexact, "description", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#catalog{description = '$4'},
	NewMatchConditions = [{'/=', '$4', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "description", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$4', []),
	NewMatchHead = MatchHead#catalog{description = '$4'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "@type", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#catalog{class_type = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#catalog{class_type = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$5', Like} | MatchConditions],
			{MatchHead#catalog{class_type = '$5'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "@type", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$5', Cond, []),
	NewMatchHead = MatchHead#catalog{class_type = '$5'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "@type", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#catalog{class_type = Name}, MatchConditions);
parse_filter([{exact, "@type", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$5', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#catalog{class_type = '$5'}, NewMatchConditions);
parse_filter([{notexact, "@type", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#catalog{class_type = '$5'},
	NewMatchConditions = [{'/=', '$5', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "@type", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$5', []),
	NewMatchHead = MatchHead#catalog{class_type = '$5'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Study"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#catalog{status = in_study}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Design"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#catalog{status = in_design}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Test"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#catalog{status = in_test}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Rejected"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#catalog{status = rejected}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Active"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#catalog{status = active}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Launched"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#catalog{status = launched}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Retired"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#catalog{status = retired}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Obsolete"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#catalog{status = obsolete}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Study"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', in_study} | MatchConditions],
	parse_filter(T, any, MatchHead#catalog{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Design"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', in_design} | MatchConditions],
	parse_filter(T, any, MatchHead#catalog{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Test"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', in_test} | MatchConditions],
	parse_filter(T, any, MatchHead#catalog{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Rejected"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', rejected} | MatchConditions],
	parse_filter(T, any, MatchHead#catalog{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Active"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', active} | MatchConditions],
	parse_filter(T, any, MatchHead#catalog{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Launched"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', launched} | MatchConditions],
	parse_filter(T, any, MatchHead#catalog{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Retired"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', retired} | MatchConditions],
	parse_filter(T, any, MatchHead#catalog{status = '$12'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Obsolete"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$12', obsolete} | MatchConditions],
	parse_filter(T, any, MatchHead#catalog{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "In Study"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', in_study} | MatchConditions],
	parse_filter(T, Cond, MatchHead#catalog{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "In Design"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', in_design} | MatchConditions],
	parse_filter(T, Cond, MatchHead#catalog{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "In Test"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', in_test} | MatchConditions],
	parse_filter(T, Cond, MatchHead#catalog{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Rejected"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', rejected} | MatchConditions],
	parse_filter(T, Cond, MatchHead#catalog{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Active"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', active} | MatchConditions],
	parse_filter(T, Cond, MatchHead#catalog{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Launched"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', launched} | MatchConditions],
	parse_filter(T, Cond, MatchHead#catalog{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Retired"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', retired} | MatchConditions],
	parse_filter(T, Cond, MatchHead#catalog{status = '$12'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Obsolete"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$12', obsolete} | MatchConditions],
	parse_filter(T, Cond, MatchHead#catalog{status = '$12'}, NewMatchConditions);
parse_filter([{in, "lifecycleStatus", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$12', []),
	NewMatchHead = MatchHead#catalog{status = '$12'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
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
