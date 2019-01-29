%%% im_rest_res_category.erl
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
%%% 	Handle `ResourceCategory' collection.
%%%
-module(im_rest_res_category).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([get_categories/2, get_category/2, post_category/1, delete_category/1]).
-export([category/1]).
 
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

-spec get_categories(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Handle `GET' request on `ResourceCategory' collection.
get_categories(Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_categories(NewQuery, Filters, Headers);
		false ->
			get_categories(Query, [], Headers)
	end.
%% @hidden
get_categories(Query, Filters, Headers) ->
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

-spec post_category(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Handle `POST' request on `ResourceCategory' collection.
post_category(RequestBody) ->
	try
		Category = category(zj:decode(RequestBody)),
		case im:add_category(Category) of
			{ok, #category{id = Id, last_modified = LM} = NewCategory} ->
				Body = zj:encode(category(NewCategory)),
				Location = ?PathCatalog ++ "category/" ++ Id,
				Headers = [{location, Location}, {etag, im_rest:etag(LM)}],
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
	case im:delete_category(Id) of
		ok ->
			{ok, [], []};
		{error, _Reason} ->
			{error, 400}
	end.

-spec category(Category) -> Category
	when
		Category :: #category{} | map().
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
	category(T, R, Acc#{"@baseType" => Type});
category([class_type | T], #{"@baseType" := Type} = M, Acc)
		when is_list(Type) ->
	category(T, M, Acc#category{class_type = Type});
category([base_type | T], #category{base_type = Type} = R, Acc)
		when is_list(Type) ->
	category(T, R, Acc#{"@type" => Type});
category([base_type | T], #{"@type" := Type} = M, Acc)
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
		#{validFor := ValidFor} = Acc) when is_integer(End) ->
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
category([related_party | T], #category{related_party = RP} = R, Acc)
		when is_list(RP) ->
	category(T, R, Acc#{"relatedParty" => im_rest:related_party_ref(RP)});
category([related_party | T], #{"relatedParty" := RP} = M, Acc)
		when is_list(RP) ->
	category(T, M, Acc#category{related_party = im_rest:related_party_ref(RP)});
category([category | T], #category{category = CatRefs} = R, Acc)
		when is_list(CatRefs) ->
	category(T, R, Acc#{"category" => im_rest:category_ref(CatRefs)});
category([category | T], #{"category" := CatRefs} = M, Acc)
		when is_list(CatRefs) ->
	category(T, M, Acc#category{category = im_rest:category_ref(CatRefs)});
category([candidate | T], #category{candidate = CanRefs} = R, Acc)
		when is_list(CanRefs) ->
	category(T, R, Acc#{"resourceCandidate" => im_rest:candidate_ref(CanRefs)});
category([candidate | T], #{"resourceCandidate" := CanRefs} = M, Acc)
		when is_list(CanRefs) ->
	category(T, M, Acc#category{candidate = im_rest:candidate_ref(CanRefs)});
category([_ | T], R, Acc) ->
	category(T, R, Acc);
category([], _, Acc) ->
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
			MFA = [im, query_category, [MatchId, MatchLocale]],
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
			Categories = lists:map(fun category/1, Events),
			Body = zj:encode(Categories),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange}],
			{ok, Headers, Body}
	end.

