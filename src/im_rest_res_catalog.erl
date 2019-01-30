%%% im_rest_res_catalog.erl
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
%%% 	Handle `ResourceCatalog' collection.
%%%
-module(im_rest_res_catalog).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([get_catalogs/2, get_catalog/2, post_catalog/1, delete_catalog/1]).
-export([catalog/1]).
 
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

-spec get_catalogs(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Handle `GET' request on `ResourceCatalog' collection.
get_catalogs(Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_catalogs(NewQuery, Filters, Headers);
		false ->
			get_catalogs(Query, [], Headers)
	end.
%% @hidden
get_catalogs(Query, Filters, Headers) ->
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
		Catalog = catalog(zj:decode(RequestBody)),
		case im:add_catalog(Catalog) of
			{ok, #catalog{id = Id, last_modified = LM} = Catalog} ->
				Body = zj:encode(catalog(Catalog)),
				Location = ?PathCatalog ++ "catalog/" ++ Id,
				Headers = [{location, Location}, {etag, im_rest:etag(LM)}],
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec delete_catalog(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Handle `DELETE' request on a `ResourceCatalog' resource.
delete_catalog(Id) ->
	case im:del_catalog(Id) of
		ok ->
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
		when status /= undefined ->
	catalog(T, R, Acc#{"lifecycleStatus" => im_rest:lifecycle_status(Status)});
catalog([status | T], #{"lifecycleStatus" := Status} = M, Acc)
		when is_list(Status) ->
	catalog(T, M, Acc#catalog{status = im_rest:lifecycle_status(Status)});
catalog([related_party | T], #catalog{related_party = RP} = R, Acc)
		when is_list(RP) ->
	catalog(T, R, Acc#{"relatedParty" => im_rest:related_party_ref(RP)});
catalog([related_party | T], #{"relatedParty" := RP} = M, Acc)
		when is_list(RP) ->
	catalog(T, M, Acc#catalog{related_party = im_rest:related_party_ref(RP)});
catalog([category | T], #catalog{category = Category} = R, Acc)
		when is_list(Category) ->
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
			MFA = [im, query_catalog, [MatchId, MatchLocale]],
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
			Catalogs = lists:map(fun catalog/1, Events),
			Body = zj:encode(Catalogs),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange}],
			{ok, Headers, Body}
	end.

