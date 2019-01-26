%%% im_rest_res_inventory.erl
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
-module(im_rest_res_inventory).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([get_resource/2, get_inventory/2, post_resource/1, delete_resource/1]).
-export([resource/1]).
 
-include("im.hrl").

-define(PathCatalog, "/resourceCatalogManagement/v3/").
-define(PathInventory, "/resourceInventoryManagement/v3/").
-define(PathFunction, "/resourceFunctionActivationConfiguration/v2/").

%%----------------------------------------------------------------------
%%  The im public API
%%----------------------------------------------------------------------

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/json-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec get_inventory(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /resourceInventoryManagement/v3/resource'
%% requests.
get_inventory(Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_inventory1(NewQuery, Filters, Headers);
		false ->
			get_inventory1(Query, [], Headers)
	end.
%% @hidden
get_inventory1(Query, Filters, Headers) ->
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

-spec get_resource(Id, Query) -> Result
	when
		Id :: string(),
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /resourceInventoryManagement/v3/resource/{id}'
%% requests.
get_resource(Id, Query) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, L}, NewQuery} ->
			get_resource(Id, NewQuery, string:tokens(L, ","));
		false ->
			get_resource(Id, Query, [])
	end.
%% @hidden
get_resource(Id, [] = _Query, _Filters) ->
	case im:get_resource(Id) of
		{ok, #resource{} = ResourceRec} ->
			ResourceMap = resource(ResourceRec),
			Chars = maps:get("characteristic", ResourceMap),
			Headers1 = case lists:keyfind(last_modified, 1, ResourceData) of
				{_, LastModified} ->
					[{etag, im_rest:etag(LastModified)}];
				false ->
					[]
			end,
			Headers2 = [{content_type, "application/json"} | Headers1],
			Body = zj:encode(NewResource),
			{ok, Headers2, Body};
		{error, _Reason} ->
			{error, 404}
	end;
get_resource(_, _, _) ->
	{error, 400}.

-spec post_resource(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Respond to `POST /resourceInventoryManagement/v3/resource'
%% requests.
post_resource(RequestBody) ->
	try
		Resource = resource(zj:decode(RequestBody)),
		case im:add_resource(Resource) of
			{ok, LastModified} ->
				Body = zj:encode(resource(Resource)),
				Location = "/resourceInventoryManagement/v3/resource/" ++ Id,
				Headers = [{location, Location}, {etag, im_rest:etag(LastModified)}],
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec delete_resource(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Respond to `DELETE /resourceInventoryManagement/v3/resource/{id}'
%% 	request and deletes a resource.
delete_resource(Id) ->
	case im:delete_resource(Id) of
		ok ->
			{ok, [], []};
		{error, _Reason} ->
			{error, 400}
	end.

-spec resource(Resource) -> Resource
	when
		Resource :: #httpd_resource{} | map().
%% @doc CODEC for `Resource' object.
resource(#resource{id = ID, characteristic = Characteristic})
		when is_list(ID), is_list(Characteristic) ->
	#{"id" => ID,
			"href" => "?PathInventory" ++ "resource/"++ ID,
			"characteristic" => Characteristic};
resource(#{"id" := ID, "characteristic" := Chars})
		when is_list(ID), is_list(Chars) ->
	resource1(Chars, #resource{id = ID, ).
%% @hidden
resource1([_H | T], Acc) ->
	resource1(T, Acc);
resource1([], Acc) ->
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
					{ok, [{array, [{complex, [{"id", like, [Id]},
							{"characteristic", contains, Contains}]}]}]} ->
						[{"language", {like, [Locale]}}] = char(Contains, '_'),
						{{like, Id}, {like, Locale}};
					{ok, [{array, [{complex, [{"id", like, [Id]}]}]}]} ->
						{{like, Id}, '_'}
				end;
			false ->
				{'_', '_'}
		end
	of
		{MatchId, MatchLocale} ->
			MFA = [im, query_inventory, [MatchId, MatchLocale]],
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
			Resources = lists:map(fun resource/1, Events),
			Body = zj:encode(Resources),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange}],
			{ok, Headers, Body}
	end.

%% @hidden
char([{complex, L1} | T], Chars) ->
	case lists:keytake("name", 1, L1) of
		{_, Name, L2} ->
			case lists:keytake("value", 1, L2) of
				{_, Value, []} ->
					char(Name, Value, T, Chars);
				_ ->
					throw({error, 400})
			end;
		false ->
			throw({error, 400})
	end;
char([], Chars) ->
	rev(Chars).
%% @hidden
char({"name", exact, "language"}, {"value", exact, Lang}, T, Chars)
			when is_list(Lang) ->
		Obj = add_char(Chars, {"language", {exact, Lang}}),
		char(T, Obj);
char({"name", exact, "language"}, {"value", like, Like}, T, Chars)
			when is_list(Like) ->
		Obj = add_char(Chars, {"language", {like, Like}}),
		char(T, Obj).

%% @hidden
add_char('_', AttributeMatch) ->
	[AttributeMatch];
add_char(Attributes, AttributeMatch) when is_list(Attributes) ->
	[AttributeMatch | Attributes].

%% @hidden
rev('_') ->
	'_';
rev(Attributes) when is_list(Attributes) ->
	lists:reverse(Attributes).

