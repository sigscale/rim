%%% im_rest_res_role.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2021 SigScale Global Inc.
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
%%% 	Handle `Role' collection.
%%%
-module(im_rest_res_role).
-copyright('Copyright (c) 2021 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0, post_role/1,
		delete_role/1, get_role/2, get_roles/2]).

-include_lib("inets/include/mod_auth.hrl").
-include("im.hrl").

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).

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

-spec post_role(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Handle `POST' request on `Role' collection.
%% 	Respond to `POST /partyRoleManagement/v4/partyRole' request.
post_role(RequestBody) ->
	try
		{ok, #{"name" := Name} = Role} = zj:decode(RequestBody),
		case im:add_user(Name, [], "en") of
			{ok, LastModified} ->
				NewRole = Role#{"id" => Name,
						"href" => "/partyRoleManagement/v4/partyRole/" ++ Name},
				Body = zj:encode(NewRole),
				Location = "/partyRoleManagement/v4/partyRole/" ++ Name,
				Headers = [{location, Location}, {etag, im_rest:etag(LastModified)}],
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec delete_role(Name) -> Result
	when
		Name :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Handle `DELETE' request on a `Role' resource.
%% 	Respond to `DELETE /partyRoleManagement/v4/partyRole/{Name}' request.
delete_role(Name) when is_list(Name) ->
	case im:del_user(Name) of
		ok ->
			{ok, [], []};
		{error, _Reason} ->
			{error, 400}
	end.

-spec get_role(Name, Query) -> Result
	when
		Name :: string(),
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Handle `GET' request on a `Role' resource.
%% 	Respond to `GET /partyRoleManagement/v4/partyRole/{Name}' request.
get_role(Name, Query) ->
	get_role(Name, Query, get_params()).
%% @hidden
get_role(Name, Query, {_, _, _, _} = Params) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, L}, NewQuery} ->
			get_role(Name, NewQuery, Params, string:tokens(L, ","));
		false ->
			get_role(Name, Query, Params, [])
	end;
get_role(_Name, _Query, {error, Reason}) ->
	{error, Reason}.
%% @hidden
get_role(Name, [] = _Query, {Port, Address, Dir, _Group}, _Filters) ->
	case mod_auth:get_user(Name, Address, Port, Dir) of
		{ok, #httpd_user{user_data = UserData} = RoleRec} ->
			Headers1 = case lists:keyfind(last_modified, 1, UserData) of
				{_, LastModified} ->
					[{etag, im_rest:etag(LastModified)}];
				false ->
					[]
			end,
			Headers2 = [{content_type, "application/json"} | Headers1],
			Body = zj:encode(role(RoleRec)),
			{ok, Headers2, Body};
		{error, _Reason} ->
			{error, 404}
	end;
get_role(_, _, _, _) ->
	{error, 400}.

-spec get_roles(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Handle `GET' request on `Role' collection.
%% 	Respond to `GET /partyRoleManagement/v4/partyRole/' request.
get_roles(Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_roles1(NewQuery, Filters, Headers);
		false ->
			get_roles1(Query, [], Headers)
	end.
%% @hidden
get_roles1(Query, Filters, Headers) ->
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

-spec role(Role) -> Role
	when
		Role :: #httpd_user{} | map().
%% @doc CODEC for `Role'.
role(#httpd_user{username = {Name, _, _, _}} = User) when is_list(Name) ->
	role(User#httpd_user{username = Name});
role(#httpd_user{username = Name, user_data = Chars})
		when is_list(Name), is_list(Chars) ->
	F = fun(Key) ->
			case lists:keyfind(Key, 1, Chars) of
				{Key, Value} ->
					Value;
				false ->
					[]
			end
	end,
	#{"id" => Name, "name" => Name, "@type" => F(type),
			"validFor" => #{"startDateTime" => im_rest:iso8601(F(start_date)),
					"endDateTime" => im_rest:iso8601(F(end_date))},
			"href" => "/partyRoleManagement/v4/partyRole/" ++ Name}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec get_params() -> Result
	when
		Result :: {Port :: integer(), Address :: string(),
				Directory :: string(), Group :: string()}
				| {error, Reason :: term()}.
%% @doc Returns configurations details for currently running
%% {@link //inets. httpd} service.
%% @hidden
get_params() ->
	get_params(inets:services_info()).
%% @hidden
get_params({error, Reason}) ->
	{error, Reason};
get_params(ServicesInfo) ->
	get_params1(lists:keyfind(httpd, 1, ServicesInfo)).
%% @hidden
get_params1({httpd, _, HttpdInfo}) ->
	{_, Address} = lists:keyfind(bind_address, 1, HttpdInfo),
	{_, Port} = lists:keyfind(port, 1, HttpdInfo),
	get_params2(Address, Port, application:get_env(inets, services));
get_params1(false) ->
	{error, httpd_not_started}.
%% @hidden
get_params2(Address, Port, {ok, Services}) ->
	get_params3(Address, Port, lists:keyfind(httpd, 1, Services));
get_params2(_, _, undefined) ->
	{error, inet_services_undefined}.
%% @hidden
get_params3(Address, Port, {httpd, Httpd}) ->
	get_params4(Address, Port, lists:keyfind(directory, 1, Httpd));
get_params3(_, _, false) ->
	{error, httpd_service_undefined}.
%% @hidden
get_params4(Address, Port, {directory, {Directory, Auth}}) ->
	get_params5(Address, Port, Directory,
			lists:keyfind(require_group, 1, Auth));
get_params4(_, _, false) ->
	{error, httpd_directory_undefined}.
%% @hidden
get_params5(Address, Port, Directory, {require_group, [Group | _]}) ->
	{Port, Address, Directory, Group};
get_params5(_, _, _, false) ->
	{error, httpd_group_undefined}.

%% @hidden
query_start(Query, Filters, RangeStart, RangeEnd) ->
	try
		{Port, Address, Directory, _Group} = get_params(),
		case lists:keyfind("filter", 1, Query) of
			{_, String} ->
				{ok, Tokens, _} = im_rest_query_scanner:string(String),
				case im_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex, [{"id", like, [Id]}]}]}]} ->
						Username = {Id ++ '_', Address, Port, Directory},
						{#httpd_user{username = Username, _ = '_'}, []};
					{ok, [{array, [{complex, [{"id", exact, [Id]}]}]}]} ->
						Username = {Id ++ '_', Address, Port, Directory},
						{#httpd_user{username = Username, _ = '_'}, []}
				end;
			false ->
				{'_', []}
		end
	of
		{MatchHead, MatchConditions} ->
			MFA = [im, query_users, [MatchHead, MatchConditions]],
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
		{Events, ContentRange} ->
			F = fun(#httpd_user{password = []} = User) ->
						{true, role(User)};
					(_) ->
						false
			end,
			Body = zj:encode(lists:filtermap(F, Events)),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange}],
			{ok, Headers, Body}
	end.
