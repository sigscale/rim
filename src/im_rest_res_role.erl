%%% im_rest_res_role.erl
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
%%% @doc This library module implements resource handling functions
%%% 	for a REST server in the {@link //im. im} application.
%%%
%%% 	Handle `Individual' collection.
%%%
-module(im_rest_res_role).
-copyright('Copyright (c) 2021 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0, post_role/1,
		delete_role/1, get_role/2]).

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
		{ok, #{"name" := Name, "@type" := Type,
				"validFor" := #{"startDateTime" := StartDate,
				"endDateTime" := EndDate}} = Role} = zj:decode(RequestBody),
		{Port, Address, Directory, _Group} = get_params(),
		LM = {erlang:system_time(?MILLISECOND),
				erlang:unique_integer([positive])},
		UserData = [{type, Type}, {start_date, im_rest:iso8601(StartDate)},
				{end_date, im_rest:iso8601(EndDate)}, {last_modified, LM}],
		case mod_auth:add_user(Name, [], UserData, Address, Port, Directory) of
			true ->
				NewRole = Role#{"id" => Name,
						"href" => "/partyRoleManagement/v4/partyRole/" ++ Name},
				Body = zj:encode(NewRole),
				Location = "/partyRoleManagement/v4/partyRole/" ++ Name,
				Headers = [{location, Location}, {etag, im_rest:etag(LM)}],
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
delete_role(Name) ->
	{Port, Address, Directory, _Group} = get_params(),
	case mod_auth:delete_user(Name, Address, Port, Directory) of
		true ->
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
%% @doc Handle `GET' request on a `Individual' resource.
%% 	Respond to `GET /partyRoleManagement/v4/partyRole/{Name}' request.
get_role(Name, Query) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, L}, NewQuery} ->
			get_role(Name, NewQuery, string:tokens(L, ","));
		false ->
			get_role(Name, Query, [])
	end.
%% @hidden
get_role(Name, [] = _Query, _Filters) ->
	{Port, Address, Dir, _Group} = get_params(),
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
get_role(_, _, _) ->
	{error, 400}.

-spec role(Role) -> Role
	when
		Role :: #httpd_user{} | map().
%% @doc CODEC for `Role'.
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
		Result :: {Port, Address, Directory, Group},
		Port :: integer(),
		Address :: string(),
		Directory :: string(),
		Group :: string().
%% @doc Get {@link //inets/httpd. httpd} configuration parameters.
%% @private
get_params() ->
	{_, _, Info} = lists:keyfind(httpd, 1, inets:services_info()),
	{_, Port} = lists:keyfind(port, 1, Info),
	{_, Address} = lists:keyfind(bind_address, 1, Info),
	{ok, EnvObj} = application:get_env(inets, services),
	{httpd, HttpdObj} = lists:keyfind(httpd, 1, EnvObj),
	{directory, {Directory, AuthObj}} = lists:keyfind(directory, 1, HttpdObj),
	case lists:keyfind(require_group, 1, AuthObj) of
		{require_group, [Group | _T]} ->
			{Port, Address, Directory, Group};
		false ->
			exit(not_found)
	end.

