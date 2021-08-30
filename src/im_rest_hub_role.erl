%%% im_rest_hub_role.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2020 - 2021 SigScale Global Inc.
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
%%%
-module(im_rest_hub_role).
-copyright('Copyright (c) 2020 - 2021 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0, post_hub/1,
		delete_hub/1, get_hubs/0, get_hub/1]).

-define(PathRoleHub, "/partyRoleManagement/v4/hub/").

%%----------------------------------------------------------------------
%%  The hub public API
%%----------------------------------------------------------------------

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/merge-patch+json",
	"application/json-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec post_hub(ReqBody) -> Result
	when
		ReqBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% Hub event to disk.
%% @doc Respond to `POST /partyRoleManagement/v4/hub'
post_hub(ReqBody) ->
	try
		case zj:decode(ReqBody) of
			{ok, #{"callback" := Callback, "query" := Query} = Hub}
					when is_list(Query) ->
				post_hub(supervisor:start_child(im_rest_hub_sup,
						[Query, Callback, ?PathRoleHub]), Hub);
			{ok, #{"callback" := Callback} = Hub} ->
				post_hub(supervisor:start_child(im_rest_hub_sup,
						[[], Callback, ?PathRoleHub]), Hub#{"query" => undefined})
		end
	catch
		_:500 ->
			{error, 500};
		_:_ ->
			{error, 400}
	end.
post_hub({ok, _PageServer, Id}, Hub) ->
	Body = zj:encode(Hub#{"id" => Id}),
	Headers = [{content_type, "application/json"},
			{location, ?PathRoleHub ++ Id}],
	{ok, Headers, Body};
post_hub({error, _Reason}, _Hub) ->
	{error, 500}.

-spec delete_hub(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% Delete hub by id.
%% @doc Respond to `POST /partyRoleManagement/v4/hub/{id}'
delete_hub(Id) ->
	{gen_fsm:send_all_state_event({global, Id}, shutdown), [], []}.

-spec get_hubs() -> Result
	when
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET|HEAD /partyRoleManagement/v4/hub/'
get_hubs() ->
	get_hubs(supervisor:which_children(im_rest_hub_sup), []).
%% @hidden
get_hubs([{_, Pid, _, _} | T], Acc) ->
	case gen_fsm:sync_send_all_state_event(Pid, get) of
		#{"href" := ?PathRoleHub ++ _, "query" := []} = Hub ->
			get_hubs(T, [Hub#{"query" => undefined} | Acc]);
		#{"href" := ?PathRoleHub ++ _} = Hub ->
			get_hubs(T, [Hub | Acc]);
		_Hub ->
			get_hubs(T, Acc)
	end;
get_hubs([], Hubs) ->
	Body = zj:encode(Hubs),
	Headers = [{content_type, "application/json"}],
	{ok, Headers, Body}.

-spec get_hub(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET|HEAD /partyRoleManagement/v4/hub/{id}'
get_hub(Id) ->
	case global:whereis_name(Id) of
		Fsm when is_pid(Fsm) ->
			case gen_fsm:sync_send_all_state_event(Fsm, get) of
				#{"id" := Id, "query" := []} = Hub ->
					Body = zj:encode(Hub#{"query" => undefined}),
					Headers = [{content_type, "application/json"}],
					{ok, Headers, Body};
				_ ->
					{error, 404}
			end;
		undefined ->
			{error, 404}
	end.

%%----------------------------------------------------------------------
%%  The internal functions
%%----------------------------------------------------------------------

