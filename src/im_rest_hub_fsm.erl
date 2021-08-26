%%% im_rest_hub_fsm.erl
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
%%%
-module(im_rest_hub_fsm).
-copyright('Copyright (c) 2021 SigScale Global Inc.').

-behaviour(gen_fsm).

%% export the public API
-export([start_link/3, start_link/4]).

%% export the private API
-export([handle_async/2]).
%% export the im_rest_hub_fsm states
-export([register/2]).

%% export the callbacks needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4,
			terminate/3, handle_info/3, code_change/4]).

-record(statedata,
		{id :: string(),
		profile :: atom(),
		module :: atom(),
		function :: atom(),
		query :: string(),
		callback :: string(),
		href :: string(),
		authorization :: string() | undefined,
		args :: list() | undefined,
		sync = true :: boolean()}).
-type statedata() :: #statedata{}.

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

%%----------------------------------------------------------------------
%%  The im_rest_hub_fsm API
%%----------------------------------------------------------------------

-spec start_link(Query, Callback, Uri) -> Result
	when
		Query :: null | string(),
		Callback :: string(),
		Uri :: string(),
		Result :: {ok, PageServer} | {error, Reason},
		PageServer :: pid(),
		Reason :: term().
%% @doc Start a hub fsm
start_link(Query, Callback, Uri) ->
	{Id, _} = unique(),
	case gen_fsm:start_link({global, Id}, ?MODULE,
			[Id, Query, Callback, Uri], []) of
		{ok, Child} ->
			{ok, Child, Id};
		{error, Reason} ->
			{error, Reason}
	end.

-spec start_link(Query, Callback, Uri, Authorization) -> Result
	when
		Query :: null | string(),
		Callback :: string(),
		Uri :: string(),
		Authorization :: string(),
		Result :: {ok, PageServer} | {error, Reason},
		PageServer :: pid(),
		Reason :: term().
%% @doc Start a hub fsm
start_link(Query, Callback, Uri, Authorization) ->
	{Id, _} = unique(),
	case gen_fsm:start_link({global, Id}, ?MODULE,
			[Id, Query, Callback, Uri, Authorization], []) of
		{ok, Child} ->
			{ok, Child, Id};
		{error, Reason} ->
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%%  The im_rest_hub_fsm gen_fsm call backs
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, StateName, StateData} | {ok, StateName, StateData, Timeout}
			| {ok, StateName, StateData, hibernate} | {stop, Reason} | ignore,
		StateName ::atom(),
		StateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: term().
%% @doc Initialize the {@module} fsm.
%% @see //stdlib/gen_fsm:init/1
%% @private
%%
init([Id, Query, Callback, Uri] = _Args) ->
	process_flag(trap_exit, true),
	{ok, Profile} = application:get_env(hub_profile),
	State = #statedata{id = Id, href = Uri ++ Id,
			profile = Profile, query = Query, callback = Callback},
	{ok, register, State, 0};
init([Id, Query, Callback, Uri, Authorization] = _Args) ->
	process_flag(trap_exit, true),
	{ok, Profile} = application:get_env(hub_profile),
	State = #statedata{id = Id, href = Uri ++ Id, profile = Profile,
			query = Query, callback = Callback, authorization = Authorization},
	{ok, register, State, 0}.

-spec register(Event1, State) -> Result
	when
		Event1 :: timeout | pos_integer(),
		State :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Reason :: normal | term().
%% @doc Handle event received in `register' state.
%% @private
register(timeout, State) ->
	case gen_event:add_sup_handler(im_event, im_event, [self()]) of
		ok ->
			{next_state, registered, State};
		{'EXIT', Reason} ->
			{stop, Reason, State}
	end.

-spec handle_event(Event, StateName, State) -> Result
	when
		Event :: term(),
		StateName :: atom(),
		State :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle a request sent using
%% 	{@link //stdlib/gen_fsm:send_all_state_event/2}.
%% @private
%%
handle_event(Reason, _StateName, State) ->
	{stop, Reason, State}.

-spec handle_sync_event(Event, From, StateName, StateData) -> Result
	when
		Event :: term(),
		From :: {Pid, Tag},
		Pid :: pid(),
		Tag :: term(),
		StateName :: atom(),
		StateData :: statedata(),
		Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData }
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData} | {stop, Reason, NewStateData},
		Reply :: term(),
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:sync_send_all_state_event/2.
%% 	gen_fsm:sync_send_all_state_event/2,3}.
%% @see //stdlib/gen_fsm:handle_sync_event/4
%% @private
%%
handle_sync_event(get, _From, StateName,
		#statedata{id = Id, href = Href,
				query = Query, callback = Callback} = StateData) ->
	Hub = #{"id" => Id, "href" => Href,
			"query" => Query, "callback" => Callback},
	{reply, Hub, StateName, StateData}.

-spec handle_info(Info, StateName, StateData) -> Result
	when
		Info :: term(),
		StateName :: atom(),
		StateData :: statedata(),
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata(),
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle a received message.
%% @see //stdlib/gen_fsm:handle_info/3
%% @private
%%
handle_info({gen_event_EXIT, _Handler, Reason}, _StateName, StateData) ->
	{stop, Reason, StateData}.

-spec terminate(Reason, StateName, StateData) -> any()
	when
		Reason :: normal | shutdown | term(),
		StateName :: atom(),
		StateData :: statedata().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_fsm:terminate/3
%% @private
%%
terminate(Reason, _StateName, _StateData)
		when Reason == shutdown; Reason == normal ->
	ok;
terminate({shutdown, Reason}, StateName, StateData) ->
	terminate(Reason, StateName, StateData);
terminate(Reason, StateName,
		#statedata{id = Id, query = Query, callback = Callback}) ->
	error_logger:warning_report(["Notification subscription cancelled",
			{reason, Reason}, {pid, self()}, {state, StateName},
			{id, Id}, {query, Query}, {callback, Callback}]).

-spec code_change(OldVsn, StateName, StateData, Extra) -> Result
	when
		OldVsn :: (Vsn :: term() | {down, Vsn :: term()}),
		StateName :: atom(),
		StateData :: statedata(),
		Extra :: term(),
		Result :: {ok, NextStateName, NewStateData},
		NextStateName :: atom(),
		NewStateData :: statedata().
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_fsm:code_change/4
%% @private
%%
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

-spec handle_async(ReplyInfo, Fsm) -> ok
	when
		ReplyInfo :: tuple(),
		Fsm :: pid().
%% @doc Handle result of httpc:request/3.
%% @private
handle_async({_RequestId,
		{{_HttpVersion, StatusCode, _ReasonPhrase}, _Headers, _Body}}, _Fsm)
		when StatusCode >= 200, StatusCode  < 300 ->
	ok;
handle_async({RequestId,
		{{_HttpVersion, StatusCode, _ReasonPhrase}, _Headers, _Body}}, Fsm) ->
	error_logger:warning_report(["Notification delivery failed",
			{module, ?MODULE}, {fsm, Fsm},
			{status, StatusCode}, {request, RequestId}]),
	gen_fsm:send_all_state_event(Fsm, {shutdown, StatusCode});
handle_async({RequestId, {error, Reason}}, Fsm) ->
	error_logger:warning_report(["Notification delivery failed",
			{module, ?MODULE}, {fsm, Fsm},
			{error, Reason}, {request, RequestId}]),
	gen_fsm:send_all_state_event(Fsm, Reason).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec unique() -> Result
	when
		Result :: {ID, TS},
		TS :: pos_integer(),
		ID :: string().
%% @doc Generate a unique identifier.
unique() ->
	TS = erlang:system_time(?MILLISECOND),
	N = erlang:unique_integer([positive]),
	ID = integer_to_list(TS) ++ integer_to_list(N),
	{ID, TS}.

