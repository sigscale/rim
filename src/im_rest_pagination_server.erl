%%% im_rest_pagination_server.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018-2019 SigScale Global Inc.
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
-module(im_rest_pagination_server).
-copyright('Copyright (c) 2018-2019 SigScale Global Inc.').

-behaviour(gen_server).

%% export the im_rest_pagination_server API
-export([start_link/1]).
-export_type([continuation/0]).

%% export the callbacks needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

-opaque continuation() :: start | eof | ets:continuation() | disk_log:continuation().
-record(state,
		{etag :: string() | undefined,
		max_page_size :: pos_integer() | undefined,
		timeout :: pos_integer() | undefined,
		module :: atom() | undefined,
		function :: atom() | undefined,
		args :: list() | undefined,
		cont = start :: continuation(),
		total :: non_neg_integer() | undefined,
		buffer = [] :: [tuple()],
		offset = 0 :: integer()}).
-type state() :: #state{}.

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

%%----------------------------------------------------------------------
%%  The im_rest_pagination_server API
%%----------------------------------------------------------------------

-spec start_link(Args) -> Result
	when
		Args :: [term()],
		Result :: {ok, PageServer} | {error, Reason},
		PageServer :: pid(),
		Reason :: term().
%% @doc Start a handler for a sequence of REST range requests.
%%
%% 	`Args' is a list of `[Log, Module, Function, Arguments]'
%% 	or `[Module, Function, Arguments]'.
%%
%% 	Each request will result in a call to the callback
%% 	with `apply(Module, Function, [start, Size | Arguments])'
%% 	or `apply(Module, Function, [Cont])'
%%
%% 	The result should be `{Cont, Items}', `{Cont, Items, Total}'
%% 	or `{error, Reason}'.  `Cont' will be `start' on the first
%% 	call and the returned `Cont' may be `eof' or an opaque
%% 	continuation value which will be used in the next call
%% 	to the callback. `Items' would normally be a list of items
%% 	in a collection but may be the number of items in the case
%% 	of a `HEAD' request. `Size' is the requested number of objects
%% 	however the actual number of returned objects may differ.
%% 	If known at the time of return `Total' should be the total
%% 	number of items available.
start_link(Args) ->
	Etag = integer_to_list(erlang:system_time(?MILLISECOND)) ++ "-"
			++ integer_to_list(erlang:unique_integer([positive])),
	case gen_server:start_link({global, Etag}, ?MODULE, [Etag | Args], []) of
		{ok, Child} ->
			{ok, Child, Etag};
		{error, Reason} ->
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%%  The im_rest_pagination_server gen_server call backs
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: [term()],
		Result :: {ok, State}
			| {ok, State, Timeout}
			| {stop, Reason} | ignore,
		State :: state(),
		Timeout :: timeout(),
		Reason :: term().
%% @doc Initialize the {@module} server.
%% @see //stdlib/gen_server:init/1
%% @private
%%
init([Etag, M, F, A] = _Args) when is_atom(M), is_atom(F), is_list(A) ->
	{ok, MaxPageSize} = application:get_env(rest_page_size),
	{ok, Timeout} = application:get_env(rest_page_timeout),
	process_flag(trap_exit, true),
	State = #state{etag = Etag, module = M, function = F,
			args = A, max_page_size = MaxPageSize, timeout = Timeout},
	{ok, State, Timeout}.

-spec handle_call(Request, From, State) -> Result
	when
		Request :: term(),
		From :: {pid(), Tag},
		Tag :: any(),
		State :: state(),
		Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, timeout() | hibernate}
			| {noreply, NewState}
			| {noreply, NewState, timeout() | hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
		Reply :: term(),
		NewState :: state(),
		Reason :: term().
%% @doc Handle a request sent using {@link //stdlib/gen_server:call/2.
%% 	gen_server:call/2,3} or {@link //stdlib/gen_server:multi_call/2.
%% 	gen_server:multi_call/2,3,4}.
%% @see //stdlib/gen_server:handle_call/3
%% @private
%%
handle_call(Range, From, State) ->
	range_request(Range, From, State).

-spec handle_cast(Request, State) -> Result
	when
		Request :: term(),
		State :: state(),
		Result :: {noreply, NewState}
			| {noreply, NewState, timeout() | hibernate}
			| {stop, Reason, NewState},
		NewState :: state(),
		Reason :: term().
%% @doc Handle a request sent using {@link //stdlib/gen_server:cast/2.
%% 	gen_server:cast/2} or {@link //stdlib/gen_server:abcast/2.
%% 	gen_server:abcast/2,3}.
%% @see //stdlib/gen_server:handle_cast/2
%% @private
%%
handle_cast(stop, State) ->
	{stop, normal, State}.

-spec handle_info(Info, State) -> Result
	when
		Info :: timeout | term(),
		State::state(),
		Result :: {noreply, NewState}
			| {noreply, NewState, timeout() | hibernate}
			| {stop, Reason, NewState},
		NewState :: state(),
		Reason :: term().
%% @doc Handle a received message.
%% @see //stdlib/gen_server:handle_info/2
%% @private
%%
handle_info(timeout, State) ->
	{stop, shutdown, State}.

-spec terminate(Reason, State) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
		State::state().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_server:terminate/3
%% @private
%%
terminate(_Reason, _State) ->
	ok.

-spec code_change(OldVsn, State, Extra) -> Result
	when
		OldVsn :: term() | {down, term()},
		State :: state(),
		Extra :: term(),
		Result :: {ok, NewState} | {error, Reason},
		NewState :: state(),
		Reason :: term().
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_server:code_change/3
%% @private
%%
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec range_request(Range, From, State) -> Result
	when
		Range :: {Start, End},
		Start :: pos_integer() | undefined,
		End :: pos_integer() | undefined,
		From :: {pid(), Tag},
		Tag :: any(),
		State :: state(),
		Result :: {reply, Reply, State, timeout()}
			| {stop, Reason, Reply, State},
		Reason :: term(),
		Reply :: {Items, ContentRange} | {error, Status},
		Items :: [tuple()] | undefined,
		ContentRange :: string(),
		Status :: 400 | 404 | 416 | 500.
%% @doc Handle a range request.
%% 	Manages a buffer of items read with the callback.
%%
%% 	Returns `{Items, ContentRange}' on success where `Items' is
%% 	a list of collection members and `ContentRange' is to be
%% 	used in a `Content-Range' header. The total items will be
%% 	included if known at the time (e.g. "items 1-100/*" or
%% 	"items 50-100/100").
%%
%% 	Returns `{error, Status}' if the request fails. `Status'
%% 	is an HTTP status code to be returned to the client.
%%
%% @private
range_request({undefined, undefined}, _From,
		#state{cont = eof, buffer = [], total = Total} = State) ->
	ContentRange = content_range(0, 0, Total),
	{stop, shutdown, {[], ContentRange}, State};
range_request({undefined, undefined}, From,
		#state{cont = Cont, max_page_size = MaxPageSize} = State)
		when Cont /= start ->
	range_request({1, MaxPageSize}, From, State);
range_request({StartRange, EndRange}, From,
		#state{max_page_size = MaxPageSize} = State)
		when StartRange /= undefined, EndRange /= undefined,
		(EndRange - StartRange) > MaxPageSize ->
	range_request({StartRange, StartRange + MaxPageSize}, From, State);
range_request({StartRange, _EndRange}, _From, #state{offset = Offset,
		timeout = Timeout} = State)
		when StartRange /= undefined, StartRange < Offset ->
	{reply, {error, 416}, State, Timeout};
range_request({StartRange, _EndRange}, _From,
		#state{cont = eof, offset = Offset, buffer = Buffer} = State)
		when StartRange /= undefined,
		StartRange > Offset + length(Buffer) ->
	{stop, shutdown, {error, 416}, State};
range_request({StartRange, EndRange}, _From,
		#state{cont = eof, offset = Offset,
		buffer = Buffer, total = Total} = State)
		when StartRange /= undefined, EndRange /= undefined,
		StartRange >= Offset, length(Buffer) =< EndRange - Offset ->
	Rest = lists:sublist(Buffer, StartRange - Offset, length(Buffer)),
	End = StartRange + length(Rest) - 1,
	ContentRange = case Total of
		undefined ->
			content_range(StartRange, End, End);
		_ ->
			content_range(StartRange, End, Total)
	end,
	{stop, shutdown, {Rest, ContentRange}, State};
range_request({StartRange, EndRange}, _From,
		#state{offset = Offset, buffer = Buffer,
		timeout = Timeout, total = Total} = State)
		when StartRange /= undefined, EndRange /= undefined,
		StartRange > Offset, length(Buffer) >= EndRange - Offset ->
	PageSize = EndRange - StartRange + 1,
	Rest = lists:sublist(Buffer, StartRange - Offset, length(Buffer)),
	{RespItems, NewBuffer} = lists:split(PageSize, Rest),
	NewState = State#state{offset = EndRange, buffer = NewBuffer},
	ContentRange = content_range(StartRange, EndRange, Total),
	{reply, {RespItems, ContentRange}, NewState, Timeout};
range_request({StartRange, EndRange}, From,
		#state{cont = Cont1, module = Module, function = Function,
		args = Args, buffer = Buffer, timeout = Timeout} = State) ->
	Size = case {EndRange, StartRange} of
		{undefined, _StartRange} ->
			undefined;
		{EndRange, StartRange}
				when is_integer(EndRange), is_integer(StartRange) ->
			(EndRange - StartRange) + 1
	end,
	case apply(Module, Function, [Cont1, Size | Args]) of
		{error, _Reason} ->
			{stop, shutdown, {error, 500}, State};
		{Cont2, Items} when is_list(Items) ->
			NewState = State#state{cont = Cont2, buffer = Buffer ++ Items},
			range_request({StartRange, EndRange}, From, NewState);
		{_Cont2, NumItems} when is_integer(NumItems), Size == undefined ->
			ContentRange = content_range(1, NumItems, undefined),
			{reply, {undefined, ContentRange}, State, Timeout};
		{_Cont2, NumItems} when is_integer(NumItems) ->
			ContentRange = content_range(StartRange, EndRange, undefined),
			{reply, {undefined, ContentRange}, State, Timeout};
		{Cont2, Items, Total} when is_list(Items) ->
			NewState = State#state{cont = Cont2,
					buffer = Buffer ++ Items, total = Total},
			range_request({StartRange, EndRange}, From, NewState);
		{_Cont2, NumItems, Total} when is_integer(NumItems), Size == undefined ->
			ContentRange = content_range(1, NumItems, Total),
			{reply, {undefined, ContentRange}, State, Timeout};
		{_Cont2, NumItems, Total} when is_integer(NumItems) ->
			ContentRange = content_range(StartRange, EndRange, Total),
			{reply, {undefined, ContentRange}, State, Timeout}
	end.

%% @hidden
content_range(Start, End, undefined) ->
	"items " ++ integer_to_list(Start)
			++ "-" ++ integer_to_list(End) ++ "/*";
content_range(Start, End, Total) ->
	"items " ++ integer_to_list(Start)
			++ "-" ++ integer_to_list(End) ++ "/" ++ integer_to_list(Total).

