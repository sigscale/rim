%%% im_rest_res_http.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2017 - 2024 SigScale Global Inc.
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
%%%   for a REST server in the {@link //im. im} application.
%%%
-module(im_rest_res_http).
-copyright('Copyright (c) 2017 - 2024 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0, get_http/0]).

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations accepted.
content_types_accepted() ->
	[].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/json", "application/problem+json"].

-spec get_http() -> Result
	when
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /im/v1/log/http'
%% requests.
get_http() ->
	{ok, MaxItems} = application:get_env(im, rest_page_size),
	Log = im:httpd_logname(transfer),
	read_http_log(Log, MaxItems).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
read_http_log(Log, MaxItems) ->
	case im:last(Log, MaxItems) of
		{error, _} ->
			{error, 500};
		{NumItems, Events} ->
			JsonObjs = lists:map(fun event/1, Events),
			Body = zj:encode(JsonObjs),
			ContentRange = "items 1-" ++ integer_to_list(NumItems) ++ "/*",
			Headers = [{"content_type", "application/json"},
					{content_range, ContentRange}],
			{ok, Headers, Body}
	end.

% @hidden
event(Event) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<Host:Offset/binary, 32, $-, 32, Rest/binary>> = Event,
	event1(Rest, #{host => binary_to_list(Host)}).
% @hidden
event1(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<User:Offset/binary, 32, $[, Rest/binary>> = Event,
	event2(Rest, Acc#{user => binary_to_list(User)}).
% @hidden
event2(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<$]>>),
	<<Date:Offset/binary, $], 32, $", Rest/binary>> = Event,
	event3(Rest, Acc#{date => binary_to_list(Date)}).
% @hidden
event3(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<Method:Offset/binary, 32, Rest/binary>> = Event,
	event4(Rest, Acc#{method => binary_to_list(Method)}).
% @hidden
event4(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<URI:Offset/binary, 32, Rest/binary>> = Event,
	event5(Rest, Acc#{uri => binary_to_list(URI)}).
% @hidden
event5(Event, Acc) ->
	{Offset, 2} = binary:match(Event, <<$", 32>>),
	<<_Http:Offset/binary, $", 32, Rest/binary>> = Event,
	event6(Rest, Acc).
% @hidden
event6(Event, Acc) ->
	{Offset, 1} = binary:match(Event, <<32>>),
	<<Status:Offset/binary, 32, _Rest/binary>> = Event,
	Acc#{httpStatus => binary_to_integer(Status)}.

