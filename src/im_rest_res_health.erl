%%% im_rest_res_health.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2021 - 2023 SigScale Global Inc.
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
%%% This module reports on the health of the system.
%%%
%%% @reference <a href="https://tools.ietf.org/id/draft-inadarei-api-health-check-05.html">
%%% 	Health Check Response Format for HTTP APIs</a>
%%%
-module(im_rest_res_health).
-copyright('Copyright (c) 2021 - 2023 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0,
		get_health/2, get_applications/2, get_application/2]).

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: [string()].
%% @doc Provide list of resource representations accepted.
content_types_accepted() ->
	[].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: [string()].
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/health+json", "application/problem+json"].

-spec get_health(Query, RequestHeaders) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		RequestHeaders :: [tuple()],
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, 503, ResponseHeaders, ResponseBody},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist().
%% @doc Body producing function for `GET /health'
%% requests.
get_health([] = _Query, _RequestHeaders) ->
	try
		Check1 = maps:merge(application([sigscale_im, inets]),
				table_size([catalog, specification,
						candidate, resource, category])),
		case scheduler() of
			{ok, Check2} ->
				#{"checks" => maps:merge(Check1, Check2)};
			{error, _Reason1} ->
				#{"checks" => Check1}
		end
	of
		#{"checks" := #{"application" := [#{"componentId" := sigscale_im,
				"status" := "up"} | _]}} = Checks ->
			Health = Checks#{"status" => "pass", "serviceId" => atom_to_list(node()),
					"description" => "Health of SigScale IM"},
			ResponseBody = zj:encode(Health),
			ResponseHeaders = [{content_type, "application/health+json"}],
			{ok, ResponseHeaders, ResponseBody};
		Checks ->
			Health = Checks#{"status" => "fail", "serviceId" => atom_to_list(node()),
					"description" => "Health of SigScale IM"},
			ResponseBody = zj:encode(Health),
			ResponseHeaders = [{content_type, "application/health+json"}],
			{error, 503, ResponseHeaders, ResponseBody}
	catch
		_:_Reason2 ->
			{error, 500}
	end.

-spec get_applications(Query, RequestHeaders) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		RequestHeaders :: [tuple()],
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, 503, ResponseHeaders, ResponseBody},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist().
%% @doc Body producing function for `GET /health/application'
%% requests.
get_applications([] = _Query, _RequestHeaders) ->
	try
		#{"checks" => application([sigscale_im, inets])}
	of
		#{"checks" := #{"application" :=  Applications}} = Checks ->
			F = fun(#{"status" := "up"}) ->
						false;
					(#{"status" := "down"}) ->
						true
			end,
			case lists:any(F, Applications) of
				false ->
					Application = Checks#{"status" => "pass",
							"serviceId" => atom_to_list(node()),
							"description" => "OTP applications"},
					ResponseBody = zj:encode(Application),
					ResponseHeaders = [{content_type, "application/health+json"}],
					{ok, ResponseHeaders, ResponseBody};
				true ->
					Application = Checks#{"status" => "fail",
							"serviceId" => atom_to_list(node()),
							"description" => "OTP applications"},
					ResponseBody = zj:encode(Application),
					ResponseHeaders = [{content_type, "application/health+json"}],
					{error, 503, ResponseHeaders, ResponseBody}
			end
	catch
		_:_Reason ->
			{error, 500}
	end.

-spec get_application(Id, RequestHeaders) -> Result
	when
		Id :: string(),
		RequestHeaders :: [tuple()],
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, 503, ResponseHeaders, ResponseBody},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist().
%% @doc Body producing function for `GET /health/application/{Id}'
%% requests.
get_application(Id, _RequestHeaders) ->
	try
		Running = application:which_applications(),
		case lists:keymember(list_to_existing_atom(Id), 1, Running) of
			true ->
				Application = #{"status" => "up",
						"serviceId" => Id},
				ResponseBody = zj:encode(Application),
				ResponseHeaders = [{content_type, "application/health+json"}],
				{ok, ResponseHeaders, ResponseBody};
			false ->
				Application = #{"status" => "down",
						"serviceId" => Id},
				ResponseBody = zj:encode(Application),
				ResponseHeaders = [{content_type, "application/health+json"}],
				{error, 503, ResponseHeaders, ResponseBody}
		end
	catch
		_:badarg ->
			{error, 404};
		_:_Reason ->
			{error, 500}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec scheduler() -> Result
	when
		Result :: {ok, Check} | {error, Reason},
		Check :: map(),
		Reason :: term().
%% @doc Check scheduler component.
%% @hidden
scheduler() ->
	scheduler(im:statistics(scheduler_utilization)).
scheduler({ok, {_Etag, _Interval, Report}}) ->
	F = fun({SchedulerId, Utilization}) ->
				#{"componentId" => integer_to_list(SchedulerId),
						"observeredValue" => Utilization, "observedUnit" => "percent",
						"componentType" => "system"}
	end,
	{ok, #{"scheduler:utilization" => lists:map(F, Report)}};
scheduler({error, Reason}) ->
	{error, Reason}.

-spec application(Names) -> Check
	when
		Names :: [atom()],
		Check :: map().
%% @doc Check application component.
%% @hidden
application(Names) ->
	application(Names, application:which_applications(), []).
%% @hidden
application([Name | T], Running, Acc) ->
	Status = case lists:keymember(Name, 1, Running) of
		true ->
			"up";
		false ->
			"down"
	end,
	NewAcc = [#{"componentId" => Name, "componentType" => "component",
			"status" => Status} | Acc],
	application(T, Running, NewAcc);
application([], _Running, Acc) ->
	#{"application" => lists:reverse(Acc)}.

-spec table_size(Names) -> Check
	when
		Names :: [atom()],
		Check :: map().
%% @doc Check table component size.
%% @hidden
table_size(Names) ->
	table_size(Names, []).
%% @hidden
table_size([Name | T], Acc) ->
	Size = mnesia:table_info(Name, size),
	NewAcc = [#{"componentId" => Name, "componentType" => "component",
			"observedUnit" => "rows", "observeredValue" => Size} | Acc],
	table_size(T, NewAcc);
table_size([], Acc) ->
	#{"table:size" => Acc}.

