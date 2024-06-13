%%% im_rest_res_rules.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018 - 2024 SigScale Global Inc.
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
%%% 	Handle `Rules' collection.
%%%
-module(im_rest_res_rules).
-copyright('Copyright (c) 2018 - 2024 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([get_rules/3, patch_rules/4, delete_rule/1]).
-export([rules/1]).

-include_lib("inets/include/mod_auth.hrl").
-include("im.hrl").
-define(MILLISECOND, milli_seconds).

-define(PathCatalog, "/resourceInventoryManagement/v1/").

%%----------------------------------------------------------------------
%%  The im public API
%%----------------------------------------------------------------------

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/json-patch+json",
	"application/merge-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations available.
content_types_provided() ->
	["application/json", "application/problem+json"].

-spec delete_rule(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()} .
%% @doc Handle `DELETE' request on a `Pee Rule'.
delete_rule(Id) ->
	case im:delete_rule(Id) of
		ok ->
			{ok, [], []};
		{error, _Reason} ->
			{error, 400}
	end.

-spec patch_rules(Id, Etag, ContentType, ReqBody) -> Result
	when
		Id :: string(),
		Etag :: undefined | string(),
		ContentType :: string(),
		ReqBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()} .
%% @doc Update a existing `peeRule'.
%%
%% 	Respond to `PATCH /resourceInventoryManagement/v1/logicalResource/{Id}' request.
%%
patch_rules(Id, Etag, "application/json-patch+json", ReqBody) ->
	try
		case Etag of
			undefined ->
				{undefined, zj:decode(ReqBody)};
			Etag ->
				{im_rest:etag(Etag) , zj:decode(ReqBody)}
		end
	of
		{_EtagT, {ok, Patch}} ->
			F = fun() ->
				case mnesia:read(pee_rule, Id, write) of
					[PeeRule] ->
						case catch rules(im_rest:patch(Patch,
								rules(PeeRule))) of
							#pee_rule{} = PeeRule1 ->
								mnesia:write(pee_rule, PeeRule1, write),
								PeeRule1;
							_ ->
								mnesia:abort(400)
						end;
					[] ->
						mnesia:abort(404)
				end
			end,
			case mnesia:transaction(F) of
				{atomic, #pee_rule{} = PeeRule3} ->
					Body = zj:encode(rules(PeeRule3)),
					Headers = [{content_type, "application/json"},
							{location, "resourceInventoryManagement/v1/logicalResource/" ++ Id}],
					{ok, Headers, Body};
				{aborted, Status} when is_integer(Status) ->
					{error, Status};
				{aborted, _Reason} ->
					{error, 500}
			end;
		_ ->
			{error, 400}
	catch
		_:_ ->
			{error, 400}
	end;
patch_rules(Id, Etag, "application/merge-patch+json", ReqBody) ->
	try
		case Etag of
			undefined ->
				{undefined, zj:decode(ReqBody)};
			Etag ->
				{im_rest:etag(Etag) , zj:decode(ReqBody)}
		end
	of
		{_EtagT, {ok, Patch}} ->
			F = fun() ->
				case mnesia:read(pee_rule, Id, write) of
					[#pee_rule{rule = Rule} = PeeRuleMerge] ->
						case catch im:merge(PeeRuleMerge#pee_rule{rule = erlang:fun_to_list(Rule)}, rules(Patch)) of
							#pee_rule{} = PeeRuleMerge2 ->
								mnesia:write(pee_rule, PeeRuleMerge2, write),
								PeeRuleMerge2;
							_ ->
								mnesia:abort(400)
								end;
							[] ->
								mnesia:abort(404)
						end
				end,
			case mnesia:transaction(F) of
				{atomic, #pee_rule{} = PeeRuleMerge3} ->
					Json = rules(PeeRuleMerge3),
					Body = zj:encode(Json),
					Headers = [{content_type, "application/json"},
						{location, "resourceInventoryManagement/v1/logicalResource/" ++ Id}],
					{ok, Headers, Body};
				{aborted, Status} when is_integer(Status) ->
					{error, Status};
				{aborted, _Reason} ->
					{error, 500}
			end;
		_ ->
			{error, 400}
	catch
		_:_ ->
			{error, 400}
		end;
patch_rules(_, _, "application/json", _) ->
	{error, 415}.

-spec get_rules(Method, Query, Headers) -> Result
	when
		Method :: string(), % "GET" | "HEAD"
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET|HEAD /resourceInventoryManagement/v1/logicalResource:'
%% 	requests.
get_rules(Method, Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_rules(Method, NewQuery, Filters, Headers);
		false ->
			get_rules(Method, Query, [], Headers)
	end.
%% @hidden
get_rules(Method, Query, Filters, Headers) ->
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
							query_start(Method, Query, Filters, Start, End)
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
		{false, false, {"range", "items=1-" ++ _ = Range}} ->
			case im_rest:range(Range) of
				{error, _} ->
					{error, 400};
				{ok, {Start, End}} ->
					query_start(Method, Query, Filters, Start, End)
			end;
		{false, false, {"range", _Range}} ->
			{error, 416};
		{false, false, false} ->
			query_start(Method, Query, Filters, undefined, undefined)
	end.

-spec rules(ResourceRules) -> ResourceRules
	when
		ResourceRules :: #pee_rule{} | #{}.
%% @doc CODEC for `PeeRules'.
rules(#pee_rule{} = ResourceRules) ->
	rules(record_info(fields, pee_rule), ResourceRules, #{});
rules(#{} = ResourceRules) ->
	rules(record_info(fields, pee_rule), ResourceRules, #pee_rule{}).
%% @hidden
rules([id | T], #pee_rule{id = Id} = R, Acc)
		when is_list(Id) ->
	rules(T, R, Acc#{"id" => Id});
rules([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	rules(T, M, Acc#pee_rule{id = Id});
rules([description| T],
		#pee_rule{description = Description} = R, Acc)
		when is_list(Description) ->
	rules(T, R, Acc#{"description" => Description});
rules([description| T], #{"description" := Description} = M, Acc)
		when is_list(Description) ->
	rules(T, M, Acc#pee_rule{description = Description});
%% @todo handle fun.
rules([rule| T],
		#pee_rule{rule = Rule} = R, Acc) when is_function(Rule) ->
	Rule1 = erlang:fun_to_list(Rule),
	rules(T, R, Acc#{"rule" => Rule1});
rules([rule| T], #{"rule" := Rule} = M, Acc) when is_list(Rule)->
	Rule1 = fun(DN) ->
		[{#resource{name = '$4', _ = '_'}, [{'==', '$4', DN}], ['$_']}]
	end,
	rules(T, M, Acc#pee_rule{rule = Rule1});
rules([_ | T], R, Acc) ->
	rules(T, R, Acc);
rules([], _, Acc) ->
	Acc.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
query_start(Method, Query, Filters, RangeStart, RangeEnd) ->
	try
		CountOnly = case Method of
			"GET" ->
				false;
			"HEAD" ->
				true
		end,
		FilterArgs = case lists:keyfind("filter", 1, Query) of
			{_, String} ->
				{ok, Tokens, _} = im_rest_query_scanner:string(String),
				{ok, _Filter} = im_rest_query_parser:parse(Tokens);
			false ->
				'_'
		end,
		MFA = [im, query, [pee_rule, [], FilterArgs, CountOnly]],
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
		{undefined, ContentRange} ->
			Headers = [{content_type, "application/json"},
				{etag, Etag}, {accept_ranges, "items"},
				{content_range, ContentRange}],
			{ok, Headers, []};
		{Events, ContentRange} ->
			Rules = lists:map(fun rules/1, Events),
			Body = zj:encode(Rules),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange}],
			{ok, Headers, Body}
	end.

