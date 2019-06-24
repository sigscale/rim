%%% mod_im_rest_get.erl
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
-module(mod_im_rest_get).
-copyright('Copyright (c) 2018-2019 SigScale Global Inc.').

-export([do/1]).

-include_lib("inets/include/httpd.hrl").

-spec do(ModData) -> Result when
	ModData :: #mod{},
	Result :: {proceed, OldData} | {proceed, NewData} | {break, NewData} | done,
	OldData :: list(),
	NewData :: [{response,{StatusCode,Body}}] | [{response,{response,Head,Body}}]
			| [{response,{already_sent,StatusCode,Size}}],
	StatusCode :: integer(),
	Body :: list() | nobody | {Fun, Arg},
	Head :: [HeaderOption],
	HeaderOption :: {Option, Value} | {code, StatusCode},
	Option :: accept_ranges | allow
			| cache_control | content_MD5
			| content_encoding | content_language
			| content_length | content_location
			| content_range | content_type | date
			| etag | expires | last_modified
			| location | pragma | retry_after
			| server | trailer | transfer_encoding,
	Value :: string(),
	Size :: term(),
	Fun :: fun((Arg) -> sent| close | Body),
	Arg :: [term()].
%% @doc Erlang web server API callback function.
do(#mod{method = "GET"} = ModData) ->
erlang:display({?MODULE, ?LINE}),
	do1(ModData);
do(#mod{method = "HEAD"} = ModData) ->
	do1(ModData);
do(#mod{data = Data} = _ModData) ->
	{proceed, Data}.
%% @hidden
do1(#mod{parsed_header = Headers, request_uri = Uri, data = Data} = ModData) ->
erlang:display({?MODULE, ?LINE}),
	case proplists:get_value(status, Data) of
		{_StatusCode, _PhraseArgs, _Reason} ->
erlang:display({?MODULE, ?LINE}),
			{proceed, Data};
		undefined ->
erlang:display({?MODULE, ?LINE}),
			case proplists:get_value(response, Data) of
				undefined ->
erlang:display({?MODULE, ?LINE}),
					case lists:keyfind(resource, 1, Data) of
						false ->
erlang:display({?MODULE, ?LINE}),
							{proceed, Data};
						{_, Resource} ->
erlang:display({?MODULE, ?LINE}),
							Path = http_uri:decode(Uri),
							content_type_available(Headers, Path, Resource, ModData)
					end;
				_Response ->
erlang:display({?MODULE, ?LINE}),
					{proceed,  Data}
			end
	end.

%% @hidden
content_type_available(Headers, Uri, Resource, ModData) ->
erlang:display({?MODULE, ?LINE}),
	case lists:keyfind("accept", 1, Headers) of
		{_, RequestingType} ->
			AvailableTypes = Resource:content_types_provided(),
			case lists:member(RequestingType, AvailableTypes) of
				true ->
erlang:display({?MODULE, ?LINE}),
					parse_query(Resource, ModData, httpd_util:split_path(Uri));
				false ->
					Response = "<h2>HTTP Error 415 - Unsupported Media Type</h2>",
					{break, [{response, {415, Response}}]}
			end;
		_ ->
			parse_query(Resource, ModData, httpd_util:split_path(Uri))
	end.

%% @hidden
parse_query(Resource, ModData, {Path, []}) ->
erlang:display({?MODULE, ?LINE}),
	do_get(Resource, ModData, string:tokens(Path, "/"), []);
parse_query(Resource, ModData, {Path, "?" ++ Query}) ->
erlang:display({?MODULE, ?LINE}),
	do_get(Resource, ModData, string:tokens(Path, "/"),
		im_rest:parse_query(Query));
parse_query(_R, _P, _Q) ->
	Response = "<h2>HTTP Error 404 - Not Found</h2>",
	{break, [{response, {404, Response}}]}.

%% @hidden
do_get(Resource, #mod{parsed_header = Headers} = ModData,
		["partyManagement", "v2", "individual"], Query) ->
	do_response(ModData, Resource:get_users(Query, Headers));
do_get(Resource, ModData,
		["partyManagement", "v2", "individual", Id], Query) ->
	do_response(ModData, Resource:get_user(Id, Query));
do_get(Resource, #mod{parsed_header = Headers, method = Method} = ModData,
		["resourceInventoryManagement", "v1", "logicalResource"], Query) ->
erlang:display({?MODULE, ?LINE, Resource, Query}),
	do_response(ModData, Resource:get_rules(Method, Query, Headers));
do_get(Resource, #mod{parsed_header = Headers, method = Method} = ModData,
		["resourceCatalogManagement", "v3", "resourceCatalog"], Query) ->
	do_response(ModData, Resource:get_catalogs(Method, Query, Headers));
do_get(Resource, ModData,
		["resourceCatalogManagement", "v3", "resourceCatalog", Id], Query) ->
	do_response(ModData, Resource:get_catalog(Id, Query));
do_get(Resource, #mod{parsed_header = Headers, method = Method} = ModData,
		["resourceCatalogManagement", "v3", "resourceCategory"], Query) ->
	do_response(ModData, Resource:get_categories(Method, Query, Headers));
do_get(Resource, ModData,
		["resourceCatalogManagement", "v3", "resourceCategory", Id], Query) ->
	do_response(ModData, Resource:get_category(Id, Query));
do_get(Resource, #mod{parsed_header = Headers, method = Method} = ModData,
		["resourceCatalogManagement", "v3", "resourceCandidate"], Query) ->
	do_response(ModData, Resource:get_candidates(Method, Query, Headers));
do_get(Resource, ModData,
		["resourceCatalogManagement", "v3", "resourceCandidate", Id], Query) ->
	do_response(ModData, Resource:get_candidate(Id, Query));
do_get(Resource, #mod{parsed_header = Headers, method = Method} = ModData,
		["resourceCatalogManagement", "v3", "resourceSpecification"], Query) ->
	do_response(ModData, Resource:get_specifications(Method, Query, Headers));
do_get(Resource, ModData,
		["resourceCatalogManagement", "v3", "resourceSpecification", Id], Query) ->
	do_response(ModData, Resource:get_specification(Id, Query));
do_get(Resource, #mod{parsed_header = Headers, method = Method} = ModData,
		["resourceInventoryManagement", "v3", "resource"], Query) ->
   do_response(ModData, Resource:get_resources(Method, Query, Headers));
do_get(Resource, #mod{parsed_header = _Headers} = ModData,
		["resourceInventoryManagement", "v3", "resource", Id], Query) ->
   do_response(ModData, Resource:get_resource(Id, Query));
do_get(_, _, _, _) ->
	Response = "<h2>HTTP Error 404 - Not Found</h2>",
	{break, [{response, {404, Response}}]}.

%% @hidden
do_response(ModData, {ok, Headers, ResponseBody}) ->
	Size = integer_to_list(iolist_size(ResponseBody)),
	NewHeaders = Headers ++ [{content_length, Size}],
	send(ModData, 200, NewHeaders, ResponseBody),
	{proceed,[{response,{already_sent, 200, Size}}]};
do_response(_ModData, {error, 400}) ->
	Response = "<h2>HTTP Error 400 - Bad Request</h2>",
	{break, [{response, {400, Response}}]};
do_response(_ModData, {error, 404}) ->
	Response = "<h2>HTTP Error 404 - Not Found</h2>",
	{break, [{response, {404, Response}}]};
do_response(_ModData, {error, 412}) ->
	Response = "<h2>HTTP Error 412 - Precondition Failed</h2>",
	{break, [{response, {412, Response}}]};
do_response(_ModData, {error, 416}) ->
	Response = "<h2>HTTP Error 416 - Range Not Satisfiable</h2>",
	{break, [{response, {416, Response}}]};
do_response(_ModData, {error, 500}) ->
	Response = "<h2>HTTP Error 500 - Server Error</h2>",
	{break, [{response, {500, Response}}]}.

%% @hidden
send(#mod{socket = Socket, socket_type = SocketType} = ModData,
		StatusCode, Headers, ResponseBody) ->
	httpd_response:send_header(ModData, StatusCode, Headers),
	httpd_socket:deliver(SocketType, Socket, ResponseBody).

