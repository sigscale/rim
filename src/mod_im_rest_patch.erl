%%% mod_im_rest_patch.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2019 SigScale Global Inc.
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
-module(mod_im_rest_patch).
-copyright('Copyright (c) 2016 - 2019 SigScale Global Inc.').

-export([do/1]).

-include_lib("inets/include/httpd.hrl").

-spec do(ModData) -> Result when
	ModData :: #mod{},
	Result :: {proceed, OldData} | {proceed, NewData} | {break, NewData} | done,
	OldData :: list(),
	NewData :: [{response,{StatusCode,Body}}] | [{response,{response,Head,Body}}]
			| [{response,{already_sent,StatusCode,Size}}],
	StatusCode :: integer(),
	Body :: iolist() | nobody | {Fun, Arg},
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
do(#mod{method = Method, parsed_header = Headers, request_uri = Uri,
		entity_body = Body, data = Data} = ModData) ->
	case Method of
		"PATCH" ->
			case proplists:get_value(status, Data) of
				{_StatusCode, _PhraseArgs, _Reason} ->
					{proceed, Data};
				undefined ->
					case proplists:get_value(response, Data) of
						undefined ->
							Path = http_uri:decode(Uri),
							{_, Resource} = lists:keyfind(resource, 1, Data),
							{_, ContentType} = lists:keyfind(content_type, 1, Data),
							content_type_available(Headers, ContentType, Body, Path,
									Resource, ModData);
						_Response ->
							{proceed,  Data}
					end
			end;
		_ ->
			{proceed, Data}
	end.

%% @hidden
content_type_available(Headers, ContentType, Body,
		Uri, Resource, #mod{data = Data} = ModData) ->
	case lists:keyfind("accept", 1, Headers) of
		{_, RequestingType} ->
			AvailableTypes = Resource:content_types_provided(),
			case lists:member(RequestingType, AvailableTypes) of
				true ->
					parse_query(Resource, ModData,
							ContentType, Body, httpd_util:split_path(Uri));
				false ->
					Response = "<h2>HTTP Error 415 - Unsupported Media Type</h2>",
					{proceed, [{response, {415, Response}} | Data]}
			end;
		_Other ->
			parse_query(Resource, ModData,
					ContentType, Body, httpd_util:split_path(Uri))
	end.

%% @hidden
parse_query(Resource, ModData, ContentType, Body,
		{Path, []}) ->
	do_patch(Resource, ModData, ContentType, Body,
			string:tokens(Path, "/"), []);
parse_query(Resource, ModData, ContentType, Body,
		{Path, "?" ++ Query}) ->
	do_patch(Resource, ModData, ContentType, Body,
			string:tokens(Path, "/"), im_rest:parse_query(Query));
parse_query(_, #mod{data = Data}, _, _, _) ->
	Response = "<h2>HTTP Error 404 - Not Found</h2>",
	{proceed, [{response, {404, Response}} | Data]}.

%% @hidden
get_etag(Headers) ->
	case lists:keyfind("if-match", 1, Headers) of
		{_, Etag} ->
			Etag;
		false ->
			undefined
	end.

%% @hidden
do_patch(Resource,
		#mod{parsed_header = Headers} = ModData, ContentType, Body,
		["resourceCatalogManagement", "v3", "resourceCatalog", Identity], []) ->
	do_response(ModData, Resource:patch_catalog(Identity,
			get_etag(Headers), ContentType, Body));
do_patch(Resource,
		#mod{parsed_header = Headers} = ModData, ContentType, Body,
		["resourceCatalogManagement", "v3", "resourceSpecification", Identity], []) ->
	do_response(ModData, Resource:patch_specification(Identity,
			get_etag(Headers), ContentType, Body));
do_patch(Resource,
		#mod{parsed_header = Headers} = ModData, ContentType, Body,
		["resourceCatalogManagement", "v3", "resourceCategory", Identity], []) ->
	do_response(ModData, Resource:patch_category(Identity,
			get_etag(Headers), ContentType, Body));
do_patch(Resource,
		#mod{parsed_header = Headers} = ModData, ContentType, Body,
		["resourceCatalogManagement", "v3", "resourceCandidate", Identity], []) ->
	do_response(ModData, Resource:patch_candidate(Identity,
			get_etag(Headers), ContentType, Body));
do_patch(Resource,
		#mod{parsed_header = Headers} = ModData, ContentType, Body,
		["resourceInventoryManagement", "v1", "logicalResource", Identity], []) ->
	do_response(ModData, Resource:patch_rules(Identity,
			get_etag(Headers), ContentType, Body));
do_patch(_, #mod{data = Data}, _, _, _, _) ->
	Response = "<h2>HTTP Error 404 - Not Found</h2>",
	{proceed, [{response, {404, Response}} | Data]}.

%% @hidden
do_response(#mod{data = Data} = ModData, {ok, Headers, []}) ->
	Size = integer_to_list(iolist_size([])),
	NewHeaders = [{content_length, Size} | Headers],
	send(ModData, 204, NewHeaders, []),
	{proceed,[{response,{already_sent,200, Size}} | Data]};
do_response(#mod{data = Data} = ModData, {ok, Headers, ResponseBody}) ->
	Size = integer_to_list(iolist_size(ResponseBody)),
	NewHeaders = Headers ++ [{content_length, Size}],
	send(ModData, 200, NewHeaders, ResponseBody),
	{proceed,[{response,{already_sent,200, Size}} | Data]};
do_response(#mod{data = Data} = _ModData, {error, 400}) ->
	Response = "<h2>HTTP Error 400 - Bad Request</h2>",
	{proceed, [{response, {400, Response}} | Data]};
do_response(#mod{data = Data} = _ModData, {error, 404}) ->
	Response = "<h2>HTTP Error 404 - Not Found</h2>",
	{proceed, [{response, {404, Response}} | Data]};
do_response(#mod{data = Data} = _ModData, {error, 409}) ->
	Response = "<h2>HTTP Error 409 - Conflict</h2>",
	{proceed, [{response, {409, Response}} | Data]};
do_response(#mod{data = Data} = _ModData, {error, 412}) ->
	Response = "<h2>HTTP Error 412 - Precondition Failed</h2>",
	{proceed, [{response, {412, Response}} | Data]};
do_response(#mod{data = Data} = _ModData, {error, 422}) ->
	Response = "<h2>HTTP Error 422  - Unprocessable Entity</h2>",
	{proceed, [{response, {422, Response}} | Data]};
do_response(#mod{data = Data} = _ModData, {error, 500}) ->
	Response = "<h2>HTTP Error 500 - Server Error</h2>",
	{proceed, [{response, {500, Response}} | Data]}.

%% @hidden
send(#mod{socket = Socket, socket_type = SocketType} = Info,
		StatusCode, Headers, ResponseBody) ->
	httpd_response:send_header(Info, StatusCode, Headers),
	httpd_socket:deliver(SocketType, Socket, ResponseBody).

