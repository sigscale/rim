%%% mod_im_rest_accepted_content.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018 - 2021 SigScale Global Inc.
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
%%% @doc Handle media type validation in HTTP requests.
%%%
%%% 	This is an {@link //inets/httpd. httpd} callback module handling
%%% 	media type validation of HTTP operations. The HTTP resources are
%%% 	managed in modules named `im_rest_res_*'.
%%%
%%% 	<h2><a name="callbacks">Resource Handler Functions</a></h2>
%%% 	The resource handler modules should implement the following
%%% 	callback functions.
%%%
%%% 	<h3 class="function">
%%% 		<a>content_types_provided/0</a>
%%% 	</h3>
%%% 	<div class="spec">
%%% 		<p>
%%% 			<tt>content_types_provided() -&gt; ContentTypes</tt>
%%% 		</p>
%%% 		<ul class="definitions">
%%% 			<li><tt>ContentTypes = [ContentType]</tt></li>
%%% 			<li><tt>ContentType = string()</tt></li>
%%% 		</ul>
%%% 	</div>
%%% 	Provides the list of possible media types in response bodies
%%% 	provided by functions in the resource handler module.
%%%
%%% 	<h3 class="function">
%%% 		<a>content_types_accepted/0</a>
%%% 	</h3>
%%% 	<div class="spec">
%%% 		<p>
%%% 			<tt>content_types_accepted() -&gt; ContentTypes</tt>
%%% 		</p>
%%% 		<ul class="definitions">
%%% 			<li><tt>ContentTypes = [ContentType]</tt></li>
%%% 			<li><tt>ContentType = string()</tt></li>
%%% 		</ul>
%%% 	</div>
%%% 	Provides the list of possible media types accepted in bodies
%%% 	provided to functions in the resource handler module.
%%%
%%% @end
%%%
-module(mod_im_rest_accepted_content).
-copyright('Copyright (c) 2018 - 2021 SigScale Global Inc.').

-export([do/1]).

-include_lib("inets/include/httpd.hrl").

-spec do(ModData) -> Result when
	ModData :: #mod{},
	Result :: {proceed, OldData} | {proceed, NewData} | {break, NewData} | done,
	OldData :: list(),
	NewData :: [{response, {StatusCode, Body}}]
			| [{response, {response, Head,Body}}]
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
% % @doc Erlang web server API callback function.
do(#mod{request_uri = Uri, data = Data} = ModData) ->
	case proplists:get_value(status, Data) of
		{_StatusCode, _PhraseArgs, _Reason} ->
			{proceed, Data};
		undefined ->
			case proplists:get_value(response, Data) of
				undefined ->
					Path = uri_string:percent_decode(Uri),
					case string:tokens(Path, "/?") of
						["partyManagement", "v2", "individual"] ->
							check_content_type_header(im_rest_res_user, ModData);
						["partyManagement", "v2", "individual", _Id] ->
							check_content_type_header(im_rest_res_user, ModData);
						["partyRoleManagement", "v4", "partyRole" | _] ->
							check_content_type_header(im_rest_res_role, ModData);
						["partyRoleManagement", "v4", "hub" | _] ->
							check_content_type_header(im_rest_hub_role, ModData);
						["resourceCatalogManagement", "v4", "resourceCatalog" | _] ->
                     check_content_type_header(im_rest_res_catalog, ModData);
						["resourceCatalogManagement", "v4", "resourceCategory" | _] ->
                     check_content_type_header(im_rest_res_category, ModData);
						["resourceCatalogManagement", "v4", "resourceCandidate" | _] ->
                     check_content_type_header(im_rest_res_candidate, ModData);
						["resourceCatalogManagement", "v4", "resourceSpecification" | _] ->
                     check_content_type_header(im_rest_res_specification, ModData);
						["resourceInventoryManagement", "v4", "resource" | _] ->
                     check_content_type_header(im_rest_res_resource, ModData);
						["resourceInventoryManagement", "v4", "logicalResource"] ->
                     check_content_type_header(im_rest_res_rules, ModData);
						["resourceInventoryManagement", "v4", "logicalResource" | _] ->
                     check_content_type_header(im_rest_res_rules, ModData);
						["im", "v1", "log", "http"] ->
							check_content_type_header(im_rest_res_http, ModData);
						["health"] ->
							check_content_type_header(im_rest_res_health, ModData);
						["health", "application" | _] ->
							check_content_type_header(im_rest_res_health, ModData);
						_ ->
							{proceed, Data}
					end;
				_ ->
					{proceed,  Data}
			end
	end.

%% @hidden
check_content_type_header(Module, #mod{method = Method,
		parsed_header = RequestHeaders, data = Data} = ModData) ->
	case lists:keyfind("content-type", 1, RequestHeaders) of
		false when Method == "DELETE"; Method == "GET"; Method == "HEAD" ->
			check_accept_header(Module,
					ModData#mod{data = [{resource, Module} | Data]});
		{_Other, []} when Method == "DELETE"; Method == "GET"; Method == "HEAD" ->
			check_accept_header(Module,
					ModData#mod{data = [{resource, Module} | Data]});
		{_Other, RequestContentType} ->
			F = fun(AcceptedType) ->
					lists:prefix(AcceptedType, RequestContentType)
			end,
			case lists:any(F, Module:content_types_accepted()) of
				true ->
					check_accept_header(Module,
							ModData#mod{data = [{resource, Module},
									{content_type,  RequestContentType} | Data]});
				false ->
					Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.13",
							title => "Unsupported Media Type",
							detail => "The client provided Content-Type which the"
									" the server does not support.",
							code => "", status => 415},
					{ResponseContentType, ResponseBody}
							= im_rest:format_problem(Problem, RequestHeaders),
					Size = integer_to_list(iolist_size(ResponseBody)),
					ResponseHeaders = [{content_length, Size},
							{content_type, ResponseContentType}],
					send(ModData, 415, ResponseHeaders, ResponseBody),
					{proceed, [{response, {already_sent, 415, Size}} | Data]}
			end;
		false ->
			Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.1",
					title => "Bad Request",
					detail => "The client failed to provide a Content-Type header",
					code => "", status => 400},
			{ResponseContentType, ResponseBody}
					= im_rest:format_problem(Problem, RequestHeaders),
			Size = integer_to_list(iolist_size(ResponseBody)),
			ResponseHeaders = [{content_length, Size},
					{content_type, ResponseContentType}],
			send(ModData, 415, ResponseHeaders, ResponseBody),
			{proceed, [{response, {already_sent, 400, Size}} | Data]}
	end.

%% @hidden
check_accept_header(Module,
		#mod{parsed_header = RequestHeaders, data = Data} = ModData) ->
	case lists:keyfind("accept", 1, RequestHeaders) of
		{_Other, Accept} ->
			AcceptTypes = string:tokens(Accept, ", "),
			case lists:member("*/*", AcceptTypes) of
				false ->
					F1 = fun(Representation) ->
							F2 = fun(AcceptType) ->
									lists:prefix(Representation, AcceptType)
							end,
							lists:any(F2, AcceptTypes)
					end,
					case lists:any(F1, Module:content_types_provided()) of
						true ->
							{proceed, [{accept, AcceptTypes} | Data]};
						false ->
							Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.13",
									title => "Unsupported Media Type",
									detail => "The client provided an Accept header"
											" which is missing the required content type.",
									code => "", status => 415},
							{ContentType, ResponseBody}
									= im_rest:format_problem(Problem, RequestHeaders),
							Size = integer_to_list(iolist_size(ResponseBody)),
							ResponseHeaders = [{content_length, Size},
									{content_type, ContentType}],
							send(ModData, 415, ResponseHeaders, ResponseBody),
							{proceed, [{response, {already_sent, 415, Size}} | Data]}
					end;
				true ->
					{proceed, Data}
			end;
		false ->
			{proceed, Data}
	end.

%% @hidden
send(#mod{socket = Socket, socket_type = SocketType} = ModData,
		StatusCode, Headers, ResponseBody) ->
	httpd_response:send_header(ModData, StatusCode, Headers),
	httpd_socket:deliver(SocketType, Socket, ResponseBody).
