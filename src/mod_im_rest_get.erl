%%% mod_im_rest_get.erl
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
%%% @doc Handle received HTTP GET requests.
%%%
%%% 	This is an {@link //inets/httpd. httpd} callback module handling
%%% 	HTTP GET operations. The HTTP resources are managed in modules named
%%% 	`im_rest_res_*'.
%%%
%%% 	<h2><a name="callbacks">Resource Handler Functions</a></h2>
%%% 	The resource handler modules should implement callback functions
%%% 	in the pattern described in the example below.
%%%
%%% 	<h3 class="function">
%%% 		<a>get_&lt;Collection&gt;/2</a>
%%% 	</h3>
%%% 	<div class="spec">
%%% 		<p>
%%% 			<tt>get_&lt;Collection&gt;(Query, Headers) -&gt; Result</tt>
%%% 		</p>
%%% 		<ul class="definitions">
%%% 			<li><tt>Query = [{Param, Value}]</tt></li>
%%% 			<li><tt>Param = string()</tt></li>
%%% 			<li><tt>Value = string()</tt></li>
%%% 			<li><tt>Headers = [{Option, Value}</tt></li>
%%% 			<li><tt>Option = accept_ranges | allow | cache_control
%%% 					| content_MD5 | content_encoding | content_language
%%% 					| content_length | content_location | content_range
%%% 					| content_type | date | etag | expires | last_modified
%%% 					| location | pragma | retry_after | server | trailer
%%% 					| transfer_encoding</tt></li>
%%% 			<li><tt>Result = {ok, Headers, ResponseBody}
%%% 					| {error, StatusCode}
%%% 					| {error, StatusCode, Problem}
%%% 					| {error, StatusCode, ResponseHeaders, ResponseBody}}
%%% 			</tt></li>
%%% 			<li><tt>ResponseBody = io_list()</tt></li>
%%% 			<li><tt>StatusCode = 200..599</tt></li>
%%% 			<li><tt>ResponseHeaders = [{Option, Value}</tt></li>
%%% 			<li><tt>Problem = #{type := uri(), title := string(),
%%% 					code := string(), cause => string(), detail => string(),
%%% 					invalidParams => [#{param := string(), reason => string()}],
%%% 					status => 200..599}</tt></li>
%%% 		</ul>
%%% 	</div>
%%% 	Resource handlers for HTTP GET operations on REST Collections.
%%%
%%% 	Response `Headers' must include `content_type' if `ResponseBody' is
%%% 	not en empty list. An optional `Problem' report may be provided in
%%% 	error responses which shall be formatted by
%%% 	{@link //im/im_rest:format_problem/2. format_problem/2} and included
%%% 	in the response body.
%%%
%%% 	<h3 class="function">
%%% 		<a>get_&lt;Resource&gt;/2</a>
%%% 	</h3>
%%% 	<div class="spec">
%%% 		<p>
%%% 			<tt>get_&lt;Resource&gt;(Id, Query, [Headers, ...]) -&gt; Result</tt>
%%% 		</p>
%%% 		<ul class="definitions">
%%% 			<li><tt>Id = string()</tt></li>
%%% 			<li><tt>Query = [{Param, Value}]</tt></li>
%%% 			<li><tt>Param = string()</tt></li>
%%% 			<li><tt>Value = string()</tt></li>
%%% 			<li><tt>Headers = [{Option, Value}</tt></li>
%%% 			<li><tt>Option = accept_ranges | allow | cache_control
%%% 					| content_MD5 | content_encoding | content_language
%%% 					| content_length | content_location | content_range
%%% 					| content_type | date | etag | expires | last_modified
%%% 					| location | pragma | retry_after | server | trailer
%%% 					| transfer_encoding</tt></li>
%%% 			<li><tt>Result = {ok, Headers, ResponseBody}
%%% 					| {error, StatusCode}
%%% 					| {error, StatusCode, Problem}
%%% 					| {error, StatusCode, ResponseHeaders, ResponseBody}}
%%% 			</tt></li>
%%% 			<li><tt>ResponseBody = io_list()</tt></li>
%%% 			<li><tt>StatusCode = 200..599</tt></li>
%%% 			<li><tt>ResponseHeaders = [{Option, Value}</tt></li>
%%% 			<li><tt>Problem = #{type := uri(), title := string(),
%%% 					code := string(), cause => string(), detail => string(),
%%% 					invalidParams => [#{param := string(), reason => string()}],
%%% 					status => 200..599}</tt></li>
%%% 		</ul>
%%% 	</div>
%%% 	Resource handlers for HTTP GET operations on REST Resources.
%%%
%%% 	Response `Headers' must include `content_type' if `ResponseBody' is
%%% 	not en empty list. An optional `Problem' report may be provided in
%%% 	error responses which shall be formatted by
%%% 	{@link //im/im_rest:format_problem/2. format_problem/2} and included
%%% 	in the response body.
%%%
%%% @end
%%%
-module(mod_im_rest_get).
-copyright('Copyright (c) 2018 - 2021 SigScale Global Inc.').

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
	do1(ModData);
do(#mod{method = "HEAD"} = ModData) ->
	do1(ModData);
do(#mod{data = Data} = _ModData) ->
	{proceed, Data}.
%% @hidden
do1(#mod{request_uri = Uri, data = Data} = ModData) ->
	case proplists:get_value(status, Data) of
		{_StatusCode, _PhraseArgs, _Reason} ->
			{proceed, Data};
		undefined ->
			case proplists:get_value(response, Data) of
				undefined ->
					case lists:keyfind(resource, 1, Data) of
						false ->
							{proceed, Data};
						{_, Resource} ->
							parse_query(Resource, ModData, httpd_util:split_path(Uri))
					end;
				_Response ->
					{proceed,  Data}
			end
	end.

%% @hidden
parse_query(Resource, ModData, {Path, []}) ->
	do_get(Resource, ModData, string:tokens(Path, "/"), []);
parse_query(Resource, ModData, {Path, "?" ++ Query}) ->
	do_get(Resource, ModData, string:tokens(Path, "/"),
		im_rest:parse_query(uri_string:percent_decode(Query)));
parse_query(Resource, ModData, {Path, Query}) when is_list(Path),
		is_list(Query) ->
	do_get(Resource, ModData, string:tokens(Path, "/"),
		im_rest:parse_query(uri_string:percent_decode(Query)));
parse_query(_, #mod{parsed_header = RequestHeaders, data = Data} = ModData, _) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.4",
			title => "Not Found",
			detail => "No resource exists at the path provided",
			code => "", status => 404},
	{ContentType, ResponseBody} = im_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 404, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 404, Size}} | Data]}.

%% @hidden
do_get(Resource, #mod{parsed_header = Headers} = ModData,
		["partyManagement", "v2", "individual"], Query) ->
	do_response(ModData, Resource:get_users(Query, Headers));
do_get(Resource, ModData,
		["partyManagement", "v2", "individual", Id], Query) ->
	do_response(ModData, Resource:get_user(Id, Query));
do_get(Resource, ModData,
		["partyRoleManagement", "v4", "partyRole", Id], Query) ->
	do_response(ModData, Resource:get_role(Id, Query));
do_get(Resource, #mod{parsed_header = Headers} = ModData,
		["partyRoleManagement", "v4", "partyRole"], Query) ->
	do_response(ModData, Resource:get_roles(Query, Headers));
do_get(Resource, ModData, ["partyRoleManagement", "v4", "hub"], []) ->
	do_response(ModData, Resource:get_hubs());
do_get(Resource, ModData, ["partyRoleManagement", "v4", "hub", Id], []) ->
	do_response(ModData, Resource:get_hub(Id));
do_get(Resource, #mod{parsed_header = Headers, method = Method} = ModData,
		["resourceInventoryManagement", "v1", "logicalResource"], Query) ->
	do_response(ModData, Resource:get_rules(Method, Query, Headers));
do_get(Resource, #mod{parsed_header = Headers, method = Method} = ModData,
		["resourceCatalogManagement", "v4", "resourceCatalog"], Query) ->
	do_response(ModData, Resource:get_catalogs(Method, Query, Headers));
do_get(Resource, ModData,
		["resourceCatalogManagement", "v4", "resourceCatalog", Id], Query) ->
	do_response(ModData, Resource:get_catalog(Id, Query));
do_get(Resource, #mod{parsed_header = Headers, method = Method} = ModData,
		["resourceCatalogManagement", "v4", "resourceCategory"], Query) ->
	do_response(ModData, Resource:get_categories(Method, Query, Headers));
do_get(Resource, ModData,
		["resourceCatalogManagement", "v4", "resourceCategory", Id], Query) ->
	do_response(ModData, Resource:get_category(Id, Query));
do_get(Resource, #mod{parsed_header = Headers, method = Method} = ModData,
		["resourceCatalogManagement", "v4", "resourceCandidate"], Query) ->
	do_response(ModData, Resource:get_candidates(Method, Query, Headers));
do_get(Resource, ModData,
		["resourceCatalogManagement", "v4", "resourceCandidate", Id], Query) ->
	do_response(ModData, Resource:get_candidate(Id, Query));
do_get(Resource, #mod{parsed_header = Headers, method = Method} = ModData,
		["resourceCatalogManagement", "v4", "resourceSpecification"], Query) ->
	do_response(ModData, Resource:get_specifications(Method, Query, Headers));
do_get(Resource, ModData,
		["resourceCatalogManagement", "v4", "resourceSpecification", Id], Query) ->
	do_response(ModData, Resource:get_specification(Id, Query));
do_get(Resource, #mod{parsed_header = Headers, method = Method} = ModData,
		["resourceInventoryManagement", "v4", "resource"], Query) ->
   do_response(ModData, Resource:get_resources(Method, Query, Headers));
do_get(Resource, #mod{parsed_header = _Headers} = ModData,
		["resourceInventoryManagement", "v4", "resource", Id], Query) ->
   do_response(ModData, Resource:get_resource(Id, Query));
do_get(Resource, ModData, ["im", "v1", "log", "http"], []) ->
   do_response(ModData, Resource:get_http());
do_get(_, #mod{parsed_header = RequestHeaders, data = Data} = ModData, _, _) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.4",
			title => "Not Found",
			detail => "No resource exists at the path provided",
			code => "", status => 404},
	{ContentType, ResponseBody} = im_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 404, ResponseHeaders, ResponseBody),
	{proceed, [{response,{already_sent, 404, Size}} | Data]}.

%% @hidden
do_response(ModData, {ok, Headers, ResponseBody}) ->
	Size = integer_to_list(iolist_size(ResponseBody)),
	NewHeaders = Headers ++ [{content_length, Size}],
	send(ModData, 200, NewHeaders, ResponseBody),
	{proceed,[{response,{already_sent, 200, Size}}]};
do_response(#mod{parsed_header = RequestHeaders,
		data = Data} = ModData, {error, 400}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.1",
			title => "Bad Request",
			detail => "The server cannot or will not process the request"
					" due to something that is perceived to be a client error.",
			code => "", status => 400},
	{ContentType, ResponseBody} = im_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 400, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 400, Size}} | Data]};
do_response(#mod{parsed_header = RequestHeaders,
		data = Data} = ModData, {error, 404}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.5.4",
			title => "Not Found",
			detail => "No resource exists at the path provided",
			code => "", status => 404},
	{ContentType, ResponseBody} = im_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 404, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 404, Size}} | Data]};
do_response(#mod{parsed_header = RequestHeaders,
		data = Data} = ModData, {error, 412}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7232#section-4.2",
			title => "Precondition Failed",
			detail => "One or more conditions given in the request header"
					" fields evaluated to false",
			code => "", status => 412},
	{ContentType, ResponseBody} = im_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 412, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 412, Size}} | Data]};
do_response(#mod{parsed_header = RequestHeaders,
		data = Data} = ModData, {error, 416}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7233#section-4.4",
			title => "Range Not Satisfiable",
			detail => "None of the ranges in the request's Range header"
					" field overlap the current extent of the selected resource",
			code => "", status => 416},
	{ContentType, ResponseBody} = im_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 416, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 416, Size}} | Data]};
do_response(#mod{parsed_header = RequestHeaders,
		data = Data} = ModData, {error, 500}) ->
	Problem = #{type => "https://datatracker.ietf.org/doc/html/rfc7231#section-6.6.1",
			title => "Internal Server Error",
			detail => "The server encountered an unexpected condition that"
					" prevented it from fulfilling the request.",
			code => "", status => 500},
	{ContentType, ResponseBody} = im_rest:format_problem(Problem, RequestHeaders),
	Size = integer_to_list(iolist_size(ResponseBody)),
	ResponseHeaders = [{content_length, Size}, {content_type, ContentType}],
	send(ModData, 500, ResponseHeaders, ResponseBody),
	{proceed, [{response, {already_sent, 500, Size}} | Data]}.

%% @hidden
send(#mod{socket = Socket, socket_type = SocketType} = ModData,
		StatusCode, Headers, ResponseBody) ->
	httpd_response:send_header(ModData, StatusCode, Headers),
	httpd_socket:deliver(SocketType, Socket, ResponseBody).

