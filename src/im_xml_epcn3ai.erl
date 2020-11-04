%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This library module implements the public API for the
%%%   {@link //sigscale_im. sigscale_im} application.
%%%
-module(im_xml_epcn3ai).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% export the im private API
-export([parse_server/2, parse_proxy/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

-define(PathCatalogSchema, "/resourceCatalogManagement/v4/schema").
-define(PathInventorySchema, "/resourceInventoryManagement/v4/schema").

%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

parse_proxy({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_proxy({startElement,  _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_proxy({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_proxy({endElement, Uri, "_3GPPAAAProxyFunction",
		{Prefix, "_3GPPAAAProxyFunction"}}, State) ->
	parse_proxy({endElement, Uri, "3GPPAAAProxyFunction",
			{Prefix, "3GPPAAAProxyFunction"}}, State);
parse_proxy({endElement, _Uri, "3GPPAAAProxyFunction", QName},
		[#state{dn_prefix = [ProxyDn | _], stack = Stack, spec_cache = Cache,
		location = Location}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	ProxyAttr = parse_proxy_attr(T2, undefined, []),
	ClassType = "3GPPAAAProxyFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = ProxyDn,
			description = "EPCN3AI 3GPP Authentication, Authorization and Accounting Proxy",
			category = "EPCN3AI",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/3GPPAAAProxyFunction",
			specification = Spec,
			characteristic = lists:reverse([PeeParam | ProxyAttr])},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_proxy({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

parse_proxy_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_proxy_attr1(Attributes, undefined, Acc).
%% @hidden
parse_proxy_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfParameterList], T2} = pop(startElement, QName, T1),
	parse_proxy_attr1(T2, undefined, Acc);
parse_proxy_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_proxy_attr1(T, Attr, Acc);
parse_proxy_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_proxy_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_proxy_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_proxy_attr1(T, undefined, Acc);
parse_proxy_attr1([], undefined, Acc) ->
	Acc.

parse_server({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_server({startElement,  _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_server({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_server({endElement, _Uri, "_3GPPAAAServerFunction",
		{Prefix, "_3GPPAAAServerFunction"}}, State) ->
	parse_server({endElement, _Uri, "3GPPAAAServerFunction",
			{Prefix, "3GPPAAAServerFunction"}}, State);
parse_server({endElement, _Uri, "AaaFunction", {Prefix, "AaaFunction"}}, State) ->
	parse_server({endElement, _Uri, "3GPPAAAServerFunction",
			{Prefix, "3GPPAAAServerFunction"}}, State);
parse_server({endElement, _Uri, "3GPPAAAServerFunction", QName},
		[#state{dn_prefix = [ServerDn | _], stack = Stack, spec_cache = Cache,
		location = Location}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	ServerAttr = parse_server_attr(T2, undefined, []),
	ClassType = "3GPPAAAServerFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = ServerDn,
			description = "EPCN3AI 3GPP Authentication, Authorization and Accounting Server",
			category = "EPCN3AI",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/3GPPAAAServerFunction",
			specification = Spec,
			characteristic = lists:reverse([PeeParam | ServerAttr])},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_server({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

parse_server_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_server_attr1(Attributes, undefined, Acc).
%% @hidden
parse_server_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfParameterList], T2} = pop(startElement, QName, T1),
	parse_server_attr1(T2, undefined, Acc);
parse_server_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_server_attr1(T, Attr, Acc);
parse_server_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_server_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_server_attr1([{characters, _Chars} | T], Attr, Acc) ->
	parse_server_attr1(T, Attr, Acc);
parse_server_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_server_attr1(T, undefined, Acc);
parse_server_attr1([], undefined, Acc) ->
	Acc.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-type event() :: {startElement,
		QName :: {Prefix :: string(), LocalName :: string()},
		Attributes :: [tuple()]} | {endElement,
		QName :: {Prefix :: string(), LocalName :: string()}}
		| {characters, string()}.
-spec pop(Element, QName, Stack) -> Result
	when
		Element :: startElement | endElement,
		QName :: {Prefix, LocalName},
		Prefix :: string(),
		LocalName :: string(),
		Stack :: [event()],
		Result :: {Value, NewStack},
		Value :: [event()],
		NewStack :: [event()].
%% @doc Pops all events up to an including `{Element, QName, ...}'.
%% @private
pop(Element, QName, Stack) ->
	pop(Element, QName, Stack, []).
%% @hidden
pop(Element, QName, [H | T], Acc)
		when element(1, H) == Element, element(2, H) == QName->
	{[H | Acc], T};
pop(Element, QName, [H | T], Acc) ->
	pop(Element, QName, T, [H | Acc]).

-spec get_specification_ref(Name, Cache) -> Result
	when
		Name :: string(),
		Cache :: [SpecRef],
		Result :: {SpecRef, Cache} | {error, Reason},
		SpecRef :: specification_ref(),
		Reason :: term().
%% @hidden
get_specification_ref(Name, Cache) ->
	case lists:keyfind(Name, #specification_ref.name, Cache) of
		#specification_ref{name = Name} = SpecRef ->
			{SpecRef, Cache};
		false ->
			case im:get_specification_name(Name) of
				{ok, #specification{id = Id, href = Href,
						name = Name, class_type = Type, version = Version}} ->
					SpecRef = #specification_ref{id = Id, href = Href,
							name = Name, ref_type = Type, version = Version},
					{SpecRef, [SpecRef | Cache]};
				{error, Reason} ->
					throw({get_specification_name, Reason})
			end
	end.
