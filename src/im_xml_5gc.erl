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
-module(im_xml_5gc).
-copyright('Copyright (c) 2020 SigScale Global Inc.').

%% export the im private API
-export([parse_amf/2, parse_ep_n2/2, parse_ep_n8/2, parse_ep_n11/2,
		parse_ep_n12/2, parse_ep_n14/2, parse_ep_n15/2, parse_ep_n17/2,
		parse_ep_n20/2, parse_ep_n22/2, parse_ep_n26/2, parse_ep_nls/2,
		parse_ep_nlg/2, parse_ep_sbi_x/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

-define(ResourcePath, "/resourceInventoryManagement/v3/resource/").

%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

%% @hidden
parse_amf({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_amf({startElement, _Uri, "EP_N2", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N2=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n2,
			parse_state = #ngc_state{ep_n2 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_amf({startElement, _Uri, "EP_N8", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N8=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n8,
			parse_state = #ngc_state{ep_n8 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_amf({startElement, _Uri, "EP_N11", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N11=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n11,
			parse_state = #ngc_state{ep_n11 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_amf({startElement, _Uri, "EP_N12", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N12=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n12,
			parse_state = #ngc_state{ep_n12 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_amf({startElement, _Uri, "EP_N14", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N14=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n14,
			parse_state = #ngc_state{ep_n14 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_amf({startElement, _Uri, "EP_N15", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N15=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n15,
			parse_state = #ngc_state{ep_n15 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_amf({startElement, _Uri, "EP_N17", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N17=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n17,
			parse_state = #ngc_state{ep_n17 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_amf({startElement, _Uri, "EP_N20", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N20=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n20,
			parse_state = #ngc_state{ep_n20 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_amf({startElement, _Uri, "EP_N22", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N22=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n22,
			parse_state = #ngc_state{ep_n22 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_amf({startElement, _Uri, "EP_N26", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N26=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n26,
			parse_state = #ngc_state{ep_n26 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_amf({startElement, _Uri, "EP_NLS", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_NLS=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_nls,
			parse_state = #ngc_state{ep_nls = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_amf({startElement, _Uri, "EP_NLG", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_NLG=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_nlg,
			parse_state = #ngc_state{ep_nlg = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_amf({startElement, _Uri, "EP_SBI_X", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_SBI_X=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_sbi_x,
			parse_state = #ngc_state{ep_sbi_x = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_amf({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_amf({endElement, _Uri, "AMFFunction", QName},
		[#state{parse_state =  #ngc_state{ep_n2s = EpN2Rels, ep_n8s = EpN8Rels,
		ep_n11s = EpN11Rels, ep_n12s = EpN12Rels, ep_n14s = EpN14Rels,
		ep_n15s = EpN15Rels, ep_n17s = EpN17Rels, ep_n20s = EpN20Rels,
		ep_n22s = EpN22Rels, ep_n26s = EpN26Rels, ep_nlss = EpNlsRels,
		ep_nlgs = EpNlgRels, ep_sbi_xs = EpSbiXRels},
		dn_prefix = [AmfDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "AMFFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	AmfAttr = parse_amf_attr(T2, undefined, []),
	Resource = #resource{name = AmfDn,
			description = "5G Core Access and Mobility Management Function (AMF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/AMFFunction",
			specification = Spec,
			characteristic = AmfAttr,
			related = EpN2Rels ++ EpN8Rels ++ EpN11Rels ++ EpN12Rels ++ EpN14Rels
					++ EpN15Rels ++ EpN17Rels ++ EpN20Rels ++ EpN22Rels ++ EpN26Rels
					++ EpNlsRels ++EpNlgRels ++EpSbiXRels,
			connection_point = EpN2Rels ++ EpN8Rels ++ EpN11Rels ++ EpN12Rels
					++ EpN14Rels ++ EpN15Rels ++ EpN17Rels ++ EpN20Rels ++ EpN22Rels
					++ EpN26Rels ++ EpNlsRels ++EpNlgRels ++EpSbiXRels},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_amf({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_amf_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_amf_attr1(Attributes, undefined, Acc).
% @hidden
parse_amf_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_amf_attr1(T2, undefined, Acc);
parse_amf_attr1([{endElement, {_, "pLMNIdList"} = QName} | T1],
		undefined, Acc) ->
	% @todo PLMNIdList
	{[_ | _PlmnIdList], T2} = pop(startElement, QName, T1),
	parse_amf_attr1(T2, undefined, Acc);
parse_amf_attr1([{endElement, {_, "aMFIdentifier"} = QName} | T1],
		undefined, Acc) ->
	% @todo aMFIdentifier
	{[_ | _AmfId], T2} = pop(startElement, QName, T1),
	parse_amf_attr1(T2, undefined, Acc);
parse_amf_attr1([{endElement, {_, "sBISerivceList"} = QName} | T1],
		undefined, Acc) ->
	% @todo SBIServiceList
	{[_ | _SbiSerivceList], T2} = pop(startElement, QName, T1),
	parse_amf_attr1(T2, undefined, Acc);
parse_amf_attr1([{endElement, {_, "WeightFactor"} = QName} | T1],
		undefined, Acc) ->
	% @todo WeightFactor
	{[_ | _WeightFactor], T2} = pop(startElement, QName, T1),
	parse_amf_attr1(T2, undefined, Acc);
parse_amf_attr1([{endElement, {_, "snssaiList"} = QName} | T1],
		undefined, Acc) ->
	% @todo SnssaiList
	{[_ | _SnssaiList], T2} = pop(startElement, QName, T1),
	parse_amf_attr1(T2, undefined, Acc);
parse_amf_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_amf_attr1(T, Attr, Acc);
parse_amf_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_amf_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_amf_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_amf_attr1(T, undefined, Acc);
parse_amf_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_ep_n2({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n2({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n2({endElement, _Uri, "EP_N2", QName},
		[#state{dn_prefix = [EpN2Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n2s = EpN2Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN2Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N2",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN2Dn,
			description = "5G Core End Point of N2 interface (between (R)AN and AMF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N2",
			specification = Spec,
			characteristic = EpN2Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN2Rel = #resource_rel{id = Id, name = EpN2Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n2s = [EpN2Rel | EpN2Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n2({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_n8({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n8({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n8({endElement, _Uri, "EP_N8", QName},
		[#state{dn_prefix = [EpN8Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n8s = EpN8Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN8Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N8",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN8Dn,
			description = "5G Core End Point of N8 interface (between AMF and UDM)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N8",
			specification = Spec,
			characteristic = EpN8Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN8Rel = #resource_rel{id = Id, name = EpN8Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n8s = [EpN8Rel | EpN8Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n8({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_n11({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n11({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n11({endElement, _Uri, "EP_N11", QName},
		[#state{dn_prefix = [EpN11Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n11s = EpN11Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN11Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N11",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN11Dn,
			description = "5G Core End Point of N11 interface"
					"(between AMF and SMF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N11",
			specification = Spec,
			characteristic = EpN11Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN11Rel = #resource_rel{id = Id, name = EpN11Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n11s = [EpN11Rel | EpN11Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n11({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_n12({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n12({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n12({endElement, _Uri, "EP_N12", QName},
		[#state{dn_prefix = [EpN12Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n12s = EpN12Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN12Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N12",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN12Dn,
			description = "5G Core End Point of N12 interface"
					"(between AMF and AUSF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N12",
			specification = Spec,
			characteristic = EpN12Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN12Rel = #resource_rel{id = Id, name = EpN12Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n12s = [EpN12Rel | EpN12Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n12({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_n14({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n14({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n14({endElement, _Uri, "EP_N14", QName},
		[#state{dn_prefix = [EpN14Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n14s = EpN14Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN14Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N14",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN14Dn,
			description = "5G Core End Point of N14 interface (between two AMFs)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N14",
			specification = Spec,
			characteristic = EpN14Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN14Rel = #resource_rel{id = Id, name = EpN14Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n14s = [EpN14Rel | EpN14Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n14({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_n15({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n15({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n15({endElement, _Uri, "EP_N15", QName},
		[#state{dn_prefix = [EpN15Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n15s = EpN15Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN15Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N15",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN15Dn,
			description = "5G Core End Point of N15 interface"
					"(between AMF and PCF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N15",
			specification = Spec,
			characteristic = EpN15Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN15Rel = #resource_rel{id = Id, name = EpN15Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n15s = [EpN15Rel | EpN15Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n15({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_n17({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n17({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n17({endElement, _Uri, "EP_N17", QName},
		[#state{dn_prefix = [EpN17Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n17s = EpN17Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN17Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N17",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN17Dn,
			description = "5G Core End Point of N17 interface"
					"(between AMF and 5G-EIR)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N17",
			specification = Spec,
			characteristic = EpN17Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN17Rel = #resource_rel{id = Id, name = EpN17Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n17s = [EpN17Rel | EpN17Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n17({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_n20({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n20({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n20({endElement, _Uri, "EP_N20", QName},
		[#state{dn_prefix = [EpN20Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n20s = EpN20Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN20Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N20",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN20Dn,
			description = "5G Core End Point of N20 interface"
					"(between AMF and SMSF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N20",
			specification = Spec,
			characteristic = EpN20Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN20Rel = #resource_rel{id = Id, name = EpN20Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n20s = [EpN20Rel | EpN20Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n20({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_n22({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n22({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n22({endElement, _Uri, "EP_N22", QName},
		[#state{dn_prefix = [EpN22Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n22s = EpN22Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN22Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N22",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN22Dn,
			description = "5G Core End Point of N22 interface"
					"(between AMF and NSSF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N22",
			specification = Spec,
			characteristic = EpN22Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN22Rel = #resource_rel{id = Id, name = EpN22Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n22s = [EpN22Rel | EpN22Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n22({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_n26({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n26({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n26({endElement, _Uri, "EP_N26", QName},
		[#state{dn_prefix = [EpN26Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n26s = EpN26Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN26Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N26",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN26Dn,
			description = "5G Core End Point of N26 interface"
					"(between AMF and MME)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N26",
			specification = Spec,
			characteristic = EpN26Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN26Rel = #resource_rel{id = Id, name = EpN26Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n26s = [EpN26Rel | EpN26Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n26({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_nls({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_nls({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_nls({endElement, _Uri, "EP_NLS", QName},
		[#state{dn_prefix = [EpNlsDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_nlss = EpNlsRels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpNlsAttr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_NLS",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpNlsDn,
			description = "5G Core End Point of NLs interface"
					"(between AMF and LMF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_NLS",
			specification = Spec,
			characteristic = EpNlsAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpNlsRel = #resource_rel{id = Id, name = EpNlsDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_nlss = [EpNlsRel | EpNlsRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_nls({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_nlg({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_nlg({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_nlg({endElement, _Uri, "EP_NLG", QName},
		[#state{dn_prefix = [EpNlgDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_nlgs = EpNlgRels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpNlgAttr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_NLG",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpNlgDn,
			description = "5G Core End Point of NLg interface"
					"(between AMF and GMLC)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_NLG",
			specification = Spec,
			characteristic = EpNlgAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpNlgRel = #resource_rel{id = Id, name = EpNlgDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_nlgs = [EpNlgRel | EpNlgRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_nlg({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_sbi_x({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_sbi_x({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_sbi_x({endElement, _Uri, "EP_SBI_X", QName},
		[#state{dn_prefix = [EpSbiXDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_sbi_xs = EpSbiXRels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpSbiXAttr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_SBI_X",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpSbiXDn,
			description = "5G Core End Point (EP) of"
					"Service Based Interface (SBI) X",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_SBI_X",
			specification = Spec,
			characteristic = EpSbiXAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpSbiXRel = #resource_rel{id = Id, name = EpSbiXDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_sbi_xs = [EpSbiXRel | EpSbiXRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_sbi_x({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_ep_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_ep_attr1(Attributes, undefined, Acc).
% @hidden
parse_ep_attr1([{endElement, {_, "localAddress"} = QName} | T1],
		undefined, Acc) ->
	% @todo IpEndPoint
	{[_ | _LocalAddress], T2} = pop(startElement, QName, T1),
	parse_ep_attr1(T2, undefined, Acc);
parse_ep_attr1([{endElement, {_, "remoteAddress"} = QName} | T1],
		undefined, Acc) ->
	% @todo IpEndPoint
	{[_ | _RemoteAddress], T2} = pop(startElement, QName, T1),
	parse_ep_attr1(T2, undefined, Acc);
parse_ep_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_ep_attr1(T, Attr, Acc);
parse_ep_attr1([{characters, Chars} | T], Attr, Acc) when is_list(Chars) ->
	parse_ep_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_ep_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_ep_attr1(T, undefined, Acc);
parse_ep_attr1([], undefined, Acc) ->
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
		Reason :: not_found | term().
%% @hidden
get_specification_ref(Name, Cache) ->
	case lists:keyfind(Name, #specification_ref.name, Cache) of
		#specification_ref{name = Name} = SpecRef ->
			{SpecRef, Cache};
		false ->
			case im:get_specification_name(Name) of
				{ok, #specification{id = Id, href = Href, name = Name,
						version = Version}} ->
					SpecRef = #specification_ref{id = Id, href = Href, name = Name,
							version = Version},
					{SpecRef, [SpecRef | Cache]};
				{error, Reason} ->
					throw({get_specification_name, Reason})
			end
	end.

