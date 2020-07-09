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
-export([parse_amf/2, parse_smf/2, parse_upf/2, parse_n3iwf/2, parse_pcf/2,
		parse_ausf/2,
		parse_ep_n2/2, parse_ep_n3/2, parse_ep_n4/2, parse_ep_n5/2, parse_ep_n6/2,
		parse_ep_n7/2, parse_ep_n8/2, parse_ep_n9/2, parse_ep_n10/2,
		parse_ep_n11/2, parse_ep_n16/2, parse_ep_n12/2, parse_ep_n13/2,
		parse_ep_n14/2, parse_ep_n15/2, parse_ep_n17/2,
		parse_ep_n20/2, parse_ep_n22/2, parse_ep_n26/2,
		parse_ep_nls/2, parse_ep_nlg/2, parse_ep_sbi_x/2, parse_ep_s5c/2,
		parse_ep_s5u/2, parse_ep_rx/2]).

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
parse_smf({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_smf({startElement, _Uri, "EP_N4", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N4=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n4,
			parse_state = #ngc_state{ep_n4 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_smf({startElement, _Uri, "EP_N7", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N7=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n7,
			parse_state = #ngc_state{ep_n7 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_smf({startElement, _Uri, "EP_N10", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N10=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n10,
			parse_state = #ngc_state{ep_n10 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_smf({startElement, _Uri, "EP_N11", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N11=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n11,
			parse_state = #ngc_state{ep_n11 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_smf({startElement, _Uri, "EP_N16", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N16=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n16,
			parse_state = #ngc_state{ep_n16 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_smf({startElement, _Uri, "EP_S5C", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_S5C=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_s5c,
			parse_state = #ngc_state{ep_s5c = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_smf({startElement, _Uri, "EP_SBI_X", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_SBI_X=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_sbi_x,
			parse_state = #ngc_state{ep_sbi_x = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_smf({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_smf({endElement, _Uri, "SMFFunction", QName},
		[#state{parse_state =  #ngc_state{ep_n4s = EpN4Rels, ep_n7s = EpN7Rels,
		ep_n10s = EpN10Rels, ep_n11s = EpN11Rels, ep_n16s = EpN16Rels,
		ep_s5cs = EpS5cRels, ep_sbi_xs = EpSbiXRels},
		dn_prefix = [SmfDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "SMFFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	SmfAttr = parse_smf_attr(T2, undefined, []),
	Resource = #resource{name = SmfDn,
			description = "5G Core Session Management Function (SMF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/SMFFunction",
			specification = Spec,
			characteristic = SmfAttr,
			related = EpN4Rels ++ EpN7Rels ++ EpN10Rels ++ EpN11Rels ++ EpN16Rels
					++ EpS5cRels ++ EpSbiXRels,
			connection_point = EpN4Rels ++ EpN7Rels ++ EpN10Rels ++ EpN11Rels
					++ EpN16Rels ++ EpS5cRels ++ EpSbiXRels},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_smf({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_smf_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_smf_attr1(Attributes, undefined, Acc).
% @hidden
parse_smf_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_smf_attr1(T2, undefined, Acc);
parse_smf_attr1([{endElement, {_, "pLMNIdList"} = QName} | T1],
		undefined, Acc) ->
	% @todo PLMNIdList
	{[_ | _PlmnIdList], T2} = pop(startElement, QName, T1),
	parse_smf_attr1(T2, undefined, Acc);
parse_smf_attr1([{endElement, {_, "nRTACList"} = QName} | T1],
		undefined, Acc) ->
	% @todo NrTACList
	{[_ | _NrTACList], T2} = pop(startElement, QName, T1),
	parse_smf_attr1(T2, undefined, Acc);
parse_smf_attr1([{endElement, {_, "sBISerivceList"} = QName} | T1],
		undefined, Acc) ->
	% @todo SBIServiceList
	{[_ | _SbiSerivceList], T2} = pop(startElement, QName, T1),
	parse_smf_attr1(T2, undefined, Acc);
parse_smf_attr1([{endElement, {_, "snssaiList"} = QName} | T1],
		undefined, Acc) ->
	% @todo SnssaiList
	{[_ | _SnssaiList], T2} = pop(startElement, QName, T1),
	parse_smf_attr1(T2, undefined, Acc);
parse_smf_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_smf_attr1(T, Attr, Acc);
parse_smf_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_smf_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_smf_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_smf_attr1(T, undefined, Acc);
parse_smf_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_upf({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_upf({startElement, _Uri, "EP_N3", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N3=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n3,
			parse_state = #ngc_state{ep_n3 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_upf({startElement, _Uri, "EP_N4", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N4=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n4,
			parse_state = #ngc_state{ep_n4 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_upf({startElement, _Uri, "EP_N6", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N6=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n6,
			parse_state = #ngc_state{ep_n6 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_upf({startElement, _Uri, "EP_N9", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N9=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n9,
			parse_state = #ngc_state{ep_n9 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_upf({startElement, _Uri, "EP_S5U", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_S5U=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_s5u,
			parse_state = #ngc_state{ep_s5u = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_upf({startElement, _Uri, "EP_SBI_X", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_SBI_X=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_sbi_x,
			parse_state = #ngc_state{ep_sbi_x = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_upf({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_upf({endElement, _Uri, "UPFFunction", QName},
		[#state{parse_state =  #ngc_state{ep_n3s = EpN3Rels, ep_n4s = EpN4Rels,
		ep_n6s = EpN6Rels, ep_n9s = EpN9Rels,
		ep_s5us = EpS5uRels, ep_sbi_xs = EpSbiXRels},
		dn_prefix = [UpfDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "UPFFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	UpfAttr = parse_upf_attr(T2, undefined, []),
	Resource = #resource{name = UpfDn,
			description = "5G Core User Plane Function (UPF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/UPFFunction",
			specification = Spec,
			characteristic = UpfAttr,
			related = EpN3Rels ++ EpN4Rels ++ EpN6Rels ++ EpN9Rels
					++ EpS5uRels ++ EpSbiXRels,
			connection_point = EpN3Rels ++ EpN4Rels ++ EpN6Rels ++ EpN9Rels
					++ EpS5uRels ++ EpSbiXRels},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_upf({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_upf_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_upf_attr1(Attributes, undefined, Acc).
% @hidden
parse_upf_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_upf_attr1(T2, undefined, Acc);
parse_upf_attr1([{endElement, {_, "pLMNIdList"} = QName} | T1],
		undefined, Acc) ->
	% @todo PLMNIdList
	{[_ | _PlmnIdList], T2} = pop(startElement, QName, T1),
	parse_upf_attr1(T2, undefined, Acc);
parse_upf_attr1([{endElement, {_, "nRTACList"} = QName} | T1],
		undefined, Acc) ->
	% @todo NrTACList
	{[_ | _NrTACList], T2} = pop(startElement, QName, T1),
	parse_upf_attr1(T2, undefined, Acc);
parse_upf_attr1([{endElement, {_, "snssaiList"} = QName} | T1],
		undefined, Acc) ->
	% @todo SnssaiList
	{[_ | _SnssaiList], T2} = pop(startElement, QName, T1),
	parse_upf_attr1(T2, undefined, Acc);
parse_upf_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_upf_attr1(T, Attr, Acc);
parse_upf_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_upf_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_upf_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_upf_attr1(T, undefined, Acc);
parse_upf_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_n3iwf({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_n3iwf({startElement, _Uri, "EP_N2", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N2=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n2,
			parse_state = #ngc_state{ep_n2 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_n3iwf({startElement, _Uri, "EP_N3", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N3=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n3,
			parse_state = #ngc_state{ep_n3 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_n3iwf({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_n3iwf({endElement, _Uri, "N3IWFFunction", QName},
		[#state{parse_state =  #ngc_state{ep_n2s = EpN2Rels, ep_n3s = EpN3Rels},
		dn_prefix = [N3iwfDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "N3IWFFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	N3iwfAttr = parse_n3iwf_attr(T2, undefined, []),
	Resource = #resource{name = N3iwfDn,
			description = "5G Core Non 3GPP Inter Working Function (N3IWF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/N3IWFFunction",
			specification = Spec,
			characteristic = N3iwfAttr,
			related = EpN2Rels ++ EpN3Rels,
			connection_point = EpN2Rels ++ EpN3Rels},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_n3iwf({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_n3iwf_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_n3iwf_attr1(Attributes, undefined, Acc).
% @hidden
parse_n3iwf_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_n3iwf_attr1(T2, undefined, Acc);
parse_n3iwf_attr1([{endElement, {_, "pLMNIdList"} = QName} | T1],
		undefined, Acc) ->
	% @todo PLMNIdList
	{[_ | _PlmnIdList], T2} = pop(startElement, QName, T1),
	parse_n3iwf_attr1(T2, undefined, Acc);
parse_n3iwf_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_n3iwf_attr1(T, Attr, Acc);
parse_n3iwf_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_n3iwf_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_n3iwf_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_n3iwf_attr1(T, undefined, Acc);
parse_n3iwf_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_pcf({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_pcf({startElement, _Uri, "EP_N5", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N5=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n5,
			parse_state = #ngc_state{ep_n5 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_pcf({startElement, _Uri, "EP_N7", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N7=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n7,
			parse_state = #ngc_state{ep_n7 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_pcf({startElement, _Uri, "EP_N15", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N15=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n15,
			parse_state = #ngc_state{ep_n15 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_pcf({startElement, _Uri, "EP_N16", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N16=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n16,
			parse_state = #ngc_state{ep_n16 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_pcf({startElement, _Uri, "EP_Rx", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_Rx=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_rx,
			parse_state = #ngc_state{ep_rx = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_pcf({startElement, _Uri, "EP_SBI_X", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_SBI_X=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_sbi_x,
			parse_state = #ngc_state{ep_sbi_x = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_pcf({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_pcf({endElement, _Uri, "PCFFunction", QName},
		[#state{parse_state =  #ngc_state{ep_n5s = EpN5Rels, ep_n7s = EpN7Rels,
		ep_n15s = EpN15Rels, ep_n16s = EpN16Rels,
		ep_rxs = EpRxRels, ep_sbi_xs = EpSbiXRels},
		dn_prefix = [PcfDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "PCFFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	PcfAttr = parse_pcf_attr(T2, undefined, []),
	Resource = #resource{name = PcfDn,
			description = "5G Core Policy Control Function (PCF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/PCFFunction",
			specification = Spec,
			characteristic = PcfAttr,
			related = EpN5Rels ++ EpN7Rels ++ EpN15Rels ++ EpN16Rels
					++ EpRxRels ++ EpSbiXRels,
			connection_point = EpN5Rels ++ EpN7Rels ++ EpN15Rels ++ EpN16Rels
					++ EpRxRels ++ EpSbiXRels},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_pcf({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_pcf_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_pcf_attr1(Attributes, undefined, Acc).
% @hidden
parse_pcf_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_pcf_attr1(T2, undefined, Acc);
parse_pcf_attr1([{endElement, {_, "pLMNIdList"} = QName} | T1],
		undefined, Acc) ->
	% @todo PLMNIdList
	{[_ | _PlmnIdList], T2} = pop(startElement, QName, T1),
	parse_pcf_attr1(T2, undefined, Acc);
parse_pcf_attr1([{endElement, {_, "sBISerivceList"} = QName} | T1],
		undefined, Acc) ->
	% @todo SBIServiceList
	{[_ | _SBIServiceList], T2} = pop(startElement, QName, T1),
	parse_pcf_attr1(T2, undefined, Acc);
parse_pcf_attr1([{endElement, {_, "snssaiList"} = QName} | T1],
		undefined, Acc) ->
	% @todo SnssaiList
	{[_ | _SnssaiList], T2} = pop(startElement, QName, T1),
	parse_pcf_attr1(T2, undefined, Acc);
parse_pcf_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_pcf_attr1(T, Attr, Acc);
parse_pcf_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_pcf_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_pcf_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_pcf_attr1(T, undefined, Acc);
parse_pcf_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_ausf({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ausf({startElement, _Uri, "EP_N12", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N12=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n12,
			parse_state = #ngc_state{ep_n12 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_ausf({startElement, _Uri, "EP_N13", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_N13=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_n13,
			parse_state = #ngc_state{ep_n13 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_ausf({startElement, _Uri, "EP_SBI_X", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_SBI_X=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_5gc, parse_function = parse_ep_sbi_x,
			parse_state = #ngc_state{ep_sbi_x = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_ausf({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ausf({endElement, _Uri, "AUSFFunction", QName},
		[#state{parse_state =  #ngc_state{ep_n12s = EpN12Rels,
		ep_n13s = EpN13Rels, ep_sbi_xs = EpSbiXRels},
		dn_prefix = [AusfDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "AUSFFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	AusfAttr = parse_ausf_attr(T2, undefined, []),
	Resource = #resource{name = AusfDn,
			description = "5G Core Authentication Server Function (AUSF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/AUSFFunction",
			specification = Spec,
			characteristic = AusfAttr,
			related = EpN12Rels ++ EpN13Rels ++ EpSbiXRels,
			connection_point = EpN12Rels ++ EpN13Rels ++ EpSbiXRels},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ausf({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_ausf_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_ausf_attr1(Attributes, undefined, Acc).
% @hidden
parse_ausf_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_ausf_attr1(T2, undefined, Acc);
parse_ausf_attr1([{endElement, {_, "pLMNIdList"} = QName} | T1],
		undefined, Acc) ->
	% @todo PLMNIdList
	{[_ | _PlmnIdList], T2} = pop(startElement, QName, T1),
	parse_ausf_attr1(T2, undefined, Acc);
parse_ausf_attr1([{endElement, {_, "sBISerivceList"} = QName} | T1],
		undefined, Acc) ->
	% @todo SBIServiceList
	{[_ | _SBIServiceList], T2} = pop(startElement, QName, T1),
	parse_ausf_attr1(T2, undefined, Acc);
parse_ausf_attr1([{endElement, {_, "snssaiList"} = QName} | T1],
		undefined, Acc) ->
	% @todo SnssaiList
	{[_ | _SnssaiList], T2} = pop(startElement, QName, T1),
	parse_ausf_attr1(T2, undefined, Acc);
parse_ausf_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_ausf_attr1(T, Attr, Acc);
parse_ausf_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_ausf_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_ausf_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_ausf_attr1(T, undefined, Acc);
parse_ausf_attr1([], undefined, Acc) ->
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
parse_ep_n3({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n3({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n3({endElement, _Uri, "EP_N3", QName},
		[#state{dn_prefix = [EpN3Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n3s = EpN3Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN3Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N3",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN3Dn,
			description = "5G Core End Point of N3 interface"
					"(between (R)AN and UPF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N3",
			specification = Spec,
			characteristic = EpN3Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN3Rel = #resource_rel{id = Id, name = EpN3Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n3s = [EpN3Rel | EpN3Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n3({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_n4({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n4({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n4({endElement, _Uri, "EP_N4", QName},
		[#state{dn_prefix = [EpN4Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n4s = EpN4Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN4Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N4",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN4Dn,
			description = "5G Core End Point of N4 interface"
					"(between SMF and UPF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N4",
			specification = Spec,
			characteristic = EpN4Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN4Rel = #resource_rel{id = Id, name = EpN4Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n4s = [EpN4Rel | EpN4Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n4({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_n5({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n5({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n5({endElement, _Uri, "EP_N5", QName},
		[#state{dn_prefix = [EpN5Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n5s = EpN5Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN5Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N5",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN5Dn,
			description = "5G Core End Point of N5 interface (between PCF and AF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N5",
			specification = Spec,
			characteristic = EpN5Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN5Rel = #resource_rel{id = Id, name = EpN5Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n5s = [EpN5Rel | EpN5Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n5({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_n6({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n6({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n6({endElement, _Uri, "EP_N6", QName},
		[#state{dn_prefix = [EpN6Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n6s = EpN6Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN6Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N6",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN6Dn,
			description = "5G Core End Point of N6 interface (between UPF and DN)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N6",
			specification = Spec,
			characteristic = EpN6Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN6Rel = #resource_rel{id = Id, name = EpN6Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n6s = [EpN6Rel | EpN6Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n6({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_n7({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n7({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n7({endElement, _Uri, "EP_N7", QName},
		[#state{dn_prefix = [EpN7Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n7s = EpN7Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN7Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N7",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN7Dn,
			description = "5G Core End Point of N7 interface"
					"(between SMF and PCF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N7",
			specification = Spec,
			characteristic = EpN7Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN7Rel = #resource_rel{id = Id, name = EpN7Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n7s = [EpN7Rel | EpN7Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n7({endElement, _Uri, _LocalName, QName},
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
parse_ep_n9({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n9({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n9({endElement, _Uri, "EP_N9", QName},
		[#state{dn_prefix = [EpN9Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n8s = EpN9Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN9Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N9",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN9Dn,
			description = "5G Core End Point of N9 interface (between two UPFs)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N9",
			specification = Spec,
			characteristic = EpN9Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN9Rel = #resource_rel{id = Id, name = EpN9Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n9s = [EpN9Rel | EpN9Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n9({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_n10({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n10({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n10({endElement, _Uri, "EP_N10", QName},
		[#state{dn_prefix = [EpN10Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n10s = EpN10Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN10Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N10",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN10Dn,
			description = "5G Core End Point of N10 interface"
					"(between SMF and UDM)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N10",
			specification = Spec,
			characteristic = EpN10Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN10Rel = #resource_rel{id = Id, name = EpN10Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n10s = [EpN10Rel | EpN10Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n10({endElement, _Uri, _LocalName, QName},
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
parse_ep_n13({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n13({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n13({endElement, _Uri, "EP_N13", QName},
		[#state{dn_prefix = [EpN13Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n13s = EpN13Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN13Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N13",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN13Dn,
			description = "5G Core End Point of N13 interface"
					"(between AUSF and UDM)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N13",
			specification = Spec,
			characteristic = EpN13Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN13Rel = #resource_rel{id = Id, name = EpN13Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n13s = [EpN13Rel | EpN13Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n13({endElement, _Uri, _LocalName, QName},
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
parse_ep_n16({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_n16({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_n16({endElement, _Uri, "EP_N16", QName},
		[#state{dn_prefix = [EpN16Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_n16s = EpN16Rels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpN16Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_N16",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpN16Dn,
			description = "5G Core End Point of N16 interface (between two SMFs)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_N16",
			specification = Spec,
			characteristic = EpN16Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpN16Rel = #resource_rel{id = Id, name = EpN16Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_n16s = [EpN16Rel | EpN16Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_n16({endElement, _Uri, _LocalName, QName},
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

%% @hidden
parse_ep_s5c({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_s5c({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_s5c({endElement, _Uri, "EP_S5C", QName},
		[#state{dn_prefix = [EpS5cDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_s5cs = EpS5cRels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpS5cAttr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_S5C",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpS5cDn,
			description = "5G Core End Point of S5-C interface",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_S5C",
			specification = Spec,
			characteristic = EpS5cAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpS5cRel = #resource_rel{id = Id, name = EpS5cDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_s5cs = [EpS5cRel | EpS5cRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_s5c({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_s5u({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_s5u({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_s5u({endElement, _Uri, "EP_S5U", QName},
		[#state{dn_prefix = [EpS5uDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_s5us = EpS5uRels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpS5uAttr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_S5U",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpS5uDn,
			description = "5G Core End Point of S5-U interface",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_S5U",
			specification = Spec,
			characteristic = EpS5uAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpS5uRel = #resource_rel{id = Id, name = EpS5uDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_s5us = [EpS5uRel | EpS5uRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_s5u({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_rx({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_rx({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_rx({endElement, _Uri, "EP_Rx", QName},
		[#state{dn_prefix = [EpRxDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NgcState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#ngc_state{ep_rxs = EpRxRels} = NgcState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpRxAttr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_Rx",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpRxDn,
			description = "5G Core End Point of Rx interface (between PCF and AF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_Rx",
			specification = Spec,
			characteristic = EpRxAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpRxRel = #resource_rel{id = Id, name = EpRxDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NgcState#ngc_state{
					ep_rxs = [EpRxRel | EpRxRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_rx({endElement, _Uri, _LocalName, QName},
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
parse_ep_attr1([{characters, Chars} | T], "farEndEntity" = Attr, Acc) ->
	[Dn] = string:tokens(Chars, "\n$\t"),
	parse_ep_attr1(T, Attr, [#resource_char{name = Attr, value = Dn} | Acc]);
parse_ep_attr1([{characters, Chars} | T], Attr, Acc) when is_list(Chars) ->
	parse_ep_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
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

