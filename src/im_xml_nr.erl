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
-module(im_xml_nr).
-copyright('Copyright (c) 2020 SigScale Global Inc.').

%% export the im private API
-export([parse_gnbdu/2, parse_nr_cell_du/2, parse_nr_sector_carrier/2,
		parse_ep_f1c/2, parse_ep_f1u/2,
		parse_gnbcucp/2, parse_nr_cell_cu/2, parse_ep_e1/2, parse_ep_xnc/2,
		parse_ep_x2c/2, parse_ep_ngc/2,
		parse_gnbcuup/2, parse_ep_xnu/2, parse_ep_ngu/2, parse_ep_x2u/2,
		parse_ep_s1u/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

-define(PathCatalogSchema, "/resourceCatalogManagement/v3/resourceCatalogManagement").
-define(PathInventorySchema, "/resourceInventoryManagement/v3/resourceInventoryManagement").
-define(ResourcePath, "/resourceInventoryManagement/v3/resource/").

%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

%% @hidden
parse_gnbdu({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_gnbdu({startElement, _Uri, "NRCellDU", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",NRCellDU=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_nr,
			parse_function = parse_nr_cell_du,
			parse_state = #nr_state{nr_cell_du = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gnbdu({startElement, _Uri, "NRSectorCarrier", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",NRSectorCarrier=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_nr,
			parse_function = parse_nr_sector_carrier,
			parse_state = #nr_state{nr_sector_carrier = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gnbdu({startElement, _Uri, "EP_F1C", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_F1C=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_f1c,
			parse_state = #nr_state{ep_f1c = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gnbdu({startElement, _Uri, "EP_F1U", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_F1U=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_f1u,
			parse_state = #nr_state{ep_f1u = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gnbdu({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_gnbdu({endElement, _Uri, "GNBDUFunction", QName},
		[#state{parse_state =  #nr_state{nr_cell_dus = NrCellDuRels,
		nr_sector_carriers = NrSCRels, ep_f1cs = EpF1cRels, ep_f1us = EpF1uRels},
		dn_prefix = [GnbduDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "GNBDUFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	GnbDUAttr = parse_gnbdu_attr(T2, undefined, []),
	Resource = #resource{name = GnbduDn,
			description = "NR gNB Distributed Unit (DU)",
			category = "NR",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/GNBDUFunction",
			specification = Spec,
			characteristic = GnbDUAttr,
			related = NrCellDuRels ++ NrSCRels ++ EpF1cRels ++ EpF1uRels,
			connection_point = EpF1cRels ++ EpF1uRels},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_gnbdu({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_gnbdu_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_gnbdu_attr1(Attributes, undefined, Acc).
% @hidden
parse_gnbdu_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_gnbdu_attr1(T2, undefined, Acc);
parse_gnbdu_attr1([{endElement, {_, "peeParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo peeParametersListType
	{[_ | _PeeplType], T2} = pop(startElement, QName, T1),
	parse_gnbdu_attr1(T2, undefined, Acc);
parse_gnbdu_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_gnbdu_attr1(T, Attr, Acc);
parse_gnbdu_attr1([{characters, Chars} | T], "gnbId" = Attr, Acc) ->
	parse_gnbdu_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_gnbdu_attr1([{characters, Chars} | T], "gnbIdLength" = Attr, Acc) ->
	parse_gnbdu_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_gnbdu_attr1([{characters, Chars} | T], "gnbDuId" = Attr, Acc) ->
	parse_gnbdu_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_gnbdu_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_gnbdu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gnbdu_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_gnbdu_attr1(T, undefined, Acc);
parse_gnbdu_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_nr_cell_du({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_nr_cell_du({startElement, _Uri, _LocalName, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_nr_cell_du({endElement, _Uri, "NRCellDU", QName},
		[#state{dn_prefix = [NrCellDuDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NrState, spec_cache = PrevCache} = PrevState | T1]) ->
	#nr_state{nr_cell_dus = NrCellDuRels } = NrState,
	ClassType = "NRCellDU",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NrCellDUAttr = parse_nr_cell_du_attr(T2, []),
	Resource = #resource{name = NrCellDuDn,
			description = "NR Cell Distributed Unit (DU)",
			category = "NR",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/NRCellDU",
			specification = Spec,
			characteristic = NrCellDUAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			NrCellDuRel = #resource_rel{id = Id, name = NrCellDuDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{
					parse_state = NrState#nr_state{nr_cell_dus = [NrCellDuRel | NrCellDuRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_nr_cell_du({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_nr_cell_du_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		Acc) ->
	{[_ | Attributes], _} = pop(endElement, QName, T1),
	parse_nr_cell_du_attr1(Attributes, undefined, Acc).
% @hidden
parse_nr_cell_du_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_nr_cell_du_attr1(T2, undefined, Acc);
parse_nr_cell_du_attr1([{endElement, {_, "availabilityStatus"} = QName} | T1],
		undefined, Acc) ->
	% @todo availabilityStatus
	{[_ | _AvailabilityStatus], T2} = pop(startElement, QName, T1),
	parse_nr_cell_du_attr1(T2, undefined, Acc);
parse_nr_cell_du_attr1([{endElement, {_, "pLMNIdList"} = QName} | T1],
		undefined, Acc) ->
	% @todo PLMNIdList
	{[_ | _PLMNIdList], T2} = pop(startElement, QName, T1),
	parse_nr_cell_du_attr1(T2, undefined, Acc);
parse_nr_cell_du_attr1([{endElement, {_, "sNSSAIList"} = QName} | T1],
		undefined, Acc) ->
	% @todo SNSSAIList
	{[_ | _SNSSAIList], T2} = pop(startElement, QName, T1),
	parse_nr_cell_du_attr1(T2, undefined, Acc);
parse_nr_cell_du_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_nr_cell_du_attr1(T, Attr, Acc);
parse_nr_cell_du_attr1([{characters, Chars} | T], "nCGI" = Attr, Acc) ->
	parse_nr_cell_du_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nr_cell_du_attr1([{characters, Chars} | T], "nRpci" = Attr, Acc) ->
	parse_nr_cell_du_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nr_cell_du_attr1([{characters, Chars} | T], "nRTac" = Attr, Acc) ->
	parse_nr_cell_du_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nr_cell_du_attr1([{characters, Chars} | T], "arfcnDL" = Attr, Acc) ->
	parse_nr_cell_du_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nr_cell_du_attr1([{characters, Chars} | T], "arfcnUL" = Attr, Acc) ->
	parse_nr_cell_du_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nr_cell_du_attr1([{characters, Chars} | T], "arfcnSUL" = Attr, Acc) ->
	parse_nr_cell_du_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nr_cell_du_attr1([{characters, Chars} | T], "bSChannelBwDL" = Attr, Acc) ->
	parse_nr_cell_du_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nr_cell_du_attr1([{characters, Chars} | T], "bSChannelBwUL" = Attr, Acc) ->
	parse_nr_cell_du_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nr_cell_du_attr1([{characters, Chars} | T], "bSChannelBwSUL" = Attr, Acc) ->
	parse_nr_cell_du_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nr_cell_du_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_nr_cell_du_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_nr_cell_du_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_nr_cell_du_attr1(T, undefined, Acc);
parse_nr_cell_du_attr1([], _Attr, Acc) ->
	Acc.

%% @hidden
parse_nr_sector_carrier({characters, Chars},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_nr_sector_carrier({startElement, _Uri, _LocalName, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_nr_sector_carrier({endElement, _Uri, "NRSectorCarrier", QName},
		[#state{dn_prefix = [NrSCDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NrState, spec_cache = PrevCache} = PrevState | T1]) ->
	#nr_state{nr_sector_carriers = NrSCRels} = NrState,
	ClassType = "NRSectorCarrier",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NrSCAttr = parse_nr_sector_carrier_attr(T2, []),
	Resource = #resource{name = NrSCDn,
			description = "NR Sector Carrier",
			category = "NR",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/NRSectorCarrier",
			specification = Spec,
			characteristic = NrSCAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			NrSCRel = #resource_rel{id = Id, name = NrSCDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{
					parse_state = NrState#nr_state{nr_sector_carriers
					= [NrSCRel | NrSCRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_nr_sector_carrier({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_nr_sector_carrier_attr([{startElement, {_, "attributes"} = QName,
		[]} | T1], Acc) ->
	{[_ | Attributes], _} = pop(endElement, QName, T1),
	parse_nr_sector_carrier_attr1(Attributes, undefined, Acc).
% @hidden
parse_nr_sector_carrier_attr1([{endElement,
		{_, "vnfParametersList"} = QName} | T1], undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_nr_sector_carrier_attr1(T2, undefined, Acc);
parse_nr_sector_carrier_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_nr_sector_carrier_attr1(T, Attr, Acc);
parse_nr_sector_carrier_attr1([{characters, Chars} | T],
		"configuredMaxTxPower" = Attr, Acc) ->
	parse_nr_sector_carrier_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nr_sector_carrier_attr1([{characters, Chars} | T],
		"arfcnDL" = Attr, Acc) ->
	parse_nr_sector_carrier_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nr_sector_carrier_attr1([{characters, Chars} | T],
		"arfcnUL" = Attr, Acc) ->
	parse_nr_sector_carrier_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nr_sector_carrier_attr1([{characters, Chars} | T],
		"bSChannelBwDL" = Attr, Acc) ->
	parse_nr_sector_carrier_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nr_sector_carrier_attr1([{characters, Chars} | T],
		"bSChannelBwUL" = Attr, Acc) ->
	parse_nr_sector_carrier_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nr_sector_carrier_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_nr_sector_carrier_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_nr_sector_carrier_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_nr_sector_carrier_attr1(T, undefined, Acc);
parse_nr_sector_carrier_attr1([], _Attr, Acc) ->
	Acc.

%% @hidden
parse_ep_f1c({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_f1c({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_f1c({endElement, _Uri, "EP_F1C", QName},
		[#state{dn_prefix = [EpF1cDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NrState, spec_cache = PrevCache} = PrevState | T1]) ->
	#nr_state{ep_f1cs = EpF1cRels} = NrState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpF1cAttr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_F1C",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpF1cDn,
			description = "NR End Point of the control plane interface (F1-C)",
			category = "NR",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_F1C",
			specification = Spec,
			characteristic = EpF1cAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpF1cRel = #resource_rel{id = Id, name = EpF1cDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NrState#nr_state{
					ep_f1cs = [EpF1cRel | EpF1cRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_f1c({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_f1u({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_f1u({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_f1u({endElement, _Uri, "EP_F1U", QName},
		[#state{dn_prefix = [EpF1uDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NrState, spec_cache = PrevCache} = PrevState | T1]) ->
	#nr_state{ep_f1us = EpF1uRels} = NrState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpF1uAttr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_F1U",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpF1uDn,
			description = "NR End Point of the control plane interface (F1-U)",
			category = "NR",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_F1U",
			specification = Spec,
			characteristic = EpF1uAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpF1uRel = #resource_rel{id = Id, name = EpF1uDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NrState#nr_state{
					ep_f1us = [EpF1uRel | EpF1uRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_f1u({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_gnbcucp({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_gnbcucp({startElement, _Uri, "NRCellCU", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",NRCellCU=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_nr,
			parse_function = parse_nr_cell_cu,
			parse_state = #nr_state{nr_cell_cu = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gnbcucp({startElement, _Uri, "EP_F1C", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_F1C=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_f1c,
			parse_state = #nr_state{ep_f1c = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gnbcucp({startElement, _Uri, "EP_E1", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_E1=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_e1,
			parse_state = #nr_state{ep_e1 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gnbcucp({startElement, _Uri, "EP_XnC", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_XnC=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_xnc,
			parse_state = #nr_state{ep_xnc = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gnbcucp({startElement, _Uri, "EP_X2C", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_X2C=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_x2c,
			parse_state = #nr_state{ep_x2c = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gnbcucp({startElement, _Uri, "EP_NgC", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_NgC=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_ngc,
			parse_state = #nr_state{ep_ngc = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gnbcucp({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_gnbcucp({endElement, _Uri, "GNBCUCPFunction", QName},
		[#state{parse_state =  #nr_state{nr_cell_cus = NrCellCuRels,
		ep_f1cs = EpF1cRels, ep_e1s = EpE1Rels, ep_xncs = EpXncRels,
		ep_x2cs = EpX2cRels, ep_ngcs = EpNgcRels},
		dn_prefix = [GnbcucpDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "GNBCUCPFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	GnbcucpAttr = parse_gnbcucp_attr(T2, undefined, []),
	Resource = #resource{name = GnbcucpDn,
			description = "NR gNB Central Unit (CU) Control Plane (CP)",
			category = "NR",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/GNBCUCPFunction",
			specification = Spec,
			characteristic = GnbcucpAttr,
			related = NrCellCuRels ++ EpF1cRels ++ EpE1Rels ++ EpXncRels ++
					EpX2cRels ++ EpNgcRels,
			connection_point = EpF1cRels ++ EpE1Rels ++ EpXncRels ++
					EpX2cRels ++ EpNgcRels},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_gnbcucp({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_gnbcucp_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_gnbcucp_attr1(Attributes, undefined, Acc).
% @hidden
parse_gnbcucp_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_gnbcucp_attr1(T2, undefined, Acc);
parse_gnbcucp_attr1([{endElement, {_, "peeParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo peeParametersListType
	{[_ | _PeeplType], T2} = pop(startElement, QName, T1),
	parse_gnbcucp_attr1(T2, undefined, Acc);
parse_gnbcucp_attr1([{endElement, {_, "pLMNIdList"} = QName} | T1],
		undefined, Acc) ->
	% @todo PLMNIdList
	{[_ | _PLMNIdList], T2} = pop(startElement, QName, T1),
	parse_gnbcucp_attr1(T2, undefined, Acc);
parse_gnbcucp_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_gnbcucp_attr1(T, Attr, Acc);
parse_gnbcucp_attr1([{characters, Chars} | T], "gnbId" = Attr, Acc) ->
	parse_gnbcucp_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_gnbcucp_attr1([{characters, Chars} | T], "gnbIdLength" = Attr, Acc) ->
	parse_gnbcucp_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_gnbcucp_attr1([{characters, Chars} | T], "gnbDuId" = Attr, Acc) ->
	parse_gnbcucp_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_gnbcucp_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_gnbcucp_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gnbcucp_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_gnbcucp_attr1(T, undefined, Acc);
parse_gnbcucp_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_nr_cell_cu({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_nr_cell_cu({startElement, _Uri, _LocalName, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_nr_cell_cu({endElement, _Uri, "NRCellCU", QName},
		[#state{dn_prefix = [NrCellCuDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NrState, spec_cache = PrevCache} = PrevState | T1]) ->
	#nr_state{nr_cell_cus = NrCellCuRels } = NrState,
	ClassType = "NRCellCU",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NrCellCUAttr = parse_nr_cell_cu_attr(T2, []),
	Resource = #resource{name = NrCellCuDn,
			description = "NR Cell Central Unit (CU)",
			category = "NR",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/NRCellCU",
			specification = Spec,
			characteristic = NrCellCUAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			NrCellCuRel = #resource_rel{id = Id, name = NrCellCuDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{
					parse_state = NrState#nr_state{nr_cell_cus = [NrCellCuRel | NrCellCuRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_nr_cell_cu({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_nr_cell_cu_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		Acc) ->
	{[_ | Attributes], _} = pop(endElement, QName, T1),
	parse_nr_cell_cu_attr1(Attributes, undefined, Acc).
% @hidden
parse_nr_cell_cu_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_nr_cell_cu_attr1(T2, undefined, Acc);
parse_nr_cell_cu_attr1([{endElement, {_, "pLMNIdList"} = QName} | T1],
		undefined, Acc) ->
	% @todo PLMNIdList
	{[_ | _PLMNIdList], T2} = pop(startElement, QName, T1),
	parse_nr_cell_cu_attr1(T2, undefined, Acc);
parse_nr_cell_cu_attr1([{endElement, {_, "sNSSAIList"} = QName} | T1],
		undefined, Acc) ->
	% @todo SNSSAIList
	{[_ | _SNSSAIList], T2} = pop(startElement, QName, T1),
	parse_nr_cell_cu_attr1(T2, undefined, Acc);
parse_nr_cell_cu_attr1([{endElement, {_, "rRMPolicyRatio2"} = QName} | T1],
		undefined, Acc) ->
	% @todo RRMPolicyRation2
	{[_ | _RRMPolicyRation2], T2} = pop(startElement, QName, T1),
	parse_nr_cell_cu_attr1(T2, undefined, Acc);
parse_nr_cell_cu_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_nr_cell_cu_attr1(T, Attr, Acc);
parse_nr_cell_cu_attr1([{characters, Chars} | T], "nCGI" = Attr, Acc) ->
	parse_nr_cell_cu_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nr_cell_cu_attr1([{characters, Chars} | T], "rRMPolicyType" = Attr, Acc) ->
	parse_nr_cell_cu_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nr_cell_cu_attr1([{characters, Chars} | T], "rRMPolicyRatio" = Attr, Acc) ->
	parse_nr_cell_cu_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nr_cell_cu_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_nr_cell_cu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_nr_cell_cu_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_nr_cell_cu_attr1(T, undefined, Acc);
parse_nr_cell_cu_attr1([], _Attr, Acc) ->
	Acc.

%% @hidden
parse_ep_e1({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_e1({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_e1({endElement, _Uri, "EP_E1", QName},
		[#state{dn_prefix = [EpE1Dn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NrState, spec_cache = PrevCache} = PrevState | T1]) ->
	#nr_state{ep_e1s = EpE1Rels} = NrState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpE1Attr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_E1",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpE1Dn,
			description = "NR End Point of the logical link, supporting E1 interface",
			category = "NR",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_E1",
			specification = Spec,
			characteristic = EpE1Attr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpE1Rel = #resource_rel{id = Id, name = EpE1Dn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NrState#nr_state{
					ep_e1s = [EpE1Rel | EpE1Rels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_e1({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_xnc({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_xnc({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_xnc({endElement, _Uri, "EP_XnC", QName},
		[#state{dn_prefix = [EpXncDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = PrevParseState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpXncAttr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_XnC",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpXncDn,
			description = "NR End Point of the logical link, supporting Xn"
					" Application protocols, to a neighbour gNB node",
			category = "NR",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_XnC",
			specification = Spec,
			characteristic = EpXncAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpXncRel = #resource_rel{id = Id, name = EpXncDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			case PrevParseState of
				#nr_state{ep_xncs = EpXncRels} ->
					[PrevState#state{parse_state = PrevParseState#nr_state{
							ep_xncs = [EpXncRel | EpXncRels]},
							spec_cache = [NewCache | PrevCache]} | T1];
				#eutran_state{ep_xncs = EpXncRels} ->
					[PrevState#state{parse_state = PrevParseState#eutran_state{
							ep_xncs = [EpXncRel | EpXncRels]},
							spec_cache = [NewCache | PrevCache]} | T1]
			end;
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_xnc({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_x2c({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_x2c({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_x2c({endElement, _Uri, "EP_X2C", QName},
		[#state{dn_prefix = [EpX2cDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = PrevParseState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpX2cAttr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_X2C",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpX2cDn,
			description = "NR End Point of the logical link, supporting X2-C"
					" application protocols",
			category = "NR",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_X2C",
			specification = Spec,
			characteristic = EpX2cAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpX2cRel = #resource_rel{id = Id, name = EpX2cDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			case PrevParseState of
				#nr_state{ep_x2cs = EpX2cRels} ->
					[PrevState#state{parse_state = PrevParseState#nr_state{
							ep_x2cs = [EpX2cRel | EpX2cRels]},
							spec_cache = [NewCache | PrevCache]} | T1];
				#eutran_state{ep_x2cs = EpX2cRels} ->
					[PrevState#state{parse_state = PrevParseState#eutran_state{
							ep_x2cs = [EpX2cRel | EpX2cRels]},
							spec_cache = [NewCache | PrevCache]} | T1]
			end;
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_x2c({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_ngc({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_ngc({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_ngc({endElement, _Uri, "EP_NgC", QName},
		[#state{dn_prefix = [EpNgcDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = PrevParseState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpNgcAttr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_NgC",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpNgcDn,
			description = "NR End Point of the control plane interface (NG-C)"
					" between the gNB and NG-Core entity",
			category = "NR",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_NgC",
			specification = Spec,
			characteristic = EpNgcAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpNgcRel = #resource_rel{id = Id, name = EpNgcDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			case PrevParseState of
				#nr_state{ep_ngcs = EpNgcRels} ->
					[PrevState#state{parse_state = PrevParseState#nr_state{
							ep_ngcs = [EpNgcRel | EpNgcRels]},
							spec_cache = [NewCache | PrevCache]} | T1];
				#eutran_state{ep_ngcs = EpNgcRels} ->
					[PrevState#state{parse_state = PrevParseState#eutran_state{
							ep_ngcs = [EpNgcRel | EpNgcRels]},
							spec_cache = [NewCache | PrevCache]} | T1]
			end;
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_ngc({endElement, _Uri, _LocalName, QName},
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

%% @hidden
parse_gnbcuup({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_gnbcuup({startElement, _Uri, "EP_E1", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_E1=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_e1,
			parse_state = #nr_state{ep_e1 = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gnbcuup({startElement, _Uri, "EP_F1U", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_F1U=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_f1u,
			parse_state = #nr_state{ep_f1u = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gnbcuup({startElement, _Uri, "EP_XnU", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_XnU=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_xnu,
			parse_state = #nr_state{ep_xnu = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gnbcuup({startElement, _Uri, "EP_NgU", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_NgU=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_ngu,
			parse_state = #nr_state{ep_ngu = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gnbcuup({startElement, _Uri, "EP_X2U", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_X2U=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_x2u,
			parse_state = #nr_state{ep_x2u = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gnbcuup({startElement, _Uri, "EP_S1U", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_S1U=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_s1u,
			parse_state = #nr_state{ep_s1u = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gnbcuup({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_gnbcuup({endElement, _Uri, "GNBCUUPFunction", QName},
		[#state{parse_state =  #nr_state{ep_e1s = EpE1Rels, ep_f1us = EpF1uRels,
		ep_xnus = EpXnuRels, ep_ngus = EpNguRels,
		ep_x2us = EpX2uRels, ep_s1us = EpS1uRels},
		dn_prefix = [GnbcuupDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "GNBCUUPFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	GnbcuupAttr = parse_gnbcuup_attr(T2, undefined, []),
	Resource = #resource{name = GnbcuupDn,
			description = "NR gNB Central Unit (CU) User Plane (UP)",
			category = "NR",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/GNBCUUPFunction",
			specification = Spec,
			characteristic = GnbcuupAttr,
			related = EpE1Rels ++ EpF1uRels ++ EpXnuRels ++ EpNguRels ++
					EpX2uRels ++ EpS1uRels,
			connection_point = EpE1Rels ++ EpF1uRels ++ EpXnuRels ++ EpNguRels ++
					EpX2uRels ++ EpS1uRels},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_gnbcuup({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_gnbcuup_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_gnbcuup_attr1(Attributes, undefined, Acc).
% @hidden
parse_gnbcuup_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_gnbcuup_attr1(T2, undefined, Acc);
parse_gnbcuup_attr1([{endElement, {_, "peeParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo peeParametersListType
	{[_ | _PeeplType], T2} = pop(startElement, QName, T1),
	parse_gnbcuup_attr1(T2, undefined, Acc);
parse_gnbcuup_attr1([{endElement, {_, "pLMNIdList"} = QName} | T1],
		undefined, Acc) ->
	% @todo PLMNIdList
	{[_ | _PLMNIdList], T2} = pop(startElement, QName, T1),
	parse_gnbcuup_attr1(T2, undefined, Acc);
parse_gnbcuup_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_gnbcuup_attr1(T, Attr, Acc);
parse_gnbcuup_attr1([{characters, Chars} | T], "gnbId" = Attr, Acc) ->
	parse_gnbcuup_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_gnbcuup_attr1([{characters, Chars} | T], "gnbIdLength" = Attr, Acc) ->
	parse_gnbcuup_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_gnbcuup_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_gnbcuup_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gnbcuup_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_gnbcuup_attr1(T, undefined, Acc);
parse_gnbcuup_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_ep_xnu({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_xnu({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_xnu({endElement, _Uri, "EP_XnU", QName},
		[#state{dn_prefix = [EpXnuDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = PrevParseState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpXnuAttr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_XnU",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpXnuDn,
			description = "NR End Point of a logical link supporting the Xn user"
					" plane (Xn-U) interface",
			category = "NR",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_XnU",
			specification = Spec,
			characteristic = EpXnuAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpXnuRel = #resource_rel{id = Id, name = EpXnuDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			case PrevParseState of
				#nr_state{ep_xnus = EpXnuRels} ->
					[PrevState#state{parse_state = PrevParseState#nr_state{
							ep_xnus = [EpXnuRel | EpXnuRels]},
							spec_cache = [NewCache | PrevCache]} | T1];
				#eutran_state{ep_xnus = EpXnuRels} ->
					[PrevState#state{parse_state = PrevParseState#eutran_state{
							ep_xnus = [EpXnuRel | EpXnuRels]},
							spec_cache = [NewCache | PrevCache]} | T1]
			end;
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_xnu({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_ngu({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_ngu({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_ngu({endElement, _Uri, "EP_NgU", QName},
		[#state{dn_prefix = [EpNguDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = PrevParseState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpNguAttr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_NgU",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpNguDn,
			description = "NR End Point of the NG user plane (NG-U) interface"
					" between the gNB and the UPGW",
			category = "NR",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_NgU",
			specification = Spec,
			characteristic = EpNguAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpNguRel = #resource_rel{id = Id, name = EpNguDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			case PrevParseState of
				#nr_state{ep_ngus = EpNguRels} ->
					[PrevState#state{parse_state = PrevParseState#nr_state{
							ep_ngus = [EpNguRel | EpNguRels]},
							spec_cache = [NewCache | PrevCache]} | T1];
				#eutran_state{ep_ngus = EpNguRels} ->
					[PrevState#state{parse_state = PrevParseState#eutran_state{
							ep_ngus = [EpNguRel | EpNguRels]},
							spec_cache = [NewCache | PrevCache]} | T1]
			end;
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_ngu({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_x2u({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_x2u({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_x2u({endElement, _Uri, "EP_X2U", QName},
		[#state{dn_prefix = [EpX2uDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = PrevParseState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpX2uAttr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_X2U",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpX2uDn,
			description = "NR End Point of the logical link, supporting the X2"
					" user plane (X2-U) interface",
			category = "NR",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_X2U",
			specification = Spec,
			characteristic = EpX2uAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpX2uRel = #resource_rel{id = Id, name = EpX2uDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			case PrevParseState of
				#nr_state{ep_x2us = EpX2uRels} ->
					[PrevState#state{parse_state = PrevParseState#nr_state{
							ep_x2us = [EpX2uRel | EpX2uRels]},
							spec_cache = [NewCache | PrevCache]} | T1];
				#eutran_state{ep_x2us = EpX2uRels} ->
					[PrevState#state{parse_state = PrevParseState#eutran_state{
							ep_x2us = [EpX2uRel | EpX2uRels]},
							spec_cache = [NewCache | PrevCache]} | T1]
			end;
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_x2u({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ep_s1u({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ep_s1u({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ep_s1u({endElement, _Uri, "EP_S1U", QName},
		[#state{dn_prefix = [EpS1uDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = NrState, spec_cache = PrevCache} = PrevState | T1]) ->
	#nr_state{ep_s1us = EpS1uRels} = NrState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EpS1uAttr = parse_ep_attr(T2, undefined, []),
	ClassType = "EP_S1U",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = EpS1uDn,
			description = "NR End Point of the logical link, supporting S1-U"
					" interface towards a S-GW node",
			category = "NR",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/EP_S1U",
			specification = Spec,
			characteristic = EpS1uAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			EpS1uRel = #resource_rel{id = Id, name = EpS1uDn, type = "contains",
					referred_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = NrState#nr_state{
					ep_s1us = [EpS1uRel | EpS1uRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ep_s1u({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

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

