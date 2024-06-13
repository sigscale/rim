%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This library module implements the public API for the
%%%   {@link //im. im} application.
%%%
-module(im_xml_zte).
-copyright('Copyright (c) 2018 - 2024 SigScale Global Inc.').

%% export the im private API
-export([parse_vsdata/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

-define(PathCatalogSchema, "/resourceCatalogManagement/v4/schema").
-define(PathInventorySchema, "/resourceInventoryManagement/v4/schema").

%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

%% @hidden
parse_vsdata({startElement, _, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], location = Location} | _T] = State) ->
	[#state{parse_module = im_xml_zte, parse_function = parse_vsdata,
			dn_prefix = [CurrentDn], location = Location,
			parse_state = #zte_state{vs_data = #{"id" => Id}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_vsdata({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_vsdata({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_vsdata({endElement, _Uri, "attributes", QName},
		[#state{parse_state = #zte_state{vs_data = VsData},
		stack = Stack} = State | T]) ->
	ZteState = State#state.parse_state,
	NewStack = [{endElement, QName} | Stack],
	{Attributes, _NextStack} = pop(startElement, QName, NewStack),
	NewVsData = parse_vsdata_attr(VsData, Attributes),
	[State#state{parse_state = ZteState#zte_state{vs_data = NewVsData},
			stack = NewStack} | T];
parse_vsdata({endElement, _, "VsDataContainer", _QName},
		[#state{dn_prefix = [CurrentDn | _], parse_state = #zte_state{
		vs_data = #{"id" := Id, "attributes" := #{"vsDataType" := "vsDataBtsFunction",
		"vsDataFormatVersion" := "ZTESpecificAttributes"}}, cells = Cells},
		spec_cache = Cache, location = Sites} = State,
		#state{parse_module = im_xml_generic, parse_function = parse_managed_element,
		spec_cache = PrevCache} = PrevState | T]) ->
	#state{parse_state = #zte_state{vs_data = #{"attributes" := NrmMap}}} = State,
	VsDataContainer = #resource_char{name = "vsDataContainer",
			class_type = "VsDataContainerList", value = NrmMap,
			schema = ?PathCatalogSchema ++ "/VsDataContainerList"},
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Sites,
			schema = ?PathCatalogSchema ++ "/PeeParametersListType"},
	GsmCell = #resource_char{name = "gsmCell",
			class_type = "DnList", value = Cells,
			schema = ?PathCatalogSchema ++ "/geranNrm#/definitions/DnList"},
	ClassType = "BtsSiteMgr",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	BtsDn = CurrentDn ++ ",vsDataBtsFunction=" ++ Id,
	Resource = #resource{name = BtsDn,
			description = "GSM Base Transceiver Station (BTS)",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/BtsSiteMgr",
			specification = Spec,
			characteristic = [VsDataContainer, PeeParam, GsmCell]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_vsdata({endElement, _, "VsDataContainer", _QName},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId,
		parse_state = #zte_state{ vs_data = #{"id" := Id,
		"attributes" := #{"vsDataType" := "vsDataBtsEquipment",
		"vsDataFormatVersion" := "ZTESpecificAttributes",
		"vsData" := #{"userLabel" := SiteId}}}}, spec_cache = Cache} = State,
		#state{parse_module = im_xml_geran, parse_function = parse_bss,
		spec_cache = PrevCache, parse_state = GeranState} = PrevState | T]) ->
	#state{parse_state = #zte_state{vs_data = #{"attributes" := NrmMap}}} = State,
	#geran_state{btss = Btss} = GeranState,
	Sites = case im:get_pee(RuleId, SiteId) of
		{ok, PEEMonitoredEntities} ->
			parse_peeParameterslist(PEEMonitoredEntities, []);
		_ ->
			[]
	end,
	VsDataContainer = #resource_char{name = "vsDataContainer",
			class_type = "VsDataContainerList", value = NrmMap,
			schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VsDataContainerList"},
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Sites,
			schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/PeeParametersListType"},
	ClassType = "BtsSiteMgr",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	BtsDn = CurrentDn ++ ",vsDataBtsEquipment=" ++ Id,
	Resource = #resource{name = BtsDn,
			description = "GSM Base Transceiver Station (BTS)",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/BtsSiteMgr",
			specification = Spec,
			characteristic = [VsDataContainer, PeeParam]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					parse_state = GeranState#geran_state{btss = [BtsDn | Btss]}} | T];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_vsdata({endElement, _, "VsDataContainer", _QName},
		[#state{dn_prefix = [CurrentDn | _],
		parse_state = #zte_state{vs_data = #{"id" := Id, "attributes" := #{"vsDataType" :=
		"vsDataGCellEquipmentFunction"}}}, spec_cache = Cache,
		location = Sites},
		#state{parse_module = ?MODULE, parse_function = parse_vsdata,
		parse_state = ZteState, spec_cache = PrevCache} = PrevState | T]) ->
	#zte_state{vs_data = #{"attributes" := NrmMap}, cells = Cells} = ZteState,
	VsDataContainer = #resource_char{name = "vsDataContainer",
			class_type = "VsDataContainerList", value = NrmMap,
			schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VsDataContainerList"},
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Sites,
			schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/PeeParametersListType"},
	ClassType = "GsmCell",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	CellDn = CurrentDn ++ ",vsDataGCellEquipmentFunction=" ++ Id,
	Resource = #resource{name = CellDn,
			description = "GSM Radio",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/GsmCell",
			specification = Spec,
			characteristic = [VsDataContainer, PeeParam]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					parse_state = ZteState#zte_state{cells = [CellDn | Cells]}} | T];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_vsdata({endElement, _, "VsDataContainer", _QName},
		[#state{parse_state = #zte_state{vs_data = #{"attributes" :=
		#{"vsDataFormatVersion" := "ZTESpecificAttributes"}}}},
		#state{parse_module = im_xml_geran} = PrevState | T]) ->
%	#state{parse_state = #zte_state{vs_data = #{"attributes" := NrmMap}}} = State,
	[PrevState | T];
parse_vsdata({endElement, _, "VsDataContainer", _QName},
		[_State, PrevState | T]) ->
	[PrevState | T];
parse_vsdata({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_peeParameterslist([#resource{characteristic = ResourceChars} | Resources], Acc) ->
	parse_peeParameterslist1(ResourceChars, Resources, Acc);
parse_peeParameterslist([], Acc) ->
	Acc.
% @hidden
parse_peeParameterslist1([#resource_char{name = "peeMeDescription",
		value = ValueMap} | _], Resources, Acc) ->
	parse_peeParameterslist(Resources, [ValueMap | Acc]);
parse_peeParameterslist1([_H | T], Resources, Acc) ->
	parse_peeParameterslist1(T, Resources, Acc);
parse_peeParameterslist1([], Resources, Acc) ->
	parse_peeParameterslist(Resources, Acc).

%% @hidden
parse_vsdata_attr(VsDataContainer, Stack) ->
	parse_vsdata_attr(Stack, [], VsDataContainer).
%% @hidden
parse_vsdata_attr([{startElement, {_, "vsDataBtsFunction"} = QName, _} | T],
		_State, Acc) ->
	{[_ | Attributes], _} = pop(endElement, QName, T),
	parse_bts_attr(Attributes, Acc);
parse_vsdata_attr([{startElement,
		{_, "vsDataGCellEquipmentFunction"} = QName, _} | T], _State, Acc) ->
	{[_ | Attributes], _} = pop(endElement, QName, T),
	parse_gcell_equipment_attr(Attributes, Acc);
parse_vsdata_attr([{startElement, {_, "vsDataBssFunction"} = QName, _} | T],
		_State, Acc) ->
	{[_ | Attributes], _} = pop(endElement, QName, T),
	parse_attr(Attributes, Acc);
parse_vsdata_attr([{startElement, {_, "vsDataBtsSiteManager"} = QName, _} | T],
		_State, Acc) ->
	{[_ | Attributes], _} = pop(endElement, QName, T),
	parse_attr(Attributes, Acc);
parse_vsdata_attr([{startElement, {_, "vsDataGsmCell"} = QName, _} | T],
		_State, Acc) ->
	{[_ | Attributes], _} = pop(endElement, QName, T),
	parse_attr(Attributes, Acc);
parse_vsdata_attr([{startElement, QName, _}| T],
		State, Acc) ->
	parse_vsdata_attr(T, [QName | State], Acc);
parse_vsdata_attr([{characters, Chars} | T],
		[{_, "vsDataType"}, {_,"attributes"}] = State, Acc) ->
	NewAcc = Acc#{"attributes" => #{"vsDataType" => Chars}},
	parse_vsdata_attr(T, State, NewAcc);
parse_vsdata_attr([{characters, Chars} | T],
		[{_, "vsDataFormatVersion"}, {_, "vsDataType"},
		{_,"attributes"}] = State, #{"attributes" := Attr} = Acc) ->
	NewAcc = Acc#{"attributes" => Attr#{"vsDataFormatVersion" => Chars}},
	parse_vsdata_attr(T, State, NewAcc);
parse_vsdata_attr([{characters, Chars} | T], [{_, Attr} | _] = State, Acc) ->
	NewAcc = attribute_add(Attr, Chars, Acc),
	parse_vsdata_attr(T, State, NewAcc);
parse_vsdata_attr([], _State, Acc) ->
	Acc;
parse_vsdata_attr([{endElement, _QName} | T], State, Acc) ->
	parse_vsdata_attr(T, State, Acc).

parse_gcell_equipment_attr(Stack, Acc) ->
	parse_gcell_equipment_attr(Stack, [], Acc).
%% @hidden
parse_gcell_equipment_attr([{endElement, QName} | T] = _Stack, State, Acc) ->
	parse_gcell_equipment_attr(T, [QName | State], Acc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "lowerThresholdOfFilterSel"} | _] = State, Acc) ->
	NewAcc = attribute_add("lowerThresholdOfFilterSel", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "gsmCarrierConfig_carrierPower"} | _] = State, Acc) ->
	NewAcc = attribute_add("gsmCarrierConfig_carrierPower", Chars, Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "gsmCarrierConfig_gsmCarrierNo"} | _] = State, Acc) ->
	NewAcc = attribute_add("gsmCarrierConfig_gsmCarrierNo", Chars, Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "gCellEquipmentFuncNo"} | _] = State, Acc) ->
	NewAcc = attribute_add("gCellEquipmentFuncNo", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "toShelf"} | _] = State, Acc) ->
	NewAcc = attribute_add("toShelf", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "channelGroupNo"} | _] = State, Acc) ->
	NewAcc = attribute_add("channelGroupNo", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "ddtOffset"} | _] = State, Acc) ->
	NewAcc = attribute_add("ddtOffset", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "rxConfig"} | _] = State, Acc) ->
	NewAcc = attribute_add("rxConfig", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "ref2RxChannel"} | _] = State, Acc) ->
	NewAcc = attribute_add("ref2RxChannel", Chars, Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "isTxPower"} | _] = State, Acc) ->
	NewAcc = attribute_add("isTxPower", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "description"} | _] = State, Acc) ->
	NewAcc = attribute_add("description", Chars, Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "multiCarrierGroupNo"} | _] = State, Acc) ->
	NewAcc = attribute_add("multiCarrierGroupNo", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "toRack"} | _] = State, Acc) ->
	NewAcc = attribute_add("toRack", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "powerLevel"} | _] = State, Acc) ->
	NewAcc = attribute_add("powerLevel", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "gsmCarrierConfig_enableIntelligentOff"} | _] = State, Acc) ->
	NewAcc = attribute_add("gsmCarrierConfig_enableIntelligentOff", Chars, Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "groupBackupNum"} | _] = State, Acc) ->
	NewAcc = attribute_add("groupBackupNum", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "rxPrimaryAtten"} | _] = State, Acc) ->
	NewAcc = attribute_add("rxPrimaryAtten", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "rruWorkMode"} | _] = State, Acc) ->
	NewAcc = attribute_add("rruWorkMode", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "toSdrNo"} | _] = State, Acc) ->
	NewAcc = attribute_add("toSdrNo", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "rxDiversityAtten"} | _] = State, Acc) ->
	NewAcc = attribute_add("rxDiversityAtten", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "upperThresholdOfFilterSel"} | _] = State, Acc) ->
	NewAcc = attribute_add("upperThresholdOfFilterSel", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "rxMode"} | _] = State, Acc) ->
	NewAcc = attribute_add("rxMode", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "gsmCarrierConfig_enableTimeSlotOff"} | _] = State, Acc) ->
	NewAcc = attribute_add("gsmCarrierConfig_enableTimeSlotOff", Chars, Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "txMode"} | _] = State, Acc) ->
	NewAcc = attribute_add("txMode", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "groupOffset"} | _] = State, Acc) ->
	NewAcc = attribute_add("groupOffset", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "refTxChannel"} | _] = State, Acc) ->
	NewAcc = attribute_add("refTxChannel", Chars, Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "uplinkFilterType"} | _] = State, Acc) ->
	NewAcc = attribute_add("uplinkFilterType", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "toSlot"} | _] = State, Acc) ->
	NewAcc = attribute_add("toSlot", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "toPortId"} | _] = State, Acc) ->
	NewAcc = attribute_add("toPortId", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, "refRxChannel"} | _] = State, Acc) ->
	NewAcc = attribute_add("refRxChannel", list_to_integer(Chars), Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([{characters, Chars} | T],
		[{_, Attr} | _] = State, Acc) ->
%default character handler
	NewAcc = attribute_add(Attr, Chars, Acc),
	parse_gcell_equipment_attr(T, State, NewAcc);
parse_gcell_equipment_attr([], _State, Acc) ->
	Acc;
parse_gcell_equipment_attr([{startElement, _QName, _} | T], State, Acc) ->
	parse_gcell_equipment_attr(T, State, Acc).

parse_bts_attr(Stack, Acc) ->
	parse_bts_attr(Stack, [], Acc).
%% @hidden
parse_bts_attr([{endElement, QName} | T] = _Stack, State, Acc) ->
	parse_bts_attr(T, [QName | State], Acc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "usedPwrCountSwitch"} | _] = State, Acc) ->
	NewAcc = attribute_add("usedPwrCountSwitch", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "isSupportLinkBackup"} | _] = State, Acc) ->
	NewAcc = attribute_add("isSupportLinkBackup", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "CpuThreshold"} | _] = State, Acc) ->
	NewAcc = attribute_add("CpuThreshold", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "isAssignedByTrxPriority"} | _] = State, Acc) ->
	NewAcc = attribute_add("isAssignedByTrxPriority", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "backupLinkSwitch"} | _] = State, Acc) ->
	NewAcc = attribute_add("backupLinkSwitch", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "operState"} | _] = State, Acc) ->
	NewAcc = attribute_add("operState", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "productStatus"} | _] = State, Acc) ->
	NewAcc = attribute_add("productStatus", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "AirSoftSyncAjustTime"} | _] = State, Acc) ->
	NewAcc = attribute_add("AirSoftSyncAjustTime", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "description"} | _] = State, Acc) ->
	NewAcc = attribute_add("description", Chars, Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "switchBackMode"} | _] = State, Acc) ->
	NewAcc = attribute_add("switchBackMode", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "autoSwitchTimer"} | _] = State, Acc) ->
	NewAcc = attribute_add("autoSwitchTimer", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "availStatus"} | _] = State, Acc) ->
	NewAcc = attribute_add("availStatus", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "adminState"} | _] = State, Acc) ->
	NewAcc = attribute_add("adminState", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "isSwitchBackOnlyIdle"} | _] = State, Acc) ->
	NewAcc = attribute_add("isSwitchBackOnlyIdle", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "siteId"} | _] = State, Acc) ->
	NewAcc = attribute_add("siteId", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "switchMode"} | _] = State, Acc) ->
	NewAcc = attribute_add("switchMode", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "isProtectTrxMax"} | _] = State, Acc) ->
	NewAcc = attribute_add("isProtectTrxMax", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "isTrafficControlByCpu"} | _] = State, Acc) ->
	NewAcc = attribute_add("isTrafficControlByCpu", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "isEnableEUIPBackup"} | _] = State, Acc) ->
	NewAcc = attribute_add("isEnableEUIPBackup", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "isAntPrimSecMeas"} | _] = State, Acc) ->
	NewAcc = attribute_add("isAntPrimSecMeas", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "isRUMeasStart"} | _] = State, Acc) ->
	NewAcc = attribute_add("isRUMeasStart", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "autoSwitchBackTimer"} | _] = State, Acc) ->
	NewAcc = attribute_add("autoSwitchBackTimer", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "bpSavingElectricitySwitch"} | _] = State, Acc) ->
	NewAcc = attribute_add("bpSavingElectricitySwitch", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, "bscMode"} | _] = State, Acc) ->
	NewAcc = attribute_add("bscMode", list_to_integer(Chars), Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([{characters, Chars} | T],
		[{_, Attr} | _] = State, Acc) ->
%default character handler
	NewAcc = attribute_add(Attr, Chars, Acc),
	parse_bts_attr(T, State, NewAcc);
parse_bts_attr([], _State, Acc) ->
	Acc;
parse_bts_attr([{startElement, _QName, _} | T], State, Acc) ->
	parse_bts_attr(T, State, Acc).

parse_attr(Stack, Acc) ->
	parse_attr(Stack, [], Acc).
%% @hidden
parse_attr([{endElement, QName} | T] = _Stack, State, Acc) ->
	parse_attr(T, [QName | State], Acc);
parse_attr([{characters, Chars} | T],
		[{_, Attr} | _] = State, Acc) ->
%default character handler
	NewAcc = attribute_add(Attr, Chars, Acc),
	parse_attr(T, State, NewAcc);
parse_attr([{startElement, _QName, _} | T], State, Acc) ->
	parse_attr(T, State, Acc);
parse_attr([], _State, Acc) ->
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
		Result :: {Value, NewStack} | [],
		Value :: [event()],
		NewStack :: [event()].
%% @doc Pops all events up to an including `{Element, QName, ...}'.
%% @private
pop(Element, QName, Stack) ->
	pop(Element, QName, Stack, []).
%% @hidden
pop(_Element, _QName, [], _Acc) ->
	[];
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

-spec attribute_add(Attribute, Value, NrmMap) -> NrmMap
	when
		Attribute :: string(),
		Value :: term(),
		NrmMap :: map().
%% @doc Add `Attribute' and `Value' to possibly missing attribute.
attribute_add(Attribute, Value, #{"attributes" := #{"vsData" := VsData}} = NrmMap) ->
%	NrmMap#{"attributes" => Attributes#{Attribute => Value}};
	#{"attributes" := Attributes} = NrmMap,
	NrmMap#{"attributes" => Attributes#{"vsData" => VsData#{Attribute => Value}}};
attribute_add(Attribute, Value, #{"attributes" := Attributes} = NrmMap) ->
	NrmMap#{"attributes" => Attributes#{"vsData" => #{Attribute => Value}}}.
