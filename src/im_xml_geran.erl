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
-module(im_xml_geran).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% export the im private API
-export([parse_bss/2, parse_bts/2, parse_gsm_cell/2, parse_gsm_rel/2]).

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
parse_bss({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_bss({startElement,  _Uri, "BtsSiteMgr", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _] = State) ->
	DnComponent = ",BtsSiteMgr=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], rule = RuleId,
			parse_module = im_xml_geran, parse_function = parse_bts,
			parse_state = #geran_state{bts = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_bss({startElement,  _Uri, "BtsSiteManager", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _] = State) ->
% zte specific
	DnComponent = ",BtsSiteMgr=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], rule = RuleId,
			parse_module = im_xml_geran, parse_function = parse_bts,
			parse_state = #geran_state{bts = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_bss({startElement,  _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _] = State) ->
	[#state{dn_prefix = [CurrentDn], rule = RuleId,
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => Id}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_bss({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_bss({endElement, _Uri, "BssFunction", QName},
		[#state{dn_prefix = [BssDn | _], stack = Stack, rule = RuleId,
		parse_state = #geran_state{btss = BtsRels, vs_data = NrmMap},
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	BssAttr = parse_bss_attr(T2, undefined, []),
	F = fun (#resource_char{name = "userLabel"}) ->
				true;
			(_) ->
				false
	end,
	[#resource_char{name = "userLabel",
			value = UserLabel}] = lists:filter(F, BssAttr),
	PeeParametersList = case im:get_pee(RuleId, UserLabel) of
		{ok, []} ->
			[];
		{ok, PEEMonitoredEntities} ->
			parse_peeParameterslist(PEEMonitoredEntities, []);
		{error, _Reason} ->
			[]
	end,
	VsData = #resource_char{name = "vsDataContainer",
			class_type = "VsDataContainerList", value = NrmMap,
			schema = "/resourceCatalogManagement/v3/schema/genericNrm#/"
					"definitions/VsDataContainerList"},
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = PeeParametersList,
			schema = "/resourceCatalogManagement/v3/schema/genericNrm#/"
					"definitions/PeeParametersListType"},
	ClassType = "BssFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = BssDn,
			description = "GSM Base Station Subsystem (BSS)",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/BssFunction",
			specification = Spec,
			characteristic = lists:reverse([VsData, PeeParam | BssAttr]),
			related = BtsRels},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_bss({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_bss_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_bss_attr1(Attributes, undefined, Acc).
% @hidden
parse_bss_attr1([{characters, Chars} | T],
		"userLabel" = Attr, Acc) ->
	parse_bss_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_bss_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_bss_attr1(T, undefined, Acc);
parse_bss_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_bss_attr1(T2, undefined, Acc);
parse_bss_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_bss_attr1(T, Attr, Acc);
parse_bss_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_bts({characters, SideId}, [#state{rule = RuleId,
		stack = [{startElement, {_, "userLabel"}, _} | _]} = State | T]) ->
	case im:get_pee(RuleId, SideId) of
		{ok, []} ->
			[State | T];
		{ok, PEEMonitoredEntities} ->
			PeeParametersList =
					parse_peeParameterslist(PEEMonitoredEntities, []),
			[State#state{location = PeeParametersList} | T];
		{error, _Reason} ->
			[State | T]
	end;
parse_bts({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_bts({startElement, _Uri, "GsmCell", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], location = Location} | _] = State) ->
	DnComponent = ",GsmCell=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], location = Location,
			parse_module = im_xml_geran, parse_function = parse_gsm_cell,
			parse_state = #geran_state{cell = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_bts({startElement, _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_bts({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_bts({endElement, _Uri, "BtsSiteManager", QName},
		[#state{dn_prefix = [BtsDn | _], location = Location,
		parse_state = #geran_state{cells = Cells, vs_data = NrmMap},
		stack = Stack, spec_cache = Cache}, #state{parse_state = GeranState,
		spec_cache = PrevCache} = PrevState | T1]) ->
% zte specific
	#geran_state{btss = Btss} = GeranState,
	{[_ | T], _NewStack} = pop(startElement, QName, Stack),
	BtsAttr = parse_bts_attr(T, undefined, []),
	GsmCell = #resource_char{name = "GsmCell", value = Cells},
	VsData = #resource_char{name = "vsDataContainer",
			class_type = "VsDataContainerList", value = NrmMap,
			schema = "/resourceCatalogManagement/v3/schema/genericNrm#/"
					"definitions/VsDataContainerList"},
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = "/resourceCatalogManagement/v3/schema/genericNrm#/"
					"definitions/PeeParametersListType"},
	ClassType = "BtsSiteMgr",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = BtsDn,
			description = "GSM Base Transceiver Station (BTS)",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/BtsSiteMgr",
			specification = Spec,
			characteristic = lists:reverse([GsmCell, VsData, PeeParam | BtsAttr])},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{parse_state = GeranState#geran_state{
			btss = [BtsDn | Btss]}, spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_bts({endElement, _Uri, "BtsSiteMgr", QName},
		[#state{dn_prefix = [BtsDn | _], location = Location,
		parse_state = #geran_state{cells = CellRels}, stack = Stack,
		spec_cache = Cache}, #state{parse_state = GeranState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#geran_state{btss = BtsRels, vs_data = NrmMap} = GeranState,
	{[_ | T], _NewStack} = pop(startElement, QName, Stack),
	BtsAttr = parse_bts_attr(T, undefined, []),
	VsData = #resource_char{name = "vsDataContainer",
			class_type = "VsDataContainerList", value = NrmMap,
			schema = "/resourceCatalogManagement/v3/schema/genericNrm#/"
					"definitions/VsDataContainerList"},
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = "/resourceCatalogManagement/v3/schema/genericNrm#/"
					"definitions/PeeParametersListType"},
	ClassType = "BtsSiteMgr",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = BtsDn,
			description = "GSM Base Transceiver Station (BTS)",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/BtsSiteMgr",
			specification = Spec,
			characteristic = lists:reverse([VsData, PeeParam | BtsAttr]),
			related = CellRels},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			BtsRel = #resource_rel{id = Id, name = BtsDn, rel_type = "contains",
					ref_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = GeranState#geran_state{
			btss = [BtsRel | BtsRels]}, spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_bts({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_bts_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_bts_attr1(Attributes, undefined, Acc).
% @hidden
parse_bts_attr1([{characters, Chars} | T],
		"userLabel" = Attr, Acc) ->
	parse_bts_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_bts_attr1([{characters, Chars} | T],
		"latitude" = Attr, Acc) ->
	parse_bts_attr1(T, Attr, [#resource_char{name = Attr,
			value = im_rest:geoaxis(Chars)} | Acc]);
parse_bts_attr1([{characters, Chars} | T],
		"Latitude" = Attr, Acc) ->
% zte specific
	parse_bts_attr1(T, Attr, [#resource_char{name = Attr,
			value = im_rest:geoaxis(Chars)} | Acc]);
parse_bts_attr1([{characters, Chars} | T],
		"longitude" = Attr, Acc) ->
	parse_bts_attr1(T, Attr, [#resource_char{name = Attr,
			value = im_rest:geoaxis(Chars)} | Acc]);
parse_bts_attr1([{characters, Chars} | T],
		"Longitude" = Attr, Acc) ->
% zte specific
	parse_bts_attr1(T, Attr, [#resource_char{name = Attr,
			value = im_rest:geoaxis(Chars)} | Acc]);
parse_bts_attr1([{characters, Chars} | T], "operationalState" = Attr, Acc) ->
	parse_bts_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_bts_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_bts_attr1(T, undefined, Acc);
parse_bts_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_bts_attr1(T2, undefined, Acc);
parse_bts_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_bts_attr1(T, Attr, Acc);
parse_bts_attr1([], undefined, Acc) ->
	Acc.

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
parse_gsm_cell({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_gsm_cell({startElement, _Uri, "GsmRelation", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",GsmRelation=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_geran, parse_function = parse_gsm_rel,
			parse_state = #geran_state{gsm_rel = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gsm_cell({startElement, _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gsm_cell({startElement, _Uri, _LocalName, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_gsm_cell({endElement, _Uri, "GsmCell", QName},
		[#state{dn_prefix = [CellDn | _], stack = Stack, spec_cache = Cache,
		parse_state = #geran_state{vs_data = NrmMap}, location = Location},
		#state{spec_cache = PrevCache,
		parse_state = GeranState} = PrevState | T1]) ->
	{[_ | T], _NewStack} = pop(startElement, QName, Stack),
	GsmCellAttr = parse_gsm_cell_attr(T, []),
	#geran_state{cells = GsmCellRels} = GeranState,
	VsData = #resource_char{name = "vsDataContainer",
			class_type = "VsDataContainerList", value = NrmMap,
			schema = "/resourceCatalogManagement/v3/schema/genericNrm#/"
					"definitions/VsDataContainerList"},
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = "/resourceCatalogManagement/v3/schema/genericNrm#/"
					"definitions/PeeParametersListType"},
	ClassType = "GsmCell",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = CellDn,
			description = "GSM Radio",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/GsmCell",
			specification = Spec,
			characteristic = lists:reverse([VsData, PeeParam | GsmCellAttr])},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			GsmCellRel = #resource_rel{id = Id, name = CellDn, rel_type = "contains",
					ref_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					parse_state = GeranState#geran_state{cells
					= [GsmCellRel | GsmCellRels]}} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_gsm_cell({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_gsm_cell_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_gsm_cell_attr1(Attributes, undefined, Acc).
% @hidden
parse_gsm_cell_attr1([{endElement, {_, "hoppingSequenceList"} = QName} | T1],
		undefined, Acc) ->
	{[_ | _HsList], T2} = pop(startElement, QName, T1),
	% @todo Implement hoppingSequenceList
	parse_gsm_cell_attr1(T2, undefined, Acc);
parse_gsm_cell_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T], "cellIdentity" = Attr, Acc) ->
	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
%parse_gsm_cell_attr1([{characters, Chars} | T], "cellAllocation" = Attr, Acc) ->
%	CellAllocation = [list_to_integer(C)
%			|| C <- string:tokens(Chars, [$\s, $\t, $\n, $\r])],
%	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
%			value = CellAllocation} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T], "cellAllocation" = Attr, Acc) ->
	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T], "ncc" = Attr, Acc) ->
	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T], "bcc" = Attr, Acc) ->
	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T], "lac" = Attr, Acc) ->
	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T], "mcc" = Attr, Acc) ->
	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T], "mnc" = Attr, Acc) ->
	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T], "rac" = Attr, Acc) ->
	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T], "racc" = Attr, Acc) ->
	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T], "tsc" = Attr, Acc) ->
	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T], "rxLevAccessMin" = Attr, Acc) ->
	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T], "msTxPwrMaxCCH" = Attr, Acc) ->
	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, "false"} | T],
		"rfHoppingEnabled" = Attr, Acc) ->
	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
			value = false} | Acc]);
parse_gsm_cell_attr1([{characters, "true"} | T],
		"rfHoppingEnabled" = Attr, Acc) ->
	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
			value = true} | Acc]);
%parse_gsm_cell_attr1([{characters, Chars} | T], "plmnPermitted" = Attr, Acc) ->
%	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
%			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T], "plmnPermitted" = Attr, Acc) ->
	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T], "administrativeState" = Attr,
		Acc) ->
	parse_gsm_cell_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_gsm_cell_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_gsm_cell_attr1(T, undefined, Acc);
parse_gsm_cell_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_gsm_cell_attr1(T, Attr, Acc);
parse_gsm_cell_attr1([], _Attr, Acc) ->
	Acc.

%% @hidden
parse_gsm_rel({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_gsm_rel({startElement, _Uri, _LocalName, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_gsm_rel({endElement, _Uri, "GsmRelation", QName},
		[#state{parse_state = #geran_state{gsm_rel = GsmRel}, stack = Stack},
		#state{parse_state = #geran_state{cell = Cell}} = PrevState | T1]) ->
	GeranState = PrevState#state.parse_state,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NewGsmRel = parse_gsm_rel_attr(T2, undefined, GsmRel),
	NewCell = choice_add(NewGsmRel, Cell),
	[PrevState#state{parse_state = GeranState#geran_state{
			cell = NewCell}} | T1];
parse_gsm_rel({endElement, _Uri, "GsmRelation", QName},
		[#state{parse_state = #geran_state{gsm_rel = GsmRel}, stack = Stack},
		#state{parse_state = #utran_state{fdd = Fdd}} = PrevState | T1]) ->
	UtranState = PrevState#state.parse_state,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NewGsmRel = parse_gsm_rel_attr(T2, undefined, GsmRel),
	NewFdd = choice_add(NewGsmRel, Fdd),
	[PrevState#state{parse_state = UtranState#utran_state{
			fdd = NewFdd}} | T1];
parse_gsm_rel({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_gsm_rel_attr([{startElement, {_, "attributes"} = QName, []} | T],
		undefined, Acc) ->
	{[_ | Attributes], _Rest} = pop(endElement, QName, T),
	parse_gsm_rel_attr1(Attributes, undefined, Acc).
% @hidden
parse_gsm_rel_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_gsm_rel_attr1(T, Attr, Acc);
parse_gsm_rel_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
% zte specific
	NewAcc = attribute_add("userLabel", Chars, Acc),
	parse_gsm_rel_attr1(T, Attr, NewAcc);
parse_gsm_rel_attr1([{characters, Chars} | T], "adjacentCell" = Attr, Acc) ->
	NewAcc = attribute_add("adjacentCell", Chars, Acc),
	parse_gsm_rel_attr1(T, Attr, NewAcc);
parse_gsm_rel_attr1([{characters, Chars} | T], "bcchFrequency" = Attr, Acc) ->
	NewAcc = attribute_add("bcch_frequency", list_to_integer(Chars), Acc),
	parse_gsm_rel_attr1(T, Attr, NewAcc);
parse_gsm_rel_attr1([{characters, Chars} | T], "ncc" = Attr, Acc) ->
	NewAcc = attribute_add("ncc", list_to_integer(Chars), Acc),
	parse_gsm_rel_attr1(T, Attr, NewAcc);
parse_gsm_rel_attr1([{characters, Chars} | T], "bcc" = Attr, Acc) ->
	NewAcc = attribute_add("bcc", list_to_integer(Chars), Acc),
	parse_gsm_rel_attr1(T, Attr, NewAcc);
parse_gsm_rel_attr1([{characters, Chars} | T], "lac" = Attr, Acc) ->
	NewAcc = attribute_add("lac", list_to_integer(Chars), Acc),
	parse_gsm_rel_attr1(T, Attr, NewAcc);
parse_gsm_rel_attr1([{characters, "true"} | T], "is_remove_allowed" = Attr, Acc) ->
	NewAcc = attribute_add("is_remove_allowed", "true", Acc),
	parse_gsm_rel_attr1(T, Attr, NewAcc);
parse_gsm_rel_attr1([{characters, "false"} | T], "is_remove_allowed" = Attr, Acc) ->
	NewAcc = attribute_add("is_remove_allowed", "false", Acc),
	parse_gsm_rel_attr1(T, Attr, NewAcc);
parse_gsm_rel_attr1([{characters, "true"} | T], "is_hoa_allowed" = Attr, Acc) ->
	NewAcc = attribute_add("is_hoa_allowed", "true", Acc),
	parse_gsm_rel_attr1(T, Attr, NewAcc);
parse_gsm_rel_attr1([{characters, "false"} | T], "is_hoa_allowed" = Attr, Acc) ->
	NewAcc = attribute_add("is_hoa_allowed", "false", Acc),
	parse_gsm_rel_attr1(T, Attr, NewAcc);
parse_gsm_rel_attr1([{characters, "true"} | T], "is_covered_by" = Attr, Acc) ->
	NewAcc = attribute_add("is_covered_by", "true", Acc),
	parse_gsm_rel_attr1(T, Attr, NewAcc);
parse_gsm_rel_attr1([{characters, "false"} | T], "is_covered_by" = Attr, Acc) ->
	NewAcc = attribute_add("is_covered_by", "false", Acc),
	parse_gsm_rel_attr1(T, Attr, NewAcc);
parse_gsm_rel_attr1([{startElement, {_, Attr}, []} | T], Attr, Acc) ->
	parse_gsm_rel_attr1(T, undefined, Acc);
parse_gsm_rel_attr1([],  _Attr, Acc) ->
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
				{ok, #specification{id = Id, href = Href, name = Name,
						version = Version}} ->
					SpecRef = #specification_ref{id = Id, href = Href, name = Name,
							version = Version},
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
attribute_add(Attribute, Value, #{"attributes" := Attributes} = NrmMap) ->
	NrmMap#{"attributes" => Attributes#{Attribute => Value}};
attribute_add(Attribute, Value, #{} = NrmMap) ->
	NrmMap#{"attributes" => #{Attribute => Value}}.

-spec choice_add(Choice, NrmMap) -> NrmMap
	when
		Choice :: map(),
		NrmMap :: map().
%% @doc Add `Choice' to possibly list of choices.
choice_add(Choice, #{"choice" := Choices} = NrmMap) ->
	NrmMap#{"choice" => [Choice | Choices]};
choice_add(Choice, #{} = NrmMap) ->
	NrmMap#{"choice" => [Choice]}.
