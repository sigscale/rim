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
-export([parse_bss/2, parse_bts/2, parse_gsm_cell/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

-define(PathCatalogSchema, "/resourceCatalogManagement/v3/resourceCatalogManagement").
-define(PathInventorySchema, "/resourceInventoryManagement/v3/resourceInventoryManagement").

-record(geran_state,
		{bss = [] :: string(),
		bts = [] :: string(),
		cell = [] :: string(),
		btss = [] :: [string()],
		cells = [] :: [string()]}).

%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

%% @hidden
parse_bss({startElement, _Uri, "BssFunction", QName,
		[{[], [], "id", Id}] = Attributes}, [#state{stack = Stack} = State | T]) ->
	DnComponent = ",BssFunction=" ++ Id,
	[State#state{parse_state = #geran_state{bss = DnComponent},
			stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_bss({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_bss({startElement,  _Uri, "BtsSiteMgr", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{parse_state = GeranState, stack = Stack} = State | T]) ->
	DnComponent = ",BtsSiteMgr=" ++ Id,
	[State#state{parse_function = parse_bts,
			parse_state = GeranState#geran_state{bts = DnComponent},
			stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_bss({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_bss({endElement, _Uri, "BssFunction", QName},
		[#state{stack = Stack} = State | T]) ->
	{[_ | T], NewStack} = pop(startElement, QName, Stack),
	parse_bss_attr(T, undefined, [State#state{stack = NewStack} | T], []);
parse_bss({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_bss_attr([{startElement, {"gn", "attributes"} = QName, []} | T1],
		undefined, State, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_bss_attr1(Attributes, undefined, State, Acc).
% @hidden
parse_bss_attr1([{characters, Chars} | T],
		"userLabel" = Attr, State, Acc) ->
	parse_bss_attr1(T, Attr, State,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_bss_attr1([{startElement, {"gn", Attr}, _} | T],
		Attr, State, Acc) ->
	parse_bss_attr1(T, undefined, State, Acc);
parse_bss_attr1([{endElement, {"gn", "vnfParametersList"}} | T],
		undefined, State, Acc) ->
	% @todo vnfParametersListType
	parse_bss_attr1(T, undefined, State, Acc);
parse_bss_attr1([{endElement, {"gn", "attributes"}}],
		undefined, State, _Acc) ->
	State;
parse_bss_attr1([{endElement, {"gn", Attr}} | T],
		undefined, State, Acc) ->
	parse_bss_attr1(T, Attr, State, Acc);
parse_bss_attr1([], undefined, [#state{dn_prefix = [CurrentDn | _],
		parse_state = GeranState, spec_cache = Cache} = State | T], Acc) ->
	#geran_state{bss = BssId, btss = Btss} = GeranState,
	BtsSiteMgr = #resource_char{name = "BtsSiteMgr", value = Btss},
	BssDn = CurrentDn ++ BssId,
	ClassType = "BssFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = BssDn,
			description = "GSM Base Station Subsystem (BSS)",
			category = "RAN",
			class_type = ClassType,
			base_type = "SubNetwork",
			schema = "/resourceInventoryManagement/v3/schema/BssFunction",
			specification = Spec,
			characteristic = lists:reverse([BtsSiteMgr | Acc])},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			Mod = im_xml_generic,
			F = parse_managed_element,
			[State#state{parse_module = Mod, parse_function = F,
					parse_state = GeranState#geran_state{bss = BssDn},
					spec_cache = NewCache} | T];
		{error, Reason} ->
			throw({add_resource, Reason})
	end.

%% @hidden
parse_bts({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_bts({startElement, _Uri, "GsmCell", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{parse_state = GeranState, stack = Stack} = State | T]) ->
	DnComponent = ",GsmCell=" ++ Id,
	[State#state{parse_function = parse_gsm_cell,
			parse_state = GeranState#geran_state{cell = DnComponent},
			stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_bts({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_bts({endElement, _Uri, "BtsSiteMgr", QName},
		[#state{stack = Stack} = State | T]) ->
	{[_ | T], NewStack} = pop(startElement, QName, Stack),
	parse_bts_attr(T, undefined, [State#state{stack = NewStack} | T], []);
parse_bts({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_bts_attr([{startElement, {"gn", "attributes"} = QName, []} | T1],
		undefined, State, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_bts_attr1(Attributes, undefined, State, Acc).
% @hidden
parse_bts_attr1([{characters, Chars} | T],
		"userLabel" = Attr, State, Acc) ->
	parse_bts_attr1(T, Attr, State,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_bts_attr1([{characters, Chars} | T],
		"latitude" = Attr, State, Acc) ->
	parse_bts_attr1(T, Attr, State,
			[#resource_char{name = Attr, value = im_rest:geoaxis(Chars)} | Acc]);
parse_bts_attr1([{characters, Chars} | T],
		"longitude" = Attr, State, Acc) ->
	parse_bts_attr1(T, Attr, State,
			[#resource_char{name = Attr, value = im_rest:geoaxis(Chars)} | Acc]);
parse_bts_attr1([{characters, Chars} | T],
		"operationalState" = Attr, State, Acc) ->
	parse_bts_attr1(T, Attr, State,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_bts_attr1([{startElement, {"gn", Attr}, _} | T],
		Attr, State, Acc) ->
	parse_bts_attr1(T, undefined, State, Acc);
parse_bts_attr1([{endElement, {"gn", "vnfParametersList"}} | T],
		undefined, State, Acc) ->
	% @todo vnfParametersListType
	parse_bts_attr1(T, undefined, State, Acc);
parse_bts_attr1([{endElement, {"gn", "attributes"}}],
		undefined, State, _Acc) ->
	State;
parse_bts_attr1([{endElement, {"gn", Attr}} | T],
		undefined, State, Acc) ->
	parse_bts_attr1(T, Attr, State, Acc);
parse_bts_attr1([], undefined, [#state{dn_prefix = [CurrentDn | _],
		parse_state = GeranState, spec_cache = Cache} = State | T], Acc) ->
	#geran_state{bss = BssId, bts = BtsId, btss = Btss,
			cells = Cells} = GeranState,
	GsmCell = #resource_char{name = "GsmCell", value = Cells},
	BtsDn = CurrentDn ++ BssId ++ BtsId,
	ClassType = "BtsSiteMgr",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = BtsDn,
			description = "GSM Base Transceiver Station (BTS)",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/BtsSiteMgr",
			specification = Spec,
			characteristic = lists:reverse([GsmCell | Acc])},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			State#state{parse_function = parse_bss,
					parse_state = [GeranState#geran_state{btss = [BtsDn | Btss],
					cells = []} | T], spec_cache = NewCache};
		{error, Reason} ->
			throw({add_resource, Reason})
	end.

%% @hidden
parse_gsm_cell({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_gsm_cell({startElement, _Uri, _LocalName, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_gsm_cell({endElement, _Uri, "GsmCell", QName},
		[#state{stack = Stack} = State | T]) ->
	{[_ | T], NewStack} = pop(startElement, QName, Stack),
	parse_gsm_cell_attr(T, [State#state{stack = NewStack} | T], []);
parse_gsm_cell({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_gsm_cell_attr([{startElement, {"gn", "attributes"} = QName, []} | T1],
		State, Acc) ->
	{[_ | Attributes], T2} = pop(endElement, QName, T1),
	parse_gsm_cell_attr1(Attributes, undefined, T2, State, Acc).
% @hidden
parse_gsm_cell_attr1([{endElement, {"gn", "hoppingSequenceList"} = QName} | T1],
		undefined, CellStack, State, Acc) ->
	{[_ | _HsList], T2} = pop(startElement, QName, T1),
	% @todo Implement hoppingSequenceList
	parse_gsm_cell_attr1(T2, undefined, CellStack, State, Acc);
parse_gsm_cell_attr1([{characters, Chars} | T],
		"userLabel" = Attr, CellStack, State, Acc) ->
	parse_gsm_cell_attr1(T, Attr, CellStack, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T],
		"cellIdentity" = Attr, CellStack, State, Acc) ->
	parse_gsm_cell_attr1(T, Attr, CellStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T],
		"cellAllocation" = Attr, CellStack, State, Acc) ->
	CellAllocation = [list_to_integer(C)
			|| C <- string:tokens(Chars, [$\s, $\t, $\n, $\r])],
	parse_gsm_cell_attr1(T, Attr, CellStack, State, [#resource_char{name = Attr,
			value = CellAllocation} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T],
		"ncc" = Attr, CellStack, State, Acc) ->
	parse_gsm_cell_attr1(T, Attr, CellStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T],
		"bcc" = Attr, CellStack, State, Acc) ->
	parse_gsm_cell_attr1(T, Attr, CellStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T],
		"lac" = Attr, CellStack, State, Acc) ->
	parse_gsm_cell_attr1(T, Attr, CellStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T],
		"mcc" = Attr, CellStack, State, Acc) ->
	parse_gsm_cell_attr1(T, Attr, CellStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T],
		"mnc" = Attr, CellStack, State, Acc) ->
	parse_gsm_cell_attr1(T, Attr, CellStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T],
		"rac" = Attr, CellStack, State, Acc) ->
	parse_gsm_cell_attr1(T, Attr, CellStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T],
		"racc" = Attr, CellStack, State, Acc) ->
	parse_gsm_cell_attr1(T, Attr, CellStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T],
		"tsc" = Attr, CellStack, State, Acc) ->
	parse_gsm_cell_attr1(T, Attr, CellStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T],
		"rxLevAccessMin" = Attr, CellStack, State, Acc) ->
	parse_gsm_cell_attr1(T, Attr, CellStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T],
		"msTxPwrMaxCCH" = Attr, CellStack, State, Acc) ->
	parse_gsm_cell_attr1(T, Attr, CellStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{characters, "false"} | T],
		"rfHoppingEnabled" = Attr, CellStack, State, Acc) ->
	parse_gsm_cell_attr1(T, Attr, CellStack, State, [#resource_char{name = Attr,
			value = false} | Acc]);
parse_gsm_cell_attr1([{characters, "true"} | T],
		"rfHoppingEnabled" = Attr, CellStack, State, Acc) ->
	parse_gsm_cell_attr1(T, Attr, CellStack, State, [#resource_char{name = Attr,
			value = true} | Acc]);
parse_gsm_cell_attr1([{characters, Chars} | T],
		"plmnPermitted" = Attr, CellStack, State, Acc) ->
	parse_gsm_cell_attr1(T, Attr, CellStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_gsm_cell_attr1([{startElement, {"gn", Attr}, _} | T],
		Attr, CellStack, State, Acc) ->
	parse_gsm_cell_attr1(T, undefined, CellStack, State, Acc);
parse_gsm_cell_attr1([{endElement, {"gn", Attr}} | T],
		undefined, CellStack, State, Acc) ->
	parse_gsm_cell_attr1(T, Attr, CellStack, State, Acc);
parse_gsm_cell_attr1([], _Attr, CellStack, State, Acc) ->
	parse_gsm_cell_rels(CellStack, State, Acc,
			#{gsmRel => [], utranRel => [], eutranRel => []}).

% @hidden
parse_gsm_cell_rels([{startElement,
		{"gn", "GsmRelation"} = QName, _} | _] = Stack,
		[#state{dn_prefix = [CurrentDn | _],
		parse_state = #geran_state{bss = BssId, bts = BtsId,
		cell = CellId}} | _T] = State, Characteristics,
		#{gsmRel := GsmRels} = Acc) ->
	CellDn = CurrentDn ++ BssId ++ BtsId ++ CellId,
	{Attributes, T} = pop(endElement, QName, Stack),
	Relation = gsm_relation(CellDn, Attributes),
	NewAcc = Acc#{gsmRel := [Relation | GsmRels]},
	parse_gsm_cell_rels(T, State, Characteristics, NewAcc);
parse_gsm_cell_rels([{startElement,
		{"un", "UtranRelation"} = QName, _} | _] = Stack,
		[#state{dn_prefix = [CurrentDn | _],
		parse_state = #geran_state{bss = BssId, bts = BtsId,
		cell = CellId}} | _T] = State, Characteristics,
		#{utranRel := UtranRels} = Acc) ->
	CellDn = CurrentDn ++ BssId ++ BtsId ++ CellId,
	{Attributes, T} = pop(endElement, QName, Stack),
	Relation = utran_relation(CellDn, Attributes),
	NewAcc = Acc#{utranRel := [Relation | UtranRels]},
	parse_gsm_cell_rels(T, State, Characteristics, NewAcc);
parse_gsm_cell_rels([{startElement,
		{"en", "EutranRelation"} = QName, _} | _] = Stack,
		[#state{dn_prefix = [CurrentDn | _],
		parse_state = #geran_state{bss = BssId, bts = BtsId,
		cell = CellId}} | T] = State, Characteristics,
		#{eutranRel := EutranRels} = Acc) ->
	CellDn = CurrentDn ++ BssId ++ BtsId ++ CellId,
	{Attributes, T} = pop(endElement, QName, Stack),
	Relation = eutran_relation(CellDn, Attributes),
	NewAcc = Acc#{eutranRel := [Relation | EutranRels]},
	parse_gsm_cell_rels(T, State, Characteristics, NewAcc);
parse_gsm_cell_rels(CellStack,
		[#state{dn_prefix = [CurrentDn | _],
				parse_state = GeranState,
				spec_cache = Cache} = State | T], Characteristics, Acc) ->
	F1 = fun(gsmRel, [], Acc1) ->
				Acc1;
			(gsmRel, R, Acc1) ->
				[#resource_char{name = "gsmRelation",
						class_type = "GsmRelationList",
						schema = ?PathInventorySchema ++ "#definitions/GsmRelationList",
						value = lists:reverse(R)} | Acc1];
			(utranRel, [], Acc1) ->
				Acc1;
			(utranRel, R, Acc1) ->
				[#resource_char{name = "utranRelation",
						class_type = "UtranRelationList",
						schema = ?PathInventorySchema ++ "#definitions/UtranRelationList",
						value = lists:reverse(R)} | Acc1];
			(eutranRel, [], Acc1) ->
				Acc1;
			(eutranRel, R, Acc1) ->
				[#resource_char{name = "eUtranRelation",
						class_type = "EUtranRelationList",
						schema = ?PathInventorySchema ++ "#definitions/EUtranRelationList",
						value = R} | Acc1]
	end,
	NewCharacteristics = maps:fold(F1, Characteristics, Acc),
	#geran_state{bss = BssId, bts = BtsId, cell = CellId,
			cells = Cells} = GeranState,
	CellDn = CurrentDn ++ BssId ++ BtsId ++ CellId,
	ClassType = "GsmCell",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = CellDn,
			description = "GSM radio",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/GsmCell",
			specification = Spec,
			characteristic = NewCharacteristics},
	case im:add_resource(Resource) of
		{ok, #resource{}} ->
			NewState = State#state{parse_state = GeranState#geran_state{
					cells = [CellDn | Cells]}, spec_cache = NewCache},
			parse_gsm_cell_pol(CellStack, [NewState | T], NewCharacteristics);
		{error, Reason} ->
			throw({add_resource, Reason})
	end.

%% @hidden
gsm_relation(DnPrefix, Stack) ->
	gsm_relation(Stack, [], DnPrefix, #{}).
%% @hidden
gsm_relation([{endElement, {"gn", "GsmRelation"} = QName} | T] = _Stack,
		[] = _State, DnPrefix, Acc) ->
	gsm_relation(T, [QName], DnPrefix, Acc);
gsm_relation([{endElement, {"xn", "VsDataContainer"} = QName} | _] = Stack,
		State, DnPrefix, Acc) ->
	{VsStack, T} = pop(startElement, QName, Stack),
	NewAcc = Acc#{"VsDataContainer" => vendor_specific(VsStack, DnPrefix)},
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{endElement, QName} | T] = _Stack, State, DnPrefix, Acc) ->
	gsm_relation(T, [QName | State], DnPrefix, Acc);
gsm_relation([{characters, Chars} | T],
		[{"gn", "adjacentCell"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("adjacentCell", Chars, Acc),
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{characters, Chars} | T],
		[{"gn", "bcchFrequency"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("bcchFrequency", Chars, Acc),
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{characters, Chars} | T],
		[{"gn", "ncc"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("ncc", Chars, Acc),
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{characters, Chars} | T],
		[{"gn", "bcc"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("bcc", Chars, Acc),
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{characters, Chars} | T],
		[{"gn", "lac"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("lac", Chars, Acc),
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{characters, Chars} | T],
		[{"gn", "isRemoveAllowed"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("isRemoveAllowed", Chars, Acc),
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{characters, Chars} | T],
		[{"gn", "isHOAllowed"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("isHOAllowed", Chars, Acc),
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{characters, Chars} | T],
		[{"gn", "isESCoveredBy"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("isESCoveredBy", Chars, Acc),
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{startElement, {"gn", "GsmRelation"} = QName, XmlAttr}],
		[QName], _DnPrefix, Acc) ->
	{_Uri, _Prefix, "id", RelId} = lists:keyfind("id", 3, XmlAttr),
	#{"@type" => "GsmRelation",
			"@schemaLocation" => ?PathInventorySchema ++ "#/definitions/GsmRelation",
			"value" => Acc#{"id" => RelId}};
gsm_relation([{startElement, QName, _} | T], [QName | State], DnPrefix, Acc) ->
	gsm_relation(T, State, DnPrefix, Acc).

%% @hidden
utran_relation(DnPrefix, Stack) ->
	utran_relation(Stack, [], DnPrefix, #{}).
%% @hidden
utran_relation([{endElement, {"un", "UtranRelation"} = QName} | T] = _Stack,
		[] = _State, DnPrefix, Acc) ->
	utran_relation(T, [QName], DnPrefix, Acc);
utran_relation([{endElement, {"xn", "VsDataContainer"} = QName} | _] = Stack,
		State, DnPrefix, Acc) ->
	{VsStack, T} = pop(startElement, QName, Stack),
	NewAcc = Acc#{"VsDataContainer" => vendor_specific(VsStack, DnPrefix)},
	utran_relation(T, State, DnPrefix, NewAcc);
utran_relation([{endElement, QName} | T] = _Stack, State, DnPrefix, Acc) ->
	utran_relation(T, [QName | State], DnPrefix, Acc);
utran_relation([{characters, Chars} | T],
		[{"un", "adjacentCell"}, {"un", "attributes"},
		{"un", "UtranRelation"}] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("adjacentCell", Chars, Acc),
	utran_relation(T, State, DnPrefix, NewAcc);
utran_relation([{startElement, {"un", "UtranRelation"} = QName, XmlAttr}],
		[QName], _DnPrefix, Acc) ->
	{_Uri, _Prefix, "id", RelId} = lists:keyfind("id", 3, XmlAttr),
	#{"@type" => "UtranRelation",
			"@schemaLocation" => ?PathInventorySchema ++ "#/definitions/UtranRelation",
			"value" => Acc#{"id" => RelId}};
utran_relation([{startElement, QName, _} | T], [QName | State], DnPrefix, Acc) ->
	utran_relation(T, State, DnPrefix, Acc).

%% @hidden
eutran_relation(DnPrefix, Stack) ->
	eutran_relation(Stack, [], DnPrefix, #{}).
%% @hidden
eutran_relation([{endElement, {"en", "EUtranRelation"} = QName} | T] = _Stack,
		[] = _State, DnPrefix, Acc) ->
	eutran_relation(T, [QName], DnPrefix, Acc);
eutran_relation([{endElement, {"xn", "VsDataContainer"} = QName} | _] = Stack,
		State, DnPrefix, Acc) ->
	{VsStack, T} = pop(startElement, QName, Stack),
	NewAcc = Acc#{"VsDataContainer" => vendor_specific(VsStack, DnPrefix)},
	eutran_relation(T, State, DnPrefix, NewAcc);
eutran_relation([{endElement, QName} | T] = _Stack, State, DnPrefix, Acc) ->
	eutran_relation(T, [QName | State], DnPrefix, Acc);
eutran_relation([{characters, Chars} | T],
		[{"en", "tCI"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("tCI", list_to_integer(Chars), Acc),
	eutran_relation(T, State, DnPrefix, NewAcc);
eutran_relation([{characters, "true"} | T],
		[{"en", "isRemoveAllowed"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("isRemoveAllowed", true, Acc),
	eutran_relation(T, State, DnPrefix, NewAcc);
eutran_relation([{characters, "false"} | T],
		[{"en", "isRemoveAllowed"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("isRemoveAllowed", false, Acc),
	eutran_relation(T, State, DnPrefix, NewAcc);
eutran_relation([{characters, "true"} | T],
		[{"en", "isHOAllowed"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("isHOAllowed", true, Acc),
	eutran_relation(T, State, DnPrefix, NewAcc);
eutran_relation([{characters, "false"} | T],
		[{"en", "isHOAllowed"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("isHOAllowed", false, Acc),
	eutran_relation(T, State, DnPrefix, NewAcc);
eutran_relation([{characters, "true"} | T],
		[{"en", "isICICInformationSendAllowed"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("isICICInformationSendAllowed", true, Acc),
	eutran_relation(T, State, DnPrefix, NewAcc);
eutran_relation([{characters, "false"} | T],
		[{"en", "isICICInformationSendAllowed"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("isICICInformationSendAllowed", false, Acc),
	eutran_relation(T, State, DnPrefix, NewAcc);
eutran_relation([{characters, "true"} | T],
		[{"en", "isLBAllowed"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("isLBAllowed", true, Acc),
	eutran_relation(T, State, DnPrefix, NewAcc);
eutran_relation([{characters, "false"} | T],
		[{"en", "isLBAllowed"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("isLBAllowed", false, Acc),
	eutran_relation(T, State, DnPrefix, NewAcc);
eutran_relation([{characters, Chars} | T],
		[{"en", "adjacentCell"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("adjacentCell", Chars, Acc),
	eutran_relation(T, State, DnPrefix, NewAcc);
eutran_relation([{characters, "no"} | T],
		[{"en", "isEsCoveredBy"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("isEsCoveredBy", "no", Acc),
	eutran_relation(T, State, DnPrefix, NewAcc);
eutran_relation([{characters, "partial"} | T],
		[{"en", "isEsCoveredBy"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("isEsCoveredBy", "partial", Acc),
	eutran_relation(T, State, DnPrefix, NewAcc);
eutran_relation([{characters, "yes"} | T],
		[{"en", "isEsCoveredBy"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("isEsCoveredBy", "yes", Acc),
	eutran_relation(T, State, DnPrefix, NewAcc);
eutran_relation([{characters, Chars} | T],
		[{"en", "cellIndividualOffset"} | _] = State, DnPrefix, Acc) ->
	case is_member(Chars) of
		true ->
			NewAcc = attribute_add("cellIndividualOffset", Chars, Acc);
		false ->
			NewAcc = attribute_add("cellIndividualOffset", undefined, Acc)
	end,
	eutran_relation(T, State, DnPrefix, NewAcc);
eutran_relation([{characters, Chars} | T],
		[{"en", "qOffset"} | _] = State, DnPrefix, Acc) ->
	case is_member(Chars) of
		true ->
			NewAcc = attribute_add("qOffset", Chars, Acc);
		false ->
			NewAcc = attribute_add("qOffset", undefined, Acc)
	end,
	eutran_relation(T, State, DnPrefix, NewAcc);
eutran_relation([{startElement, {"en", "EUtranRelation"} = QName, XmlAttr}],
		[QName], _DnPrefix, Acc) ->
	{_Uri, _Prefix, "id", RelId} = lists:keyfind("id", 3, XmlAttr),
	#{"@type" => "EUtranRelation",
			"@schemaLocation" => ?PathInventorySchema ++ "#/definitions/EUtranRelation",
			"value" => Acc#{"id" => RelId}};
eutran_relation([{startElement, QName, _} | T], [QName | State], DnPrefix, Acc) ->
	eutran_relation(T, State, DnPrefix, Acc).

% @hidden
vendor_specific(Stack, DnPrefix) ->
	vendor_specific(Stack, [], DnPrefix, #{}).
% @hidden
vendor_specific([{startElement, {"xn", "VsDataContainer"} = QName,
		XmlAttr} | T] = _Stack, [], DnPrefix, Acc) ->
	{_Uri, _Prefix, "id", ID} = lists:keyfind("id", 3, XmlAttr),
	vendor_specific(T, [QName], DnPrefix, Acc#{"id" => ID});
vendor_specific([{startElement, QName, _XmlAttr} | T] = _Stack,
		State, DnPrefix, Acc) ->
	vendor_specific(T, [QName | State], DnPrefix, Acc);
vendor_specific([{characters, Chars} | T],
		[{"xn", "vsDataType"}, {"xn", "attributes"},
		{"xn", "VsDataContainer"} | _] = State, DnPrefix, Acc) ->
	vendor_specific(T, State, DnPrefix, Acc#{"vsDataType" => Chars});
vendor_specific([{characters, Chars} | T],
		[{"xn", "vsDataFormatVersion"}, {"xn", "attributes"},
		{"xn", "VsDataContainer"} | _] = State,
		DnPrefix, Acc) ->
	vendor_specific(T, State, DnPrefix, Acc#{"vsDataFormatVersion" => Chars});
vendor_specific([{characters, _Chars} | T], [{"xn", "vsData"},
		{"xn", "attributes"}, {"xn", "VsDataContainer"} | _] = State,
		DnPrefix, Acc) ->
	% @todo handle vsData
	vendor_specific(T, State, DnPrefix, Acc);
vendor_specific([{endElement, {"xn", "VsDataContainer"} = QName}],
		[QName], _DnPrefix, Acc) ->
	Acc;
vendor_specific([{endElement, QName} | T],
		[QName | State], DnPrefix, Acc) ->
	vendor_specific(T, State, DnPrefix, Acc).

%% @hidden
parse_gsm_cell_pol([{startElement,
		{"sp", "InterRatEsPolicies"} = QName, _} | _] = Stack,
		[#state{dn_prefix = [CurrentDn | _],
		parse_state = #geran_state{bss = BssId, bts = BtsId,
		cell = CellId}} = State | _], _Characteristics) ->
	CellDn = CurrentDn ++ BssId ++ BtsId ++ CellId,
	{Attributes, _T} = pop(endElement, QName, Stack),
	inter_rates_policy(CellDn, Attributes),
	State#state{parse_function = parse_bts};
parse_gsm_cell_pol(_Stack, State, _Characteristics) ->
	State#state{parse_function = parse_bts}.

%% @hidden
inter_rates_policy(DnPrefix, Stack) ->
	inter_rates_policy(Stack, [], DnPrefix, #{}).
%% @hidden
inter_rates_policy([{endElement,
		{"sp", "InterRatEsPolicies"} = QName} | T] = _Stack,
		[] = _State, DnPrefix, Acc) ->
	inter_rates_policy(T, [QName], DnPrefix, Acc);
inter_rates_policy([{endElement, QName} | T] = _Stack, State,
		DnPrefix, Acc) ->
	inter_rates_policy(T, [QName | State], DnPrefix, Acc);
inter_rates_policy([{characters, Chars} | T],
		[{"sp", "LoadThreshold"},
		{"sp", "interRatEsActivationOriginalCellParameters"} | _] = State,
		DnPrefix, Acc) ->
	NewAcc = Acc#{"interRatEsActivationOriginalCellParameters" =>
			#{"LoadThreshold" => list_to_integer(Chars)}},
	inter_rates_policy(T, State, DnPrefix, NewAcc);
inter_rates_policy([{characters, Chars} | T],
		[{"sp", "TimeDuration"},
		{"sp", "interRatEsActivationOriginalCellParameters"} | _] = State,
		DnPrefix, Acc) ->
	NewAcc = Acc#{"interRatEsActivationOriginalCellParameters" =>
			#{"TimeDuration" => list_to_integer(Chars)}},
	inter_rates_policy(T, State, DnPrefix, NewAcc);
inter_rates_policy([{characters, Chars} | T],
		[{"sp", "LoadThreshold"},
		{"sp", "interRatEsActivationCandidateCellParameters"} | _] = State,
		DnPrefix, Acc) ->
	NewAcc = Acc#{"interRatEsActivationCandidateCellParameters" =>
			#{"LoadThreshold" => list_to_integer(Chars)}},
	inter_rates_policy(T, State, DnPrefix, NewAcc);
inter_rates_policy([{characters, Chars} | T],
		[{"sp", "TimeDuration"},
		{"sp", "interRatEsActivationCandidateCellParameters"} | _] = State,
		DnPrefix, Acc) ->
	NewAcc = Acc#{"interRatEsActivationCandidateCellParameters" =>
			#{"TimeDuration" => list_to_integer(Chars)}},
	inter_rates_policy(T, State, DnPrefix, NewAcc);
inter_rates_policy([{characters, Chars} | T],
		[{"sp", "LoadThreshold"},
		{"sp", "interRatEsDeactivationCandidateCellParameters"} | _] = State,
		DnPrefix, Acc) ->
	NewAcc = Acc#{"interRatEsDeactivationCandidateCellParameters" =>
			#{"LoadThreshold" => list_to_integer(Chars)}},
	inter_rates_policy(T, State, DnPrefix, NewAcc);
inter_rates_policy([{characters, Chars} | T],
		[{"sp", "TimeDuration"},
		{"sp", "interRatEsDeactivationCandidateCellParameters"} | _] = State,
		DnPrefix, Acc) ->
	NewAcc = Acc#{"interRatEsDeactivationCandidateCellParameters" =>
			#{"TimeDuration" => list_to_integer(Chars)}},
	inter_rates_policy(T, State, DnPrefix, NewAcc);
inter_rates_policy([{startElement, {"sp", "InterRatEsPolicies"} = QName,
		XmlAttr}], [QName], _DnPrefix, Acc) ->
	{_Uri, _Prefix, "id", Id} = lists:keyfind("id", 3, XmlAttr),
	Acc#{id => Id};
inter_rates_policy([{startElement, QName, _} | T], [QName | State],
		DnPrefix, Acc) ->
	inter_rates_policy(T, State, DnPrefix, Acc).

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

-spec is_member(Element) -> Boolean
	when
		Element :: string(),
		Boolean :: boolean().
%% @doc check whether `Element' is a member of the list.
is_member(Element) when is_list(Element) ->
	List = ["dB-24", "dB-22", "dB-20", "dB-18", "dB-16", "dB-14", "dB-12",
			"dB-10", "dB-8", "dB-6", "dB-5", "dB-4", "dB-3", "dB-2", "dB-1",
			"dB0", "dB1", "dB2", "dB3", "dB4", "dB5", "dB6", "dB8", "dB10",
			"dB12", "dB14", "dB16", "dB18", "dB20", "dB22", "dB24"],
	lists:member(Element, List).
