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

-record(state,
		{parse_module :: atom(),
		parse_function :: atom(),
		parse_state :: geran_state(),
		dn_prefix = [] :: string(),
      subnet = []:: string(),
		stack = [] :: list()}).

-record(geran_state,
		{bss = [] :: string(),
		bts = [] :: string(),
		cell = [] :: string(),
		btss = [] :: [string()],
		cells = [] :: [string()]}).
-type geran_state() :: #geran_state{}.

%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

%% @hidden
parse_bss({startElement, _Uri, "BssFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{parse_state = _GeranState, stack = Stack} = State) ->
	DnComponent = ",BssFunction=" ++ Id,
	State#state{parse_module = ?MODULE, parse_function = parse_bss,
			parse_state = #geran_state{bss = DnComponent},
			stack = [{startElement, QName, Attributes} | Stack]};
parse_bss({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_bss({startElement,  _Uri, "BtsSiteMgr", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{parse_state = ParseState,
		stack = Stack} = State) ->
	DnComponent = ",BtsSiteMgr=" ++ Id,
	State#state{parse_module = ?MODULE, parse_function = parse_bts,
			parse_state = ParseState#geran_state{bts = DnComponent},
			stack = [{startElement, QName, Attributes} | Stack]};
parse_bss({startElement, _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_bss({endElement, _Uri, "BssFunction", QName},
		#state{stack = Stack} = State) ->
	{[_ | T], NewStack} = pop(startElement, QName, Stack),
	parse_bss_attr(T, undefined, State#state{stack = NewStack}, []);
parse_bss({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

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
parse_bss_attr1([], undefined, #state{dn_prefix = DnPrefix,
		subnet = SubId, parse_state = GeranState} = State, Acc) ->
	#geran_state{bss = BssId, btss = Btss} = GeranState,
	BtsSiteMgr = #resource_char{name = "BtsSiteMgr", value = Btss},
	BssDn = DnPrefix ++ SubId ++ BssId,
	Resource = #resource{name = BssDn,
			description = "GSM Base Station Subsystem (BSS)",
			category = "RAN",
			class_type = "BssFunction",
			base_type = "SubNetwork",
			schema = "/resourceInventoryManagement/v3/schema/BssFunction",
			specification = #specification_ref{},
			characteristic = lists:reverse([BtsSiteMgr | Acc])},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			Mod = im_xml_cm_bulk,
			F = parse_generic,
			State#state{parse_module = Mod, parse_function = F,
					parse_state = #geran_state{bss = BssDn, btss = []}};
		{error, Reason} ->
			{error, Reason}
	end.

%% @hidden
parse_bts({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_bts({startElement, _Uri, "GsmCell", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{parse_state = ParseState, stack = Stack} = State) ->
	DnComponent = ",GsmCell=" ++ Id,
	State#state{parse_module = ?MODULE, parse_function = parse_gsm_cell,
			parse_state = ParseState#geran_state{cell = DnComponent},
			stack = [{startElement, QName, Attributes} | Stack]};
parse_bts({startElement, _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_bts({endElement, _Uri, "BtsSiteMgr", QName},
		#state{stack = Stack} = State) ->
	{[_ | T], NewStack} = pop(startElement, QName, Stack),
	parse_bts_attr(T, undefined, State#state{stack = NewStack}, []);
parse_bts({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

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
parse_bts_attr1([], undefined, #state{dn_prefix = DnPrefix, subnet = SubId,
		parse_state = ParseState} = State, Acc) ->
	#geran_state{bss = BssId, bts = BtsId, btss = Btss,
			cells = Cells} = ParseState,
	GsmCell = #resource_char{name = "GsmCell", value = Cells},
	BtsDn = DnPrefix ++ SubId ++ BssId ++ BtsId,
	Resource = #resource{name = BtsDn,
			description = "GSM Base Transceiver Station (BTS)",
			category = "RAN",
			class_type = "BtsSiteMgr",
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/BtsSiteMgr",
			specification = #specification_ref{},
			characteristic = lists:reverse([GsmCell | Acc])},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			State#state{parse_module = ?MODULE, parse_function = parse_bss,
					parse_state = ParseState#geran_state{btss = [BtsDn | Btss],
					cells = []}};
		{error, Reason} ->
			{error, Reason}
	end.

%% @hidden
parse_gsm_cell({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_gsm_cell({startElement, _Uri, _LocalName, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_gsm_cell({endElement,  _Uri, "GsmCell", QName},
		#state{stack = Stack} = State) ->
	{[_ | T], NewStack} = pop(startElement, QName, Stack),
	parse_gsm_cell_attr(T, State#state{stack = NewStack}, []);
parse_gsm_cell({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

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
		{"gn", "GsmRelation"} = QName, XmlAttr} | T1],
		State, Characteristics, #{gsmRel := GsmRels} = Acc) ->
	{_Uri, _Prefix, "id", RelID} = lists:keyfind("id", 3, XmlAttr),
	{[_ | Attributes], T2} = pop(endElement, QName, T1),
	Relation = parse_gsm_cell_rel(Attributes,
			undefined, #gsm_relation{id = RelID}),
	NewAcc = Acc#{gsmRel := [Relation | GsmRels]},
	parse_gsm_cell_rels(T2, State, Characteristics, NewAcc);
parse_gsm_cell_rels([{startElement,
		{"un", "UtranRelation"} = QName, XmlAttr} | T1],
		State, Characteristics, #{utranRel := UtranRels} = Acc) ->
	{_Uri, _Prefix, "id", RelID} = lists:keyfind("id", 3, XmlAttr),
	{[_ | Attributes], T2} = pop(endElement, QName, T1),
	Relation = parse_utran_cell_rel(Attributes,
			undefined, #utran_relation{id = RelID}),
	NewAcc = Acc#{utranRel := [Relation | UtranRels]},
	parse_gsm_cell_rels(T2, State, Characteristics, NewAcc);
parse_gsm_cell_rels([{startElement,
		{"en", "EutranRelation"} = QName, XmlAttr} | T1],
		State, Characteristics, #{eutranRel := EutranRels} = Acc) ->
	{_Uri, _Prefix, "id", RelID} = lists:keyfind("id", 3, XmlAttr),
	{[_ | Attributes], T2} = pop(endElement, QName, T1),
	Relation = parse_eutran_cell_rel(Attributes,
			undefined, #eutran_relation{id = RelID}),
	NewAcc = Acc#{eutranRel := [Relation | EutranRels]},
	parse_gsm_cell_rels(T2, State, Characteristics, NewAcc);
parse_gsm_cell_rels(CellStack,
		#state{dn_prefix = DnPrefix, subnet = SubId,
				parse_state = #geran_state{bss = BssId, bts = BtsId,
				cell = CellId, cells = Cells} = ParseState} = State, Characteristics, Acc) ->
	F1 = fun(gsmRel, [], Acc1) ->
				Acc1;
			(gsmRel, R, Acc1) ->
				[#resource_char{name = "gsmRelation", value = R} | Acc1];
			(utranReln, [], Acc1) ->
				Acc1;
			(utranRel, R, Acc1) ->
				[#resource_char{name = "utranRelation", value = R} | Acc1];
			(eutranRel, [], Acc1) ->
				Acc1;
			(eutranRel, R, Acc1) ->
				[#resource_char{name = "eUtranRelation", value = R} | Acc1]
	end,
	NewCharacteristics = maps:fold(F1, Characteristics, Acc),
	CellDn = DnPrefix ++ SubId ++ BssId ++ BtsId ++ CellId,
	Resource = #resource{name = CellDn,
			description = "GSM radio",
			category = "RAN",
			class_type = "GsmCell",
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/GsmCell",
			specification = #specification_ref{},
			characteristic = NewCharacteristics},
	case im:add_resource(Resource) of
		{ok, #resource{}} ->
			NewState = State#state{parse_state = ParseState#geran_state{cells = [CellDn | Cells]}},
			parse_gsm_cell_pol(NewCharacteristics, CellStack, NewState);
		{error, Reason} ->
			{error, Reason}
	end.
% @hidden
parse_gsm_cell_rel([{endElement, {"gn", "attributes"}} | T],
		undefined, Acc) ->
	parse_gsm_cell_rel(T, undefined, Acc);
parse_gsm_cell_rel([{endElement, {"gn", Attr}} | T], undefined, Acc) ->
	parse_gsm_cell_rel(T, Attr, Acc);
parse_gsm_cell_rel([{characters, Chars} | T], "adjacentCell" = Attr, Acc) ->
	parse_gsm_cell_rel(T, Attr, Acc#gsm_relation{adjacent_cell = Chars});
parse_gsm_cell_rel([{characters, Chars} | T], "bcch_frequency" = Attr, Acc) ->
	parse_gsm_cell_rel(T, Attr,
		Acc#gsm_relation{bcch_frequency = list_to_integer(Chars)});
parse_gsm_cell_rel([{characters, Chars} | T], "ncc" = Attr, Acc) ->
	parse_gsm_cell_rel(T, Attr,
		Acc#gsm_relation{ncc = list_to_integer(Chars)});
parse_gsm_cell_rel([{characters, Chars} | T], "bcc" = Attr, Acc) ->
	parse_gsm_cell_rel(T, Attr,
		Acc#gsm_relation{bcc = list_to_integer(Chars)});
parse_gsm_cell_rel([{characters, Chars} | T], "lac" = Attr, Acc) ->
	parse_gsm_cell_rel(T, Attr,
		Acc#gsm_relation{lac = list_to_integer(Chars)});
parse_gsm_cell_rel([{characters, Chars} | T], "is_remove_allowed" = Attr, Acc) ->
	parse_gsm_cell_rel(T, Attr,
		Acc#gsm_relation{lac = list_to_atom(Chars)});
parse_gsm_cell_rel([{characters, Chars} | T], "is_hoa_allowed" = Attr, Acc) ->
	parse_gsm_cell_rel(T, Attr,
		Acc#gsm_relation{is_hoa_allowed = list_to_atom(Chars)});
parse_gsm_cell_rel([{characters, Chars} | T], "is_covered_by" = Attr, Acc) ->
	parse_gsm_cell_rel(T, Attr,
		Acc#gsm_relation{is_covered_by = list_to_atom(Chars)});
parse_gsm_cell_rel([{startElement, {"gn", Attr}, []} | T], Attr, Acc) ->
	parse_gsm_cell_rel(T, undefined, Acc);
parse_gsm_cell_rel([{startElement, {"gn", "attributes"}, []}], _Attr, Acc) ->
	Acc.

% @hidden
parse_utran_cell_rel([{endElement, {"un", "attributes"}} | T],
		undefined, Acc) ->
	parse_utran_cell_rel(T, undefined, Acc);
parse_utran_cell_rel([{endElement, {"un", Attr}} | T],
		undefined, Acc) ->
	parse_utran_cell_rel(T, Attr, Acc);
parse_utran_cell_rel([{characters, Chars} | T],
		"adjacentCell" = Attr, Acc) ->
	parse_utran_cell_rel(T, Attr, Acc#utran_relation{adjacent_cell = Chars});
parse_utran_cell_rel([{startElement, {"un", "attributes"}, []}],
		_Attr, Acc) ->
	Acc.

% @hidden
parse_eutran_cell_rel([{endElement, {"en", "attributes"}} | T],
		undefined, Acc) ->
	parse_eutran_cell_rel(T, undefined, Acc);
parse_eutran_cell_rel([{endElement, {"en", Attr}} | T],
		undefined, Acc) ->
	parse_eutran_cell_rel(T, Attr, Acc);
parse_eutran_cell_rel([{characters, Chars} | T],
		"tci" = Attr, Acc) ->
	parse_eutran_cell_rel(T, Attr,
		Acc#eutran_relation{tci = list_to_integer(Chars)});
parse_eutran_cell_rel([{characters, Chars} | T],
		"isRemoveAllowed" = Attr, Acc) ->
	parse_eutran_cell_rel(T, Attr,
			Acc#eutran_relation{is_remove_allowed = list_to_atom(Chars)});
parse_eutran_cell_rel([{characters, Chars} | T],
		"isHoaAllowed" = Attr, Acc) ->
	parse_eutran_cell_rel(T, Attr,
			Acc#eutran_relation{is_hoa_allowed = list_to_atom(Chars)});
parse_eutran_cell_rel([{characters, Chars} | T],
		"isIcicInformationSendAllowed" = Attr, Acc) ->
	parse_eutran_cell_rel(T, Attr,
			Acc#eutran_relation{is_icic_information_send_allowed = list_to_atom(Chars)});
parse_eutran_cell_rel([{characters, Chars} | T],
		"isLbAllowed" = Attr, Acc) ->
	parse_eutran_cell_rel(T, Attr,
			Acc#eutran_relation{is_lb_allowed = list_to_atom(Chars)});
parse_eutran_cell_rel([{characters, Chars} | T],
		"adjacentCell" = Attr, Acc) ->
	parse_eutran_cell_rel(T, Attr,
			Acc#eutran_relation{adjacent_cell = Chars});
parse_eutran_cell_rel([{characters, Chars} | T],
		"isEsCoveredBy" = Attr, Acc) ->
	parse_eutran_cell_rel(T, Attr,
			Acc#eutran_relation{is_es_covered_by = list_to_atom(Chars)});
parse_eutran_cell_rel([{characters, Chars} | T],
		"cellIndividualOffset" = Attr, Acc) ->
	parse_eutran_cell_rel(T, Attr,
			Acc#eutran_relation{cell_individual_offset = Chars});
parse_eutran_cell_rel([{characters, Chars} | T],
		"qOffset" = Attr, Acc) ->
	parse_eutran_cell_rel(T, Attr,
			Acc#eutran_relation{q_offset = Chars});
parse_eutran_cell_rel([{startElement, {"en", "attributes"}, []}],
		_Attr, Acc) ->
	Acc.

%% @hidden
parse_gsm_cell_pol(_Characteristics,
		[{startElement, {"sp", "IneractEsPolicies"} = QName, []} | T1], State) ->
	{[_ | _Attributes], _T2} = pop(endElement, QName, T1),
	State#state{parse_function = parse_bts};
parse_gsm_cell_pol(_Characteristics, _CellStack, State) ->
	State#state{parse_function = parse_bts}.

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

