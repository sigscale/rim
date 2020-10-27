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
-module(im_xml_eutran).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% export the im private API
-export([parse_enb/2, parse_fdd/2, parse_tdd/2, parse_eutran_rel/2]).

-export([fraction1/1]).

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
parse_enb({characters, SideId}, [#state{rule = RuleId,
		stack = [{startElement, {_, "userLabel"}, _} | _]} = State | T])
		when is_list(RuleId) ->
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
parse_enb({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_enb({startElement, _Uri, "EUtranCellFDD", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], location = Location} | _] = State) ->
	DnComponent = ",EUtranCellFDD=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_eutran,
			parse_function = parse_fdd, location = Location,
			parse_state = #eutran_state{fdd = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_enb({startElement, _Uri, "EUtranCellTDD", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], location = Location} | _] = State) ->
	DnComponent = ",EUtranCellTDD=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_eutran,
			parse_function = parse_tdd, location = Location,
			parse_state = #eutran_state{tdd = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_enb({startElement, _Uri, "EP_RP_EPS", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_RP_EPS=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_epc, parse_function = parse_eprpeps,
			parse_state = #epc_state{ep_rp_eps = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_enb({startElement, _Uri, "EP_X2C", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_X2C=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_x2c,
			parse_state = #nr_state{ep_x2c = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_enb({startElement, _Uri, "EP_X2U", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_X2U=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_x2u,
			parse_state = #nr_state{ep_x2u = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_enb({startElement, _Uri, "EP_NgC", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_NgC=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_ngc,
			parse_state = #nr_state{ep_ngc = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_enb({startElement, _Uri, "EP_NgU", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_NgU=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_ngu,
			parse_state = #nr_state{ep_ngu = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_enb({startElement, _Uri, "EP_XnC", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_XnC=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_xnc,
			parse_state = #nr_state{ep_xnc = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_enb({startElement, _Uri, "EP_XnU", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_XnU=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_nr, parse_function = parse_ep_xnu,
			parse_state = #nr_state{ep_xnu = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_enb({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_enb({endElement, _Uri, "ENBFunction", QName},
		[#state{parse_state =  #eutran_state{fdds = FddRels, tdds = TddRels,
		ep_rp_epss = EpRpEpsRels, ep_x2cs = EpX2cRels, ep_x2us = EpX2uRels,
		ep_ngcs = EpNgcRels, ep_ngus = EpNguRels, ep_xncs = EpXncRels,
		ep_xnus = EpXnuRels}, location = Location,
		dn_prefix = [EnbDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "ENBFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EnbAttr = parse_enb_attr(T2, undefined, []),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = "/resourceCatalogManagement/v3/schema/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = EnbDn,
			description = "LTE Evolved Node B (ENB)",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/ENBFunction",
			specification = Spec,
			characteristic = lists:reverse([PeeParam | EnbAttr]),
			related = FddRels ++ TddRels,
			connection_point = EpRpEpsRels ++ EpX2cRels ++ EpX2uRels ++ EpNgcRels
					++ EpNguRels ++ EpXncRels ++ EpXnuRels},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_enb({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_enb_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_enb_attr1(Attributes, undefined, Acc).
% @hidden
parse_enb_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_enb_attr1(T2, undefined, Acc);
parse_enb_attr1([{endElement, {_, "peeParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo peeParametersListType
	{[_ | _PeeplType], T2} = pop(startElement, QName, T1),
	parse_enb_attr1(T2, undefined, Acc);
parse_enb_attr1([{endElement, {_, "x2BlackList"} = QName} | T1],
		undefined, Acc) ->
	% @todo dnList
	{[_ | _X2BlackList], T2} = pop(startElement, QName, T1),
	parse_enb_attr1(T2, undefined, Acc);
parse_enb_attr1([{endElement, {_, "x2WhiteList"} = QName} | T1],
		undefined, Acc) ->
	% @todo dnList
	{[_ | _X2WhiteList], T2} = pop(startElement, QName, T1),
	parse_enb_attr1(T2, undefined, Acc);
parse_enb_attr1([{endElement, {_, "x2HOBlackList"} = QName} | T1],
		undefined, Acc) ->
	% @todo dnList
	{[_ | _X2HOBlackList], T2} = pop(startElement, QName, T1),
	parse_enb_attr1(T2, undefined, Acc);
parse_enb_attr1([{endElement, {_, "tceIDMappingInfoList"} = QName} | T1],
		undefined, Acc) ->
	% @todo TceIDMappingInfoList
	{[_ | _TceIDMappingInfoList], T2} = pop(startElement, QName, T1),
	parse_enb_attr1(T2, undefined, Acc);
parse_enb_attr1([{endElement, {_, "sharNetTceMappingInfoList"} = QName} | T1],
		undefined, Acc) ->
	% @todo SharNetTceMappingInfoList
	{[_ | _SharNetTceMappingInfoList], T2} = pop(startElement, QName, T1),
	parse_enb_attr1(T2, undefined, Acc);
parse_enb_attr1([{endElement, {_, "netListeningRSForRIBS"} = QName} | T1],
		undefined, Acc) ->
	% @todo NetListeningRSForRIBS
	{[_ | _NetListeningRSForRIBS], T2} = pop(startElement, QName, T1),
	parse_enb_attr1(T2, undefined, Acc);
parse_enb_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_enb_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_enb_attr1([{characters, "true"} | T], "intraANRSwitch" = Attr, Acc) ->
	parse_enb_attr1(T, Attr,
			[#resource_char{name = Attr, value = true} | Acc]);
parse_enb_attr1([{characters, "false"} | T], "intraANRSwitch" = Attr, Acc) ->
	parse_enb_attr1(T, Attr,
			[#resource_char{name = Attr, value = false} | Acc]);
parse_enb_attr1([{characters, "true"} | T], "iRATANRSwitch" = Attr, Acc) ->
	parse_enb_attr1(T, Attr,
			[#resource_char{name = Attr, value = true} | Acc]);
parse_enb_attr1([{characters, "false"} | T], "iRATANRSwitch" = Attr, Acc) ->
	parse_enb_attr1(T, Attr,
			[#resource_char{name = Attr, value = false} | Acc]);
parse_enb_attr1([{characters, Chars} | T], "enbId" = Attr, Acc) ->
	parse_enb_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_enb_attr1([{characters, Chars} | T], "x2IpAddressList" = Attr, Acc) ->
	parse_enb_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_enb_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_enb_attr1(T, undefined, Acc);
parse_enb_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_enb_attr1(T, Attr, Acc);
parse_enb_attr1([], undefined, Acc) ->
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
parse_fdd({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_fdd({startElement, _Uri, "EUtranRelation", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",EUtranRelation=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_eutran, parse_function = parse_eutran_rel,
			parse_state = #eutran_state{eutran_rel = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_fdd({startElement, _Uri, "UtranRelation", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",UtranRelation=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_utran, parse_function = parse_utran_rel,
			parse_state = #utran_state{utran_rel = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_fdd({startElement, _Uri, "GsmRelation", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",GsmRelation=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_geran, parse_function = parse_gsm_rel,
			parse_state = #geran_state{gsm_rel = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_fdd({startElement, _Uri, _LocalName, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_fdd({endElement, _Uri, "EUtranCellFDD", QName},
		[#state{dn_prefix = [FddDn | _], stack = Stack,
		spec_cache = Cache, location = Location},
		#state{parse_state = EUtranState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#eutran_state{fdds = FddRels} = EUtranState,
	ClassType = "EUtranCellFDD",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	FddAttr = parse_fdd_attr(T2, []),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = "/resourceCatalogManagement/v3/schema/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = FddDn,
			description = "LTE",
			category = "RAN",
			class_type = ClassType,
			base_type = "EUtranGenericCell",
			schema = "/resourceInventoryManagement/v3/schema/EUtranCellFDD",
			specification = Spec,
			characteristic = [PeeParam | FddAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			FddRel = #resource_rel{id = Id, name = FddDn, rel_type = "contains",
					ref_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{
					parse_state = EUtranState#eutran_state{fdds = [FddRel | FddRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_fdd({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_fdd_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		Acc) ->
	{[_ | Attributes], _} = pop(endElement, QName, T1),
	parse_fdd_attr1(Attributes, undefined, Acc).
% @hidden
parse_fdd_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "cellLocalId" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "localCellId" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, "verysmall"} | T], "cellSize" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "verysmall"} | Acc]);
parse_fdd_attr1([{characters, "small"} | T], "cellSize" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "small"} | Acc]);
parse_fdd_attr1([{characters, "medium"} | T], "cellSize" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "medium"} | Acc]);
parse_fdd_attr1([{characters, "large"} | T], "cellSize" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "large"} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "tac" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "pci" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"maximumTransmissionPower" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "partOfSectorPower" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "referenceSignalPower" = Attr,
		Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "pb" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "relatedSector" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_fdd_attr1([{characters, "enabled"} | T],
		"operationalState" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr,
			[#resource_char{name = Attr, value = "enabled"} | Acc]);
parse_fdd_attr1([{characters, "disabled"} | T],
		"operationalState" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr,
			[#resource_char{name = Attr, value = "disabled"} | Acc]);
parse_fdd_attr1([{characters, "locked"} | T],
		"administrativeState" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr,
			[#resource_char{name = Attr, value = "locked"} | Acc]);
parse_fdd_attr1([{characters, "unlocked"} | T],
		"administrativeState" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr,
			[#resource_char{name = Attr, value = "unlocked"} | Acc]);
parse_fdd_attr1([{characters, "shuttingDown"} | T],
		"administrativeState" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr,
			[#resource_char{name = Attr, value = "shuttingDown"} | Acc]);
parse_fdd_attr1([{characters, "reservedCell"} | T],
		"cellResvInfo" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "reservedCell"} | Acc]);
parse_fdd_attr1([{characters, "nonReservedCell"} | T],
		"cellResvInfo" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "nonReservedCell"} | Acc]);
parse_fdd_attr1([{characters, "yes"} | T], "nbIoTcellFlag" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "yes"} | Acc]);
parse_fdd_attr1([{characters, "no"} | T], "nbIoTcellFlag" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "no"} | Acc]);
parse_fdd_attr1([{characters, "yes"} | T],
		"isChangeForEnergySavingAllowed" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "yes"} | Acc]);
parse_fdd_attr1([{characters, "no"} | T],
		"isChangeForEnergySavingAllowed" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "no"} | Acc]);
parse_fdd_attr1([{characters, "yes"} | T], "ngranCellFlag" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "yes"} | Acc]);
parse_fdd_attr1([{characters, "no"} | T], "ngranCellFlag" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "no"} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "earfcnDl" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "earfcnUl" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_fdd_attr1(T, undefined, Acc);
parse_fdd_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_fdd_attr1(T2, undefined, Acc);
parse_fdd_attr1([{endElement, {_, "cellLocalIdList"} = QName} | T1],
		undefined, Acc) ->
	% @todo CellLocalIdList
	{[_ | _CellLocalIdList], T2} = pop(startElement, QName, T1),
	parse_fdd_attr1(T2, undefined, Acc);
parse_fdd_attr1([{endElement, {_, "pLMNIdList"} = QName} | T1],
		undefined, Acc) ->
	% @todo PLMNIdList
	{[_ | _PLMNIdList], T2} = pop(startElement, QName, T1),
	parse_fdd_attr1(T2, undefined, Acc);
parse_fdd_attr1([{endElement, {_, "cellAccessInfoList"} = QName} | T1],
		undefined, Acc) ->
	% @todo CellAccessInfoList
	{[_ | _CellAccessInfoList], T2} = pop(startElement, QName, T1),
	parse_fdd_attr1(T2, undefined, Acc);
parse_fdd_attr1([{endElement, {_, "pciList"} = QName} | T1],
		undefined, Acc) ->
	% @todo PciList
	{[_ | _PciList], T2} = pop(startElement, QName, T1),
	parse_fdd_attr1(T2, undefined, Acc);
parse_fdd_attr1([{endElement, {_, "relatedTmaList"} = QName} | T1],
		undefined, Acc) ->
	% @todo relatedTmaList
	{[_ | _RelatedTmaList], T2} = pop(startElement, QName, T1),
	parse_fdd_attr1(T2, undefined, Acc);
parse_fdd_attr1([{endElement, {_, "relatedAntennaList"} = QName} | T1],
		undefined, Acc) ->
	% @todo relatedAntennaList
	{[_ | _RelatedAntennaList], T2} = pop(startElement, QName, T1),
	parse_fdd_attr1(T2, undefined, Acc);
parse_fdd_attr1([{endElement, {_, "availabilityStatus"} = QName} | T1],
		undefined, Acc) ->
	% @todo AvailabilityStatusType
	{[_ | _AvailabilityStatus], T2} = pop(startElement, QName, T1),
	parse_fdd_attr1(T2, undefined, Acc);

parse_fdd_attr1([{endElement, {_, "allowedAccessClasses"} = QName} | T1],
		undefined, Acc) ->
	% @todo AllowedAccessClassesType
	{[_ | _AllowedAccessClasses], T2} = pop(startElement, QName, T1),
	parse_fdd_attr1(T2, undefined, Acc);
parse_fdd_attr1([{endElement, {_, "nSSAI"} = QName} | T1],
		undefined, Acc) ->
	% @todo SnssaiList
	{[_ | _NSSAI], T2} = pop(startElement, QName, T1),
	parse_fdd_attr1(T2, undefined, Acc);
parse_fdd_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_fdd_attr1(T, Attr, Acc);
parse_fdd_attr1([], _Attr, Acc) ->
	Acc.

%% @hidden
parse_tdd({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_tdd({startElement, _Uri, "EUtranRelation", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",EUtranRelation=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_eutran, parse_function = parse_eutran_rel,
			parse_state = #eutran_state{eutran_rel = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_tdd({startElement, _Uri, "UtranRelation", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",UtranRelation=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_utran, parse_function = parse_utran_rel,
			parse_state = #utran_state{utran_rel = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_tdd({startElement, _Uri, "GsmRelation", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",GsmRelation=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_geran, parse_function = parse_gsm_rel,
			parse_state = #geran_state{gsm_rel = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_tdd({startElement, _Uri, _LocalName, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_tdd({endElement, _Uri, "EUtranCellTDD", QName},
		[#state{dn_prefix = [TddDn | _], stack = Stack,
		spec_cache = Cache, location = Location},
		#state{parse_state = EUtranState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#eutran_state{tdds = TddRels} = EUtranState,
	ClassType = "EUtranCellTDD",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	TddAttr = parse_tdd_attr(T2, []),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = "/resourceCatalogManagement/v3/schema/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = TddDn,
			description = "LTE",
			category = "RAN",
			class_type = ClassType,
			base_type = "EUtranGenericCell",
			schema = "/resourceInventoryManagement/v3/schema/EUtranCellTDD",
			specification = Spec,
			characteristic = [PeeParam | TddAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			TddRel = #resource_rel{id = Id, name = TddDn, rel_type = "contains",
					ref_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{
					parse_state = EUtranState#eutran_state{tdds = [TddRel | TddRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_tdd({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_tdd_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		Acc) ->
	{[_ | Attributes], _} = pop(endElement, QName, T1),
	parse_tdd_attr1(Attributes, undefined, Acc).
% @hidden
parse_tdd_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_tdd_attr1([{characters, Chars} | T], "cellLocalId" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_attr1([{characters, "verysmall"} | T], "cellSize" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "verysmall"} | Acc]);
parse_tdd_attr1([{characters, "small"} | T], "cellSize" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "small"} | Acc]);
parse_tdd_attr1([{characters, "medium"} | T], "cellSize" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "medium"} | Acc]);
parse_tdd_attr1([{characters, "large"} | T], "cellSize" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "large"} | Acc]);
parse_tdd_attr1([{characters, Chars} | T], "tac" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_attr1([{characters, Chars} | T], "pci" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_attr1([{characters, Chars} | T],
		"maximumTransmissionPower" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_attr1([{characters, Chars} | T], "partOfSectorPower" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_attr1([{characters, Chars} | T], "referenceSignalPower" = Attr,
		Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_attr1([{characters, Chars} | T], "pb" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_attr1([{characters, Chars} | T], "relatedSector" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_tdd_attr1([{characters, "enabled"} | T],
		"operationalState" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr,
			[#resource_char{name = Attr, value = "enabled"} | Acc]);
parse_tdd_attr1([{characters, "disabled"} | T],
		"operationalState" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr,
			[#resource_char{name = Attr, value = "disabled"} | Acc]);
parse_tdd_attr1([{characters, "locked"} | T],
		"administrativeState" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr,
			[#resource_char{name = Attr, value = "locked"} | Acc]);
parse_tdd_attr1([{characters, "unlocked"} | T],
		"administrativeState" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr,
			[#resource_char{name = Attr, value = "unlocked"} | Acc]);
parse_tdd_attr1([{characters, "shuttingDown"} | T],
		"administrativeState" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr,
			[#resource_char{name = Attr, value = "shuttingDown"} | Acc]);
parse_tdd_attr1([{characters, "reservedCell"} | T],
		"cellResvInfo" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "reservedCell"} | Acc]);
parse_tdd_attr1([{characters, "nonReservedCell"} | T],
		"cellResvInfo" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "nonReservedCell"} | Acc]);
parse_tdd_attr1([{characters, "yes"} | T], "nbIoTcellFlag" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "yes"} | Acc]);
parse_tdd_attr1([{characters, "no"} | T], "nbIoTcellFlag" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "no"} | Acc]);
parse_tdd_attr1([{characters, "yes"} | T],
		"isChangeForEnergySavingAllowed" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "yes"} | Acc]);
parse_tdd_attr1([{characters, "no"} | T],
		"isChangeForEnergySavingAllowed" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "no"} | Acc]);
parse_tdd_attr1([{characters, "yes"} | T], "ngranCellFlag" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "yes"} | Acc]);
parse_tdd_attr1([{characters, "no"} | T], "ngranCellFlag" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "no"} | Acc]);
parse_tdd_attr1([{characters, Chars} | T], "earfcn" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_attr1([{characters, Chars} | T], "sfAssignment" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_attr1([{characters, Chars} | T], "specialSfPatterns" = Attr, Acc) ->
	parse_tdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_tdd_attr1(T, undefined, Acc);
parse_tdd_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_tdd_attr1(T2, undefined, Acc);
parse_tdd_attr1([{endElement, {_, "cellLocalIdList"} = QName} | T1],
		undefined, Acc) ->
	% @todo CellLocalIdList
	{[_ | _CellLocalIdList], T2} = pop(startElement, QName, T1),
	parse_tdd_attr1(T2, undefined, Acc);
parse_tdd_attr1([{endElement, {_, "pLMNIdList"} = QName} | T1],
		undefined, Acc) ->
	% @todo PLMNIdList
	{[_ | _PLMNIdList], T2} = pop(startElement, QName, T1),
	parse_tdd_attr1(T2, undefined, Acc);
parse_tdd_attr1([{endElement, {_, "cellAccessInfoList"} = QName} | T1],
		undefined, Acc) ->
	% @todo CellAccessInfoList
	{[_ | _CellAccessInfoList], T2} = pop(startElement, QName, T1),
	parse_tdd_attr1(T2, undefined, Acc);
parse_tdd_attr1([{endElement, {_, "pciList"} = QName} | T1],
		undefined, Acc) ->
	% @todo PciList
	{[_ | _PciList], T2} = pop(startElement, QName, T1),
	parse_tdd_attr1(T2, undefined, Acc);
parse_tdd_attr1([{endElement, {_, "relatedTmaList"} = QName} | T1],
		undefined, Acc) ->
	% @todo relatedTmaList
	{[_ | _RelatedTmaList], T2} = pop(startElement, QName, T1),
	parse_tdd_attr1(T2, undefined, Acc);
parse_tdd_attr1([{endElement, {_, "relatedAntennaList"} = QName} | T1],
		undefined, Acc) ->
	% @todo relatedAntennaList
	{[_ | _RelatedAntennaList], T2} = pop(startElement, QName, T1),
	parse_tdd_attr1(T2, undefined, Acc);
parse_tdd_attr1([{endElement, {_, "availabilityStatus"} = QName} | T1],
		undefined, Acc) ->
	% @todo AvailabilityStatusType
	{[_ | _AvailabilityStatus], T2} = pop(startElement, QName, T1),
	parse_tdd_attr1(T2, undefined, Acc);
parse_tdd_attr1([{endElement, {_, "allowedAccessClasses"} = QName} | T1],
		undefined, Acc) ->
	% @todo AllowedAccessClassesType
	{[_ | _AllowedAccessClasses], T2} = pop(startElement, QName, T1),
	parse_tdd_attr1(T2, undefined, Acc);
parse_tdd_attr1([{endElement, {_, "nSSAI"} = QName} | T1],
		undefined, Acc) ->
	% @todo SnssaiList
	{[_ | _NSSAI], T2} = pop(startElement, QName, T1),
	parse_tdd_attr1(T2, undefined, Acc);
parse_tdd_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_tdd_attr1(T, Attr, Acc);
parse_tdd_attr1([], _Attr, Acc) ->
	Acc.

%% @hidden
parse_eutran_rel({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_eutran_rel({startElement, _Uri, _LocalName, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_eutran_rel({endElement, _Uri, "EUtranRelation", QName},
		[#state{stack = Stack, parse_state = #eutran_state{eutran_rel = EUtranRel}},
		#state{parse_state = #eutran_state{fdd = Fdd, tdd = undefined}}
		= PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NewEUtranRel = parse_eutran_rel_attr(T2, undefined, EUtranRel),
	EUtranState = PrevState#state.parse_state,
	NewFdd = choice_add(NewEUtranRel, Fdd),
	[PrevState#state{parse_state = EUtranState#eutran_state{
			fdd = NewFdd}} | T1];
parse_eutran_rel({endElement, _Uri, "EUtranRelation", QName},
		[#state{stack = Stack, parse_state = #eutran_state{eutran_rel = EUtranRel}},
		#state{parse_state = #eutran_state{tdd = Tdd, fdd = undefined}}
		= PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NewEUtranRel = parse_eutran_rel_attr(T2, undefined, EUtranRel),
	EUtranState = PrevState#state.parse_state,
	NewTdd = choice_add(NewEUtranRel, Tdd),
	[PrevState#state{parse_state = EUtranState#eutran_state{
			tdd = NewTdd}} | T1];
parse_eutran_rel({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_eutran_rel_attr([{startElement, {_, "attributes"} = QName, []} | T],
		undefined, Acc) ->
	{[_ | Attributes], _Rest} = pop(endElement, QName, T),
	parse_eutran_rel_attr1(Attributes, undefined, Acc).
% @hidden
parse_eutran_rel_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_eutran_rel_attr1(T, Attr, Acc);
parse_eutran_rel_attr1([{characters, Chars} | T], "tCI" = Attr, Acc) ->
	NewAcc = attribute_add(Attr, list_to_integer(Chars), Acc),
	parse_eutran_rel_attr1(T, Attr, NewAcc);
parse_eutran_rel_attr1([{characters, "yes"} | T],
		"isRemoveAllowed" = Attr, Acc) ->
	NewAcc = attribute_add(Attr, yes, Acc),
	parse_eutran_rel_attr1(T, Attr, NewAcc);
parse_eutran_rel_attr1([{characters, "no"} | T],
		"isRemoveAllowed" = Attr, Acc) ->
	NewAcc = attribute_add(Attr, no, Acc),
	parse_eutran_rel_attr1(T, Attr, NewAcc);
parse_eutran_rel_attr1([{characters, "yes"} | T], "isHOAllowed" = Attr, Acc) ->
	NewAcc = attribute_add(Attr, yes, Acc),
	parse_eutran_rel_attr1(T, Attr, NewAcc);
parse_eutran_rel_attr1([{characters, "no"} | T], "isHOAllowed" = Attr, Acc) ->
	NewAcc = attribute_add(Attr, no, Acc),
	parse_eutran_rel_attr1(T, Attr, NewAcc);
parse_eutran_rel_attr1([{characters, "yes"} | T],
		"isICICInformationSendAllowed" = Attr, Acc) ->
	NewAcc = attribute_add(Attr, yes, Acc),
	parse_eutran_rel_attr1(T, Attr, NewAcc);
parse_eutran_rel_attr1([{characters, "no"} | T],
		"isICICInformationSendAllowed" = Attr, Acc) ->
	NewAcc = attribute_add(Attr, no, Acc),
	parse_eutran_rel_attr1(T, Attr, NewAcc);
parse_eutran_rel_attr1([{characters, "yes"} | T], "isLBAllowed" = Attr, Acc) ->
	NewAcc = attribute_add(Attr, yes, Acc),
	parse_eutran_rel_attr1(T, Attr, NewAcc);
parse_eutran_rel_attr1([{characters, "no"} | T], "isLBAllowed" = Attr, Acc) ->
	NewAcc = attribute_add(Attr, no, Acc),
	parse_eutran_rel_attr1(T, Attr, NewAcc);
parse_eutran_rel_attr1([{characters, Chars} | T], "adjacentCell" = Attr, Acc) ->
	NewAcc = attribute_add("adjacentCell", Chars, Acc),
	parse_eutran_rel_attr1(T, Attr, NewAcc);
parse_eutran_rel_attr1([{characters, "no"} | T], "isEsCoveredBy" = Attr, Acc) ->
	NewAcc = attribute_add("userLabel", "no", Acc),
	parse_eutran_rel_attr1(T, Attr, NewAcc);
parse_eutran_rel_attr1([{characters, "partial"} | T],
		"isEsCoveredBy" = Attr, Acc) ->
	NewAcc = attribute_add("userLabel", "partial", Acc),
	parse_eutran_rel_attr1(T, Attr, NewAcc);
parse_eutran_rel_attr1([{characters, "yes"} | T],
		"isEsCoveredBy" = Attr, Acc) ->
	NewAcc = attribute_add("userLabel", "yes", Acc),
	parse_eutran_rel_attr1(T, Attr, NewAcc);
parse_eutran_rel_attr1([{characters, Chars} | T],
		"cellIndividualOffset" = Attr, Acc) ->
	NewAcc = attribute_add(Attr, Chars, Acc),
	parse_eutran_rel_attr1(T, Attr, NewAcc);
parse_eutran_rel_attr1([{characters, Chars} | T], "qOffset" = Attr, Acc) ->
	NewAcc = attribute_add(Attr, Chars, Acc),
	parse_eutran_rel_attr1(T, Attr, NewAcc);
parse_eutran_rel_attr1([{startElement, {_, Attr}, []} | T], Attr, Acc) ->
	parse_eutran_rel_attr1(T, undefined, Acc);
parse_eutran_rel_attr1([],  _Attr, Acc) ->
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

-spec fraction1(Fraction) -> Fraction
	when
		Fraction :: string() | non_neg_integer().
%% @doc CODEC for fraction with one decimal place.
%%
%% Internally an integer value is used to represent a fraction.
%% Externally a string representation of a decimal number, with
%% one decimal place.
%%
fraction1(Fraction) when Fraction rem 10 =:= 0 ->
	integer_to_list(Fraction div 10);
fraction1(Fraction) when is_integer(Fraction) ->
	lists:flatten(io_lib:fwrite("~b.~1.10.0b", [Fraction div 10, abs(Fraction) rem 10]));
fraction1(Fraction) when is_list(Fraction) ->
	case string:tokens(Fraction, ".") of
		[[$- | Int], Dec] when length(Dec) =:= 1 ->
			-((list_to_integer(Int) * 10) + (list_to_integer(Dec)));
		[Int, Dec] when length(Dec) =:= 1 ->
			(list_to_integer(Int) * 10) + (list_to_integer(Dec));
		[Int] ->
			list_to_integer(Int) * 10
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
