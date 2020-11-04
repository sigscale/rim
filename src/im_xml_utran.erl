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
-module(im_xml_utran).
-copyright('Copyright (c) 2018-2020 SigScale Global Inc.').

%% export the im private API
-export([parse_rnc/2, parse_fdd/2, parse_nodeb/2, parse_iub/2,
			parse_iucs/2, parse_iups/2, parse_iur/2, parse_tdd_hcr/2,
			parse_tdd_lcr/2, parse_utran_rel/2]).

-export([fraction1/1]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

-define(PathCatalogSchema, "/resourceCatalogManagement/v4/schema").
-define(PathInventorySchema, "/resourceInventoryManagement/v4/schema").
-define(ResourcePath, "/resourceInventoryManagement/v4/resource/").


%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

%% @hidden
parse_nodeb({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_nodeb({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_nodeb({endElement, _Uri, "NodeBFunction", QName},
		[#state{dn_prefix = [NodebDn | _], stack = Stack,
		spec_cache = Cache, location = Location},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	NodeBAttr = parse_nodeb_attr(T2, undefined, []),
	ClassType = "NodeBFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = NodebDn,
			description = "UMTS NodeB",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/NodeBFunction",
			specification = Spec,
			characteristic = [PeeParam | NodeBAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_nodeb({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_nodeb_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_nodeb_attr1(Attributes, undefined, Acc).
% @hidden
parse_nodeb_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	% @todo vnfParametersListType
	parse_nodeb_attr1(T2, undefined, Acc);
parse_nodeb_attr1([{endElement, {_, "peeParametersList"} = QName} | T1],
		undefined, Acc) ->
	{[_ | _PeeplType], T2} = pop(startElement, QName, T1),
	% @todo peeParametersListType
	parse_nodeb_attr1(T2, undefined, Acc);
parse_nodeb_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_nodeb_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_nodeb_attr1([{characters, Chars} | T], "mcc" = Attr, Acc) ->
	parse_nodeb_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_nodeb_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_nodeb_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_nodeb_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_nodeb_attr1(T, undefined, Acc);
parse_nodeb_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_nodeb_attr1(T, Attr, Acc);
parse_nodeb_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_rnc({characters, SideId}, [#state{rule = RuleId,
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
parse_rnc({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_rnc({startElement, _Uri, "UtranCellFDD", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], location = Location} | _] = State) ->
	DnComponent = ",UtranCellFDD=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_utran,
			parse_function = parse_fdd, location = Location,
			parse_state = #utran_state{fdd = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_rnc({startElement, _Uri, "EP_IuCS", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_IuCS=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_utran,
			parse_function = parse_iucs,
			parse_state = #utran_state{iucs = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_rnc({startElement, _Uri, "EP_IuPS", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_IuPS=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_utran,
			parse_function = parse_iups,
			parse_state = #utran_state{iups = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_rnc({startElement, _Uri, "EP_Iur", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",EP_Iur=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_utran,
			parse_function = parse_iur,
			parse_state = #utran_state{iur = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_rnc({startElement, _Uri, "IubLink", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], location = Location} | _] = State) ->
	DnComponent = ",IubLink=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_utran,
			parse_function = parse_iub, location = Location,
			parse_state = #utran_state{iub = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_rnc({startElement, _Uri, "UtranCellTDDLcr", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",UtranCellTDDLcr=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_utran,
			parse_function = parse_tdd_lcr,
			parse_state = #utran_state{tdd_lcr = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_rnc({startElement, _Uri, "UtranCellTDDHcr", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",UtranCellTDDHcr=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_utran,
			parse_function = parse_tdd_hcr,
			parse_state = #utran_state{tdd_hcr = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_rnc({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_rnc({endElement, _Uri, "RncFunction", QName},
		[#state{parse_state =  #utran_state{fdds = Fdds, tdd_lcrs = Lcrs,
		tdd_hcrs = Hcrs, iubs = IubLinks}, location = Location,
		dn_prefix = [RncDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "RncFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	RncAttr = parse_rnc_attr(T2, undefined, []),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = RncDn,
			description = "UMTS Radio Network Controller (RNC)",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/RncFunction",
			specification = Spec,
			characteristic = lists:reverse([PeeParam | RncAttr]),
			related = Fdds ++ Lcrs ++ Hcrs ++ IubLinks},
	case im:add_resource(Resource) of
		{ok, #resource{id = RncId} = R} ->
			F = fun F([#resource_rel{id = IubId, name = IubDn,
					href = Href, ref_type = IubRefType} = ResourceRel | T], Acc) ->
						RncEndpoint = #endpoint_ref{name = RncDn, ref_type = ClassType,
								id = RncId, href = ?ResourcePath ++ RncId},
						IubEndpoint = #endpoint_ref{name = IubDn, id = IubId,
								href = Href, ref_type = IubRefType},
						RncIubConnectivity = #connection{ass_type = "pointtoPoint",
								endpoint = [RncEndpoint, IubEndpoint]},
						CellIubConnectivity = case im:get_resource(IubId) of
							{ok, #resource{characteristic = Chars}} ->
								build_iub_cell_connectivity(Chars, ResourceRel, []);
							{error, Reason} ->
								{error, Reason}
						end,
						F(T, [RncIubConnectivity] ++ CellIubConnectivity ++ Acc);
					F([], Acc) ->
						Acc
			end,
			Connectivity = [#resource_graph{connection = F(IubLinks, [])}],
			Ftrans = fun() ->
					[R] = mnesia:read(resource, RncId, write),
					mnesia:write(resource,
						R#resource{connectivity = Connectivity}, write)
			end,
			case mnesia:transaction(Ftrans) of
				{aborted, Reason} ->
					{error, Reason};
				{atomic, ok} ->
					[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1]
			end;
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_rnc({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
build_iub_cell_connectivity([#resource_char{name = "iubLinkUtranCell",
		value = IubUtranCellDnList} | T1], #resource_rel{id = IubId, name = IubDn,
		href = IubHref, ref_type = IubRefType} = IubRel, Acc) ->
	F = fun F([CellDn | T2], ConnectivityList) when is_list(CellDn) ->
				CellResource = case im:get_resource_name(CellDn) of
					{ok, Resource} ->
						Resource;
					{error, Reason} ->
						{error, Reason}
				end,
				#resource{id = CellId, href = CellHref,
					class_type = CellType} = CellResource,
				IubEndpoint = #endpoint_ref{name = IubDn, id = IubId,
						href = IubHref, ref_type = IubRefType},
				CellEndpoint = #endpoint_ref{name = CellDn, id = CellId,
						href = CellHref, ref_type = CellType},
				Connectivity = #connection{ass_type = "pointtoPoint",
						endpoint = [IubEndpoint, CellEndpoint]},
				F(T2, [Connectivity | ConnectivityList]);
			F([], ConnectivityList) ->
				ConnectivityList
	end,
	build_iub_cell_connectivity(T1, IubRel, F(IubUtranCellDnList, Acc));
build_iub_cell_connectivity([_ | T], IubRel, Acc) ->
	build_iub_cell_connectivity(T, IubRel, Acc);
build_iub_cell_connectivity([], _IubRel, Acc) ->
	Acc.

% @hidden
parse_rnc_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_rnc_attr1(Attributes, undefined, Acc).
% @hidden
parse_rnc_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_rnc_attr1(T2, undefined, Acc);
parse_rnc_attr1([{endElement, {_, "peeParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo peeParametersListType
	{[_ | _PeeplType], T2} = pop(startElement, QName, T1),
	parse_rnc_attr1(T2, undefined, Acc);
parse_rnc_attr1([{endElement, {_, "tceIDMappingInfoList"} = QName} | T1],
		undefined, Acc) ->
	% @todo TceIDMappingInfoList
	{[_ | _TceimiList], T2} = pop(startElement, QName, T1),
	parse_rnc_attr1(T2, undefined, Acc);
parse_rnc_attr1([{endElement, {_, "sharNetTceMappingInfoList"} = QName} | T1],
		undefined, Acc) ->
	% @todo SharNetTceMappingInfoList
	{[_ | _SntcemiList], T2} = pop(startElement, QName, T1),
	parse_rnc_attr1(T2, undefined, Acc);
parse_rnc_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_rnc_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_rnc_attr1([{characters, Chars} | T], "mcc" = Attr, Acc) ->
	parse_rnc_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_rnc_attr1([{characters, Chars} | T], "mnc" = Attr, Acc) ->
	parse_rnc_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_rnc_attr1([{characters, Chars} | T], "rncId" = Attr, Acc) ->
	parse_rnc_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_rnc_attr1([{characters, "0"} | T], "siptoSupported" = Attr, Acc) ->
	parse_rnc_attr1(T, Attr,
			[#resource_char{name = Attr, value = 0} | Acc]);
parse_rnc_attr1([{characters, "1"} | T], "siptoSupported" = Attr, Acc) ->
	parse_rnc_attr1(T, Attr,
			[#resource_char{name = Attr, value = 1} | Acc]);
parse_rnc_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_rnc_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_rnc_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_rnc_attr1(T, undefined, Acc);
parse_rnc_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_rnc_attr1(T, Attr, Acc);
parse_rnc_attr1([], undefined, Acc) ->
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
parse_fdd({endElement, _Uri, "UtranCellFDD", QName},
		[#state{dn_prefix = [FddDn | _], stack = Stack,
		spec_cache = Cache, location = Location},
		#state{parse_state = UtranState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#utran_state{fdds = FddRels} = UtranState,
	ClassType = "UtranCellFDD",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	FddAttr = parse_fdd_attr(T2, []),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = FddDn,
			description = "UMTS radio",
			category = "RAN",
			class_type = ClassType,
			base_type = "UtranGenericCell",
			schema = ?PathInventorySchema ++ "/UtranCellFDD",
			specification = Spec,
			characteristic = [PeeParam | FddAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			FddRel = #resource_rel{id = Id, name = FddDn,
					rel_type = "contains", ref_type = ClassType,
					href = ?ResourcePath ++ Id},
			[PrevState#state{
					parse_state = UtranState#utran_state{fdds = [FddRel | FddRels]},
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
parse_fdd_attr1([{characters, Chars} | T], "cId" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "localCellId" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"maximumTransmissionPower" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, "FDDMode"} | T], "cellMode" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "FDDMode"} | Acc]);
parse_fdd_attr1([{characters, "3-84McpsTDDMode"} | T],
		"cellMode" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "3-84McpsTDDMode"} | Acc]);
parse_fdd_attr1([{characters, "1-28McpsTDDMode"} | T],
		"cellMode" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "1-28McpsTDDMode"} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "pichPower" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "pchPower" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "fachPower" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "lac" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "rac" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "sac" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "utranCellIubLink" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_fdd_attr1([{characters, "enabled"} | T],
		"operationalState" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "enabled"} | Acc]);
parse_fdd_attr1([{characters, "disabled"} | T],
		"operationalState" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "disabled"} | Acc]);
parse_fdd_attr1([{characters, "0"} | T], "hsFlag" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr, value = 0} | Acc]);
parse_fdd_attr1([{characters, "1"} | T], "hsFlag" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr, value = 1} | Acc]);
parse_fdd_attr1([{characters, "1"} | T], "hsEnable" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr, value = 1} | Acc]);
parse_fdd_attr1([{characters, "0"} | T], "hsEnable" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr, value = 0} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "numOfHspdschs" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "numOfHsscchs" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "frameOffset" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"cellIndividualOffset" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "hcsPrio" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"maximumAllowedUlTxPower" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "qrxlevMin" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "deltaQrxlevmin" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "qhcs" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "penaltyTime" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"referenceTimeDifferenceToCell" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, "TRUE"} | T], "readSFNIndicator" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "TRUE"} | Acc]);
parse_fdd_attr1([{characters, "FALSE"} | T], "readSFNIndicator" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "FALSE"} | Acc]);
parse_fdd_attr1([{characters, "cellReservedForOperatorUse"} | T],
		"restrictionStateIndicator" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "cellReservedForOperatorUse"} | Acc]);
parse_fdd_attr1([{characters, "cellAccessible"} | T],
		"restrictionStateIndicator" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "cellAccessible"} | Acc]);
parse_fdd_attr1([{characters, "dpcModeChangeSupported"} | T],
		"dpcModechangeSupportIndicator" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "dpcModeChangeSupported"} | Acc]);
parse_fdd_attr1([{characters, "dpcModeChangeNotSupported"} | T],
		"dpcModechangeSupportIndicator" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "dpcModeChangeNotSupported"} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"relatedSectorEquipment" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "uarfcnUl" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "uarfcnDl" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"primaryScramblingCode" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "primaryCpichPower" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "primarySchPower" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "secondarySchPower" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "bchPower" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "aichPower" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "qqualMin" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, "none"} | T],
		"txDiversityIndicator" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "none"} | Acc]);
parse_fdd_attr1([{characters, "PrimaryCpichBroadcastFrom2Antennas"} | T],
		"txDiversityIndicator" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "PrimaryCpichBroadcastFrom2Antennas"} | Acc]);
parse_fdd_attr1([{characters, "SttdAppliedToPrimaryCCPCH"} | T],
		"txDiversityIndicator" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "SttdAppliedToPrimaryCCPCH"} | Acc]);
parse_fdd_attr1([{characters, "TstdAppliedToPrimarySchAndSecondarySch"} | T],
		"txDiversityIndicator" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "TstdAppliedToPrimarySchAndSecondarySch"} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "temporaryOffset1" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], "temporaryOffset2" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, "active"} | T],
		"sttdSupportIndicator" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "active"} | Acc]);
parse_fdd_attr1([{characters, "inactive"} | T],
		"sttdSupportIndicator" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "inactive"} | Acc]);
parse_fdd_attr1([{characters, "closedLoopMode1Supported"} | T],
		"closedLoopModelSupportIndicator" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "closedLoopMode1Supported"} | Acc]);
parse_fdd_attr1([{characters, "closedLoopMode1NotSupported"} | T],
		"closedLoopModelSupportIndicator" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "closedLoopMode1NotSupported"} | Acc]);
parse_fdd_attr1([{characters, Chars} | T], Attr, Acc) ->
	% @todo default handler
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_fdd_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	% @todo default handler
	parse_fdd_attr1(T, undefined, Acc);
parse_fdd_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
			undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_fdd_attr1(T2, undefined, Acc);
parse_fdd_attr1([{endElement, {_, "uraList"} = QName} | T1], undefined, Acc) ->
	% @todo uraList
	{[_ | _UraList], T2} = pop(startElement, QName, T1),
	parse_fdd_attr1(T2, undefined, Acc);
parse_fdd_attr1([{endElement, {_, "relatedAntennaList"} = QName} | T1],
			undefined, Acc) ->
	% @todo relatedAntennaList
	{[_ | _RelatedAntennaList], T2} = pop(startElement, QName, T1),
	parse_fdd_attr1(T2, undefined, Acc);
parse_fdd_attr1([{endElement, {_, "relatedTmaList"} = QName} | T1],
		undefined, Acc) ->
	% @todo relatedTmaList
	{[_ | _RelatedTmaList], T2} = pop(startElement, QName, T1),
	parse_fdd_attr1(T2, undefined, Acc);
parse_fdd_attr1([{endElement, {_, "snaInformation"} = QName} | T1],
		undefined, Acc) ->
	% @todo snaInformation
	{[_ | _SnaiInfo], T2} = pop(startElement, QName, T1),
	parse_fdd_attr1(T2, undefined, Acc);
parse_fdd_attr1([{endElement, {_, "nsPlmnIdList"} = QName} | T1],
		undefined, Acc) ->
	% @todo NsPlmnIdListType
	{[_ | _NsPlmnIdList], T2} = pop(startElement, QName, T1),
	parse_fdd_attr1(T2, undefined, Acc);
parse_fdd_attr1([{endElement, {_, "cellCapabilityContainerFDD"} = QName} | T1],
		undefined, Acc) ->
	% @todo cellCapabilityContainerFDD
	{[_ | _CccFdd], T2} = pop(startElement, QName, T1),
	parse_fdd_attr1(T2, undefined, Acc);
parse_fdd_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_fdd_attr1(T, Attr, Acc);
parse_fdd_attr1([], _Attr, Acc) ->
	Acc.

%% @hidden
parse_utran_rel({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_utran_rel({startElement, _Uri, _LocalName, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_utran_rel({endElement, _Uri, "UtranRelation", QName},
		[#state{stack = Stack, parse_state = #utran_state{utran_rel = UtranRel}},
		#state{parse_state = UtranState} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NewUtranRel = parse_utran_rel_attr(T2, undefined, UtranRel),
	#utran_state{fdd = Fdd} = UtranState,
	NewFdd = choice_add(NewUtranRel, Fdd),
	[PrevState#state{parse_state = UtranState#utran_state{
			fdd = NewFdd}} | T1];
parse_utran_rel({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_utran_rel_attr([{startElement, {_, "attributes"} = QName, []} | T],
		undefined, Acc) ->
	{[_ | Attributes], _Rest} = pop(endElement, QName, T),
	parse_utran_rel_attr1(Attributes, undefined, Acc).
% @hidden
parse_utran_rel_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_utran_rel_attr1(T, Attr, Acc);
parse_utran_rel_attr1([{characters, Chars} | T], "adjacentCell" = Attr, Acc) ->
	NewAcc = attribute_add("adjacentCell", Chars, Acc),
	parse_utran_rel_attr1(T, Attr, NewAcc);
parse_utran_rel_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	NewAcc = attribute_add("userLabel", Chars, Acc),
	parse_utran_rel_attr1(T, Attr, NewAcc);
parse_utran_rel_attr1([{startElement, {_, Attr}, []} | T], Attr, Acc) ->
	parse_utran_rel_attr1(T, undefined, Acc);
parse_utran_rel_attr1([],  _Attr, Acc) ->
	Acc.

%% @hidden
parse_tdd_hcr({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_tdd_hcr({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_tdd_hcr({endElement, _Uri, "UtranCellTDDHcr", QName},
		[#state{dn_prefix = [TddHcrDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = UtranState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#utran_state{tdd_hcrs = HcrRels} = UtranState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	TddHcrAttr = parse_tdd_hcr_attr(T2, undefined, []),
	ClassType = "UtranCellTDDHcr",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = TddHcrDn,
			description = "UMTS Time Division Duplex High Chip Rate",
			category = "RAN",
			class_type = ClassType,
			base_type = "UtranCellTDD",
			schema = ?PathInventorySchema ++ "/UtranCellTDDHcr",
			specification = Spec,
			characteristic = TddHcrAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id} = _R} ->
			HcrRel = #resource_rel{id = Id, name = TddHcrDn,
					rel_type = "contains", ref_type = ClassType,
					href = ?ResourcePath ++ Id},
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					parse_state = UtranState#utran_state{tdd_hcrs = [HcrRel | HcrRels]}} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_tdd_hcr({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_tdd_hcr_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_tdd_hcr_attr1(Attributes, undefined, Acc).
% @hidden
parse_tdd_hcr_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_tdd_hcr_attr1(T2, undefined, Acc);
parse_tdd_hcr_attr1([{endElement, {_, "uraList"} = QName} | T1],
		undefined, Acc) ->
	% @todo uraList
	{[_ | _UraList], T2} = pop(startElement, QName, T1),
	parse_tdd_hcr_attr1(T2, undefined, Acc);
parse_tdd_hcr_attr1([{endElement, {_, "relatedAntennaList"} = QName} | T1],
		undefined, Acc) ->
	% @todo relatedAntennaList
	{[_ | _RelatedAntennaList], T2} = pop(startElement, QName, T1),
	parse_tdd_hcr_attr1(T2, undefined, Acc);
parse_tdd_hcr_attr1([{endElement, {_, "relatedTmaList"} = QName} | T1],
		undefined, Acc) ->
	% @todo relatedTmaList
	{[_ | _RelatedTmaList], T2} = pop(startElement, QName, T1),
	parse_tdd_hcr_attr1(T2, undefined, Acc);
parse_tdd_hcr_attr1([{endElement, {_, "snaInformation"} = QName} | T1],
		undefined, Acc) ->
	% @todo snaInformation
	{[_ | _SnaInfo], T2} = pop(startElement, QName, T1),
	parse_tdd_hcr_attr1(T2, undefined, Acc);
parse_tdd_hcr_attr1([{endElement, {_, "nsPlmnIdList"} = QName} | T1],
		undefined, Acc) ->
	% @todo NsPlmnIdListType
	{[_ | _UraList], T2} = pop(startElement, QName, T1),
	parse_tdd_hcr_attr1(T2, undefined, Acc);
parse_tdd_hcr_attr1([{endElement,
		{_, "cellCapabilityContainerTDD"} = QName} | T1], undefined, Acc) ->
	% @todo cellCapabilityContainerTDD
	{[_ | _CellccTdd], T2} = pop(startElement, QName, T1),
	parse_tdd_hcr_attr1(T2, undefined, Acc);
parse_tdd_hcr_attr1([{endElement, {_, "timeSlotHCRList"} = QName} | T1],
		undefined, Acc) ->
	% @todo timeSlotHCRList
	{[_ | _TimeSlotHCRList], T2} = pop(startElement, QName, T1),
	parse_tdd_hcr_attr1(T2, undefined, Acc);
parse_tdd_hcr_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "cId" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "localCellId" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"maximumTransmissionPower" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, "FDDMode"} | T], "cellMode" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "FDDMode"} | Acc]);
parse_tdd_hcr_attr1([{characters, "3-84McpsTDDMode"} | T],
		"cellMode" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "3-84McpsTDDMode"} | Acc]);
parse_tdd_hcr_attr1([{characters, "1-28McpsTDDMode"} | T],
		"cellMode" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "1-28McpsTDDMode"} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "pichPower" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "pchPower" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "fachPower" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "lac" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "rac" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "sac" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"utranCellIubLink" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_hcr_attr1([{characters, "enabled"} | T],
		"operationalState" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "enabled"} | Acc]);
parse_tdd_hcr_attr1([{characters, "disabled"} | T],
		"operationalState" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "disabled"} | Acc]);
parse_tdd_hcr_attr1([{characters, "0"} | T], "hsFlag" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr, value = 0} | Acc]);
parse_tdd_hcr_attr1([{characters, "1"} | T], "hsFlag" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr, value = 1} | Acc]);
parse_tdd_hcr_attr1([{characters, "1"} | T], "hsEnable" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr, value = 1} | Acc]);
parse_tdd_hcr_attr1([{characters, "0"} | T], "hsEnable" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr, value = 0} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "numOfHspdschs" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "numOfHsscchs" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "frameOffset" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"cellIndividualOffset" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "hcsPrio" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"maximumAllowedUlTxPower" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "qrxlevMin" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "deltaQrxlevmin" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "qhcs" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "penaltyTime" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"referenceTimeDifferenceToCell" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, "TRUE"} | T],
		"readSFNIndicator" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "TRUE"} | Acc]);
parse_tdd_hcr_attr1([{characters, "FALSE"} | T],
		"readSFNIndicator" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "FALSE"} | Acc]);
parse_tdd_hcr_attr1([{characters, "cellReservedForOperatorUse"} | T],
		"restrictionStateIndicator" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "cellReservedForOperatorUse"} | Acc]);
parse_tdd_hcr_attr1([{characters, "cellAccessible"} | T],
		"restrictionStateIndicator" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "cellAccessible"} | Acc]);
parse_tdd_hcr_attr1([{characters, "dpcModeChangeSupported"} | T],
		"dpcModechangeSupportIndicator" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "dpcModeChangeSupported"} | Acc]);
parse_tdd_hcr_attr1([{characters, "dpcModeChangeNotSupported"} | T],
		"dpcModechangeSupportIndicator" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "dpcModeChangeNotSupported"} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"relatedSectorEquipment" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "uarfcn" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "cellParameterId" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"primaryCcpchPower" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, "active"} | T],
		"sctdIndicator" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "active"} | Acc]);
parse_tdd_hcr_attr1([{characters, "inactive"} | T],
		"sctdIndicator" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "inactive"} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"dpchConstantValue" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "schPower" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"temporaryOffset1" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, "SCH and PCCPCH allocated in a single TS"} | T],
		"syncCase" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "SCH and PCCPCH allocated in a single TS"} | Acc]);
parse_tdd_hcr_attr1([{characters,
		"SCH and PCCPCH allocated in two TS, TS#k and TS#k+8"} | T],
		"syncCase" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "SCH and PCCPCH allocated in two TS, TS#k and TS#k+8"} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "timeSlotForSch" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], "schTimeSlot" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_hcr_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_tdd_hcr_attr1(T, undefined, Acc);
parse_tdd_hcr_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, Acc);
parse_tdd_hcr_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_tdd_lcr({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_tdd_lcr({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_tdd_lcr({endElement, _Uri, "UtranCellTDDLcr", QName},
		[#state{dn_prefix = [TddLcrDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state = UtranState,
		spec_cache = PrevCache} = PrevState | T1]) ->
	#utran_state{tdd_lcrs = LcrRels} = UtranState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	TddLcrAttr = parse_tdd_lcr_attr(T2, undefined, []),
	ClassType = "UtranCellTDDLcr",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = TddLcrDn,
			description = "UMTS Time Division Duplex Low Chip Rate",
			category = "RAN",
			class_type = ClassType,
			base_type = "UtranCellTDD",
			schema = ?PathInventorySchema ++ "/UtranCellTDDLcr",
			specification = Spec,
			characteristic = TddLcrAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id} = _R} ->
			LcrRel = #resource_rel{id = Id, name = TddLcrDn,
					rel_type = "contains", ref_type = ClassType,
					href = ?ResourcePath ++ Id},
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					parse_state = UtranState#utran_state{tdd_lcrs = [LcrRel | LcrRels]}} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_tdd_lcr({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_tdd_lcr_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_tdd_lcr_attr1(Attributes, undefined, Acc).
% @hidden
parse_tdd_lcr_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_tdd_lcr_attr1(T2, undefined, Acc);
parse_tdd_lcr_attr1([{endElement, {_, "uraList"} = QName} | T1],
		undefined, Acc) ->
	% @todo uraList
	{[_ | _UraList], T2} = pop(startElement, QName, T1),
	parse_tdd_lcr_attr1(T2, undefined, Acc);
parse_tdd_lcr_attr1([{endElement, {_, "relatedAntennaList"} = QName} | T1],
		undefined, Acc) ->
	% @todo relatedAntennaList
	{[_ | _RelatedAnntenaList], T2} = pop(startElement, QName, T1),
	parse_tdd_lcr_attr1(T2, undefined, Acc);
parse_tdd_lcr_attr1([{endElement, {_, "relatedTmaList"} = QName} | T1],
		undefined, Acc) ->
	% @todo relatedTmaList
	{[_ | _RelatedTmaList], T2} = pop(startElement, QName, T1),
	parse_tdd_lcr_attr1(T2, undefined, Acc);
parse_tdd_lcr_attr1([{endElement, {_, "snaInformation"} = QName} | T1],
		undefined, Acc) ->
	% @todo snaInformation
	{[_ | _SnaInfo], T2} = pop(startElement, QName, T1),
	parse_tdd_lcr_attr1(T2, undefined, Acc);
parse_tdd_lcr_attr1([{endElement, {_, "nsPlmnIdList"} = QName} | T1],
		undefined, Acc) ->
	% @todo NsPlmnIdListType
	{[_ | _NsplmnIdListType], T2} = pop(startElement, QName, T1),
	parse_tdd_lcr_attr1(T2, undefined, Acc);
parse_tdd_lcr_attr1([{endElement, {_, "cellCapabilityContainerTDD"} = QName} | T1],
		undefined, Acc) ->
	% @todo cellCapabilityContainerTDD
	{[_ | _CellccTdd], T2} = pop(startElement, QName, T1),
	parse_tdd_lcr_attr1(T2, undefined, Acc);
parse_tdd_lcr_attr1([{endElement, {_, "uarfcnLCRList"} = QName} | T1],
		undefined, Acc) ->
	% @todo uarfcnLCRList
	{[_ | _UarnfcnLcrList], T2} = pop(startElement, QName, T1),
	parse_tdd_lcr_attr1(T2, undefined, Acc);
parse_tdd_lcr_attr1([{endElement, {_, "timeSlotLCRList"} = QName} | T1],
		undefined, Acc) ->
	% @todo timeSlotLCRList
	{[_ | _TimeSlotLcrList], T2} = pop(startElement, QName, T1),
	parse_tdd_lcr_attr1(T2, undefined, Acc);
parse_tdd_lcr_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "cId" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "localCellId" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"maximumTransmissionPower" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, "FDDMode"} | T], "cellMode" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "FDDMode"} | Acc]);
parse_tdd_lcr_attr1([{characters, "3-84McpsTDDMode"} | T],
		"cellMode" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "3-84McpsTDDMode"} | Acc]);
parse_tdd_lcr_attr1([{characters, "1-28McpsTDDMode"} | T],
		"cellMode" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "1-28McpsTDDMode"} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "pichPower" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "pchPower" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "fachPower" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "lac" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "rac" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "sac" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"utranCellIubLink" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_lcr_attr1([{characters, "enabled"} | T],
		"operationalState" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "enabled"} | Acc]);
parse_tdd_lcr_attr1([{characters, "disabled"} | T],
		"operationalState" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "disabled"} | Acc]);
parse_tdd_lcr_attr1([{characters, "0"} | T], "hsFlag" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr, value = 0} | Acc]);
parse_tdd_lcr_attr1([{characters, "1"} | T], "hsFlag" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr, value = 1} | Acc]);
parse_tdd_lcr_attr1([{characters, "1"} | T], "hsEnable" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr, value = 1} | Acc]);
parse_tdd_lcr_attr1([{characters, "0"} | T], "hsEnable" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr, value = 0} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "numOfHspdschs" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "numOfHsscchs" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "frameOffset" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"cellIndividualOffset" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "hcsPrio" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"maximumAllowedUlTxPower" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "qrxlevMin" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "deltaQrxlevmin" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "qhcs" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "penaltyTime" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"referenceTimeDifferenceToCell" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, "TRUE"} | T],
		"readSFNIndicator" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "TRUE"} | Acc]);
parse_tdd_lcr_attr1([{characters, "FALSE"} | T],
		"readSFNIndicator" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "FALSE"} | Acc]);
parse_tdd_lcr_attr1([{characters, "cellReservedForOperatorUse"} | T],
		"restrictionStateIndicator" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "cellReservedForOperatorUse"} | Acc]);
parse_tdd_lcr_attr1([{characters, "cellAccessible"} | T],
		"restrictionStateIndicator" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "cellAccessible"} | Acc]);
parse_tdd_lcr_attr1([{characters, "dpcModeChangeSupported"} | T],
		"dpcModechangeSupportIndicator" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "dpcModeChangeSupported"} | Acc]);
parse_tdd_lcr_attr1([{characters, "dpcModeChangeNotSupported"} | T],
		"dpcModechangeSupportIndicator" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "dpcModeChangeNotSupported"} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"relatedSectorEquipment" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "uarfcn" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "cellParameterId" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"primaryCcpchPower" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, "active"} | T],
		"sctdIndicator" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "active"} | Acc]);
parse_tdd_lcr_attr1([{characters, "inactive"} | T],
		"sctdIndicator" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "inactive"} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"dpchConstantValue" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "fpachPower" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], "dwPchPower" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, "active"} | T],
		"tstdIndicator" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "active"} | Acc]);
parse_tdd_lcr_attr1([{characters, "inactive"} | T],
		"tstdIndicator" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = "inactive"} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_lcr_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_tdd_lcr_attr1(T, undefined, Acc);
parse_tdd_lcr_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, Acc);
parse_tdd_lcr_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_iub({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_iub({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_iub({endElement, _Uri, "IubLink", QName},
		[#state{dn_prefix = [IubDn | _], stack = Stack,
		spec_cache = Cache, location = Location},
		#state{spec_cache = PrevCache,
		parse_state = UtranState} = PrevState | T1]) ->
	#utran_state{iubs = IubRels} = UtranState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	IubAttr = parse_iub_attr(T2, undefined, []),
	ClassType = "IubLink",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = IubDn,
			description = "UMTS IUB interface",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/IubLink",
			specification = Spec,
			characteristic = [PeeParam | IubAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id} = _R} ->
			IubRel = #resource_rel{id = Id, name = IubDn,
					rel_type = "contains", ref_type = ClassType,
					href = ?ResourcePath ++ Id},
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					parse_state = UtranState#utran_state{iubs = [IubRel | IubRels]}} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_iub({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_iub_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_iub_attr1(Attributes, undefined, Acc).
% @hidden
parse_iub_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfpList], T2} = pop(startElement, QName, T1),
	parse_iub_attr1(T2, undefined, Acc);
parse_iub_attr1([{endElement, {_, "dn"} = QName} | T1],
		"iubLinkUtranCell", Acc) ->
	{[_ | IubLinkUtranCell], T2} = pop(startElement, {"un", "iubLinkUtranCell"}, T1),
	IubUtranCellDnList = parse_iub_utrancell(IubLinkUtranCell ++
			[{endElement, QName}], undefined, []),
	parse_iub_attr1(T2, undefined, [#resource_char{name = "iubLinkUtranCell",
			value = IubUtranCellDnList} | Acc]);
parse_iub_attr1([{characters, Chars} | T],
		"iubLinkUtranCell" = Attr, Acc) ->
	DnList = string:tokens(Chars, ";"),
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr, value = DnList} | Acc]);
parse_iub_attr1([{characters, Chars} | T],
		"layerProtocolNameList" = Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T], "aEnd" = Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T], "zEnd" = Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T], "linkType" = Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T], "protocolVersion" = Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T],
		"iubLinkATMChannelTerminationPoint" = Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T],
		"iubLinkNodeBFunction" = Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_iub_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_iub_attr1(T, undefined, Acc);
parse_iub_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_iub_attr1(T, Attr, Acc);
parse_iub_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_iub_utrancell([{startElement, {_, "dn"} = _QName, []} | T],
		undefined, Acc) ->
	parse_iub_utrancell(T, "dn", Acc);
parse_iub_utrancell([{characters, DN} | T], "dn" = Attr, Acc) ->
	parse_iub_utrancell(T, Attr, [DN | Acc]);
parse_iub_utrancell([{endElement, {_, Attr}} | T], Attr, Acc) ->
	parse_iub_utrancell(T, undefined, Acc);
parse_iub_utrancell([], undefined, Acc) ->
	Acc.

%% @hidden
parse_iucs({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_iucs({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_iucs({endElement, _Uri, "EP_IuCS", QName},
		[#state{parse_state = #utran_state{iucs = Iucs}, stack = Stack},
		#state{parse_state = UtranState} = PrevState | T1]) ->
	#utran_state{rnc = Rnc} = UtranState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NewIucs = parse_iucs_attr(T2, undefined, Iucs),
	NewRnc = choice_add(NewIucs, Rnc),
	[PrevState#state{parse_state = UtranState#utran_state{rnc = NewRnc}} | T1];
parse_iucs({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_iucs_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_iucs_attr1(Attributes, undefined, Acc).
% @hidden
parse_iucs_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	NewAcc = attribute_add("userLabel", Chars, Acc),
	parse_iucs_attr1(T, Attr, NewAcc);
parse_iucs_attr1([{characters, Chars} | T], "farEndEntity" = Attr, Acc) ->
	NewAcc = attribute_add("farEndEntity", Chars, Acc),
	parse_iucs_attr1(T, Attr, NewAcc);
parse_iucs_attr1([{characters, Chars} | T], "connMscNumber" = Attr, Acc) ->
	NewAcc = attribute_add("connMscNumber", list_to_integer(Chars), Acc),
	parse_iucs_attr1(T, Attr, NewAcc);
parse_iucs_attr1([{characters, Chars} | T], Attr, Acc) ->
	NewAcc = attribute_add(Attr, Chars, Acc),
	parse_iucs_attr1(T, Attr, NewAcc);
parse_iucs_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_iucs_attr1(T, undefined, Acc);
parse_iucs_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_iucs_attr1(T, Attr, Acc);
parse_iucs_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_iups({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_iups({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_iups({endElement, _Uri, "EP_IuPS", QName},
		[#state{parse_state = #utran_state{iups = Iups}, stack = Stack},
		#state{parse_state = UtranState} = PrevState | T1]) ->
	#utran_state{rnc = Rnc} = UtranState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NewIups = parse_iups_attr(T2, undefined, Iups),
	NewRnc = choice_add(NewIups, Rnc),
	[PrevState#state{parse_state = UtranState#utran_state{
			rnc = NewRnc}} | T1];
parse_iups({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_iups_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_iups_attr1(Attributes, undefined, Acc).
% @hidden
parse_iups_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	NewAcc = attribute_add("userLabel", Chars, Acc),
	parse_iups_attr1(T, Attr, NewAcc);
parse_iups_attr1([{characters, Chars} | T], "farEndEntity" = Attr, Acc) ->
	NewAcc = attribute_add("farEndEntity", Chars, Acc),
	parse_iups_attr1(T, Attr, NewAcc);
parse_iups_attr1([{characters, Chars} | T], "connSgsnNumber" = Attr, Acc) ->
	NewAcc = attribute_add("connSgsnNumber", list_to_integer(Chars), Acc),
	parse_iups_attr1(T, Attr, NewAcc);
parse_iups_attr1([{characters, Chars} | T], Attr, Acc) ->
	NewAcc = attribute_add(Attr, Chars, Acc),
	parse_iups_attr1(T, Attr, NewAcc);
parse_iups_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_iups_attr1(T, undefined, Acc);
parse_iups_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_iups_attr1(T, Attr, Acc);
parse_iups_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_iur({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_iur({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_iur({endElement, _Uri, "EP_Iur", QName},
		[#state{parse_state = #utran_state{iur = Iur}, stack = Stack},
		#state{parse_state = UtranState} = PrevState | T1]) ->
	#utran_state{rnc = Rnc} = UtranState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NewIur = parse_iur_attr(T2, undefined, Iur),
	NewRnc = choice_add(NewIur, Rnc),
	[PrevState#state{parse_state = UtranState#utran_state{
			rnc = NewRnc}} | T1];
parse_iur({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_iur_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_iur_attr1(Attributes, undefined, Acc).
% @hidden
parse_iur_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	NewAcc = attribute_add("userLabel", Chars, Acc),
	parse_iur_attr1(T, Attr, NewAcc);
parse_iur_attr1([{characters, Chars} | T], "farEndEntity" = Attr, Acc) ->
	NewAcc = attribute_add("farEndEntity", Chars, Acc),
	parse_iur_attr1(T, Attr, NewAcc);
parse_iur_attr1([{characters, Chars} | T], "connectedRncId" = Attr, Acc) ->
	NewAcc = attribute_add("connectedRncId", Chars, Acc),
	parse_iur_attr1(T, Attr, NewAcc);
parse_iur_attr1([{characters, Chars} | T], Attr, Acc) ->
	NewAcc = attribute_add(Attr, Chars, Acc),
	parse_iur_attr1(T, Attr, NewAcc);
parse_iur_attr1([{startElement, {_, Attr}, _} | T],
		Attr, Acc) ->
	parse_iur_attr1(T, undefined, Acc);
parse_iur_attr1([{endElement, {_, Attr}} | T],
		undefined, Acc) ->
	parse_iur_attr1(T, Attr, Acc);
parse_iur_attr1([], undefined, Acc) ->
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
				{ok, #specification{id = Id, href = Href,
						name = Name, class_type = Type, version = Version}} ->
					SpecRef = #specification_ref{id = Id, href = Href,
							name = Name, ref_type = Type, version = Version},
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
