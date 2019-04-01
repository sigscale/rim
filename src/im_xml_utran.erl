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
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% export the im private API
-export([parse_rnc/2, parse_fdd/2, parse_nodeb/2, parse_iub/2,
			parse_iucs/2, parse_iups/2, parse_iur/2, parse_tdd_hcr/2,
			parse_tdd_lcr/2, parse_utran_rel/2]).

-export([fraction1/1]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

-define(PathCatalogSchema, "/resourceCatalogManagement/v3/resourceCatalogManagement").
-define(PathInventorySchema, "/resourceInventoryManagement/v3/resourceInventoryManagement").


%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

%% @hidden
parse_nodeb({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_nodeb({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_nodeb({endElement, _Uri, "NodeBFunction", QName} = Event,
		[#state{stack = Stack, parse_state = UtranState} = State,
		#state{parse_module = M, parse_function = F} = PrevState | T1]) ->
	#utran_state{nodeb = NodeB} = UtranState,
	{[_ | T2], NewStack} = pop(startElement, QName, Stack),
	NodeBAttr = parse_nodeb_attr(T2, undefined, []),
	StateStack = [State#state{stack = NewStack,
			parse_state = UtranState#utran_state{
			nodeb = NodeB#{"attributes" => NodeBAttr}}},
			PrevState | T1],
	M:F(Event, StateStack);
parse_nodeb({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_nodeb_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_nodeb_attr1(Attributes, undefined, Acc).
% @hidden
parse_nodeb_attr1([{endElement, {"un", "vnfParametersList" = Attr}} | T],
		undefined, Acc) ->
	% @todo vnfParametersListType
	parse_nodeb_attr1(T, Attr, Acc);
parse_nodeb_attr1([{endElement, {"un", "peeParametersList"}} | T],
		undefined, Acc) ->
	% @todo peeParametersListType
	parse_nodeb_attr1(T, undefined, Acc);
parse_nodeb_attr1([{characters, Chars} | T],
		"userLabel" = Attr, Acc) ->
	parse_nodeb_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_nodeb_attr1([{characters, Chars} | T],
		"mcc" = Attr, Acc) ->
	parse_nodeb_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nodeb_attr1([{characters, Chars} | T],
		Attr, Acc) ->
	parse_nodeb_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_nodeb_attr1([{startElement, {"xn", "vnfInstanceId"}, _} | T],
		Attr, Acc) ->
	parse_nodeb_attr1(T, Attr, Acc);
parse_nodeb_attr1([{startElement, {"xn", "autoScalable"}, _} | T],
		Attr, Acc) ->
	parse_nodeb_attr1(T, Attr, Acc);
parse_nodeb_attr1([{startElement, {"un", Attr}, _} | T],
		Attr, Acc) ->
	parse_nodeb_attr1(T, undefined, Acc);
parse_nodeb_attr1([{endElement, {"un", Attr}} | T],
		undefined, Acc) ->
	parse_nodeb_attr1(T, Attr, Acc);
parse_nodeb_attr1([{endElement, {"xn", "autoScalable"}} | T],
		Attr, Acc) ->
	parse_nodeb_attr1(T, Attr, Acc);
parse_nodeb_attr1([{endElement, {"xn", "vnfInstanceId"}} | T],
		Attr, Acc) ->
	parse_nodeb_attr1(T, Attr, Acc);
parse_nodeb_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_rnc({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_rnc({startElement, _Uri, "UtranCellFDD", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",UtranCellFDD=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_utran,
			parse_function = parse_fdd,
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
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",IubLink=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_utran,
			parse_function = parse_iub,
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
parse_rnc({endElement, _Uri, "EP_IuCS", _QName},
		[#state{parse_state = #utran_state{iucs = Iucs}},
		#state{parse_state = UtranState} = PrevState | T]) ->
	#utran_state{rnc = Rnc} = UtranState,
	NewRnc = choice_add(Iucs, Rnc),
	[PrevState#state{parse_state = UtranState#utran_state{
			rnc = NewRnc}} | T];
parse_rnc({endElement, _Uri, "EP_IuPS", _QName},
		[#state{parse_state = #utran_state{iups = Iups}},
		#state{parse_state = UtranState} = PrevState | T]) ->
	#utran_state{rnc = Rnc} = UtranState,
	NewRnc = choice_add(Iups, Rnc),
	[PrevState#state{parse_state = UtranState#utran_state{
			rnc = NewRnc}} | T];
parse_rnc({endElement, _Uri, "EP_Iur", _QName},
		[#state{parse_state = #utran_state{iur = Iur}},
		#state{parse_state = UtranState} = PrevState | T]) ->
	#utran_state{rnc = Rnc} = UtranState,
	NewRnc = choice_add(Iur, Rnc),
	[PrevState#state{parse_state = UtranState#utran_state{
			rnc = NewRnc}} | T];
parse_rnc({endElement, _Uri, "UtranCellFDD", _QName},
		[#state{dn_prefix = [FddDn | _], parse_state = #utran_state{
		fdd = #{"attributes" := FddAttr, "choice" := Choice}},
		spec_cache = Cache},
		#state{spec_cache = PrevCache, parse_state = UtranState} = PrevState | T]) ->
	#utran_state{fdds = Fdds} = UtranState,
	ClassType = "UtranCellFDD",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = FddDn,
			description = "UMTS radio",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/UtranCellFDD",
			specification = Spec,
			characteristic = [FddAttr | Choice]},
	case im:add_resource(Resource) of
		{ok, #resource{}} ->
			[PrevState#state{
					parse_state = UtranState#utran_state{fdds = [FddDn | Fdds]},
					spec_cache = [NewCache | PrevCache]} | T];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_rnc({endElement, _Uri, "IubLink", _QName},
		[#state{dn_prefix = [IubDn | _], parse_state = #utran_state{
		iub = #{"attributes" := IubAttr}}, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T]) ->
	ClassType = "IubLink",
%	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = IubDn,
			description = "UMTS IUB interface",
			category = "RAN",
			class_type = ClassType,
			base_type = "SubNetwork",
			schema = "/resourceInventoryManagement/v3/schema/IubLink",
%			specification = Spec,
			characteristic = [IubAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [Cache | PrevCache]} | T];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_rnc({endElement, _Uri, "UtranCellTDDHcr", _QName},
		[#state{dn_prefix = [TddHcrDn | _], parse_state = #utran_state{
		tdd_hcr = #{"attributes" := TddHcrAttr}}, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T]) ->
	ClassType = "UtranCellTDDHcr",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = TddHcrDn,
			description = "UMTS Time Division Duplex High Chip Rate",
			category = "RAN",
			class_type = ClassType,
			base_type = "SubNetwork",
			schema = "/resourceInventoryManagement/v3/schema/UtranCellTDDHcr",
			specification = Spec,
			characteristic = [TddHcrAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_rnc({endElement, _Uri, "UtranCellTDDLcr", _QName},
		[#state{dn_prefix = [TddLcrDn | _], parse_state = #utran_state{
		tdd_lcr = #{"attributes" := TddLcrAttr}}, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T]) ->
	ClassType = "UtranCellTDDLcr",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = TddLcrDn,
			description = "UMTS Time Division Duplex Low Chip Rate",
			category = "RAN",
			class_type = ClassType,
			base_type = "SubNetwork",
			schema = "/resourceInventoryManagement/v3/schema/UtranCellTDDLcr",
			specification = Spec,
			characteristic = [TddLcrAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_rnc({endElement, _Uri, "RncFunction", QName} = Event,
		[#state{parse_state = UtranState, stack = Stack} = State,
		#state{parse_module = M, parse_function = F} = PrevState | T1]) ->
	#utran_state{rnc = Rnc} = UtranState,
	{[_ | T2], NewStack} = pop(startElement, QName, Stack),
	RncAttr = parse_rnc_attr(T2, undefined, []),
	StateStack = [State#state{stack = NewStack,
			parse_state = UtranState#utran_state{
			rnc = Rnc#{"attributes" => RncAttr}}}, PrevState | T1],
	M:F(Event, StateStack);
parse_rnc({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_rnc_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_rnc_attr1(Attributes, undefined, Acc).
% @hidden
parse_rnc_attr1([{endElement, {"un", "vnfParametersList" = Attr}} | T],
		undefined, Acc) ->
	% @todo vnfParametersListType
	parse_rnc_attr1(T, Attr, Acc);
parse_rnc_attr1([{endElement, {"un", "peeParametersList"}} | T],
		undefined, Acc) ->
	% @todo peeParametersListType
	parse_rnc_attr1(T, undefined, Acc);
parse_rnc_attr1([{endElement, {"un", "tceIDMappingInfoList"}} | T],
		undefined, Acc) ->
	% @todo TceIDMappingInfoList
	parse_rnc_attr1(T, undefined, Acc);
parse_rnc_attr1([{endElement, {"un", "sharNetTceMappingInfoList"}} | T],
		undefined, Acc) ->
	% @todo SharNetTceMappingInfoList
	parse_rnc_attr1(T, undefined, Acc);
parse_rnc_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_rnc_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_rnc_attr1([{characters, Chars} | T], "mcc" = Attr, Acc) ->
	parse_rnc_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_rnc_attr1([{characters, Chars} | T], "mnc" = Attr, Acc) ->
	parse_rnc_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_rnc_attr1([{characters, Chars} | T], "rncId" = Attr, Acc) ->
	parse_rnc_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_rnc_attr1([{characters, "0"} | T], "siptoSupported" = Attr, Acc) ->
	parse_rnc_attr1(T, Attr, [#resource_char{name = Attr,
			value = 0} | Acc]);
parse_rnc_attr1([{characters, "1"} | T], "siptoSupported" = Attr, Acc) ->
	parse_rnc_attr1(T, Attr, [#resource_char{name = Attr,
			value = 1} | Acc]);
parse_rnc_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_rnc_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_rnc_attr1([{startElement, {"xn", "vnfInstanceId"}, _} | T], Attr, Acc) ->
	parse_rnc_attr1(T, Attr, Acc);
parse_rnc_attr1([{startElement, {"xn", "autoScalable"}, _} | T], Attr, Acc) ->
	parse_rnc_attr1(T, Attr, Acc);
parse_rnc_attr1([{startElement, {"un", Attr}, _} | T], Attr, Acc) ->
	parse_rnc_attr1(T, undefined, Acc);
parse_rnc_attr1([{endElement, {"un", Attr}} | T], undefined, Acc) ->
	parse_rnc_attr1(T, Attr, Acc);
parse_rnc_attr1([{endElement, {"xn", "vnfInstanceId"}} | T], Attr, Acc) ->
	parse_rnc_attr1(T, Attr, Acc);
parse_rnc_attr1([{endElement, {"xn", "autoScalable"}} | T], Attr, Acc) ->
	parse_rnc_attr1(T, Attr, Acc);
parse_rnc_attr1([], undefined, Acc) ->
	Acc.

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
parse_fdd({endElement, _Uri, "UtranRelation", _QName},
		[#state{parse_state = #utran_state{utran_rel = UtranRel}},
		#state{parse_state = UtranState} = PrevState | T]) ->
	#utran_state{fdd = Fdd} = UtranState,
	NewFdd = choice_add(UtranRel, Fdd),
	[PrevState#state{parse_state = UtranState#utran_state{
			fdd = NewFdd}} | T];
parse_fdd({endElement, _Uri, "GsmRelation", _QName},
		[#state{parse_state = #geran_state{gsm_rel = GsmRel}},
		#state{parse_state = UtranState} = PrevState | T]) ->
	#utran_state{fdd = Fdd} = UtranState,
	NewFdd = choice_add(GsmRel, Fdd),
	[PrevState#state{parse_state = UtranState#utran_state{
			fdd = NewFdd}} | T];
parse_fdd({endElement, _Uri, "UtranCellFDD", QName} = Event,
		[#state{stack = Stack, parse_state = UtranState} = State,
		#state{parse_module = M, parse_function = F} = PrevState | T1]) ->
	#utran_state{fdd = Fdd} = UtranState,
	{[_ | T2], NewStack} = pop(startElement, QName, Stack),
	FddAttr = parse_fdd_attr(T2, []),
	StateStack = [State#state{stack = NewStack,
			parse_state = UtranState#utran_state{
			fdd = Fdd#{"attributes" => FddAttr}}}, PrevState | T1],
	M:F(Event, StateStack);
parse_fdd({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_fdd_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
		Acc) ->
	{[_ | Attributes], _} = pop(endElement, QName, T1),
	parse_fdd_attr1(Attributes, undefined, Acc).
% @hidden
parse_fdd_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
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
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_fdd_attr1([{characters, "enabled"} | T],
		"operationalState" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "enabled"} | Acc]);
parse_fdd_attr1([{characters, "disabled"} | T],
		"operationalState" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = "disabled"} | Acc]);
parse_fdd_attr1([{characters, "0"} | T], "hsFlag" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = 0} | Acc]);
parse_fdd_attr1([{characters, "1"} | T], "hsFlag" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = 1} | Acc]);
parse_fdd_attr1([{characters, "1"} | T], "hsEnable" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = 1} | Acc]);
parse_fdd_attr1([{characters, "0"} | T], "hsEnable" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, [#resource_char{name = Attr,
			value = 0} | Acc]);
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
parse_fdd_attr1([{startElement, {"un", "relatedAntennaList" = Attr}, _} | T],
			Attr, Acc) ->
	parse_fdd_attr1(T, undefined, Acc);
parse_fdd_attr1([{startElement, {"xn", "dn"}, _} | T],
			"relatedAntennaList" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, Acc);
parse_fdd_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	% @todo default handler
	parse_fdd_attr1(T, undefined, Acc);
parse_fdd_attr1([{endElement, {"un", "vnfParametersList"}} | T],
			undefined, Acc) ->
	% @todo vnfParametersListType
	parse_fdd_attr1(T, undefined, Acc);
parse_fdd_attr1([{endElement, {"un", "uraList"}} | T], undefined, Acc) ->
	% @todo uraList
	parse_fdd_attr1(T, undefined, Acc);
parse_fdd_attr1([{endElement, {"un", "relatedAntennaList" = Attr}} | T],
			undefined, Acc) ->
	% @todo relatedAntennaList
	parse_fdd_attr1(T, Attr, Acc);
parse_fdd_attr1([{endElement, {"un", "relatedTmaList"}} | T],
		undefined, Acc) ->
	% @todo relatedTmaList
	parse_fdd_attr1(T, undefined, Acc);
parse_fdd_attr1([{endElement, {"un", "snaInformation"}} | T],
		undefined, Acc) ->
	% @todo snaInformation
	parse_fdd_attr1(T, undefined, Acc);
parse_fdd_attr1([{endElement, {"un", "nsPlmnIdList"}} | T], undefined, Acc) ->
	% @todo NsPlmnIdListType
	parse_fdd_attr1(T, undefined, Acc);
parse_fdd_attr1([{endElement, {"un", "cellCapabilityContainerFDD"}} | T],
		undefined, Acc) ->
	% @todo cellCapabilityContainerFDD
	parse_fdd_attr1(T, undefined, Acc);
parse_fdd_attr1([{endElement, {"xn", Attr}} | T], Attr, Acc) ->
	% @todo default handler
	parse_fdd_attr1(T, Attr, Acc);
parse_fdd_attr1([{endElement, {"xn", "dn"}} | T],
		"relatedAntennaList" = Attr, Acc) ->
	parse_fdd_attr1(T, Attr, Acc);
parse_fdd_attr1([{endElement, {"xn", Attr}} | T], undefined, Acc) ->
	parse_fdd_attr1(T, Attr, Acc);
parse_fdd_attr1([{endElement, {"un", Attr}} | T], undefined, Acc) ->
	parse_fdd_attr1(T, Attr, Acc);
parse_fdd_attr1([], _Attr, Acc) ->
	Acc.

%% @hidden
parse_utran_rel({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_utran_rel({startElement, _Uri, _LocalName, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_utran_rel({endElement, _Uri, "UtranRelation", QName} = Event,
		[#state{stack = Stack, parse_state = UtranState} = State,
		#state{parse_module = M, parse_function = F} = PrevState | T1]) ->
	#utran_state{utran_rel = UtranRel} = UtranState,
	{[_ | T], NewStack} = pop(startElement, QName, Stack),
	NewUtranRel = parse_utran_rel_attr(T, undefined, UtranRel),
	StateStack = [State#state{stack = NewStack, parse_state = UtranState#utran_state{
			utran_rel = NewUtranRel}}, PrevState | T1],
	M:F(Event, StateStack);
parse_utran_rel({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_utran_rel_attr([{startElement, {"un", "attributes"} = QName, []} | T],
		undefined, Acc) ->
	{[_ | Attributes], _Rest} = pop(endElement, QName, T),
	parse_utran_rel_attr1(Attributes, undefined, Acc).
% @hidden
parse_utran_rel_attr1([{endElement, {"un", Attr}} | T], undefined, Acc) ->
	parse_utran_rel_attr1(T, Attr, Acc);
parse_utran_rel_attr1([{characters, Chars} | T], "adjacentCell" = Attr, Acc) ->
	NewAcc = attribute_add("adjacentCell", Chars, Acc),
	parse_utran_rel_attr1(T, Attr, NewAcc);
parse_utran_rel_attr1([{startElement, {"un", Attr}, []} | T], Attr, Acc) ->
	parse_utran_rel_attr1(T, undefined, Acc);
parse_utran_rel_attr1([],  _Attr, Acc) ->
	Acc.

%% @hidden
parse_tdd_hcr({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_tdd_hcr({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_tdd_hcr({endElement, _Uri, "UtranCellTDDHcr", QName} = Event,
		[#state{parse_state = UtranState, stack = Stack} = State,
		#state{parse_module = M, parse_function = F} = PrevState | T1]) ->
	{[_ | T2], NewStack} = pop(startElement, QName, Stack),
	HcrAttr = parse_tdd_hcr_attr(T2, undefined, []),
	#utran_state{tdd_hcr = TddHcr} = UtranState,
	StateStack = [State#state{stack = NewStack,
			parse_state = UtranState#utran_state{
			tdd_hcr = TddHcr#{"attributes" => HcrAttr}}}, PrevState | T1],
	M:F(Event, StateStack);
parse_tdd_hcr({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_tdd_hcr_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_tdd_hcr_attr1(Attributes, undefined, Acc).
% @hidden
parse_tdd_hcr_attr1([{endElement, {"un", "vnfParametersList" = Attr}} | T],
		undefined, Acc) ->
	% @todo vnfParametersListType
	parse_tdd_hcr_attr1(T, Attr, Acc);
parse_tdd_hcr_attr1([{endElement, {"un", "uraList"}} | T],
		undefined, Acc) ->
	% @todo uraList
	parse_tdd_hcr_attr1(T, undefined, Acc);
parse_tdd_hcr_attr1([{endElement, {"un", "relatedAntennaList" = Attr}} | T],
		undefined, Acc) ->
	% @todo relatedAntennaList
	parse_tdd_hcr_attr1(T, Attr, Acc);
parse_tdd_hcr_attr1([{endElement, {"un", "relatedTmaList"}} | T],
		undefined, Acc) ->
	% @todo relatedTmaList
	parse_tdd_hcr_attr1(T, undefined, Acc);
parse_tdd_hcr_attr1([{endElement, {"un", "snaInformation"}} | T],
		undefined, Acc) ->
	% @todo snaInformation
	parse_tdd_hcr_attr1(T, undefined, Acc);
parse_tdd_hcr_attr1([{endElement, {"un", "nsPlmnIdList"}} | T],
		undefined, Acc) ->
	% @todo NsPlmnIdListType
	parse_tdd_hcr_attr1(T, undefined, Acc);
parse_tdd_hcr_attr1([{endElement, {"un", "cellCapabilityContainerTDD"}} | T],
		undefined, Acc) ->
	% @todo cellCapabilityContainerTDD 
	parse_tdd_hcr_attr1(T, undefined, Acc);
parse_tdd_hcr_attr1([{endElement, {"un", "timeSlotHCRList"}} | T],
		undefined, Acc) ->
	% @todo timeSlotHCRList
	parse_tdd_hcr_attr1(T, undefined, Acc);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"userLabel" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"cId" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"localCellId" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"maximumTransmissionPower" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, "FDDMode"} | T],
		"cellMode" = Attr, Acc) ->
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
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"pichPower" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"pchPower" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"fachPower" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"lac" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"rac" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"sac" = Attr, Acc) ->
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
parse_tdd_hcr_attr1([{characters, "0"} | T],
		"hsFlag" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = 0} | Acc]);
parse_tdd_hcr_attr1([{characters, "1"} | T],
		"hsFlag" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = 1} | Acc]);
parse_tdd_hcr_attr1([{characters, "1"} | T],
		"hsEnable" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = 1} | Acc]);
parse_tdd_hcr_attr1([{characters, "0"} | T],
		"hsEnable" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = 0} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"numOfHspdschs" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"numOfHsscchs" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"frameOffset" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"cellIndividualOffset" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"hcsPrio" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"maximumAllowedUlTxPower" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"qrxlevMin" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"deltaQrxlevmin" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"qhcs" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"penaltyTime" = Attr, Acc) ->
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
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"uarfcn" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"cellParameterId" = Attr, Acc) ->
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
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"schPower" = Attr, Acc) ->
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
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"timeSlotForSch" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"schTimeSlot" = Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_hcr_attr1([{startElement, {"xn", "dn"}, _} | T], Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, Acc);
parse_tdd_hcr_attr1([{startElement, {"un", Attr}, _} | T], Attr, Acc) ->
	parse_tdd_hcr_attr1(T, undefined, Acc);
parse_tdd_hcr_attr1([{endElement, {"un", Attr}} | T], undefined, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, Acc);
parse_tdd_hcr_attr1([{endElement, {"xn", "dn"}} | T], Attr, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, Acc);
parse_tdd_hcr_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_tdd_lcr({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_tdd_lcr({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_tdd_lcr({endElement, _Uri, "UtranCellTDDLcr", QName} = Event,
		[#state{parse_state = UtranState, stack = Stack} = State,
		#state{parse_module = M, parse_function = F} = PrevState | T1]) ->
	{[_ | T2], NewStack} = pop(startElement, QName, Stack),
	TddLcrAttr = parse_tdd_lcr_attr(T2, undefined, []),
	#utran_state{tdd_lcr = TddLcr} = UtranState,
	StateStack = [State#state{stack = NewStack,
			parse_state = UtranState#utran_state{
			tdd_lcr = TddLcr#{"attributes" => TddLcrAttr}}}, PrevState | T1],
	M:F(Event, StateStack);
parse_tdd_lcr({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_tdd_lcr_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_tdd_lcr_attr1(Attributes, undefined, Acc).
% @hidden
parse_tdd_lcr_attr1([{endElement, {"un", "vnfParametersList" = Attr}} | T],
		undefined, Acc) ->
	% @todo vnfParametersListType
	parse_tdd_lcr_attr1(T, Attr, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", "uraList"}} | T],
		undefined, Acc) ->
	% @todo uraList
	parse_tdd_lcr_attr1(T, undefined, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", "relatedAntennaList" = Attr}} | T],
		undefined, Acc) ->
	% @todo relatedAntennaList
	parse_tdd_lcr_attr1(T, Attr, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", "relatedTmaList"}} | T],
		undefined, Acc) ->
	% @todo relatedTmaList
	parse_tdd_lcr_attr1(T, undefined, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", "snaInformation"}} | T],
		undefined, Acc) ->
	% @todo snaInformation
	parse_tdd_lcr_attr1(T, undefined, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", "nsPlmnIdList"}} | T],
		undefined, Acc) ->
	% @todo NsPlmnIdListType
	parse_tdd_lcr_attr1(T, undefined, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", "cellCapabilityContainerTDD"}} | T],
		undefined, Acc) ->
	% @todo cellCapabilityContainerTDD 
	parse_tdd_lcr_attr1(T, undefined, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", "uarfcnLCRList"}} | T],
		undefined, Acc) ->
	% @todo uarfcnLCRList
	parse_tdd_lcr_attr1(T, undefined, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", "timeSlotLCRList"}} | T],
		undefined, Acc) ->
	% @todo timeSlotLCRList
	parse_tdd_lcr_attr1(T, undefined, Acc);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"userLabel" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"cId" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"localCellId" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"maximumTransmissionPower" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, "FDDMode"} | T],
		"cellMode" = Attr, Acc) ->
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
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"pichPower" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"pchPower" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"fachPower" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"lac" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"rac" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"sac" = Attr, Acc) ->
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
parse_tdd_lcr_attr1([{characters, "0"} | T],
		"hsFlag" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = 0} | Acc]);
parse_tdd_lcr_attr1([{characters, "1"} | T],
		"hsFlag" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = 1} | Acc]);
parse_tdd_lcr_attr1([{characters, "1"} | T],
		"hsEnable" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = 1} | Acc]);
parse_tdd_lcr_attr1([{characters, "0"} | T],
		"hsEnable" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = 0} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"numOfHspdschs" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"numOfHsscchs" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"frameOffset" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"cellIndividualOffset" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"hcsPrio" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"maximumAllowedUlTxPower" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"qrxlevMin" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"deltaQrxlevmin" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"qhcs" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"penaltyTime" = Attr, Acc) ->
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
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"uarfcn" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"cellParameterId" = Attr, Acc) ->
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
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"fpachPower" = Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"dwPchPower" = Attr, Acc) ->
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
parse_tdd_lcr_attr1([{characters, Chars} | T],
		Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_lcr_attr1([{startElement, {"xn", "dn"}, _} | T], Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, Acc);
parse_tdd_lcr_attr1([{startElement, {"un", Attr}, _} | T], Attr, Acc) ->
	parse_tdd_lcr_attr1(T, undefined, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", Attr}} | T], undefined, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, Acc);
parse_tdd_lcr_attr1([{endElement, {"xn", "dn"}} | T], Attr, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, Acc);
parse_tdd_lcr_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_iub({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_iub({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_iub({endElement, _Uri, "IubLink", QName} = Event,
		[#state{parse_state = UtranState, stack = Stack} = State,
		#state{parse_module = M, parse_function = F} = PrevState | T1]) ->
	{[_ | T2], NewStack} = pop(startElement, QName, Stack),
	IubAttr = parse_iub_attr(T2, undefined, []),
	#utran_state{iub = Iub} = UtranState,
	StateStack = [State#state{stack = NewStack,
			parse_state = UtranState#utran_state{
			iub = Iub#{"attributes" => IubAttr}}}, PrevState | T1],
	M:F(Event, StateStack);
parse_iub({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_iub_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_iub_attr1(Attributes, undefined, Acc).
% @hidden
parse_iub_attr1([{endElement, {"un", "vnfParametersList" = Attr}} | T],
		undefined, Acc) ->
	% @todo vnfParametersListType
	parse_iub_attr1(T, Attr, Acc);
parse_iub_attr1([{endElement, {"un", "iubLinkUtranCell" = Attr}} | T],
		undefined, Acc) ->
	% @todo dnList
	parse_iub_attr1(T, Attr, Acc);
parse_iub_attr1([{characters, Chars} | T],
		"layerProtocolNameList" = Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T], "aEnd" = Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T], "zEnd" = Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T], "linkType" = Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T], "protocolVersion" = Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T],
		"iubLinkATMChannelTerminationPoint" = Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T],
		"iubLinkNodeBFunction" = Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_iub_attr1(T, Attr, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{startElement, {"xn", "dn"}, _} | T], Attr, Acc) ->
	parse_iub_attr1(T, Attr, Acc);
parse_iub_attr1([{startElement, {"un", Attr}, _} | T], Attr, Acc) ->
	parse_iub_attr1(T, undefined, Acc);
parse_iub_attr1([{endElement, {"un", Attr}} | T], undefined, Acc) ->
	parse_iub_attr1(T, Attr, Acc);
parse_iub_attr1([{endElement, {"xn", "dn"}} | T], Attr, Acc) ->
	parse_iub_attr1(T, Attr, Acc);
parse_iub_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_iucs({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_iucs({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_iucs({endElement, _Uri, "EP_IuCS", QName} = Event,
		[#state{parse_state = UtranState, stack = Stack} = State,
		#state{parse_module = M, parse_function = F} = PrevState | T1]) ->
	#utran_state{iucs = Iucs} = UtranState,
	{[_ | T2], NewStack} = pop(startElement, QName, Stack),
	NewIucs = parse_iucs_attr(T2, undefined, Iucs),
	StateStack = [State#state{stack = NewStack, parse_state = UtranState#utran_state{
			iucs = NewIucs}}, PrevState | T1],
	M:F(Event, StateStack);
parse_iucs({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_iucs_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
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
parse_iucs_attr1([{startElement, {"un", Attr}, _} | T], Attr, Acc) ->
	parse_iucs_attr1(T, undefined, Acc);
parse_iucs_attr1([{endElement, {"un", Attr}} | T], undefined, Acc) ->
	parse_iucs_attr1(T, Attr, Acc);
parse_iucs_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_iups({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_iups({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_iups({endElement, _Uri, "EP_IuPS", QName} = Event,
		[#state{parse_state = UtranState, stack = Stack} = State,
		#state{parse_module = M, parse_function = F} = PrevState | T1]) ->
	#utran_state{iups = Iups} = UtranState,
	{[_ | T2], NewStack} = pop(startElement, QName, Stack),
	NewIups = parse_iups_attr(T2, undefined, Iups),
	StateStack = [State#state{stack = NewStack, parse_state = UtranState#utran_state{
			iups = NewIups}}, PrevState | T1],
	M:F(Event, StateStack);
parse_iups({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_iups_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
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
parse_iups_attr1([{startElement, {"un", Attr}, _} | T], Attr, Acc) ->
	parse_iups_attr1(T, undefined, Acc);
parse_iups_attr1([{endElement, {"un", Attr}} | T], undefined, Acc) ->
	parse_iups_attr1(T, Attr, Acc);
parse_iups_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_iur({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_iur({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_iur({endElement, _Uri, "EP_Iur", QName} = Event,
		[#state{parse_state = UtranState, stack = Stack} = State,
		#state{parse_module = M, parse_function = F} = PrevState | T1]) ->
	#utran_state{iur = Iur} = UtranState,
	{[_ | T2], NewStack} = pop(startElement, QName, Stack),
	NewIur = parse_iur_attr(T2, undefined, Iur),
	StateStack = [State#state{stack = NewStack, parse_state = UtranState#utran_state{
			iur = NewIur}}, PrevState | T1],
	M:F(Event, StateStack);
parse_iur({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_iur_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
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
parse_iur_attr1([{startElement, {"un", Attr}, _} | T],
		Attr, Acc) ->
	parse_iur_attr1(T, undefined, Acc);
parse_iur_attr1([{endElement, {"un", Attr}} | T],
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
