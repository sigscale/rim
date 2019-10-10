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
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
-module(im_xml_nokia).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% export the im private API
-export([import/2, parse_mo/2, parse_bss/2, parse_bts/2, parse_gsm_cell/2,
		parse_hw/2,
		parse_rnc/2, parse_nodeb/2, parse_iub_link/2,
		parse_enb/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

-define(PathCatalogSchema, "/resourceCatalogManagement/v3/resourceCatalogManagement").
-define(PathInventorySchema, "/resourceInventoryManagement/v3/resourceInventoryManagement").

%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

-spec import(File, RuleId) -> Result
	when
		File :: string(),
		RuleId :: string(),
		Result :: ok | ignore | {error, Reason},
		Reason :: term().
%% @doc Import a file into the inventory table.
import(File, RuleId) when is_list(File), is_list(RuleId) ->
	Options = [{event_fun, fun parse_xml/3},
		{event_state, [#state{rule = RuleId}]}],
	case xmerl_sax_parser:file(File, Options) of
		{ok, _EventState, _Rest} ->
			ok;
		{Tag, {CurrentLocation, EntityName, LineNo},
				Reason, EndTags, _EventState} ->
			Message = case Tag of
				get_specification_name ->
					"Error getting specification for resource";
				add_resource ->
					"Error adding resource";
				fatal_error ->
					"Error parsing import file"
			end,
			error_logger:error_report([Message,
					{file, File}, {location, CurrentLocation},
					{line, LineNo}, {entity, EntityName},
					{tags, EndTags}, {error, Reason}]),
			{error, Reason};
		{error, Reason} ->
			error_logger:error_report(["Error parsing import file",
					{file, File}, {error, Reason}]),
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

-spec parse_xml(Event, Location, State) -> NewState
	when
		Event :: xmerl_sax_parser:event(),
		Location :: {CurrentLocation, Entityname, LineNo},
		CurrentLocation :: string(),
		Entityname :: string(),
		LineNo :: integer(),
		State :: [state()],
		NewState :: state().
%% @doc Parse xml.
parse_xml(startDocument = _Event, _Location, State) ->
	State;
parse_xml({startElement, _, "cmData", _, _}, _Location,
		[#state{parse_function = undefined} = State | T]) ->
	[State#state{parse_module = ?MODULE, parse_function = parse_mo} | T];
parse_xml(endDocument = _Event, _Location, State) ->
	State;
parse_xml(_Event, _Location, [#state{parse_function = undefined} | _] = State) ->
	State;
parse_xml({startPrefixMapping, _Prefix, _Uri}, _, State) ->
	State;
parse_xml({endPrefixMapping, _Prefix}, _, State) ->
	State;
parse_xml({ignorableWhitespace, _}, _, State) ->
	State;
parse_xml({comment, _Comment}, _, State) ->
	State;
parse_xml(_Event, _Location, [#state{parse_module = Mod, parse_function = F} | _] = State) ->
	Mod:F(_Event, State).

%% @hidden
parse_mo({startElement, _, "managedObject", QName,
		[{[], [], "class", "BCF"}, _, {[], [], "distName", DN}, _] = Attributes},
		[#state{dn_prefix = [], stack = Stack, rule = RuleId} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_bss,
		dn_prefix = [DN], rule = RuleId,
		stack = [{startElement, QName,Attributes} | Stack]} | State];
parse_mo({startElement, _, "managedObject", QName,
		[{[], [], "class", "BTS"}, _, {[], [], "distName", DN}, _] = Attributes},
		[#state{dn_prefix = [], stack = Stack, rule = RuleId} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_bts,
		dn_prefix = [DN], rule = RuleId,
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo({startElement, _, "managedObject", QName,
		[{[], [], "class", "TRX"}, _, {[], [], "distName", DN}, _] = Attributes},
		[#state{dn_prefix = [], stack = Stack, rule = RuleId} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_gsm_cell,
		dn_prefix = [DN], rule = RuleId,
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo({startElement, _, "managedObject", QName,
		[{[], [], "class", "HW"}, _, {[], [], "distName", DN}, _] = Attributes},
		[#state{dn_prefix = [], stack = Stack, rule = RuleId} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_hw,
		dn_prefix = [DN], rule = RuleId,
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo({startElement, _, "managedObject", QName,
		[{[], [], "class", "RNC"}, _, {[], [], "distName", DN}, _] = Attributes},
		[#state{dn_prefix = [], stack = Stack, rule = RuleId} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_rnc,
		dn_prefix = [DN], rule = RuleId,
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo({startElement, _, "managedObject", QName,
		[{[], [], "class", "WBTS"}, _, {[], [], "distName", DN}, _] = Attributes},
		[#state{dn_prefix = [], stack = Stack, rule = RuleId} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_nodeb,
		dn_prefix = [DN], rule = RuleId,
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo({startElement, _, "managedObject", QName,
		[{[], [], "class", "IPNB"}, _, {[], [], "distName", DN}, _] = Attributes},
		[#state{dn_prefix = [], stack = Stack, rule = RuleId} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_iub_link,
		dn_prefix = [DN], rule = RuleId,
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo({startElement, _, "managedObject", QName,
		[{[], [], "class", "LNBTS"}, _, {[], [], "distName", DN}, _] = Attributes},
		[#state{dn_prefix = [], stack = Stack, rule = RuleId} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_enb,
		dn_prefix = [DN], rule = RuleId,
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo(_Event, [#state{parse_module = ?MODULE,
		parse_function = parse_mo} | _] = State) ->
	State.

%% @hidden
parse_bss({characters, SideId}, [#state{rule = RuleId,
		stack = [{startElement, {_, "p"}, [{[], [], "name", "name"}]} | _]} = State | T]) ->
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
parse_bss({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_bss({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_bss({endElement, _Uri, "managedObject", QName},
		[#state{dn_prefix = [BscDn | _], stack = Stack, location = Location,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	BscAttr = parse_bss_attr(T2, undefined, []),
	ClassType = "BssFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = "/resourceCatalogManagement/v3/schema/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = BscDn,
			description = "GSM Base Station Subsystem (BSS)",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/BssFunction",
			specification = Spec,
			characteristic = [PeeParam | BscAttr]},
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
parse_bss_attr([{startElement, {[], "p"},
		[{[], [], "name", Attr}]} | T], undefined, Acc) ->
	parse_bss_attr(T, Attr, Acc);
parse_bss_attr([{startElement, {_, "list"} = QName, _} | T1], undefined, Acc) ->
	% @todo bscOptions
	{[_ | _BscOptions], T2} = pop(endElement, QName, T1),
	parse_bss_attr(T2, undefined, Acc);
parse_bss_attr([{characters, Chars} | T], Attr, Acc) ->
	parse_bss_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_bss_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_bss_attr(T, undefined, Acc);
parse_bss_attr([], undefined, Acc) ->
	Acc.

%% @hidden
parse_bts({characters, SideId}, [#state{rule = RuleId,
		stack = [{startElement, {_, "p"}, [{[], [], "name", "name"}]} | _]} = State | T]) ->
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
parse_bts({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_bts({endElement, _Uri, "managedObject", QName},
		[#state{dn_prefix = [BtsDn | _], stack = Stack, location = Location,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	GsmBtsAttr = parse_bts_attr(T2, undefined, []),
	ClassType = "BtsSiteMgr",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = "/resourceCatalogManagement/v3/schema/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = BtsDn,
			description = "GSM Base Transceiver Station (BTS)",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/BtsSiteMgr",
			specification = Spec,
			characteristic = [PeeParam | GsmBtsAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_bts({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_bts_attr([{startElement, {[], "p"},
		[{[], [], "name", Attr}]} | T], undefined, Acc) ->
	parse_bts_attr(T, Attr, Acc);
parse_bts_attr([{startElement, {_, "list"} = QName, _} | T1], undefined, Acc) ->
	% @todo bscOptions
	{[_ | _BscOptions], T2} = pop(endElement, QName, T1),
	parse_bts_attr(T2, undefined, Acc);
parse_bts_attr([{characters, Chars} | T], Attr, Acc) ->
	parse_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_bts_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_bts_attr(T, undefined, Acc);
parse_bts_attr([], undefined, Acc) ->
	Acc.

%% @hidden
parse_gsm_cell({characters, SideId}, [#state{rule = RuleId,
		stack = [{startElement, {_, "p"}, [{[], [], "name", "name"}]} | _]} = State | T]) ->
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
parse_gsm_cell({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_gsm_cell({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_gsm_cell({endElement, _Uri, "managedObject", QName},
		[#state{dn_prefix = [CellDn | _], stack = Stack, location = Location,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	GsmCellAttr = parse_gsm_cell_attr(T2, undefined, []),
	ClassType = "GsmCell",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = "/resourceCatalogManagement/v3/schema/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = CellDn,
			description = "GSM Radio",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/GsmCell",
			specification = Spec,
			characteristic = [PeeParam | GsmCellAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_gsm_cell({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_gsm_cell_attr([{startElement, {[], "p"},
		[{[], [], "name", Attr}]} | T], undefined, Acc) ->
	parse_gsm_cell_attr(T, Attr, Acc);
parse_gsm_cell_attr([{startElement, {_, "list"} = QName, _} | T1], undefined, Acc) ->
	% @todo bscOptions
	{[_ | _BscOptions], T2} = pop(endElement, QName, T1),
	parse_gsm_cell_attr(T2, undefined, Acc);
parse_gsm_cell_attr([{characters, Chars} | T], Attr, Acc) ->
	parse_gsm_cell_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_cell_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_gsm_cell_attr(T, undefined, Acc);
parse_gsm_cell_attr([], undefined, Acc) ->
	Acc.

%% @hidden
parse_hw({characters, SideId}, [#state{rule = RuleId, stack = [{startElement,
		{_, "p"}, [{[], [], "name", "locationName"}]} | _]} = State | T]) ->
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
parse_hw({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_hw({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_hw({endElement, _Uri, "managedObject", QName},
		[#state{dn_prefix = [HwDn | _], stack = Stack, location = Location,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	HwAttr = parse_hw_attr(T2, undefined, []),
	ClassType = "InventoryUnit",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = "/resourceCatalogManagement/v3/schema/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = HwDn,
			description = "Hardware Inventory",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/InventoryUnit",
			specification = Spec,
			characteristic = [PeeParam | HwAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_hw({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_hw_attr([{startElement, {[], "p"},
		[{[], [], "name", Attr}]} | T], undefined, Acc) ->
	parse_hw_attr(T, Attr, Acc);
parse_hw_attr([{startElement, {_, "list"} = QName, _} | T1], undefined, Acc) ->
	% @todo bscOptions
	{[_ | _BscOptions], T2} = pop(endElement, QName, T1),
	parse_hw_attr(T2, undefined, Acc);
parse_hw_attr([{characters, Chars} | T], Attr, Acc) ->
	parse_hw_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_hw_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_hw_attr(T, undefined, Acc);
parse_hw_attr([], undefined, Acc) ->
	Acc.

%% @hidden
parse_rnc({characters, SideId}, [#state{rule = RuleId,
		stack = [{startElement, {_, "p"}, [{[], [], "name", "name"}]} | _]} = State | T]) ->
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
parse_rnc({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_rnc({endElement, _Uri, "managedObject", QName},
		[#state{dn_prefix = [RncDn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	RncAttr = parse_rnc_attr(T2, undefined, []),
	ClassType = "RncFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = "/resourceCatalogManagement/v3/schema/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = RncDn,
			description = "UMTS Radio Network Controller (RNC)",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/RncFunction",
			specification = Spec,
			characteristic = [PeeParam | RncAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_rnc({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_rnc_attr([{startElement, {[], "p"},
		[{[], [], "name", Attr}]} | T], undefined, Acc) ->
	parse_rnc_attr(T, Attr, Acc);
parse_rnc_attr([{startElement, {_, "list"} = QName, _} | T1], undefined, Acc) ->
	% @todo bscOptions
	{[_ | _BscOptions], T2} = pop(endElement, QName, T1),
	parse_rnc_attr(T2, undefined, Acc);
parse_rnc_attr([{characters, Chars} | T], Attr, Acc) ->
	parse_rnc_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_rnc_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_rnc_attr(T, undefined, Acc);
parse_rnc_attr([], undefined, Acc) ->
	Acc.

%% @hidden
parse_nodeb({characters, SideId}, [#state{rule = RuleId,
		stack = [{startElement, {_, "p"}, [{[], [], "name", "name"}]} | _]} = State | T]) ->
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
parse_nodeb({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_nodeb({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_nodeb({endElement, _Uri, "managedObject", QName},
		[#state{dn_prefix = [NodebDn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NodebAttr = parse_nodeb_attr(T2, undefined, []),
	ClassType = "NodeBFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = "/resourceCatalogManagement/v3/schema/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = NodebDn,
			description = "UMTS NodeB",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/NodeBFunction",
			specification = Spec,
			characteristic = [PeeParam | NodebAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_nodeb({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_nodeb_attr([{startElement, {[], "p"},
		[{[], [], "name", Attr}]} | T], undefined, Acc) ->
	parse_nodeb_attr(T, Attr, Acc);
parse_nodeb_attr([{startElement, {_, "list"} = QName, _} | T1], undefined, Acc) ->
	% @todo
	{[_ | _BscOptions], T2} = pop(endElement, QName, T1),
	parse_nodeb_attr(T2, undefined, Acc);
parse_nodeb_attr([{characters, Chars} | T], Attr, Acc) ->
	parse_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_nodeb_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_nodeb_attr(T, undefined, Acc);
parse_nodeb_attr([], undefined, Acc) ->
	Acc.

%% @hidden
parse_iub_link({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_iub_link({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_iub_link({endElement, _Uri, "managedObject", QName},
		[#state{dn_prefix = [IubLinkDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	IubLinkAttr = parse_iub_link_attr(T2, undefined, []),
	ClassType = "IubLink",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = IubLinkDn,
			description = "UMTS IUB interface",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/IubLink",
			specification = Spec,
			characteristic = IubLinkAttr},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_iub_link({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_iub_link_attr([{startElement, {[], "p"},
		[{[], [], "name", Attr}]} | T], undefined, Acc) ->
	parse_iub_link_attr(T, Attr, Acc);
parse_iub_link_attr([{startElement, {_, "list"} = QName, _} | T1], undefined, Acc) ->
	% @todo
	{[_ | _BscOptions], T2} = pop(endElement, QName, T1),
	parse_iub_link_attr(T2, undefined, Acc);
parse_iub_link_attr([{characters, Chars} | T], "MinSCTPPortIub", Acc) ->
	parse_iub_link_attr(T, "MinSCTPPortIub",
			[#resource_char{name ="iubLinkATMChannelTerminationPoint",
			value = Chars} | Acc]);
parse_iub_link_attr([{characters, Chars} | T], "WBTSId", Acc) ->
	parse_iub_link_attr(T, "WBTSId",
			[#resource_char{name = "iubLinkNodeBFunction", value = Chars} | Acc]);
parse_iub_link_attr([{characters, Chars} | T], Attr, Acc) ->
	parse_iub_link_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_iub_link_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_iub_link_attr(T, undefined, Acc);
parse_iub_link_attr([], undefined, Acc) ->
	Acc.

%% @hidden
parse_enb({characters, SideId}, [#state{rule = RuleId,
		stack = [{startElement, {_, "p"}, [{[], [], "name", "name"}]} | _]} = State | T]) ->
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
parse_enb({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_enb({endElement, _Uri, "managedObject", QName},
		[#state{dn_prefix = [EnbDn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EnbAttr = parse_enb_attr(T2, undefined, []),
	ClassType = "ENBFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
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
			characteristic = [PeeParam | EnbAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_enb({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_enb_attr([{startElement, {[], "p"},
		[{[], [], "name", Attr}]} | T], undefined, Acc) ->
	parse_enb_attr(T, Attr, Acc);
parse_enb_attr([{startElement, {_, "list"} = QName, _} | T1], undefined, Acc) ->
	% @todo
	{[_ | _BscOptions], T2} = pop(endElement, QName, T1),
	parse_enb_attr(T2, undefined, Acc);
parse_enb_attr([{characters, Chars} | T], Attr, Acc) ->
	parse_enb_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_enb_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_enb_attr(T, undefined, Acc);
parse_enb_attr([], undefined, Acc) ->
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
