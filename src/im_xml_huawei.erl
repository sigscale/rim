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
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
-module(im_xml_huawei).
-copyright('Copyright (c) 2018 - 2024 SigScale Global Inc.').

%% export the im private API
-export([import/2, parse_mo/2,
		parse_core_mo/2, parse_msc_server/2, parse_mgw/2, parse_usn_function/2,
		parse_ugw_function/2, parse_cgpomu_function/2, parse_igwb_function/2,
		parse_uscdb_function/2, parse_spsv3_function/2,
		parse_msc_sig_point/2, parse_msc_server_office/2,
		parse_ran_me/2, parse_gsm_function/2, parse_gsm_bts/2, parse_gsm_gcell/2,
		parse_umts_function/2, parse_umts_nodeb/2, parse_umts_ucell/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

-define(PathCatalogSchema, "/resourceCatalogManagement/v4/schema").
-define(PathInventorySchema, "/resourceInventoryManagement/v4/schema").

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
parse_xml({startElement, _, "MOTree", _, _}, _Location,
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
parse_mo({startElement, _, "MO", QName,
		[{[], [], "className", ClassName}, {[], [], "fdn", DN}] = Attributes},
		[#state{rule = RuleId, stack = Stack} | _] = State)
		when ClassName == "BSC6910GSMNE"; ClassName == "BSC6900GSMNE";
		ClassName == "BSC6910UMTSNE" ->
		[#state{parse_module = ?MODULE, parse_function = parse_ran_me,
		dn_prefix = [DN], rule = RuleId,
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo({startElement, _Uri, _, QName,
		[{[], [], "className", ClassName}, {[], [], "fdn", DN}] = Attributes},
		[#state{location = Location} | _] = State) when
		ClassName == "BSC6910GSMFunction"; ClassName == "BSC6900GSMFunction" ->
	[#state{dn_prefix = [DN], location = Location,
			parse_module = ?MODULE, parse_function = parse_gsm_function,
			parse_state = #huawei_state{gsm_function = #{"id" => "BSC6910GSMFunction"}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_mo({startElement, _Uri, _, QName, [{[], [], "className", ClassName},
		{[], [], "fdn", DN}] = Attributes},
		[#state{rule = RuleId} | _] = State) when
		ClassName == "BSC6910GSMBTS"; ClassName == "BSC6900GSMBTS" ->
	[#state{dn_prefix = [DN], rule = RuleId,
			parse_module = ?MODULE, parse_function = parse_gsm_bts,
			parse_state = #huawei_state{bts = #{"id" => "BSC6910GSMBTS"}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_mo({startElement, _Uri, _, QName,
		[{[], [], "className", "BSC6910GSMGCELL"}, {[], [], "fdn", DN}] = Attributes},
		[#state{rule = Rule} | _] = State) ->
	[#state{dn_prefix = [DN], rule = Rule,
			parse_module = ?MODULE, parse_function = parse_gsm_gcell,
			parse_state = #huawei_state{gcell = #{"id" => "BSC6910GSMGCELL"}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_mo({startElement, _Uri, _, QName,
		[{[], [], "className", "BSC6910UMTSFunction"},
		{[], [], "fdn", DN}] = Attributes},
		[#state{location = Location} | _] = State) ->
	[#state{dn_prefix = [DN], location = Location,
			parse_module = ?MODULE, parse_function = parse_umts_function,
			parse_state = #huawei_state{umts_function = #{"id" => "BSC6910UMTSFunction"}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_mo({startElement, _Uri, _, QName,
		[{[], [], "className", "BSC6910UMTSNODEB"},
		{[], [], "fdn", DN}] = Attributes}, [#state{rule = RuleId} | _] = State) ->
	[#state{dn_prefix = [DN], rule = RuleId,
			parse_module = ?MODULE, parse_function = parse_umts_nodeb,
			stack = [{startElement, QName, Attributes}]} | State];
parse_mo({startElement, _Uri, _, QName,
		[{[], [], "className", "BSC6910UMTSUCELL"},
		{[], [], "fdn", DN}] = Attributes}, [#state{rule = RuleId} | _] = State) ->
	[#state{dn_prefix = [DN], rule = RuleId,
			parse_module = ?MODULE, parse_function = parse_umts_ucell,
			stack = [{startElement, QName, Attributes}]} | State];
parse_mo({startElement, _, "MO", QName,
		[{[], [], "className", ClassName}, {[], [], "fdn", DN}] = Attributes},
		[#state{dn_prefix = [], rule = RuleId, stack = Stack} | _] = State)
		when ClassName == "MSCServerNE"; ClassName == "MGWNE";
		ClassName == "CGPOMUNE"; ClassName == "iGWBNE";
		ClassName == "UGWNE"; ClassName == "USNNE";
		ClassName == "USCDBNE"; ClassName == "SPSV3NE" ->
		[#state{parse_module = ?MODULE, parse_function = parse_core_mo,
		rule = RuleId, dn_prefix = [DN],
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo({startElement, _, "MO", QName,
		[{[], [], "className", "MSCServerFunction"},
		{[], [], "fdn", DN}] = Attributes},
		[#state{dn_prefix = [], stack = Stack, location = Location} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_msc_server,
		location = Location, dn_prefix = [DN],
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo({startElement, _, "MO", QName,
		[{[], [], "className", "MgwFunction"},
		{[], [], "fdn", DN}] = Attributes},
		[#state{dn_prefix = [], stack = Stack, location = Location} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_mgw,
		location = Location, dn_prefix = [DN],
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo({startElement, _, "MO", QName,
		[{[], [], "className", "USNFunction"},
		{[], [], "fdn", DN}] = Attributes},
		[#state{dn_prefix = [], stack = Stack, location = Location} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_usn_function,
		location = Location, dn_prefix = [DN],
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo({startElement, _, "MO", QName,
		[{[], [], "className", "UGWFunction"},
		{[], [], "fdn", DN}] = Attributes},
		[#state{dn_prefix = [], stack = Stack, location = Location} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_ugw_function,
		location = Location, dn_prefix = [DN],
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo({startElement, _, "MO", QName,
		[{[], [], "className", "CGPOMUFunction"},
		{[], [], "fdn", DN}] = Attributes},
		[#state{dn_prefix = [], stack = Stack, location = Location} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_cgpomu_function,
		location = Location, dn_prefix = [DN],
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo({startElement, _, "MO", QName,
		[{[], [], "className", "iGWBFunction"},
		{[], [], "fdn", DN}] = Attributes},
		[#state{dn_prefix = [], stack = Stack, location = Location} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_igwb_function,
		location = Location, dn_prefix = [DN],
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo({startElement, _, "MO", QName,
		[{[], [], "className", "USCDBFunction"},
		{[], [], "fdn", DN}] = Attributes},
		[#state{dn_prefix = [], stack = Stack, location = Location} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_uscdb_function,
		location = Location, dn_prefix = [DN],
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo({startElement, _, "MO", QName,
		[{[], [], "className", "SPSV3Function"},
		{[], [], "fdn", DN}] = Attributes},
		[#state{dn_prefix = [], stack = Stack, location = Location} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_spsv3_function,
		location = Location, dn_prefix = [DN],
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo({startElement, _, "MO", QName,
		[{[], [], "className", "MSCServerIntraOfiSigPoint"},
		{[], [], "fdn", DN}] = Attributes},
		[#state{dn_prefix = [], stack = Stack, location = Location} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_msc_sig_point,
		location = Location, dn_prefix = [DN],
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo({startElement, _, "MO", QName,
		[{[], [], "className", "MSCServerOffice"},
		{[], [], "fdn", DN}] = Attributes},
		[#state{dn_prefix = [], stack = Stack, location = Location} | _] = State) ->
		[#state{parse_module = ?MODULE, parse_function = parse_msc_server_office,
		location = Location, dn_prefix = [DN],
		stack = [{startElement, QName, Attributes} | Stack]} | State];
parse_mo(_Event, [#state{parse_module = ?MODULE,
		parse_function = parse_mo} | _] = State) ->
	State.

%% @hidden
parse_core_mo({characters, DN}, [#state{rule = RuleId, stack = [{startElement,
		{_, "attr"}, [{[], [], "name", "name"}]} | _] = Stack} = State | T]) ->
	case im:get_pee(RuleId, DN) of
		{ok, []} ->
			[State#state{stack = [{characters, DN} | Stack]} | T];
		{ok, PEEMonitoredEntities} ->
			PeeParametersList =
					parse_peeParameterslist(PEEMonitoredEntities, []),
			[State#state{location = PeeParametersList,
					stack = [{characters, DN} | Stack]} | T];
		{error, _Reason} ->
			[State | T]
	end;
parse_core_mo({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_core_mo({startElement, _, "MO", _QName, _Attributes},
		[#state{dn_prefix = [MeDn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, {[], "MO"}, Stack),
	MeAttr = parse_core_mo_attr(T2, undefined, []),
	ClassType = "ManagedElement",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = MeDn,
			description = "",
			category = "",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/ManagedElement",
			specification = Spec,
			characteristic = [PeeParam | MeAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					location = Location} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_core_mo({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_core_mo({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_msc_server({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_msc_server({startElement, _, "MO", _QName, _Attributes},
		[#state{dn_prefix = [MscDn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, {[], "MO"}, Stack),
	MscServerAttr = parse_core_mo_attr(T2, undefined, []),
	ClassType = "MscServerFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = MscDn,
			description = "Mobile Switch Center (MSC) Server",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/MscServerFunction",
			specification = Spec,
			characteristic = [PeeParam | MscServerAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					location = Location} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_msc_server({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_msc_server({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_mgw({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_mgw({startElement, _, "MO", _QName, _Attributes},
		[#state{dn_prefix = [MgwDn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, {[], "MO"}, Stack),
	MgwAttr = parse_core_mo_attr(T2, undefined, []),
	ClassType = "CsMgwFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = MgwDn,
			description = "Circuit switched Media Gateway",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/CsMgwFunction",
			specification = Spec,
			characteristic = [PeeParam | MgwAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					location = Location} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_mgw({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_mgw({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_usn_function({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_usn_function({startElement, _, "MO", _QName, _Attributes},
		[#state{dn_prefix = [UsnDn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, {[], "MO"}, Stack),
	UsnAttr = parse_core_mo_attr(T2, undefined, []),
	ClassType = "USNFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = UsnDn,
			description = "Circuit switched Media Gateway",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/CsMgwFunction",
			specification = Spec,
			characteristic = [PeeParam | UsnAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					location = Location} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_usn_function({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_usn_function({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ugw_function({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ugw_function({startElement, _, "MO", _QName, _Attributes},
		[#state{dn_prefix = [UgwDn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, {[], "MO"}, Stack),
	UgwAttr = parse_core_mo_attr(T2, undefined, []),
	ClassType = "UGWFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = UgwDn,
			description = "Circuit switched Media Gateway",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/CsMgwFunction",
			specification = Spec,
			characteristic = [PeeParam | UgwAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					location = Location} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ugw_function({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ugw_function({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_cgpomu_function({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_cgpomu_function({startElement, _, "MO", _QName, _Attributes},
		[#state{dn_prefix = [CgpomuDn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, {[], "MO"}, Stack),
	CgpomuAttr = parse_core_mo_attr(T2, undefined, []),
	ClassType = "CGPOMUFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = CgpomuDn,
			description = "Carrier Grade Platform Operation and Management Unit",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/CGPOMUFunction",
			specification = Spec,
			characteristic = [PeeParam | CgpomuAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					location = Location} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_cgpomu_function({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_cgpomu_function({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_igwb_function({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_igwb_function({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_igwb_function({endElement, _, "MO", QName},
		[#state{dn_prefix = [IgwbDn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	IgwbAttr = parse_core_mo_attr(T2, undefined, []),
	ClassType = "iGWBFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = IgwbDn,
			description = "Carrier Grade Platform Operation and Management Unit",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/iGWBFunction",
			specification = Spec,
			characteristic = [PeeParam | IgwbAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					location = Location} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_igwb_function({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_uscdb_function({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_uscdb_function({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_uscdb_function({endElement, _, "MO", QName},
		[#state{dn_prefix = [UscdbDn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	UscdbAttr = parse_core_mo_attr(T2, undefined, []),
	ClassType = "USCDBFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = UscdbDn,
			description = "",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/USCDBFunction",
			specification = Spec,
			characteristic = [PeeParam | UscdbAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					location = Location} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_uscdb_function({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_spsv3_function({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_spsv3_function({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_spsv3_function({endElement, _, "MO", QName},
		[#state{dn_prefix = [Spsv3Dn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	Spsv3Attr = parse_core_mo_attr(T2, undefined, []),
	ClassType = "SPSV3Function",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = Spsv3Dn,
			description = "",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/SPSV3Function",
			specification = Spec,
			characteristic = [PeeParam | Spsv3Attr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					location = Location} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_spsv3_function({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_msc_sig_point({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_msc_sig_point({startElement, _, "MO", _QName, _Attributes},
		[#state{dn_prefix = [MscSigPointDn | _], location = Location,
		stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, {[], "MO"}, Stack),
	MscSigPointAttr = parse_core_mo_attr(T2, undefined, []),
	ClassType = "MSCServerIntraOfiSigPoint",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = MscSigPointDn,
			description = "",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/MSCServerIntraOfiSigPoint",
			specification = Spec,
			characteristic = [PeeParam | MscSigPointAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					location = Location} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_msc_sig_point({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_msc_sig_point({endElement, _, "MO", _QName},
		[#state{dn_prefix = [MscSigPointDn | _], location = Location,
		stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, {[], "MO"}, Stack),
	MscSigPointAttr = parse_core_mo_attr(T2, undefined, []),
	ClassType = "MSCServerIntraOfiSigPoint",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = MscSigPointDn,
			description = "",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/MSCServerIntraOfiSigPoint",
			specification = Spec,
			characteristic = [PeeParam | MscSigPointAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					location = Location} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_msc_sig_point({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_msc_server_office({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_msc_server_office({startElement, _, "MO", _QName, _Attributes},
		[#state{dn_prefix = [MscServerOfficeDn | _], location = Location,
		stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, {[], "MO"}, Stack),
	MscServerOfficeAttr = parse_core_mo_attr(T2, undefined, []),
	ClassType = "MSCServerOffice",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = MscServerOfficeDn,
			description = "",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/MSCServerOffice",
			specification = Spec,
			characteristic = [PeeParam | MscServerOfficeAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					location = Location} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_msc_server_office({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_msc_server_office({endElement, _, "MO", _QName},
		[#state{dn_prefix = [MscServerOfficeDn | _], location = Location,
		stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, {[], "MO"}, Stack),
	MscServerOfficeAttr = parse_core_mo_attr(T2, undefined, []),
	ClassType = "MSCServerOffice",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = MscServerOfficeDn,
			description = "",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/MSCServerOffice",
			specification = Spec,
			characteristic = [PeeParam | MscServerOfficeAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					location = Location} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_msc_server_office({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_core_mo_attr([{startElement, {[], "attr"},
		[{[], [], "name", Attr}]} | T], undefined, Acc) ->
	parse_core_mo_attr(T, Attr, Acc);
parse_core_mo_attr([{characters, Chars} | T], Attr, Acc) ->
	parse_core_mo_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_core_mo_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_core_mo_attr(T, undefined, Acc);
parse_core_mo_attr([], undefined, Acc) ->
	Acc.

%% @hidden
parse_ran_me({characters, SiteId}, [#state{rule = RuleId, stack = [{startElement,
		{_, "attr"}, [{[], [], "name", "name"}]} | _] = Stack} = State | T]) ->
	case im:get_pee(RuleId, SiteId) of
		{ok, []} ->
			[State#state{stack = [{characters, SiteId} | Stack]} | T];
		{ok, PEEMonitoredEntities} ->
			PeeParametersList =
					parse_peeParameterslist(PEEMonitoredEntities, []),
			[State#state{location = PeeParametersList,
					stack = [{characters, SiteId} | Stack]} | T];
		{error, _Reason} ->
			[State | T]
	end;
parse_ran_me({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ran_me({startElement, _, "MO", _QName, _Attributes} = Event,
		[#state{parse_module = M, parse_function = F,
		dn_prefix = [MeDn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, {[], "MO"}, Stack),
	MeAttr = parse_ran_me_attr(T2, undefined, []),
	ClassType = "ManagedElement",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = MeDn,
			description = "",
			category = "",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/ManagedElement",
			specification = Spec,
			characteristic = [PeeParam | MeAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			M:F(Event, [PrevState#state{spec_cache = [NewCache | PrevCache],
					location = Location} | T1]);
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ran_me({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ran_me({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_ran_me_attr([{startElement, {[], "attr"},
		[{[], [], "name", Attr}]} | T], undefined, Acc) ->
	parse_ran_me_attr(T, Attr, Acc);
parse_ran_me_attr([{characters, Chars} | T], "name" = Attr, Acc) ->
	parse_ran_me_attr(T, Attr,
			[#resource_char{name = "userLabel", value = Chars} | Acc]);
parse_ran_me_attr([{characters, Chars} | T], Attr, Acc) ->
	parse_ran_me_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_ran_me_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_ran_me_attr(T, undefined, Acc);
parse_ran_me_attr([], undefined, Acc) ->
	Acc.

%% @hidden
parse_gsm_function({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_gsm_function({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_gsm_function({endElement, _Uri, "MO", QName},
		[#state{dn_prefix = [GsmFunDn | _], stack = Stack, location = Location,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	GsmFunAttr = parse_gsm_function_attr(T2, undefined, []),
	ClassType = "BssFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = GsmFunDn,
			description = "GSM Base Station Subsystem (BSS)",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/BssFunction",
			specification = Spec,
			characteristic = [PeeParam | GsmFunAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_gsm_function({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_gsm_function_attr([{startElement, {[], "attr"},
		[{[], [], "name", Attr}]} | T], undefined, Acc) ->
	parse_gsm_function_attr(T, Attr, Acc);
parse_gsm_function_attr([{characters, _Chars} | T], "fdn" = Attr, Acc) ->
	parse_gsm_function_attr(T, Attr, Acc);
parse_gsm_function_attr([{characters, _Chars} | T], "MOIndex" = Attr, Acc) ->
	parse_gsm_function_attr(T, Attr, Acc);
parse_gsm_function_attr([{characters, _Chars} | T], "className" = Attr, Acc) ->
	parse_gsm_function_attr(T, Attr, Acc);
parse_gsm_function_attr([{characters, Chars} | T], "name" = Attr, Acc) ->
	parse_gsm_function_attr(T, Attr,
			[#resource_char{name = "userLabel", value = Chars} | Acc]);
parse_gsm_function_attr([{characters, _Chars} | T], "neID" = Attr, Acc) ->
	parse_gsm_function_attr(T, Attr, Acc);
parse_gsm_function_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_gsm_function_attr(T, undefined, Acc);
parse_gsm_function_attr([], undefined, Acc) ->
	Acc.

%% @hidden
parse_gsm_bts({characters, SiteId}, [#state{rule = RuleId, stack = [{startElement,
		{_, "attr"}, [{[], [], "name", "name"}]} | _] = Stack} = State | T]) ->
	case im:get_pee(RuleId, SiteId) of
		{ok, []} ->
			[State#state{stack = [{characters, SiteId} | Stack]} | T];
		{ok, PEEMonitoredEntities} ->
			PeeParametersList =
					parse_peeParameterslist(PEEMonitoredEntities, []),
			[State#state{location = PeeParametersList,
					stack = [{characters, SiteId} | Stack]} | T];
		{error, _Reason} ->
			[State | T]
	end;
parse_gsm_bts({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_gsm_bts({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_gsm_bts({endElement, _Uri, "MO", QName},
		[#state{dn_prefix = [BtsDn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	GsmBtsAttr = parse_gsm_bts_attr(T2, undefined, []),
	ClassType = "BtsSiteMgr",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = BtsDn,
			description = "GSM Base Transceiver Station (BTS)",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/BtsSiteMgr",
			specification = Spec,
			characteristic = [PeeParam | GsmBtsAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_gsm_bts({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_gsm_bts_attr([{startElement, {[], "attr"},
		[{[], [], "name", Attr}]} | T], undefined, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "fdn" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "ABISBYPASSMODE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "ACTSTATUS" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "BTSDESC" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "BTSID" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "BTSNAME" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "BTSTYPE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "CURE1INPORTNUM" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "FLEXABISMODE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "IDTYPE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "IPPHYTRANSTYPE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "ISCONFIGEDRING" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "MAINPORTNO" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "MOIndex" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "MPMODE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "NEWNAME" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "SEPERATEMODE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "SERVICEMODE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "SRANMODE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, _Chars} | T], "className" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{characters, Chars} | T], "name" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = "userLabel", value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, _Chars} | T], "neID" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr, Acc);
parse_gsm_bts_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_gsm_bts_attr(T, undefined, Acc);
parse_gsm_bts_attr([], undefined, Acc) ->
	Acc.

%% @hidden
parse_gsm_gcell({characters, SiteId}, [#state{rule = RuleId, stack = [{startElement,
		{_, "attr"}, [{[], [], "name", "CELLNAME"}]} | _] = Stack} = State | T]) ->
	case im:get_pee(RuleId, SiteId) of
		{ok, []} ->
			[State#state{stack = [{characters, SiteId} | Stack]} | T];
		{ok, PEEMonitoredEntities} ->
			PeeParametersList =
					parse_peeParameterslist(PEEMonitoredEntities, []),
			[State#state{location = PeeParametersList,
					stack = [{characters, SiteId} | Stack]} | T];
		{error, _Reason} ->
			[State | T]
	end;
parse_gsm_gcell({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_gsm_gcell({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_gsm_gcell({endElement, _Uri, "MO", QName},
		[#state{dn_prefix = [GCellDn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	GCellAttr = parse_gsm_gcell_attr(T2, undefined, []),
	ClassType = "GsmCell",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = GCellDn,
			description = "GSM Radio",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/GsmCell",
			specification = Spec,
			characteristic = [PeeParam | GCellAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_gsm_gcell({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_gsm_gcell_attr([{startElement, {[], "attr"},
		[{[], [], "name", Attr}]} | T], undefined, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "fdn" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "ACTSTATUS" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, Chars} | T], "BCC" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr,
			[#resource_char{name = "bcc", value = list_to_integer(Chars)} | Acc]);
parse_gsm_gcell_attr([{characters, _Chars} | T], "BCCHNO" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "BSPAGBLKSRES" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "BSPBCCHBLKS" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "BSPRACHBLKS" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "BTSID" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, Chars} | T], "CELLID" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr,
			[#resource_char{name = "cellIdentity", value = list_to_integer(Chars)} | Acc]);
parse_gsm_gcell_attr([{characters, _Chars} | T], "CELLINEXTP" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "CELLNAME" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "CI" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "CSDSP" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "CSVSP" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "DBFREQBCCHIUO" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "ENIUO" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "DBLFREQADJCID" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "DBLFREQADJIDTYPE" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "EXTTP" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "FLEXMAIO" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "IUOTP" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, Chars} | T], "LAC" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr,
			[#resource_char{name = "lac", value = list_to_integer(Chars)} | Acc]);
parse_gsm_gcell_attr([{characters, Chars} | T], "MCC" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr,
			[#resource_char{name = "mcc", value = list_to_integer(Chars)} | Acc]);
parse_gsm_gcell_attr([{characters, Chars} | T], "MNC" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr,
			[#resource_char{name = "mnc", value = list_to_integer(Chars)} | Acc]);
parse_gsm_gcell_attr([{characters, _Chars} | T], "MOIndex" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, Chars} | T], "NCC" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr,
			[#resource_char{name = "ncc", value = list_to_integer(Chars)} | Acc]);
parse_gsm_gcell_attr([{characters, _Chars} | T], "OPNAME" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "PSHPSP" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "PSLPSVP" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "REMARK" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "TYPE" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, _Chars} | T], "className" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{characters, Chars} | T], "name" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr,
			[#resource_char{name = "userLabel", value = Chars} | Acc]);
parse_gsm_gcell_attr([{characters, _Chars} | T], "neID" = Attr, Acc) ->
	parse_gsm_gcell_attr(T, Attr, Acc);
parse_gsm_gcell_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_gsm_gcell_attr(T, undefined, Acc);
parse_gsm_gcell_attr([], undefined, Acc) ->
	Acc.

%% @hidden
parse_umts_function({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_umts_function({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_umts_function({endElement, _Uri, "MO", QName},
		[#state{dn_prefix = [UmtsFunDn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	UmtsFunAttr = parse_umts_function_attr(T2, undefined, []),
	ClassType = "RncFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = UmtsFunDn,
			description = "UMTS Radio Network Controller (RNC)",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/RncFunction",
			specification = Spec,
			characteristic = [PeeParam | UmtsFunAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_umts_function({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].


% @hidden
parse_umts_function_attr([{startElement, {[], "attr"},
		[{[], [], "name", Attr}]} | T], undefined, Acc) ->
	parse_umts_function_attr(T, Attr, Acc);
parse_umts_function_attr([{characters, _Chars} | T], "fdn" = Attr, Acc) ->
	parse_umts_function_attr(T, Attr, Acc);
parse_umts_function_attr([{characters, _Chars} | T], "MOIndex" = Attr, Acc) ->
	parse_umts_function_attr(T, Attr, Acc);
parse_umts_function_attr([{characters, _Chars} | T], "className" = Attr, Acc) ->
	parse_umts_function_attr(T, Attr, Acc);
parse_umts_function_attr([{characters, Chars} | T], "name" = Attr, Acc) ->
	parse_umts_function_attr(T, Attr,
			[#resource_char{name = "userLabel", value = Chars} | Acc]);
parse_umts_function_attr([{characters, _Chars} | T], "neID" = Attr, Acc) ->
	parse_umts_function_attr(T, Attr, Acc);
parse_umts_function_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_umts_function_attr(T, undefined, Acc);
parse_umts_function_attr([], undefined, Acc) ->
	Acc.

%% @hidden
parse_umts_nodeb({characters, SiteId}, [#state{rule = RuleId, stack = [{startElement,
		{_, "attr"}, [{[], [], "name", "NODEBNAME"}]} | _] = Stack} = State | T]) ->
	case im:get_pee(RuleId, SiteId) of
		{ok, []} ->
			[State#state{stack = [{characters, SiteId} | Stack]} | T];
		{ok, PEEMonitoredEntities} ->
			PeeParametersList =
					parse_peeParameterslist(PEEMonitoredEntities, []),
			[State#state{location = PeeParametersList,
					stack = [{characters, SiteId} | Stack]} | T];
		{error, _Reason} ->
			[State | T]
	end;
parse_umts_nodeb({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_umts_nodeb({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_umts_nodeb({endElement, _Uri, "MO", QName},
		[#state{dn_prefix = [UmtsNodebDn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	UmtsNodebAttr = parse_umts_nodeb_attr(T2, undefined, []),
	ClassType = "NodeBFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = UmtsNodebDn,
			description = "UMTS NodeB",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/NodeBFunction",
			specification = Spec,
			characteristic = [PeeParam | UmtsNodebAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_umts_nodeb({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_umts_nodeb_attr([{startElement, {[], "attr"},
		[{[], [], "name", Attr}]} | T], undefined, Acc) ->
	parse_umts_nodeb_attr(T, Attr, Acc);
parse_umts_nodeb_attr([{characters, _Chars} | T], "fdn" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr, Acc);
parse_umts_nodeb_attr([{characters, _Chars} | T], "CNOPINDEX" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr, Acc);
parse_umts_nodeb_attr([{characters, _Chars} | T], "HOSTTYPE" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr, Acc);
parse_umts_nodeb_attr([{characters, _Chars} | T], "LOGICRNCID" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr, Acc);
parse_umts_nodeb_attr([{characters, _Chars} | T], "MOIndex" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr, Acc);
parse_umts_nodeb_attr([{characters, _Chars} | T], "NODEBID" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr, Acc);
parse_umts_nodeb_attr([{characters, _Chars} | T], "NODEBNAME" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr, Acc);
parse_umts_nodeb_attr([{characters, _Chars} | T], "NODEBPROTCLVER" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr, Acc);
parse_umts_nodeb_attr([{characters, _Chars} | T], "NODEBTRACESWITCH" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr, Acc);
parse_umts_nodeb_attr([{characters, _Chars} | T], "RSCMNGMODE" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr, Acc);
parse_umts_nodeb_attr([{characters, _Chars} | T], "SATELLITEIND" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr, Acc);
parse_umts_nodeb_attr([{characters, _Chars} | T], "SHARINGTYPE" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr, Acc);
parse_umts_nodeb_attr([{characters, _Chars} | T], "TNLBEARERTYPE" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr, Acc);
parse_umts_nodeb_attr([{characters, _Chars} | T], "TRANSDELAY" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr, Acc);
parse_umts_nodeb_attr([{characters, _Chars} | T], "className" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr, Acc);
parse_umts_nodeb_attr([{characters, Chars} | T], "name" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = "userLabel", value = Chars} | Acc]);
parse_umts_nodeb_attr([{characters, _Chars} | T], "neID" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr, Acc);
parse_umts_nodeb_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_umts_nodeb_attr(T, undefined, Acc);
parse_umts_nodeb_attr([], undefined, Acc) ->
	Acc.

%% @hidden
parse_umts_ucell({characters, SiteId}, [#state{rule = RuleId, stack = [{startElement,
		{_, "attr"}, [{[], [], "name", "CELLNAME"}]} | _] = Stack} = State | T]) ->
	case im:get_pee(RuleId, SiteId) of
		{ok, []} ->
			[State#state{stack = [{characters, SiteId} | Stack]} | T];
		{ok, PEEMonitoredEntities} ->
			PeeParametersList =
					parse_peeParameterslist(PEEMonitoredEntities, []),
			[State#state{location = PeeParametersList,
					stack = [{characters, SiteId} | Stack]} | T];
		{error, _Reason} ->
			[State | T]
	end;
parse_umts_ucell({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_umts_ucell({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_umts_ucell({endElement, _Uri, "MO", QName},
		[#state{dn_prefix = [UmtsUCellDn | _], location = Location, stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	UmtsUCellAttr = parse_umts_ucell_attr(T2, undefined, []),
	ClassType = "UtranCellFDD",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = UmtsUCellDn,
			description = "UMTS Frequency Division Duplex (FDD) Radio Cell",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/UtranCellFDD",
			specification = Spec,
			characteristic = [PeeParam | UmtsUCellAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_umts_ucell({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_umts_ucell_attr([{startElement, {[], "attr"},
		[{[], [], "name", Attr}]} | T], undefined, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "fdn" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "ACTSTATUS" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "BANDIND" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "BLKSTATUS" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "CELLID" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "CELLNAME" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "CFGRACIND" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "CIO" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "CNOPGRPINDEX" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "DLPOWERAVERAGEWINDOWSIZE" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "DLTPCPATTERN01COUNT" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "IPDLFLAG" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, Chars} | T], "LAC" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr,
			[#resource_char{name = "lac", value = list_to_integer(Chars)} | Acc]);
parse_umts_ucell_attr([{characters, _Chars} | T], "LOCELL" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "LOGICRNCID" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, Chars} | T], "MAXTXPOWER" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr,
			[#resource_char{name = "maximumAllowedUlTxPower", value = list_to_integer(Chars)} | Acc]);
parse_umts_ucell_attr([{characters, _Chars} | T], "MBMSACTFLG" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "MIMOACTFLAG" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "MOIndex" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "NINSYNCIND" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "NODEBNAME" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "NODESYNSWITCH" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "NOUTSYNCIND" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "POWERRAISELIMIT" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "PSCRAMBCODE" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, Chars} | T], "RAC" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr,
			[#resource_char{name = "rac", value = list_to_integer(Chars)} | Acc]);
parse_umts_ucell_attr([{characters, _Chars} | T], "REMARK" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, Chars} | T], "SAC" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr,
			[#resource_char{name = "sac", value = list_to_integer(Chars)} | Acc]);
parse_umts_ucell_attr([{characters, _Chars} | T], "SPGID" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "SUPBMC" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "TCELL" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "TRLFAILURE" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, Chars} | T], "TXDIVERSITYIND" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr,
			[#resource_char{name = "txDiversityIndicator", value = Chars} | Acc]);
parse_umts_ucell_attr([{characters, Chars} | T], "UARFCNDOWNLINK" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr,
			[#resource_char{name = "uarfcnDl", value = list_to_integer(Chars)} | Acc]);
parse_umts_ucell_attr([{characters, Chars} | T], "UARFCNUPLINK" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr,
			[#resource_char{name = "uarfcnUl", value = list_to_integer(Chars)} | Acc]);
parse_umts_ucell_attr([{characters, _Chars} | T], "UARFCNUPLINKIND" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "VPLIMITIND" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, _Chars} | T], "className" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{characters, Chars} | T], "name" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr,
			[#resource_char{name = "userLabel", value = Chars} | Acc]);
parse_umts_ucell_attr([{characters, _Chars} | T], "neID" = Attr, Acc) ->
	parse_umts_ucell_attr(T, Attr, Acc);
parse_umts_ucell_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_umts_ucell_attr(T, undefined, Acc);
parse_umts_ucell_attr([], undefined, Acc) ->
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
				{ok, #specification{id = Id, href = Href,
						name = Name, class_type = Type, version = Version}} ->
					SpecRef = #specification_ref{id = Id, href = Href,
							name = Name, ref_type = Type, version = Version},
					{SpecRef, [SpecRef | Cache]};
				{error, Reason} ->
					throw({get_specification_name, Reason})
			end
	end.
