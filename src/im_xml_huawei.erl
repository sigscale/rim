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
-module(im_xml_huawei).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% export the im private API
-export([import/1, parse_mo/2, parse_gsm_mo/2, parse_gsm_function/2,
		parse_gsm_bts/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

-define(PathCatalogSchema, "/resourceCatalogManagement/v3/resourceCatalogManagement").
-define(PathInventorySchema, "/resourceInventoryManagement/v3/resourceInventoryManagement").

%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

-spec import(File) -> Result
	when
		File :: string(),
		Result :: ok | ignore | {error, Reason},
		Reason :: term().
%% @doc Import a file into the inventory table.
import(File) when is_list(File) ->
	Options = [{event_fun, fun parse_xml/3},
		{event_state, [#state{}]}],
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
		[{[], [], "className", "BSC6910GSMNE"} | _] = Attributes},
		[#state{dn_prefix = [], stack = Stack} = State | T]) ->
		[State#state{parse_module = ?MODULE, parse_function = parse_gsm_mo,
		dn_prefix = ["BSC6910GSMNE"],
		stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_mo({startElement, _, "MO", QName,
		[{[], [], "className", "BSC6900GSMNE"} | _] = Attributes},
		[#state{dn_prefix = [], stack = Stack} = State | T]) ->
		[State#state{parse_module = ?MODULE, parse_function = parse_gsm_mo,
		dn_prefix = ["BSC6900GSMNE"],
		stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_mo({startElement, _, "MO", QName,
		[{[], [], "className", "BSC6910UMTSNE"} | _] = Attributes},
		[#state{dn_prefix = [], stack = Stack} = State | T]) ->
		[State#state{parse_module = ?MODULE, parse_function = parse_gsm_mo,
		dn_prefix = ["BSC6900GSMNE"],
		stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_mo(_Event, [#state{parse_module = ?MODULE,
		parse_function = parse_mo} | _] = State) ->
	State.

%% @hidden
parse_gsm_mo({startElement, _Uri, _, QName,
		[{[], [], "className", "BSC6910GSMFunction"} | _] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",BSC6910GSMFunction",
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = ?MODULE, parse_function = parse_gsm_function,
			parse_state = #huawei_state{gsm_function = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gsm_mo({startElement, _Uri, _, QName,
		[{[], [], "className", "BSC6910GSMBTS"} | _] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",BSC6910GSMBTS",
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [DnComponent],
			parse_module = ?MODULE, parse_function = parse_gsm_bts,
			parse_state = #huawei_state{bts = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gsm_mo({startElement, _Uri, _, QName,
		[{[], [], "className", "BSC6900GSMFunction"} | _] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",BSC6900GSMFunction",
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = ?MODULE, parse_function = parse_gsm_function,
			parse_state = #huawei_state{gsm_function = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gsm_mo({startElement, _Uri, _, QName,
		[{[], [], "className", "BSC6900GSMBTS"} | _] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",BSC6900GSMBTS",
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [DnComponent],
			parse_module = ?MODULE, parse_function = parse_gsm_bts,
			parse_state = #huawei_state{bts = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gsm_mo(_Event, [#state{parse_module = ?MODULE,
	parse_function = parse_gsm_mo} | _] = State) ->
	State.

%% @hidden
parse_umts_mo({startElement, _Uri, _, QName,
		[{[], [], "className", "BSC6910UMTSFunction"} | _] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",BSC6910UMTSFunction",
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = ?MODULE, parse_function = parse_umts_function,
			parse_state = #huawei_state{umts_function = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_umts_mo({startElement, _Uri, _, QName,
		[{[], [], "className", "BSC6910UMTSNODEB"} | _] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",BSC6910UMTSNODEB",
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [DnComponent],
			parse_module = ?MODULE, parse_function = parse_umts_nodeb,
			parse_state = #huawei_state{nodeb = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_umts_mo(_Event, [#state{parse_module = ?MODULE,
	parse_function = parse_umts_mo} | _] = State) ->
	State.

%% @hidden
parse_gsm_function({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_gsm_function({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_gsm_function({endElement, _Uri, "MO", QName},
		[#state{dn_prefix = [GsmFunDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	GsmFunAttr = parse_gsm_function_attr(T2, undefined, []),
	ClassType = "BssFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = GsmFunDn,
			description = "GSM Base Station Subsystem (BSS)",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/BssFunction",
			specification = Spec,
			characteristic = [GsmFunAttr]},
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
parse_gsm_function_attr([{characters, Chars} | T], "fdn" = Attr, Acc) ->
	parse_gsm_function_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_function_attr([{characters, Chars} | T], "MOIndex" = Attr, Acc) ->
	parse_gsm_function_attr(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_gsm_function_attr([{characters, Chars} | T], "className" = Attr, Acc) ->
	parse_gsm_function_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_function_attr([{characters, Chars} | T], "name" = Attr, Acc) ->
	parse_gsm_function_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_function_attr([{characters, Chars} | T], "neID" = Attr, Acc) ->
	parse_gsm_function_attr(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_gsm_function_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_gsm_function_attr(T, undefined, Acc);
parse_gsm_function_attr([], undefined, Acc) ->
	Acc.

%% @hidden
parse_gsm_bts({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_gsm_bts({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_gsm_bts({endElement, _Uri, "MO", QName},
		[#state{dn_prefix = [BtsDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	GsmBtsAttr = parse_gsm_bts_attr(T2, undefined, []),
	ClassType = "BtsSiteMgr",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = BtsDn,
			description = "GSM Base Transceiver Station (BTS)",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/BtsSiteMgr",
			specification = Spec,
			characteristic = [GsmBtsAttr]},
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
parse_gsm_bts_attr([{characters, Chars} | T], "fdn" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "ABISBYPASSMODE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "ACTSTATUS" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "BTSDESC" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "BTSID" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "BTSNAME" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "BTSTYPE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "CURE1INPORTNUM" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "FLEXABISMODE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "IDTYPE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "IPPHYTRANSTYPE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "ISCONFIGEDRING" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "MAINPORTNO" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "MOIndex" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "MPMODE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "NEWNAME" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "SEPERATEMODE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "SERVICEMODE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "SRANMODE" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "className" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "name" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_gsm_bts_attr([{characters, Chars} | T], "neID" = Attr, Acc) ->
	parse_gsm_bts_attr(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_gsm_bts_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_gsm_bts_attr(T, undefined, Acc);
parse_gsm_bts_attr([], undefined, Acc) ->
	Acc.

%% @hidden
parse_umts_function({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_umts_function({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_umts_function({endElement, _Uri, "MO", QName},
		[#state{dn_prefix = [UmtsFunDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	UmtsFunAttr = parse_umts_function_attr(T2, undefined, []),
	ClassType = "RncFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = UmtsFunDn,
			description = "UMTS Radio Network Controller (RNC)",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/RncFunction",
			specification = Spec,
			characteristic = [UmtsFunAttr]},
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
parse_umts_function_attr([{characters, Chars} | T], "fdn" = Attr, Acc) ->
	parse_umts_function_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_umts_function_attr([{characters, Chars} | T], "MOIndex" = Attr, Acc) ->
	parse_umts_function_attr(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_umts_function_attr([{characters, Chars} | T], "className" = Attr, Acc) ->
	parse_umts_function_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_umts_function_attr([{characters, Chars} | T], "name" = Attr, Acc) ->
	parse_umts_function_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_umts_function_attr([{characters, Chars} | T], "neID" = Attr, Acc) ->
	parse_umts_function_attr(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_umts_function_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_umts_function_attr(T, undefined, Acc);
parse_umts_function_attr([], undefined, Acc) ->
	Acc.

%% @hidden
parse_umts_nodeb({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_umts_nodeb({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_umts_nodeb({endElement, _Uri, "MO", QName},
		[#state{dn_prefix = [UmtsNodebDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	UmtsNodebAttr = parse_umts_nodeb_attr(T2, undefined, []),
	ClassType = "NodeBFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = UmtsNodebDn,
			description = "UMTS Telecommunication Nodes",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/NodeBFunction",
			specification = Spec,
			characteristic = [UmtsNodebAttr]},
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
parse_umts_nodeb_attr([{characters, Chars} | T], "fdn" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_umts_nodeb_attr([{characters, Chars} | T], "CNOPINDEX" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_umts_nodeb_attr([{characters, Chars} | T], "HOSTTYPE" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_umts_nodeb_attr([{characters, Chars} | T], "LOGICRNCID" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_umts_nodeb_attr([{characters, Chars} | T], "MOIndex" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_umts_nodeb_attr([{characters, Chars} | T], "NODEBID" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_umts_nodeb_attr([{characters, Chars} | T], "NODEBNAME" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_umts_nodeb_attr([{characters, Chars} | T], "NODEBPROTCLVER" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_umts_nodeb_attr([{characters, Chars} | T], "NODEBTRACESWITCH" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_umts_nodeb_attr([{characters, Chars} | T], "RSCMNGMODE" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_umts_nodeb_attr([{characters, Chars} | T], "SATELLITEIND" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_umts_nodeb_attr([{characters, Chars} | T], "SHARINGTYPE" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_umts_nodeb_attr([{characters, Chars} | T], "TNLBEARERTYPE" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_umts_nodeb_attr([{characters, Chars} | T], "TRANSDELAY" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_umts_nodeb_attr([{characters, Chars} | T], "className" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_umts_nodeb_attr([{characters, Chars} | T], "name" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_umts_nodeb_attr([{characters, Chars} | T], "neID" = Attr, Acc) ->
	parse_umts_nodeb_attr(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_umts_nodeb_attr([{endElement, {[], _}} | T], _Attr, Acc) ->
	parse_umts_nodeb_attr(T, undefined, Acc);
parse_umts_nodeb_attr([], undefined, Acc) ->
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
