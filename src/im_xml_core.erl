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
-module(im_xml_core).
-copyright('Copyright (c) 2018 - 2023 SigScale Global Inc.').

%% export the im private API
-export([parse_msc/2, parse_iucs/2, parse_alink/2, parse_mgw/2, parse_ggsn/2,
		parse_sgsn/2, parse_iups/2, parse_gb_link/2,
		parse_auc/2, parse_hlr/2, parse_eir/2, parse_mnp_srf/2, parse_cgf/2,
		parse_sgw/2, parse_cbc/2, parse_iubc/2]).

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
parse_msc({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_msc({startElement,  _Uri, "IucsLink", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",IucsLink=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_core, parse_function = parse_iucs,
			parse_state = #core_state{iucs_link = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_msc({startElement,  _Uri, "ALink", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",ALink=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_core, parse_function = parse_alink,
			parse_state = #core_state{a_link = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_msc({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_msc({endElement, _Uri, "MscServerFunction", QName},
		[#state{parse_state = #core_state{iucs_links = IucsLinks,
		a_links = ALinks}, dn_prefix = [MscDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	MscAttr = parse_msc_attr(T2, undefined, []),
	ClassType = "MscServerFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = MscDn,
			description = "Mobile Switch Center (MSC) Server",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/MscServerFunction",
			specification = Spec,
			characteristic = MscAttr,
			related = IucsLinks ++ ALinks},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_msc({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_msc_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_msc_attr1(Attributes, undefined, Acc).
% @hidden
parse_msc_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _vnfParametersList], T2} = pop(startElement, QName, T1),
	parse_msc_attr1(T2, undefined, Acc);
parse_msc_attr1([{endElement, {_, "mccList"} = QName} | T1], undefined, Acc) ->
	% @todo longList
	{[_ | _mccList], T2} = pop(startElement, QName, T1),
	parse_msc_attr1(T2, undefined, Acc);
parse_msc_attr1([{endElement, {_, "mncList"} = QName} | T1], undefined, Acc) ->
	% @todo longList
	{[_ | _mncList], T2} = pop(startElement, QName, T1),
	parse_msc_attr1(T2, undefined, Acc);
parse_msc_attr1([{endElement, {_, "lacList"} = QName} | T1], undefined, Acc) ->
	% @todo longList
	{[_ | _lacList], T2} = pop(startElement, QName, T1),
	parse_msc_attr1(T2, undefined, Acc);
parse_msc_attr1([{endElement, {_, "sacList"} = QName} | T1], undefined, Acc) ->
	% @todo longList
	{[_ | _sacList], T2} = pop(startElement, QName, T1),
	parse_msc_attr1(T2, undefined, Acc);
parse_msc_attr1([{endElement, {_, "gcaList"} = QName} | T1], undefined, Acc) ->
	% @todo longList
	{[_ | _gcaList], T2} = pop(startElement, QName, T1),
	parse_msc_attr1(T2, undefined, Acc);
parse_msc_attr1([{endElement, {_, "mscServerFunctionGsmCell"} = QName} | T1],
		undefined, Acc) ->
	% @todo dnList
	{[_ | _mscServerFunctionGsmCell], T2} = pop(startElement, QName, T1),
	parse_msc_attr1(T2, undefined, Acc);
parse_msc_attr1([{endElement,
		{_, "mscServerFunctionExternalGsmCell"} = QName} | T1], undefined, Acc) ->
	% @todo dnList
	{[_ | _MscsFunction], T2} = pop(startElement, QName, T1),
	parse_msc_attr1(T2, undefined, Acc);
parse_msc_attr1([{endElement,
		{_, "mscServerFunctionCsMgwFunction"} = QName} | T1], undefined, Acc) ->
	% @todo dnList
	{[_ | _MscsfcsmgwFunction], T2} = pop(startElement, QName, T1),
	parse_msc_attr1(T2, undefined, Acc);
parse_msc_attr1([{endElement, {_, "mscServerFunctionMscPool"} = QName} | T1],
		undefined, Acc) ->
	% @todo dnList
	{[_ | _MscsfmscPool], T2} = pop(startElement, QName, T1),
	parse_msc_attr1(T2, undefined, Acc);
parse_msc_attr1([{endElement, {_, "nriList"} = QName} | T1], undefined, Acc) ->
	% @todo longList
	{[_ | _NriList], T2} = pop(startElement, QName, T1),
	parse_msc_attr1(T2, undefined, Acc);
parse_msc_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_msc_attr1(T, Attr, Acc);
parse_msc_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_msc_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_msc_attr1([{characters, Chars} | T], "mscId" = Attr, Acc) ->
	parse_msc_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_msc_attr1([{characters, "1"} | T], "defaultMscType" = Attr, Acc) ->
	parse_msc_attr1(T, Attr,
			[#resource_char{name = Attr, value = 1} | Acc]);
parse_msc_attr1([{characters, "0"} | T], "defaultMscType" = Attr, Acc) ->
	parse_msc_attr1(T, Attr,
			[#resource_char{name = Attr, value = 0} | Acc]);
parse_msc_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_msc_attr1(T, undefined, Acc);
parse_msc_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_iucs({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_iucs({startElement, _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_iucs({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_iucs({endElement, _Uri, "IucsLink", QName},
		[#state{dn_prefix = [IucsDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache,
		parse_state = CoreState} = PrevState | T1]) ->
	#core_state{iucs_links = IucsRels} = CoreState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	IucsAttr = parse_link_attr(T2, undefined, []),
	ClassType = "IucsLink",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = IucsDn,
			description = "Core Iu-cs interface link",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/IucsLink",
			specification = Spec,
			characteristic = IucsAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			IucsRel = #resource_rel{id = Id, name = IucsDn,rel_type = "contains",
					ref_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					parse_state = CoreState#core_state{iucs_links
					= [IucsRel | IucsRels]}} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_iucs({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_alink({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_alink({startElement, _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_alink({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_alink({endElement, _Uri, "ALink", QName},
		[#state{dn_prefix = [ALinkDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache,
		parse_state = CoreState} = PrevState | T1]) ->
	#core_state{a_links = ALinkRels} = CoreState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	ALinkAttr = parse_link_attr(T2, undefined, []),
	ClassType = "ALink",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = ALinkDn,
			description = "Core A interface link",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/ALink",
			specification = Spec,
			characteristic = ALinkAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			ALinkRel = #resource_rel{id = Id, name = ALinkDn,rel_type = "contains",
					ref_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					parse_state = CoreState#core_state{a_links
					= [ALinkRel| ALinkRels]}} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_alink({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_mgw({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_mgw({startElement,  _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_mgw({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_mgw({endElement, _Uri, "CsMgwFunction", QName},
		[#state{dn_prefix = [MgwDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	MgwAttr = parse_mgw_attr(T2, undefined, []),
	ClassType = "CsMgwFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = MgwDn,
			description = "Circuit switched Media Gateway",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/CsMgwFunction",
			specification = Spec,
			characteristic = MgwAttr},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_mgw({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_mgw_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_mgw_attr1(Attributes, undefined, Acc).
% @hidden
parse_mgw_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _vnfParametersList], T2} = pop(startElement, QName, T1),
	parse_mgw_attr1(T2, undefined, Acc);
parse_mgw_attr1([{endElement, {_, "csMgwFunctionIucsLink"} = QName} | T1],
		undefined, Acc) ->
	% @todo dnList
	{[_ | _CsmgwfiucsLink], T2} = pop(startElement, QName, T1),
	parse_mgw_attr1(T2, undefined, Acc);
parse_mgw_attr1([{endElement, {_, "csMgwFunctionALink"} = QName} | T1],
		undefined, Acc) ->
	% @todo dnList
	{[_ | _CsmgfaLink], T2} = pop(startElement, QName, T1),
	parse_mgw_attr1(T2, undefined, Acc);
parse_mgw_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_mgw_attr1(T, Attr, Acc);
parse_mgw_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_mgw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_mgw_attr1([{characters, Chars} | T],
		"csMgwFunctionMscServerFunction" = Attr, Acc) ->
	parse_mgw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_mgw_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_mgw_attr1(T, undefined, Acc);
parse_mgw_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_ggsn({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ggsn({startElement, _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_ggsn({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ggsn({endElement, _Uri, "GgsnFunction", QName},
		[#state{dn_prefix = [GgsnDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	GgsnAttr = parse_ggsn_attr(T2, undefined, []),
	ClassType = "GgsnFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = GgsnDn,
			description = "Gateway GPRS Support Node (GGSN)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/CgsnFunction",
			specification = Spec,
			characteristic = GgsnAttr},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ggsn({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_ggsn_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_ggsn_attr1(Attributes, undefined, Acc).
% @hidden
parse_ggsn_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _vnfParametersList], T2} = pop(startElement, QName, T1),
	parse_ggsn_attr1(T2, undefined, Acc);
parse_ggsn_attr1([{endElement, {_, "proceduralStatus"} = QName} | T1],
		undefined, Acc) ->
	% @todo proceduralStatusType
	{[_ | _ProceduralStatus], T2} = pop(startElement, QName, T1),
	parse_ggsn_attr1(T2, undefined, Acc);
parse_ggsn_attr1([{endElement, {_, "apnInfoList"} = QName} | T1],
		undefined, Acc) ->
	% zte specific attributes
	{[_ | _ApniList], T2} = pop(startElement, QName, T1),
	parse_ggsn_attr1(T2, undefined, Acc);
parse_ggsn_attr1([{endElement, {_, "ggsnPortInfo"} = QName} | T1],
		undefined, Acc) ->
	% zte specific attributes
	{[_ | _GgsnpInfo], T2} = pop(startElement, QName, T1),
	parse_ggsn_attr1(T2, undefined, Acc);
parse_ggsn_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_ggsn_attr1(T, Attr, Acc);
parse_ggsn_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_ggsn_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_ggsn_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_ggsn_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_ggsn_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_ggsn_attr1(T, undefined, Acc);
parse_ggsn_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_sgsn({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_sgsn({startElement,  _Uri, "IupsLink", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",IupsLink=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_core, parse_function = parse_iups,
			parse_state = #core_state{iups_link = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_sgsn({startElement,  _Uri, "GbLink", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",GbLink=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_core, parse_function = parse_gb_link,
			parse_state = #core_state{gb_link = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_sgsn({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_sgsn({endElement, _Uri, "SgsnFunction", QName},
		[#state{parse_state =  #core_state{iups_links = IupsLinks,
		gb_links = GbLinks}, dn_prefix = [SgsnDn | _], stack = Stack,
		spec_cache = Cache, location = Location},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	SgsnAttr = parse_sgsn_attr(T2, undefined, []),
	ClassType = "SgsnFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = SgsnDn,
			description = "Serving GPRS Support Node (SGSN)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/SgsnFunction",
			specification = Spec,
			characteristic = [PeeParam | SgsnAttr],
			related = IupsLinks ++ GbLinks},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_sgsn({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_sgsn_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_sgsn_attr1(Attributes, undefined, Acc).
% @hidden
parse_sgsn_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _vnfParametersList], T2} = pop(startElement, QName, T1),
	parse_sgsn_attr1(T2, undefined, Acc);
parse_sgsn_attr1([{endElement, {_, "mccList"} = QName} | T1], undefined, Acc) ->
	% @todo longList
	{[_ | _MccList], T2} = pop(startElement, QName, T1),
	parse_sgsn_attr1(T2, undefined, Acc);
parse_sgsn_attr1([{endElement, {_, "mncList"} = QName} | T1], undefined, Acc) ->
	% @todo longList
	{[_ | _MncList], T2} = pop(startElement, QName, T1),
	parse_sgsn_attr1(T2, undefined, Acc);
parse_sgsn_attr1([{endElement, {_, "lacList"} = QName} | T1], undefined, Acc) ->
	% @todo longList
	{[_ | _LacList], T2} = pop(startElement, QName, T1),
	parse_sgsn_attr1(T2, undefined, Acc);
parse_sgsn_attr1([{endElement, {_, "racList"} = QName} | T1], undefined, Acc) ->
	% @todo longList
	{[_ | _RacList], T2} = pop(startElement, QName, T1),
	parse_sgsn_attr1(T2, undefined, Acc);
parse_sgsn_attr1([{endElement, {_, "sacList"} = QName} | T1], undefined, Acc) ->
	% @todo longList
	{[_ | _SacList], T2} = pop(startElement, QName, T1),
	parse_sgsn_attr1(T2, undefined, Acc);
parse_sgsn_attr1([{endElement, {_, "sgsnFunctionGsmCell"} = QName} | T1],
		undefined, Acc) ->
	% @todo dnList
	{[_ | _SgsnfgsmCell], T2} = pop(startElement, QName, T1),
	parse_sgsn_attr1(T2, undefined, Acc);
parse_sgsn_attr1([{endElement, {_, "sgsnFunctionExternalGsmCell"} = QName} | T1],
		undefined, Acc) ->
	% @todo dnList
	{[_ | _SgsnfegsmCell], T2} = pop(startElement, QName, T1),
	parse_sgsn_attr1(T2, undefined, Acc);
parse_sgsn_attr1([{endElement, {_, "proceduralStatus"} = QName} | T1],
		undefined, Acc) ->
	% @todo proceduralStatusType
	{[_ | _ProceduralStatus], T2} = pop(startElement, QName, T1),
	parse_sgsn_attr1(T2, undefined, Acc);
parse_sgsn_attr1([{endElement, {_, "nriList"} = QName} | T1], undefined, Acc) ->
	% @todo longList
	{[_ | _NriList], T2} = pop(startElement, QName, T1),
	parse_sgsn_attr1(T2, undefined, Acc);
parse_sgsn_attr1([{endElement, {_, "sgsnPortInfo"} = QName} | T1],
		undefined, Acc) ->
	% zte specific complex attributes
	{[_ | _SgsnpInfo], T2} = pop(startElement, QName, T1),
	parse_sgsn_attr1(T2, undefined, Acc);
parse_sgsn_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_sgsn_attr1(T, Attr, Acc);
parse_sgsn_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_sgsn_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_sgsn_attr1([{characters, Chars} | T], "sgsnId" = Attr, Acc) ->
	parse_sgsn_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_sgsn_attr1([{characters, Chars} | T], "sgsnFunctionSgsnPool" = Attr, Acc) ->
	parse_sgsn_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_sgsn_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_sgsn_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_sgsn_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_iups({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_iups({startElement, _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_iups({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_iups({endElement, _Uri, "IupsLink", QName},
		[#state{dn_prefix = [IupsDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache,
		parse_state = CoreState} = PrevState | T1]) ->
	#core_state{iups_links = IupsRels} = CoreState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	IupsAttr = parse_link_attr(T2, undefined, []),
	ClassType = "IupsLink",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = IupsDn,
			description = "Core Iu-ps interface link",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/IupsLink",
			specification = Spec,
			characteristic = IupsAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			IupsRel = #resource_rel{id = Id, name = IupsDn,rel_type = "contains",
					ref_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					parse_state = CoreState#core_state{iups_links
					= [IupsRel | IupsRels]}} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_iups({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_gb_link({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_gb_link({startElement, _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_gb_link({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_gb_link({endElement, _Uri, "GbLink", QName},
		[#state{dn_prefix = [GbLinkDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache,
		parse_state = CoreState} = PrevState | T1]) ->
	#core_state{gb_links = GbLinkRels} = CoreState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	GbLinkAttr = parse_link_attr(T2, undefined, []),
	ClassType = "GbLink",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = GbLinkDn,
			description = "Core Gb interface link",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/GbLink",
			specification = Spec,
			characteristic = GbLinkAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			GbLinkRel = #resource_rel{id = Id, name = GbLinkDn,rel_type = "contains",
					ref_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					parse_state = CoreState#core_state{gb_links
					= [GbLinkRel| GbLinkRels]}} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_gb_link({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_auc({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_auc({startElement,  _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_auc({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_auc({endElement, _Uri, "AucFunction", QName},
		[#state{dn_prefix = [AucDn | _], stack = Stack, location = Location,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	AucAttr = parse_auc_attr(T2, undefined, []),
	ClassType = "AucFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = AucDn,
			description = "Authentication Center (AUC)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/AucFunction",
			specification = Spec,
			characteristic = [PeeParam | AucAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_auc({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_auc_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_auc_attr1(Attributes, undefined, Acc).
% @hidden
parse_auc_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _vnfParametersList], T2} = pop(startElement, QName, T1),
	parse_auc_attr1(T2, undefined, Acc);
parse_auc_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_auc_attr1(T, Attr, Acc);
parse_auc_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_auc_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_auc_attr1([{characters, _Chars} | T], Attr, Acc) ->
	parse_auc_attr1(T, Attr, Acc);
parse_auc_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_auc_attr1(T, undefined, Acc);
parse_auc_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_hlr({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_hlr({startElement,  _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_hlr({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_hlr({endElement, _Uri, "HlrFunction", QName},
		[#state{dn_prefix = [HlrDn | _], stack = Stack, location = Location,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	HlrAttr = parse_hlr_attr(T2, undefined, []),
	ClassType = "HlrFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = HlrDn,
			description = "Home Location Register (HLR)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/AucFunction",
			specification = Spec,
			characteristic = [PeeParam | HlrAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_hlr({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_hlr_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_hlr_attr1(Attributes, undefined, Acc).
% @hidden
parse_hlr_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _vnfParametersList], T2} = pop(startElement, QName, T1),
	parse_hlr_attr1(T2, undefined, Acc);
parse_hlr_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_hlr_attr1(T, Attr, Acc);
parse_hlr_attr1([{endElement, {_, _Attribute}} | T], Attr, Acc) ->
	parse_hlr_attr1(T, Attr, Acc);
parse_hlr_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_hlr_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_hlr_attr1([{characters, _Chars} | T], Attr, Acc) ->
	parse_hlr_attr1(T, Attr, Acc);
parse_hlr_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_hlr_attr1(T, undefined, Acc);
parse_hlr_attr1([{startElement, {_, _Attribute}, _} | T], Attr, Acc) ->
	parse_hlr_attr1(T, Attr, Acc);
parse_hlr_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_eir({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_eir({startElement,  _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_eir({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_eir({endElement, _Uri, "EirFunction", QName},
		[#state{dn_prefix = [EirDn | _], stack = Stack, location = Location,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	EirAttr = parse_eir_attr(T2, undefined, []),
	ClassType = "EirFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = EirDn,
			description = "Equipment Identity Register (EIR)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/EirFunction",
			specification = Spec,
			characteristic = [PeeParam | EirAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_eir({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_eir_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_eir_attr1(Attributes, undefined, Acc).
% @hidden
parse_eir_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _vnfParametersList], T2} = pop(startElement, QName, T1),
	parse_eir_attr1(T2, undefined, Acc);
parse_eir_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_eir_attr1(T, Attr, Acc);
parse_eir_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_eir_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_eir_attr1([{characters, _Chars} | T], Attr, Acc) ->
	parse_eir_attr1(T, Attr, Acc);
parse_eir_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_eir_attr1(T, undefined, Acc);
parse_eir_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_mnp_srf({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_mnp_srf({startElement,  _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_mnp_srf({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_mnp_srf({endElement, _Uri, "MnpSrfFunction", QName},
		[#state{dn_prefix = [MnpSrfDn | _], stack = Stack, location = Location,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	MnpSrfAttr = parse_mnp_srf_attr(T2, undefined, []),
	ClassType = "MnpSrfFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = MnpSrfDn,
			description = "Mobile Number Portability-Signaling Relay Function (MNP-SRF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/MnpSrFunction",
			specification = Spec,
			characteristic = [PeeParam | MnpSrfAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_mnp_srf({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_mnp_srf_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_mnp_srf_attr1(Attributes, undefined, Acc).
% @hidden
parse_mnp_srf_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _vnfParametersList], T2} = pop(startElement, QName, T1),
	parse_mnp_srf_attr1(T2, undefined, Acc);
parse_mnp_srf_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_mnp_srf_attr1(T, Attr, Acc);
parse_mnp_srf_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_mnp_srf_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_mnp_srf_attr1([{characters, _Chars} | T], Attr, Acc) ->
	parse_mnp_srf_attr1(T, Attr, Acc);
parse_mnp_srf_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_mnp_srf_attr1(T, undefined, Acc);
parse_mnp_srf_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_cgf({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_cgf({startElement, _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_cgf({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_cgf({endElement, _Uri, "CgfFunction", QName},
		[#state{dn_prefix = [CgfDn | _], stack = Stack, location = Location,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	CgfAttr = parse_cgf_attr(T2, undefined, []),
	ClassType = "CgfFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = CgfDn,
			description = "Charging Gateway Function (CGF)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/CgfFunction",
			specification = Spec,
			characteristic = [PeeParam | CgfAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_cgf({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_cgf_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_cgf_attr1(Attributes, undefined, Acc).
% @hidden
parse_cgf_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _vnfParametersList], T2} = pop(startElement, QName, T1),
	parse_cgf_attr1(T2, undefined, Acc);
parse_cgf_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_cgf_attr1(T, Attr, Acc);
parse_cgf_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_cgf_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_cgf_attr1([{characters, _Chars} | T], Attr, Acc) ->
	parse_cgf_attr1(T, Attr, Acc);
parse_cgf_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_cgf_attr1(T, undefined, Acc);
parse_cgf_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_sgw({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_sgw({startElement, _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_sgw({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_sgw({endElement, _Uri, "SgwFunction", QName},
		[#state{dn_prefix = [SgwDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	SgwAttr = parse_sgw_attr(T2, undefined, []),
	ClassType = "SgwFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = SgwDn,
			description = "Core Signalling Gateway (Sgw)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/SgwFunction",
			specification = Spec,
			characteristic = SgwAttr},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_sgw({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_sgw_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_sgw_attr1(Attributes, undefined, Acc).
% @hidden
parse_sgw_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _vnfParametersList], T2} = pop(startElement, QName, T1),
	parse_sgw_attr1(T2, undefined, Acc);
parse_sgw_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_sgw_attr1(T, Attr, Acc);
parse_sgw_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_sgw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_sgw_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_sgw_attr1(T, undefined, Acc);
parse_sgw_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_cbc({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_cbc({startElement,  _Uri, "IubcLink", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",IubcLink=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_core, parse_function = parse_iubc,
			parse_state = #core_state{iubc_link = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_cbc({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_cbc({endElement, _Uri, "CbcFunction", QName},
		[#state{parse_state = #core_state{iubc_links = IubcLinks},
		dn_prefix = [CbcDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	CbcAttr = parse_cbc_attr(T2, undefined, []),
	ClassType = "CbcFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = CbcDn,
			description = "Core Cell Broadcast Centre (CBC)",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/CbcFunction",
			specification = Spec,
			characteristic = CbcAttr,
			related = IubcLinks},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_cbc({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_cbc_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_cbc_attr1(Attributes, undefined, Acc).
% @hidden
parse_cbc_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _vnfParametersList], T2} = pop(startElement, QName, T1),
	parse_cbc_attr1(T2, undefined, Acc);
parse_cbc_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_cbc_attr1(T, Attr, Acc);
parse_cbc_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_cbc_attr1(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
parse_cbc_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_cbc_attr1(T, undefined, Acc);
parse_cbc_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_iubc({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_iubc({startElement, _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_iubc({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_iubc({endElement, _Uri, "IubcLink", QName},
		[#state{dn_prefix = [IubcDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache,
		parse_state = CoreState} = PrevState | T1]) ->
	#core_state{iubc_links = IubcRels} = CoreState,
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	IubcAttr = parse_link_attr(T2, undefined, []),
	ClassType = "IubcLink",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = IubcDn,
			description = "Core Iu-bc interface link",
			category = "Core",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/IubcLink",
			specification = Spec,
			characteristic = IubcAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			IubcRel = #resource_rel{id = Id, name = IubcDn,rel_type = "contains",
					ref_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					parse_state = CoreState#core_state{iubc_links
					= [IubcRel | IubcRels]}} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_iubc({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_link_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_link_attr1(Attributes, undefined, Acc).
% @hidden
parse_link_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _vnfParametersList], T2} = pop(startElement, QName, T1),
	parse_link_attr1(T2, undefined, Acc);
parse_link_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_link_attr1(T, Attr, Acc);
parse_link_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_link_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_link_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_link_attr1(T, undefined, Acc);
parse_link_attr1([], undefined, Acc) ->
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
				{ok, #specification{id = Id, href = Href,
						name = Name, class_type = Type, version = Version}} ->
					SpecRef = #specification_ref{id = Id, href = Href,
							name = Name, ref_type = Type, version = Version},
					{SpecRef, [SpecRef | Cache]};
				{error, Reason} ->
					throw({get_specification_name, Reason})
			end
	end.
