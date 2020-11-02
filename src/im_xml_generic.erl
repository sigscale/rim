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
-module(im_xml_generic).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% export the im private API
-export([parse_generic/2, parse_subnetwork/2, parse_mecontext/2,
			parse_link_mme_sgw/2, parse_link_mme_mme/2, parse_link_mme_sgsn/2,
			parse_link_hss_mme/2, parse_link_enb_mme/2,
			parse_managed_element/2, parse_management_node/2, parse_vsdata/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

-define(PathInventorySchema, "/resourceInventoryManagement/v4/schema").
-define(ResourcePath, "/resourceInventoryManagement/v4/resource/").

%%----------------------------------------------------------------------
%%  The im public API
%%----------------------------------------------------------------------

parse_generic({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_generic({startElement, _Uri, "SubNetwork", QName,
		[{[], [], "id", Id}] = Attributes}, [#state{dn_prefix = [],
		rule = RuleId} | _] = State) ->
	DnComponent = "SubNetwork=" ++ Id,
	[#state{dn_prefix = [DnComponent], rule = RuleId,
			parse_module = im_xml_generic, parse_function = parse_subnetwork,
			parse_state = #generic_state{subnet = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_generic({startElement, _Uri, "SubNetwork", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _] = State) ->
	DnComponent = ",SubNetwork=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], rule = RuleId,
			parse_module = im_xml_generic, parse_function = parse_subnetwork,
			parse_state = #generic_state{subnet = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_generic({startElement, _Uri, "MeContext", QName,
		[{[], [], "id", Id}] = Attributes} = _Event, State) ->
	DnComponent = ",MeContext=" ++ Id,
	[#state{parse_module = im_xml_generic, parse_function = parse_mecontext,
			parse_state = #generic_state{me_context = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_generic({startElement, _Uri, "ManagedElement", QName,
		[{[], [], "id", Id}] = Attributes}, State) ->
	DnComponent = ",ManagedElement=" ++ Id,
	[#state{parse_module = im_xml_generic,
			parse_function = parse_managed_element,
			parse_state = #generic_state{managed_element = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_generic({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_generic({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_subnetwork({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_subnetwork({startElement, _Uri, "SubNetwork", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _] = State) ->
	DnComponent = ",SubNetwork=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], rule = RuleId,
			parse_module = im_xml_generic, parse_function = parse_subnetwork,
			parse_state = #generic_state{subnet = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_subnetwork({startElement, _Uri, [M | "eContext"], QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _] = State)
		when M =:= $M; M =:= $m -> % work around zte bug
	DnComponent = ",MeContext=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], rule = RuleId,
			parse_module = im_xml_generic, parse_function = parse_mecontext,
			parse_state = #generic_state{me_context = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_subnetwork({startElement, _Uri, "ManagementNode", QName,
		[{[], [], "id", Id}] = Attributes}, State) ->
	DnComponent = ",ManagementNode=" ++ Id,
	[#state{parse_module = im_xml_generic,
			parse_function = parse_management_node,
			parse_state = #generic_state{node = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_subnetwork({startElement, _Uri, "Link_MME_ServingGW", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",Link_MME_ServingGW=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_link_mme_sgw,
			parse_state = #generic_state{link_mme_sgw = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_subnetwork({startElement, _Uri, "Link_MME_MME", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",Link_MME_MME=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_link_mme_mme,
			parse_state = #generic_state{link_mme_mme = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_subnetwork({startElement, _Uri, "Link_MME_SGSN", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",Link_MME_SGSN=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_link_mme_sgsn,
			parse_state = #generic_state{link_mme_sgsn = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_subnetwork({startElement, _Uri, "Link_HSS_MME", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",Link_HSS_MME=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_link_hss_mme,
			parse_state = #generic_state{link_hss_mme = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_subnetwork({startElement, _Uri, "Link_ENB_MME", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",Link_ENB_MME=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_link_enb_mme,
			parse_state = #generic_state{link_enb_mme = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_subnetwork({startElement, _Uri, "ManagedElement", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _] = State) ->
	DnComponent = ",ManagedElement=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], rule = RuleId,
			parse_module = im_xml_generic, parse_function = parse_managed_element,
			parse_state = #generic_state{managed_element = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_subnetwork({startElement, _Uri, "NetworkSlice", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _] = State) ->
	DnComponent = ",NetworkSlice=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], rule = RuleId,
			parse_module = im_xml_slice, parse_function = parse_network_slice,
			parse_state = #slice_state{network_slice = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_subnetwork({startElement, _Uri, "NetworkSliceSubnet", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _] = State) ->
	DnComponent = ",NetworkSliceSubnet=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], rule = RuleId,
			parse_module = im_xml_slice, parse_function = parse_ns_subnet,
			parse_state = #slice_state{ns_subnet = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_subnetwork({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_subnetwork({endElement, _Uri, "SubNetwork", QName},
		[#state{dn_prefix = [SubNetworkDn | _], stack = Stack, spec_cache = Cache,
		parse_state = #generic_state{links = LinkMmeSgwRels}},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	SubNetworkAttr = parse_subnetwork_attr(T2, undefined, []),
	ClassType = "SubNetwork",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = SubNetworkDn,
			description = "",
			category = "",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/SubNetwork",
			specification = Spec,
			related = LinkMmeSgwRels,
			characteristic = SubNetworkAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = SubNetworkId}} ->
			FConnectivity = fun F([#resource_rel{id = LinkId, name = LinkDn,
					href = LinkHref, ref_type = LinkRefType} | T3], Acc) ->
						LinkEndpoint = #endpoint_ref{id = LinkId, name = LinkDn,
								href = LinkHref, ref_type = LinkRefType},
						case im:get_resource(LinkId) of
							{ok, #resource{characteristic = LinkChars}} ->
								FaEnd = fun (#resource_char{name = "aEnd"}) ->
											true;
										(_) ->
											false
								end,
								[#resource_char{name = "aEnd", value = AEndDn}]
										= lists:filter(FaEnd, LinkChars),
								AEndPoint = build_function_endpoint(AEndDn),
								AEndLinkConnectivity
										= #connection{ass_type = "pointtoPoint",
										endpoint = [AEndPoint, LinkEndpoint]},
								FzEnd = fun (#resource_char{name = "zEnd"}) ->
											true;
										(_) ->
											false
								end,
								[#resource_char{name = "zEnd", value = ZEndDn}]
										= lists:filter(FzEnd, LinkChars),
								ZEndPoint = build_function_endpoint(ZEndDn),
								LinkZEndConnectivity
										= #connection{ass_type = "pointtoPoint",
										endpoint = [LinkEndpoint, ZEndPoint]},
								F(T3, [AEndLinkConnectivity,
										LinkZEndConnectivity | Acc]);
							{error, Reason} ->
								{error, Reason}
						end;
					F([], Acc) ->
						Acc
			end,
			Connectivity = [#resource_graph{connection = FConnectivity(LinkMmeSgwRels, [])}],
			Ftrans = fun() ->
					[R] = mnesia:read(resource, SubNetworkId, write),
					mnesia:write(resource,
						R#resource{connectivity = Connectivity}, write)
			end,
			case mnesia:transaction(Ftrans) of
				{aborted, Reason} ->
					throw({add_resource, Reason});
				{atomic, ok} ->
					[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1]
			end;
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_subnetwork({endElement,  _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
build_function_endpoint(EndDn) ->
	case im:get_resource_name(EndDn) of
		{ok, #resource{id = EndId, class_type = EndType,
				connection_point = EndPoints}} ->
			#endpoint_ref{id = EndId, href = ?ResourcePath ++ EndId,
					name = EndDn, ref_type = EndType,
					connection_point = EndPoints};
		{error, Reason} ->
			{error, Reason}
	end.

% @hidden
parse_subnetwork_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_subnetwork_attr1(Attributes, undefined, Acc).
% @hidden
parse_subnetwork_attr1([{characters, Chars} | T], Attr, Acc) when is_list(Chars) ->
	parse_subnetwork_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_subnetwork_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_subnetwork_attr1(T, undefined, Acc);
parse_subnetwork_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_subnetwork_attr1(T, Attr, Acc);
parse_subnetwork_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_mecontext({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_mecontext({startElement, _Uri, "ManagedElement", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _] = State) ->
	DnComponent = ",ManagedElement=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], rule = RuleId, parse_module = im_xml_generic,
			parse_function = parse_managed_element,
			parse_state = #generic_state{managed_element = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_mecontext({startElement,  _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_mecontext({endElement, _Uri, [M | "eContext"], _QName},
		[_State, PrevState | T]) when M =:= $M; M =:= $m -> % workaround ZTE bug
	[PrevState | T];
parse_mecontext({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_management_node({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_management_node({startElement,  _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_management_node({endElement, _Uri, "ManagementNode", _QName},
		[_State, PrevState | T]) ->
	[PrevState | T];
parse_management_node({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_link_mme_sgw({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_link_mme_sgw({startElement,  _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_link_mme_sgw({endElement, _Uri, "Link_MME_ServingGW", QName},
		[#state{dn_prefix = [LinkMmeSgwDn | _],
		stack = Stack, spec_cache = Cache}, #state{spec_cache = PrevCache,
		parse_state = GenericState} = PrevState | T1]) ->
	#generic_state{links = LinkRels} = GenericState,
	{[_ | T], _NewStack} = pop(startElement, QName, Stack),
	LinkMmeSgwAttr = parse_mme_link_attr(T, undefined, []),
	ClassType = "Link_MME_ServingGW",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = LinkMmeSgwDn,
			description = "EPC MMEFunction and ServingGWFunction Link",
			category = "EPC",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/Link_MME_ServingGW",
			specification = Spec,
			characteristic = LinkMmeSgwAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			LinkMmeSgwRel = #resource_rel{id = Id, name = LinkMmeSgwDn, rel_type = "contains",
					ref_type = ClassType, href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = GenericState#generic_state{
					links = [LinkMmeSgwRel | LinkRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_link_mme_sgw({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_link_mme_mme({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_link_mme_mme({startElement,  _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_link_mme_mme({endElement, _Uri, "Link_MME_MME", QName},
		[#state{dn_prefix = [LinkMmeMmeDn | _],
		stack = Stack, spec_cache = Cache}, #state{spec_cache = PrevCache,
		parse_state = GenericState} = PrevState | T1]) ->
	#generic_state{links = LinkRels} = GenericState,
	{[_ | T], _NewStack} = pop(startElement, QName, Stack),
	LinkMmeMmeAttr = parse_mme_link_attr(T, undefined, []),
	ClassType = "Link_MME_MME",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = LinkMmeMmeDn,
			description = "EPC MMEFunction and MMEFunction Link",
			category = "EPC",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/Link_MME_MME",
			specification = Spec,
			characteristic = LinkMmeMmeAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			LinkMmeMmeRel = #resource_rel{id = Id, name = LinkMmeMmeDn,
					rel_type = "contains", ref_type = ClassType,
					href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = GenericState#generic_state{
					links = [LinkMmeMmeRel | LinkRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_link_mme_mme({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_link_mme_sgsn({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_link_mme_sgsn({startElement,  _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_link_mme_sgsn({endElement, _Uri, "Link_MME_SGSN", QName},
		[#state{dn_prefix = [LinkMmeSgsnDn | _],
		stack = Stack, spec_cache = Cache}, #state{spec_cache = PrevCache,
		parse_state = GenericState} = PrevState | T1]) ->
	#generic_state{links = LinkRels} = GenericState,
	{[_ | T], _NewStack} = pop(startElement, QName, Stack),
	LinkMmeSgsnAttr = parse_mme_link_attr(T, undefined, []),
	ClassType = "Link_MME_SGSN",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = LinkMmeSgsnDn,
			description = "EPC MMEFunction and SgsnFunction Link",
			category = "EPC",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/Link_MME_SGSN",
			specification = Spec,
			characteristic = LinkMmeSgsnAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			LinkMmeSgsnRel = #resource_rel{id = Id, name = LinkMmeSgsnDn,
					rel_type = "contains", ref_type = ClassType,
					href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = GenericState#generic_state{
					links = [LinkMmeSgsnRel | LinkRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_link_mme_sgsn({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_link_hss_mme({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_link_hss_mme({startElement,  _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_link_hss_mme({endElement, _Uri, "Link_HSS_MME", QName},
		[#state{dn_prefix = [LinkHssMmeDn | _],
		stack = Stack, spec_cache = Cache}, #state{spec_cache = PrevCache,
		parse_state = GenericState} = PrevState | T1]) ->
	#generic_state{links = LinkRels} = GenericState,
	{[_ | T], _NewStack} = pop(startElement, QName, Stack),
	LinkHssMmeAttr = parse_mme_link_attr(T, undefined, []),
	ClassType = "Link_HSS_MME",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = LinkHssMmeDn,
			description = "EPC HSSFunction and MMEFunction Link",
			category = "EPC",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/Link_HSS_MME",
			specification = Spec,
			characteristic = LinkHssMmeAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			LinkHssMmeRel = #resource_rel{id = Id, name = LinkHssMmeDn,
					rel_type = "contains", ref_type = ClassType,
					href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = GenericState#generic_state{
					links = [LinkHssMmeRel | LinkRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_link_hss_mme({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_link_enb_mme({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_link_enb_mme({startElement,  _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_link_enb_mme({endElement, _Uri, "Link_ENB_MME", QName},
		[#state{dn_prefix = [LinkEnbMmeDn | _],
		stack = Stack, spec_cache = Cache}, #state{spec_cache = PrevCache,
		parse_state = GenericState} = PrevState | T1]) ->
	#generic_state{links = LinkRels} = GenericState,
	{[_ | T], _NewStack} = pop(startElement, QName, Stack),
	LinkEnbMmeAttr = parse_mme_link_attr(T, undefined, []),
	ClassType = "Link_ENB_MME",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = LinkEnbMmeDn,
			description = "EPC ENBFunction and MMEFunction Link",
			category = "EPC",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/Link_ENB_MME",
			specification = Spec,
			characteristic = LinkEnbMmeAttr},
	case im:add_resource(Resource) of
		{ok, #resource{id = Id}} ->
			LinkEnbMmeRel = #resource_rel{id = Id, name = LinkEnbMmeDn,
					rel_type = "contains", ref_type = ClassType,
					href = ?ResourcePath ++ Id},
			[PrevState#state{parse_state = GenericState#generic_state{
					links = [LinkEnbMmeRel | LinkRels]},
					spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_link_enb_mme({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_mme_link_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_mme_link_attr1(Attributes, undefined, Acc).
% @hidden
parse_mme_link_attr1([{characters, Chars} | T], "linkType" = Attr, Acc)
		when Chars == "Signalling"; Chars == "Bearer";
		Chars == "OAM_AND_P"; Chars == "Other" ->
	parse_mme_link_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_mme_link_attr1([{characters, Chars} | T], Attr, Acc) when is_list(Chars) ->
	parse_mme_link_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_mme_link_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_mme_link_attr1(T, undefined, Acc);
parse_mme_link_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_mme_link_attr1(T, Attr, Acc);
parse_mme_link_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_managed_element({characters, Location}, [#state{rule = RuleId,
		stack = [{startElement, {_, "locationName"}, _} | _]}
		= CurrentState | T] = State) ->
	case im:get_pee(RuleId, Location) of
		{ok, []} ->
			State;
		{ok, PEEMonitoredEntities} ->
			PeeParametersList =
					parse_peeParameterslist(PEEMonitoredEntities, []),
			[CurrentState#state{location = PeeParametersList} | T];
		{error, _Reason} ->
			State
	end;
parse_managed_element({characters, Location}, [#state{rule = RuleId,
		stack = [{startElement, {_, "userLabel"}, _} | _]}
		= CurrentState | T] = State) ->
	case im:get_pee(RuleId, Location) of
		{ok, []} ->
			State;
		{ok, PEEMonitoredEntities} ->
			PeeParametersList =
					parse_peeParameterslist(PEEMonitoredEntities, []),
			[CurrentState#state{location = PeeParametersList} | T];
		{error, _Reason} ->
			State
	end;
parse_managed_element({characters, Chars},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_managed_element({startElement, _, "BssFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _T] = State) ->
	DnComponent = ",BssFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_geran, parse_function = parse_bss,
			dn_prefix = [NewDn], rule = RuleId,
			parse_state = #geran_state{bss = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "NodeBFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId,
		location = Location} | _T] = State) ->
	DnComponent = ",NodeBFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_utran, parse_function = parse_nodeb,
			rule = RuleId, dn_prefix = [NewDn], location = Location,
			parse_state = #utran_state{nodeb = #{"id" => CurrentDn}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "RncFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _T] = State) ->
	DnComponent = ",RncFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_utran, parse_function = parse_rnc,
			dn_prefix = [NewDn], rule = RuleId,
			parse_state = #utran_state{rnc = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "EPDGFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _T] = State) ->
	DnComponent = ",EPDGFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_epc, parse_function = parse_epdg,
			dn_prefix = [NewDn], rule = RuleId,
			parse_state = #epc_state{epdg = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "MMEFunction", QName,
		[{[], [], "id", Id}] = Attributes}, [#state{dn_prefix = [CurrentDn | _],
		location = Location} | _T] = State) ->
	DnComponent = ",MMEFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_epc, parse_function = parse_mme,
			dn_prefix = [NewDn], location = Location,
			parse_state = #epc_state{mme = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "PCRFFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _T] = State) ->
	DnComponent = ",PCRFFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_epc, parse_function = parse_pcrf,
			dn_prefix = [NewDn], rule = RuleId,
			parse_state = #epc_state{pcrf = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "PGWFunction", QName,
		[{[], [], "id", Id}] = Attributes}, [#state{dn_prefix = [CurrentDn | _],
		location = Location} | _T] = State) ->
	DnComponent = ",PGWFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_epc, parse_function = parse_pgw,
			dn_prefix = [NewDn], location = Location,
			parse_state = #epc_state{pgw = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "ServingGWFunction", QName,
		[{[], [], "id", Id}] = Attributes}, [#state{dn_prefix = [CurrentDn | _],
		location = Location} | _T] = State) ->
	DnComponent = ",ServingGWFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_epc, parse_function = parse_sgw,
			dn_prefix = [NewDn], location = Location,
			parse_state = #epc_state{sgw = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "MscServerFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _T] = State) ->
	DnComponent = ",MscServerFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_core, parse_function = parse_msc,
			dn_prefix = [NewDn], rule = RuleId,
			parse_state = #core_state{msc = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "CsMgwFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _T] = State) ->
	DnComponent = ",CsMgwFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_core, parse_function = parse_mgw,
			dn_prefix = [NewDn], rule = RuleId,
			parse_state = #core_state{mgw = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "GgsnFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _T] = State) ->
	DnComponent = ",GgsnFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_core, parse_function = parse_ggsn,
			dn_prefix = [NewDn], rule = RuleId,
			parse_state = #core_state{ggsn = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "SgsnFunction", QName,
		[{[], [], "id", Id}] = Attributes}, [#state{dn_prefix = [CurrentDn | _],
		location = Location} | _T] = State) ->
	DnComponent = ",SgsnFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_core, parse_function = parse_sgsn,
			dn_prefix = [NewDn], location = Location,
			parse_state = #core_state{sgsn = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "AucFunction", QName,
		[{[], [], "id", Id}] = Attributes}, [#state{dn_prefix = [CurrentDn | _],
		rule = RuleId, location = Location} | _T] = State) ->
	DnComponent = ",AucFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_core, parse_function = parse_auc,
			dn_prefix = [NewDn], rule = RuleId, location = Location,
			parse_state = #core_state{auc = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "HlrFunction", QName,
		[{[], [], "id", Id}] = Attributes}, [#state{dn_prefix = [CurrentDn | _],
		rule = RuleId, location = Location} | _T] = State) ->
	DnComponent = ",HlrFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_core, parse_function = parse_hlr,
			dn_prefix = [NewDn], rule = RuleId, location = Location,
			parse_state = #core_state{hlr = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "EirFunction", QName,
		[{[], [], "id", Id}] = Attributes}, [#state{dn_prefix = [CurrentDn | _],
		rule = RuleId, location = Location} | _T] = State) ->
	DnComponent = ",EirFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_core, parse_function = parse_eir,
			dn_prefix = [NewDn], rule = RuleId, location = Location,
			parse_state = #core_state{eir = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "MnpSrfFunction", QName,
		[{[], [], "id", Id}] = Attributes}, [#state{dn_prefix = [CurrentDn | _],
		rule = RuleId, location = Location} | _T] = State) ->
	DnComponent = ",MnpSrfFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_core, parse_function = parse_mnp_srf,
			dn_prefix = [NewDn], rule = RuleId, location = Location,
			parse_state = #core_state{mnp_srf = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "CgfFunction", QName,
		[{[], [], "id", Id}] = Attributes}, [#state{dn_prefix = [CurrentDn | _],
		rule = RuleId, location = Location} | _T] = State) ->
	DnComponent = ",CgfFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_core, parse_function = parse_cgf,
			dn_prefix = [NewDn], rule = RuleId, location = Location,
			parse_state = #core_state{cgf = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "SgwFunction", QName,
		[{[], [], "id", Id}] = Attributes}, [#state{dn_prefix = [CurrentDn | _],
		rule = RuleId, location = Location} | _T] = State) ->
	DnComponent = ",SgwFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_core, parse_function = parse_sgw,
			dn_prefix = [NewDn], rule = RuleId, location = Location,
			parse_state = #core_state{sgw = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "CbcFunction", QName,
		[{[], [], "id", Id}] = Attributes}, [#state{dn_prefix = [CurrentDn | _],
		rule = RuleId, location = Location} | _T] = State) ->
	DnComponent = ",CbcFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_core, parse_function = parse_cbc,
			dn_prefix = [NewDn], rule = RuleId, location = Location,
			parse_state = #core_state{cbc = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], location = Location} | _T] = State) ->
	[#state{parse_module = im_xml_generic, parse_function = parse_vsdata,
			dn_prefix = [CurrentDn], location = Location,
			parse_state = #generic_state{vs_data = #{"id" => Id}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "PEEMonitoredEntity", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",PEEMonitoredEntity.",
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_pee, parse_function = parse_pee_me,
			dn_prefix = [NewDn],
			parse_state = #pee_state{me = #{"id" => "PEEMonitoredEntity=" ++ Id}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "ENBFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",ENBFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_eutran, parse_function = parse_enb,
			dn_prefix = [NewDn],
			parse_state = #eutran_state{enb = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, Uri, "_3GPPAAAProxyFunction",
		{Prefix, "_3GPPAAAProxyFunction"}, Attributes}, State) ->
	parse_managed_element({startElement, Uri, "3GPPAAAProxyFunction",
			{Prefix, "3GPPAAAProxyFunction"}, Attributes}, State);
parse_managed_element({startElement, _, "3GPPAAAProxyFunction", QName,
		[{[], [], "id", Id}] = Attributes},[#state{dn_prefix = [CurrentDn | _],
		location = Location} | _T] = State) ->
	DnComponent = ",3GPPAAAProxyFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_epcn3ai, parse_function = parse_proxy,
			dn_prefix = [NewDn], location = Location,
			parse_state = #epcn3ai_state{proxy = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, Uri, "_3GPPAAAServerFunction",
		{Prefix, "_3GPPAAAServerFunction"}, Attributes}, State) ->
	parse_managed_element({startElement, Uri, "3GPPAAAServerFunction",
			{Prefix, "3GPPAAAServerFunction"}, Attributes}, State);
parse_managed_element({startElement, Uri, "AaaFunction",
		{Prefix, "AaaFunction"}, Attributes}, State) ->
	parse_managed_element({startElement, Uri, "3GPPAAAServerFunction",
			{Prefix, "3GPPAAAServerFunction"}, Attributes}, State);
parse_managed_element({startElement, _, "3GPPAAAServerFunction", QName,
		[{[], [], "id", Id}] = Attributes}, [#state{dn_prefix = [CurrentDn | _],
		location = Location} | _T] = State) ->
	DnComponent = ",3GPPAAAServerFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_epcn3ai, parse_function = parse_server,
			dn_prefix = [NewDn], location = Location,
			parse_state = #epcn3ai_state{server = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "ASFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",ASFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_ims, parse_function = parse_as,
			dn_prefix = [NewDn],
			parse_state = #ims_state{as = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "HSSFunction", QName,
		[{[], [], "id", Id}] = Attributes}, [#state{dn_prefix = [CurrentDn | _],
		location = Location} | _T] = State) ->
	DnComponent = ",HSSFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_ims, parse_function = parse_hss,
			dn_prefix = [NewDn], location = Location,
			parse_state = #ims_state{hss = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "ICSCFFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",ICSCFFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_ims, parse_function = parse_icscf,
			dn_prefix = [NewDn],
			parse_state = #ims_state{icscf = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "PCSCFFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",PCSCFFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_ims, parse_function = parse_pcscf,
			dn_prefix = [NewDn],
			parse_state = #ims_state{pcscf = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "SCSCFFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",SCSCFFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_ims, parse_function = parse_scscf,
			dn_prefix = [NewDn],
			parse_state = #ims_state{scscf = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "InventoryUnit", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",InventoryUnit=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_inventory, parse_function = parse_iu,
			dn_prefix = [NewDn],
			parse_state = #im1_state{iu = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "TmaInventoryUnit", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",TmaInventoryUnit=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_inventory, parse_function = parse_tmaiu,
			dn_prefix = [NewDn],
			parse_state = #im1_state{tmaiu = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "AntennaInventoryUnit", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",AntennaInventoryUnit=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_inventory, parse_function = parse_aiu,
			dn_prefix = [NewDn],
			parse_state = #im1_state{aiu = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "InventoryUnitNE", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",InventoryUnitNE=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_inventory2, parse_function = parse_ne,
			dn_prefix = [NewDn],
			parse_state = #im2_state{iu_ne = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "InventoryUnitHw", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",InventoryUnitHw=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_inventory2, parse_function = parse_hw,
			dn_prefix = [NewDn],
			parse_state = #im2_state{iu_hw = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "InventoryUnitSw", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",InventoryUnitSw=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_inventory2, parse_function = parse_sw,
			dn_prefix = [NewDn],
			parse_state = #im2_state{iu_sw = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "InventoryUnitLic", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",InventoryUnitLic=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_inventory2, parse_function = parse_lic,
			dn_prefix = [NewDn],
			parse_state = #im2_state{iu_lic = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "GNBDUFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",GNBDUFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_nr, parse_function = parse_gnbdu,
			dn_prefix = [NewDn],
			parse_state = #nr_state{gnbdu = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "GNBCUCPFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",GNBCUCPFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_nr, parse_function = parse_gnbcucp,
			dn_prefix = [NewDn],
			parse_state = #nr_state{gnbcucp = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "GNBCUUPFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",GNBCUUPFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_nr, parse_function = parse_gnbcuup,
			dn_prefix = [NewDn],
			parse_state = #nr_state{gnbcuup = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "AMFFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",AMFFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_5gc, parse_function = parse_amf,
			dn_prefix = [NewDn],
			parse_state = #ngc_state{amf = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "SMFFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",SMFFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_5gc, parse_function = parse_smf,
			dn_prefix = [NewDn],
			parse_state = #ngc_state{smf = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "UPFFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",UPFFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_5gc, parse_function = parse_upf,
			dn_prefix = [NewDn],
			parse_state = #ngc_state{upf = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "N3IWFFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",N3IWFFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_5gc, parse_function = parse_n3iwf,
			dn_prefix = [NewDn],
			parse_state = #ngc_state{n3iwf = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "PCFFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",PCFFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_5gc, parse_function = parse_pcf,
			dn_prefix = [NewDn],
			parse_state = #ngc_state{pcf = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "AUSFFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",AUSFFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_5gc, parse_function = parse_ausf,
			dn_prefix = [NewDn],
			parse_state = #ngc_state{ausf = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "UDMFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",UDMFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_5gc, parse_function = parse_udm,
			dn_prefix = [NewDn],
			parse_state = #ngc_state{udm = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "UDRFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",UDRFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_5gc, parse_function = parse_udr,
			dn_prefix = [NewDn],
			parse_state = #ngc_state{udr = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "UDSFFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",UDSFFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_5gc, parse_function = parse_udsf,
			dn_prefix = [NewDn],
			parse_state = #ngc_state{udsf = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "NRFFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",NRFFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_5gc, parse_function = parse_nrf,
			dn_prefix = [NewDn],
			parse_state = #ngc_state{nrf = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "NSSFFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",NSSFFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_5gc, parse_function = parse_nssf,
			dn_prefix = [NewDn],
			parse_state = #ngc_state{nssf = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "SMSFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",SMSFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_5gc, parse_function = parse_sms,
			dn_prefix = [NewDn],
			parse_state = #ngc_state{sms = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "LMFFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",LMFFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_5gc, parse_function = parse_lmf,
			dn_prefix = [NewDn],
			parse_state = #ngc_state{lmf = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "NGEIRFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",NGEIRFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_5gc, parse_function = parse_ngeir,
			dn_prefix = [NewDn],
			parse_state = #ngc_state{ngeir = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "SEPPFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",SEPPFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_5gc, parse_function = parse_sepp,
			dn_prefix = [NewDn],
			parse_state = #ngc_state{sepp = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "NWDAFFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",NWDAFFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_5gc, parse_function = parse_nwdaf,
			dn_prefix = [NewDn],
			parse_state = #ngc_state{nwdaf = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement,  _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_managed_element({endElement, _Uri, "ManagedElement", _QName},
		[_State, PrevState | T]) ->
	[PrevState | T];
parse_managed_element({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

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
parse_vsdata({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_vsdata({characters, "ZTESpecificAttributes" = Chars},
		[#state{dn_prefix = [CurrentDn | _], location = Location,
		rule = RuleId, parse_state = #generic_state{vs_data = VsData},
		stack = [{startElement, {_, "vsDataFormatVersion"}, _} | _]}
		= CurrentState | T]) ->
	#state{stack = Stack} = CurrentState,
	[#state{parse_module = im_xml_zte, parse_function = parse_vsdata,
			rule = RuleId,
			dn_prefix = [CurrentDn], parse_state = #zte_state{vs_data = VsData},
			stack = [{characters, Chars} | Stack], location = Location} | T];
parse_vsdata({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_vsdata({endElement, _, "VsDataContainer", _QName},
		[_State, PrevState | T]) ->
	[PrevState | T];
parse_vsdata({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

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

