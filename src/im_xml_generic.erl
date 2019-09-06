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
			parse_managed_element/2, parse_management_node/2, parse_vsdata/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

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
parse_subnetwork({startElement, _Uri, "meContext", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], rule = RuleId} | _] = State) ->
% only for zte xml files
	DnComponent = ",meContext=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_generic, parse_function = parse_mecontext,
			dn_prefix = [NewDn], rule = RuleId,
			parse_state = #generic_state{me_context = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_subnetwork({startElement, _Uri, "MeContext", QName,
		[{[], [], "id", Id}] = Attributes}, State) ->
	DnComponent = ",MeContext=" ++ Id,
	[#state{parse_module = im_xml_generic, parse_function = parse_mecontext,
			parse_state = #generic_state{me_context = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_subnetwork({startElement, _Uri, "ManagementNode", QName,
		[{[], [], "id", Id}] = Attributes}, State) ->
	DnComponent = ",ManagementNode=" ++ Id,
	[#state{parse_module = im_xml_generic,
			parse_function = parse_management_node,
			parse_state = #generic_state{node = [DnComponent]},
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
parse_subnetwork({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_subnetwork({endElement, _Uri, "SubNetwork", _QName},
		[_State, PrevState | T]) ->
	[PrevState | T];
parse_subnetwork({endElement,  _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

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
parse_mecontext({endElement, _Uri, "meContext", _QName},
		[_State, PrevState | T]) ->
% only for zte xml files
	[PrevState | T];
parse_mecontext({endElement, _Uri, "MeContext", _QName},
		[_State, PrevState | T]) ->
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
parse_managed_element({startElement, _, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _], location = Location} | _T] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_generic, parse_function = parse_vsdata,
			dn_prefix = [NewDn], location = Location,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
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
		parse_state = #generic_state{vs_data = VsData},
		stack = [{startElement, {_, "vsDataFormatVersion"}, _} | _]}
		= CurrentState | T]) ->
	#state{stack = Stack} = CurrentState,
	[#state{parse_module = im_xml_zte, parse_function = parse_vsdata,
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

