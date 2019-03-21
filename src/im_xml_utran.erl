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
			parse_tdd_lcr/2]).

-export([fraction1/1]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

-define(PathCatalogSchema, "/resourceCatalogManagement/v3/resourceCatalogManagement").
-define(PathInventorySchema, "/resourceInventoryManagement/v3/resourceInventoryManagement").

-record(utran_state,
		{rnc = [] :: string(),
		tdd_lcr = [] :: string(),
		tdd_hcr = [] :: string(),
		nodeb = [] :: string(),
		iub = [] :: string(),
		fdd = [] :: string(),
		fdds = [] :: [string()]}).

%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

%% @hidden
parse_nodeb({startElement, _Uri, "NodeBFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{parse_state = StateStack, stack = Stack} = State) ->
	DnComponent = ",NodeBFunction=" ++ Id,
	State#state{parse_state = [#utran_state{nodeb = DnComponent} | StateStack],
			stack = [{startElement, QName, Attributes} | Stack]};
parse_nodeb({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_nodeb({startElement, _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_nodeb({endElement, _Uri, "NodeBFunction", QName},
		#state{stack = Stack} = State) ->
	{[_ | T], NewStack} = pop(startElement, QName, Stack),
	parse_nodeb_attr(T, undefined, State#state{stack = NewStack}, []);
parse_nodeb({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

% @hidden
parse_nodeb_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
		undefined, State, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_nodeb_attr1(Attributes, undefined, State, Acc).
% @hidden
parse_nodeb_attr1([{endElement, {"un", "vnfParametersList" = Attr}} | T],
		undefined, State, Acc) ->
	% @todo vnfParametersListType
	parse_nodeb_attr1(T, Attr, State, Acc);
parse_nodeb_attr1([{endElement, {"un", "peeParametersList"}} | T],
		undefined, State, Acc) ->
	% @todo peeParametersListType
	parse_nodeb_attr1(T, undefined, State, Acc);
parse_nodeb_attr1([{characters, Chars} | T],
		"userLabel" = Attr, State, Acc) ->
	parse_nodeb_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_nodeb_attr1([{characters, Chars} | T],
		"mcc" = Attr, State, Acc) ->
	parse_nodeb_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_nodeb_attr1([{characters, Chars} | T],
		Attr, State, Acc) ->
	parse_nodeb_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_nodeb_attr1([{startElement, {"xn", "vnfInstanceId"}, _} | T],
		Attr, State, Acc) ->
	parse_nodeb_attr1(T, Attr, State, Acc);
parse_nodeb_attr1([{startElement, {"xn", "autoScalable"}, _} | T],
		Attr, State, Acc) ->
	parse_nodeb_attr1(T, Attr, State, Acc);
parse_nodeb_attr1([{startElement, {"un", Attr}, _} | T],
		Attr, State, Acc) ->
	parse_nodeb_attr1(T, undefined, State, Acc);
parse_nodeb_attr1([{endElement, {"un", "attributes"}}],
		undefined, State, _Acc) ->
	State;
parse_nodeb_attr1([{endElement, {"un", Attr}} | T],
		undefined, State, Acc) ->
	parse_nodeb_attr1(T, Attr, State, Acc);
parse_nodeb_attr1([{endElement, {"xn", "autoScalable"}} | T],
		Attr, State, Acc) ->
	parse_nodeb_attr1(T, Attr, State, Acc);
parse_nodeb_attr1([{endElement, {"xn", "vnfInstanceId"}} | T],
		Attr, State, Acc) ->
	parse_nodeb_attr1(T, Attr, State, Acc);
parse_nodeb_attr1([], undefined, #state{dn_prefix = [CurrentDn | _],
		parse_state = [#utran_state{nodeb = NodebId} = UtranState | T],
		spec_cache = Cache} = State, _Acc) ->
	NodebDn = CurrentDn ++ NodebId,
	ClassType = "NodeBFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = NodebDn,
			description = "UMTS Telecommunication Nodes",
			category = "RAN",
			class_type = ClassType,
			base_type = "SubNetwork",
			schema = "/resourceInventoryManagement/v3/schema/NodeBFunction",
			specification = Spec},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			State#state{parse_module = im_xml_generic,
					parse_function = parse_managed_element,
					parse_state = [UtranState#utran_state{nodeb = NodebDn} | T],
					spec_cache = NewCache};
		{error, Reason} ->
			throw({add_resource, Reason})
	end.

%% @hidden
parse_rnc({startElement, _Uri, "RncFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{parse_state = StateStack, stack = Stack} = State) ->
	DnComponent = ",RncFunction=" ++ Id,
	State#state{parse_state = [#utran_state{rnc = DnComponent} | StateStack],
			stack = [{startElement, QName, Attributes} | Stack]};
parse_rnc({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_rnc({startElement,  _Uri, "UtranCellFDD", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{parse_state = [#utran_state{} = UtranState | T],
		stack = Stack} = State) ->
	DnComponent = ",UtranCellFDD=" ++ Id,
	State#state{parse_module = ?MODULE, parse_function = parse_fdd,
			parse_state = [UtranState#utran_state{fdd = DnComponent} | T],
			stack = [{startElement, QName, Attributes} | Stack]};
parse_rnc({startElement,  _Uri, "EP_IuCS", QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{parse_module = ?MODULE, parse_function = parse_iucs,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_rnc({startElement,  _Uri, "EP_IuPS", QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{parse_module = ?MODULE, parse_function = parse_iups,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_rnc({startElement,  _Uri, "EP_Iur", QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{parse_module = ?MODULE, parse_function = parse_iur,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_rnc({startElement,  _Uri, "IubLink", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{parse_state = [#utran_state{} = UtranState | T],
		stack = Stack} = State) ->
	DnComponent = ",IubLink=" ++ Id,
	State#state{parse_module = ?MODULE, parse_function = parse_iub,
			parse_state = [UtranState#utran_state{iub = DnComponent} | T],
			stack = [{startElement, QName, Attributes} | Stack]};
parse_rnc({startElement,  _Uri, "UtranCellTDDLcr", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{parse_state = [#utran_state{} = UtranState | T],
		stack = Stack} = State) ->
	DnComponent = ",UtranCellTDDLcr=" ++ Id,
	State#state{parse_module = ?MODULE, parse_function = parse_tdd_lcr,
			parse_state = [UtranState#utran_state{tdd_lcr = DnComponent} | T],
			stack = [{startElement, QName, Attributes} | Stack]};
parse_rnc({startElement,  _Uri, "UtranCellTDDHcr", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{parse_state = [#utran_state{} = UtranState | T],
		stack = Stack} = State) ->
	DnComponent = ",UtranCellTDDHcr=" ++ Id,
	State#state{parse_module = ?MODULE, parse_function = parse_tdd_hcr,
			parse_state = [UtranState#utran_state{tdd_hcr = DnComponent} | T],
			stack = [{startElement, QName, Attributes} | Stack]};
parse_rnc({startElement, _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_rnc({endElement, _Uri, "RncFunction", QName},
		#state{stack = Stack} = State) ->
	{[_ | T], NewStack} = pop(startElement, QName, Stack),
	parse_rnc_attr(T, undefined, State#state{stack = NewStack}, []);
parse_rnc({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

% @hidden
parse_rnc_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
		undefined, State, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_rnc_attr1(Attributes, undefined, State, Acc).
% @hidden
parse_rnc_attr1([{endElement, {"un", "vnfParametersList" = Attr}} | T],
		undefined, State, Acc) ->
	% @todo vnfParametersListType
	parse_rnc_attr1(T, Attr, State, Acc);
parse_rnc_attr1([{endElement, {"un", "peeParametersList"}} | T],
		undefined, State, Acc) ->
	% @todo peeParametersListType
	parse_rnc_attr1(T, undefined, State, Acc);
parse_rnc_attr1([{endElement, {"un", "tceIDMappingInfoList"}} | T],
		undefined, State, Acc) ->
	% @todo TceIDMappingInfoList
	parse_rnc_attr1(T, undefined, State, Acc);
parse_rnc_attr1([{endElement, {"un", "sharNetTceMappingInfoList"}} | T],
		undefined, State, Acc) ->
	% @todo SharNetTceMappingInfoList
	parse_rnc_attr1(T, undefined, State, Acc);
parse_rnc_attr1([{characters, Chars} | T],
		"userLabel" = Attr, State, Acc) ->
	parse_rnc_attr1(T, Attr, State,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_rnc_attr1([{characters, Chars} | T],
		"mcc" = Attr, State, Acc) ->
	parse_rnc_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_rnc_attr1([{characters, Chars} | T],
		"mnc" = Attr, State, Acc) ->
	parse_rnc_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_rnc_attr1([{characters, Chars} | T],
		"rncId" = Attr, State, Acc) ->
	parse_rnc_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_rnc_attr1([{characters, "0"} | T],
		"siptoSupported" = Attr, State, Acc) ->
	parse_rnc_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = 0} | Acc]);
parse_rnc_attr1([{characters, "1"} | T],
		"siptoSupported" = Attr, State, Acc) ->
	parse_rnc_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = 1} | Acc]);
parse_rnc_attr1([{characters, Chars} | T],
		Attr, State, Acc) ->
	parse_rnc_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_rnc_attr1([{startElement, {"xn", "vnfInstanceId"}, _} | T],
		Attr, State, Acc) ->
	parse_rnc_attr1(T, Attr, State, Acc);
parse_rnc_attr1([{startElement, {"xn", "autoScalable"}, _} | T],
		Attr, State, Acc) ->
	parse_rnc_attr1(T, Attr, State, Acc);
parse_rnc_attr1([{startElement, {"un", Attr}, _} | T],
		Attr, State, Acc) ->
	parse_rnc_attr1(T, undefined, State, Acc);
parse_rnc_attr1([{endElement, {"un", "attributes"}}],
		undefined, State, _Acc) ->
	State;
parse_rnc_attr1([{endElement, {"un", Attr}} | T],
		undefined, State, Acc) ->
	parse_rnc_attr1(T, Attr, State, Acc);
parse_rnc_attr1([{endElement, {"xn", "vnfInstanceId"}} | T],
		Attr, State, Acc) ->
	parse_rnc_attr1(T, Attr, State, Acc);
parse_rnc_attr1([{endElement, {"xn", "autoScalable"}} | T],
		Attr, State, Acc) ->
	parse_rnc_attr1(T, Attr, State, Acc);
parse_rnc_attr1([], undefined, #state{dn_prefix = [CurrentDn | _],
		parse_state = [#utran_state{rnc = RncId, fdds = Fdds} = UtranState | T],
		spec_cache = Cache} = State, Acc) ->
	UtranCellFDD = #resource_char{name = "UtranCellFDD", value = Fdds},
	RncDn = CurrentDn ++ RncId,
	ClassType = "RncFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = RncDn,
			description = "UMTS Radio Network Controller (RNC)",
			category = "RAN",
			class_type = ClassType,
			base_type = "SubNetwork",
			schema = "/resourceInventoryManagement/v3/schema/RncFunction",
			specification = Spec,
			characteristic = lists:reverse([UtranCellFDD | Acc])},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			State#state{parse_module = im_xml_generic,
					parse_function = parse_managed_element,
					parse_state = [UtranState#utran_state{rnc = RncDn, fdds = []} | T],
					spec_cache = NewCache};
		{error, Reason} ->
			throw({add_resource, Reason})
	end.

%% @hidden
parse_fdd({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_fdd({startElement, _Uri, _LocalName, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_fdd({endElement,  _Uri, "UtranCellFDD", QName},
		#state{stack = Stack} = State) ->
	{[_ | T], NewStack} = pop(startElement, QName, Stack),
	parse_fdd_attr(T, State#state{stack = NewStack}, []);
parse_fdd({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

% @hidden
parse_fdd_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
		State, Acc) ->
	{[_ | Attributes], T2} = pop(endElement, QName, T1),
	parse_fdd_attr1(Attributes, undefined, T2, State, Acc).
% @hidden
parse_fdd_attr1([{characters, Chars} | T],
		"userLabel" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"cId" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"localCellId" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"maximumTransmissionPower" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, "FDDMode"} | T],
		"cellMode" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "FDDMode"} | Acc]);
parse_fdd_attr1([{characters, "3-84McpsTDDMode"} | T],
		"cellMode" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "3-84McpsTDDMode"} | Acc]);
parse_fdd_attr1([{characters, "1-28McpsTDDMode"} | T],
		"cellMode" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "1-28McpsTDDMode"} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"pichPower" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"pchPower" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"fachPower" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"lac" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"rac" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"sac" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"utranCellIubLink" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_fdd_attr1([{characters, "enabled"} | T],
		"operationalState" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "enabled"} | Acc]);
parse_fdd_attr1([{characters, "disabled"} | T],
		"operationalState" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "disabled"} | Acc]);
parse_fdd_attr1([{characters, "0"} | T],
		"hsFlag" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = 0} | Acc]);
parse_fdd_attr1([{characters, "1"} | T],
		"hsFlag" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = 1} | Acc]);
parse_fdd_attr1([{characters, "1"} | T],
		"hsEnable" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = 1} | Acc]);
parse_fdd_attr1([{characters, "0"} | T],
		"hsEnable" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = 0} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"numOfHspdschs" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"numOfHsscchs" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"frameOffset" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"cellIndividualOffset" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"hcsPrio" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"maximumAllowedUlTxPower" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"qrxlevMin" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"deltaQrxlevmin" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"qhcs" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"penaltyTime" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"referenceTimeDifferenceToCell" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, "TRUE"} | T],
		"readSFNIndicator" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "TRUE"} | Acc]);
parse_fdd_attr1([{characters, "FALSE"} | T],
		"readSFNIndicator" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "FALSE"} | Acc]);
parse_fdd_attr1([{characters, "cellReservedForOperatorUse"} | T],
		"restrictionStateIndicator" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "cellReservedForOperatorUse"} | Acc]);
parse_fdd_attr1([{characters, "cellAccessible"} | T],
		"restrictionStateIndicator" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "cellAccessible"} | Acc]);
parse_fdd_attr1([{characters, "dpcModeChangeSupported"} | T],
		"dpcModechangeSupportIndicator" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "dpcModeChangeSupported"} | Acc]);
parse_fdd_attr1([{characters, "dpcModeChangeNotSupported"} | T],
		"dpcModechangeSupportIndicator" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "dpcModeChangeNotSupported"} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"relatedSectorEquipment" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"uarfcnUl" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"uarfcnDl" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"primaryScramblingCode" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"primaryCpichPower" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"primarySchPower" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"secondarySchPower" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"bchPower" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"aichPower" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"qqualMin" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, "none"} | T],
		"txDiversityIndicator" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "none"} | Acc]);
parse_fdd_attr1([{characters, "PrimaryCpichBroadcastFrom2Antennas"} | T],
		"txDiversityIndicator" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "PrimaryCpichBroadcastFrom2Antennas"} | Acc]);
parse_fdd_attr1([{characters, "SttdAppliedToPrimaryCCPCH"} | T],
		"txDiversityIndicator" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "SttdAppliedToPrimaryCCPCH"} | Acc]);
parse_fdd_attr1([{characters, "TstdAppliedToPrimarySchAndSecondarySch"} | T],
		"txDiversityIndicator" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "TstdAppliedToPrimarySchAndSecondarySch"} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"temporaryOffset1" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		"temporaryOffset2" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_fdd_attr1([{characters, "active"} | T],
		"sttdSupportIndicator" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "active"} | Acc]);
parse_fdd_attr1([{characters, "inactive"} | T],
		"sttdSupportIndicator" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "inactive"} | Acc]);
parse_fdd_attr1([{characters, "closedLoopMode1Supported"} | T],
		"closedLoopModelSupportIndicator" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "closedLoopMode1Supported"} | Acc]);
parse_fdd_attr1([{characters, "closedLoopMode1NotSupported"} | T],
		"closedLoopModelSupportIndicator" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = "closedLoopMode1NotSupported"} | Acc]);
parse_fdd_attr1([{characters, Chars} | T],
		Attr, FddStack, State, Acc) ->
	% @todo default handler
	parse_fdd_attr1(T, Attr, FddStack, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_fdd_attr1([{startElement, {"un", "relatedAntennaList" = Attr}, _} | T],
		Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, undefined, FddStack, State, Acc);
parse_fdd_attr1([{startElement, {"xn", "dn"}, _} | T],
		"relatedAntennaList" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, Acc);
parse_fdd_attr1([{startElement, {_, Attr}, _} | T],
		Attr, FddStack, State, Acc) ->
	% @todo default handler
	parse_fdd_attr1(T, undefined, FddStack, State, Acc);
parse_fdd_attr1([{endElement, {"un", "vnfParametersList"}} | T],
		undefined, FddStack, State, Acc) ->
	% @todo vnfParametersListType
	parse_fdd_attr1(T, undefined, FddStack, State, Acc);
parse_fdd_attr1([{endElement, {"un", "uraList"}} | T],
		undefined, FddStack, State, Acc) ->
	% @todo uraList
	parse_fdd_attr1(T, undefined, FddStack, State, Acc);
parse_fdd_attr1([{endElement, {"un", "relatedAntennaList" = Attr}} | T],
		undefined, FddStack, State, Acc) ->
	% @todo relatedAntennaList
	parse_fdd_attr1(T, Attr, FddStack, State, Acc);
parse_fdd_attr1([{endElement, {"un", "relatedTmaList"}} | T],
		undefined, FddStack, State, Acc) ->
	% @todo relatedTmaList
	parse_fdd_attr1(T, undefined, FddStack, State, Acc);
parse_fdd_attr1([{endElement, {"un", "snaInformation"}} | T],
		undefined, FddStack, State, Acc) ->
	% @todo snaInformation
	parse_fdd_attr1(T, undefined, FddStack, State, Acc);
parse_fdd_attr1([{endElement, {"un", "nsPlmnIdList"}} | T],
		undefined, FddStack, State, Acc) ->
	% @todo NsPlmnIdListType
	parse_fdd_attr1(T, undefined, FddStack, State, Acc);
parse_fdd_attr1([{endElement, {"un", "cellCapabilityContainerFDD"}} | T],
		undefined, FddStack, State, Acc) ->
	% @todo cellCapabilityContainerFDD
	parse_fdd_attr1(T, undefined, FddStack, State, Acc);
parse_fdd_attr1([{endElement, {"xn", Attr}} | T],
		Attr, FddStack, State, Acc) ->
	% @todo default handler
	parse_fdd_attr1(T, Attr, FddStack, State, Acc);
parse_fdd_attr1([{endElement, {"xn", "dn"}} | T],
		"relatedAntennaList" = Attr, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, Acc);
parse_fdd_attr1([{endElement, {"xn", Attr}} | T],
		undefined, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, Acc);
parse_fdd_attr1([{endElement, {"un", Attr}} | T],
		undefined, FddStack, State, Acc) ->
	parse_fdd_attr1(T, Attr, FddStack, State, Acc);
parse_fdd_attr1([], _Attr, FddStack, State, Acc) ->
	parse_fdd_rels(FddStack, State, Acc,
			#{gsmRel => [], utranRel => []}).

%% @hidden
parse_tdd_hcr({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_tdd_hcr({startElement, _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_tdd_hcr({endElement, _Uri, "UtranCellTDDHcr", QName},
		#state{stack = Stack} = State) ->
	{[_ | T], NewStack} = pop(startElement, QName, Stack),
	parse_tdd_hcr_attr(T, undefined, State#state{stack = NewStack}, []);
parse_tdd_hcr({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

% @hidden
parse_tdd_hcr_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
		undefined, State, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_tdd_hcr_attr1(Attributes, undefined, State, Acc).
% @hidden
parse_tdd_hcr_attr1([{endElement, {"un", "vnfParametersList" = Attr}} | T],
		undefined, State, Acc) ->
	% @todo vnfParametersListType
	parse_tdd_hcr_attr1(T, Attr, State, Acc);
parse_tdd_hcr_attr1([{endElement, {"un", "uraList"}} | T],
		undefined, State, Acc) ->
	% @todo uraList
	parse_tdd_hcr_attr1(T, undefined, State, Acc);
parse_tdd_hcr_attr1([{endElement, {"un", "relatedAntennaList" = Attr}} | T],
		undefined, State, Acc) ->
	% @todo relatedAntennaList
	parse_tdd_hcr_attr1(T, Attr, State, Acc);
parse_tdd_hcr_attr1([{endElement, {"un", "relatedTmaList"}} | T],
		undefined, State, Acc) ->
	% @todo relatedTmaList
	parse_tdd_hcr_attr1(T, undefined, State, Acc);
parse_tdd_hcr_attr1([{endElement, {"un", "snaInformation"}} | T],
		undefined, State, Acc) ->
	% @todo snaInformation
	parse_tdd_hcr_attr1(T, undefined, State, Acc);
parse_tdd_hcr_attr1([{endElement, {"un", "nsPlmnIdList"}} | T],
		undefined, State, Acc) ->
	% @todo NsPlmnIdListType
	parse_tdd_hcr_attr1(T, undefined, State, Acc);
parse_tdd_hcr_attr1([{endElement, {"un", "cellCapabilityContainerTDD"}} | T],
		undefined, State, Acc) ->
	% @todo cellCapabilityContainerTDD 
	parse_tdd_hcr_attr1(T, undefined, State, Acc);
parse_tdd_hcr_attr1([{endElement, {"un", "timeSlotHCRList"}} | T],
		undefined, State, Acc) ->
	% @todo timeSlotHCRList
	parse_tdd_hcr_attr1(T, undefined, State, Acc);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"userLabel" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"cId" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"localCellId" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"maximumTransmissionPower" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, "FDDMode"} | T],
		"cellMode" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "FDDMode"} | Acc]);
parse_tdd_hcr_attr1([{characters, "3-84McpsTDDMode"} | T],
		"cellMode" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "3-84McpsTDDMode"} | Acc]);
parse_tdd_hcr_attr1([{characters, "1-28McpsTDDMode"} | T],
		"cellMode" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "1-28McpsTDDMode"} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"pichPower" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"pchPower" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"fachPower" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"lac" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"rac" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"sac" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"utranCellIubLink" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_hcr_attr1([{characters, "enabled"} | T],
		"operationalState" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "enabled"} | Acc]);
parse_tdd_hcr_attr1([{characters, "disabled"} | T],
		"operationalState" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "disabled"} | Acc]);
parse_tdd_hcr_attr1([{characters, "0"} | T],
		"hsFlag" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = 0} | Acc]);
parse_tdd_hcr_attr1([{characters, "1"} | T],
		"hsFlag" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = 1} | Acc]);
parse_tdd_hcr_attr1([{characters, "1"} | T],
		"hsEnable" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = 1} | Acc]);
parse_tdd_hcr_attr1([{characters, "0"} | T],
		"hsEnable" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = 0} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"numOfHspdschs" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"numOfHsscchs" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"frameOffset" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"cellIndividualOffset" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"hcsPrio" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"maximumAllowedUlTxPower" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"qrxlevMin" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"deltaQrxlevmin" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"qhcs" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"penaltyTime" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"referenceTimeDifferenceToCell" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, "TRUE"} | T],
		"readSFNIndicator" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "TRUE"} | Acc]);
parse_tdd_hcr_attr1([{characters, "FALSE"} | T],
		"readSFNIndicator" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "FALSE"} | Acc]);
parse_tdd_hcr_attr1([{characters, "cellReservedForOperatorUse"} | T],
		"restrictionStateIndicator" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "cellReservedForOperatorUse"} | Acc]);
parse_tdd_hcr_attr1([{characters, "cellAccessible"} | T],
		"restrictionStateIndicator" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "cellAccessible"} | Acc]);
parse_tdd_hcr_attr1([{characters, "dpcModeChangeSupported"} | T],
		"dpcModechangeSupportIndicator" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "dpcModeChangeSupported"} | Acc]);
parse_tdd_hcr_attr1([{characters, "dpcModeChangeNotSupported"} | T],
		"dpcModechangeSupportIndicator" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "dpcModeChangeNotSupported"} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"relatedSectorEquipment" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"uarfcn" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"cellParameterId" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"primaryCcpchPower" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, "active"} | T],
		"sctdIndicator" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "active"} | Acc]);
parse_tdd_hcr_attr1([{characters, "inactive"} | T],
		"sctdIndicator" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "inactive"} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"dpchConstantValue" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"schPower" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"temporaryOffset1" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, "SCH and PCCPCH allocated in a single TS"} | T],
		"syncCase" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "SCH and PCCPCH allocated in a single TS"} | Acc]);
parse_tdd_hcr_attr1([{characters,
		"SCH and PCCPCH allocated in two TS, TS#k and TS#k+8"} | T],
		"syncCase" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "SCH and PCCPCH allocated in two TS, TS#k and TS#k+8"} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"timeSlotForSch" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		"schTimeSlot" = Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_hcr_attr1([{characters, Chars} | T],
		Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_hcr_attr1([{startElement, {"xn", "dn"}, _} | T],
		Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, Acc);
parse_tdd_hcr_attr1([{startElement, {"un", Attr}, _} | T],
		Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, undefined, State, Acc);
parse_tdd_hcr_attr1([{endElement, {"un", "attributes"}}],
		undefined, State, _Acc) ->
	State;
parse_tdd_hcr_attr1([{endElement, {"un", Attr}} | T],
		undefined, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, Acc);
parse_tdd_hcr_attr1([{endElement, {"xn", "dn"}} | T],
		Attr, State, Acc) ->
	parse_tdd_hcr_attr1(T, Attr, State, Acc);
parse_tdd_hcr_attr1([], undefined, #state{dn_prefix = [CurrentDn | _],
		parse_state = [#utran_state{rnc = RncId,
		tdd_hcr = TddHcrId} = UtranState | T],
		spec_cache = Cache} = State, _Acc) ->
	TddHcrDn = CurrentDn ++ RncId ++ TddHcrId,
	ClassType = "UtranCellTDDHcr",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = TddHcrDn,
			description = "UMTS Time Division Duplex High Chip Rate",
			category = "RAN",
			class_type = ClassType,
			base_type = "SubNetwork",
			schema = "/resourceInventoryManagement/v3/schema/UtranCellTDDHcr",
			specification = Spec},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			State#state{parse_module = ?MODULE,
					parse_function = parse_rnc,
					parse_state = [UtranState#utran_state{tdd_hcr = TddHcrDn} | T],
					spec_cache = NewCache};
		{error, Reason} ->
			throw({add_resource, Reason})
	end.

%% @hidden
parse_tdd_lcr({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_tdd_lcr({startElement, _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_tdd_lcr({endElement, _Uri, "UtranCellTDDLcr", QName},
		#state{stack = Stack} = State) ->
	{[_ | T], NewStack} = pop(startElement, QName, Stack),
	parse_tdd_lcr_attr(T, undefined, State#state{stack = NewStack}, []);
parse_tdd_lcr({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

% @hidden
parse_tdd_lcr_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
		undefined, State, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_tdd_lcr_attr1(Attributes, undefined, State, Acc).
% @hidden
parse_tdd_lcr_attr1([{endElement, {"un", "vnfParametersList" = Attr}} | T],
		undefined, State, Acc) ->
	% @todo vnfParametersListType
	parse_tdd_lcr_attr1(T, Attr, State, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", "uraList"}} | T],
		undefined, State, Acc) ->
	% @todo uraList
	parse_tdd_lcr_attr1(T, undefined, State, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", "relatedAntennaList" = Attr}} | T],
		undefined, State, Acc) ->
	% @todo relatedAntennaList
	parse_tdd_lcr_attr1(T, Attr, State, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", "relatedTmaList"}} | T],
		undefined, State, Acc) ->
	% @todo relatedTmaList
	parse_tdd_lcr_attr1(T, undefined, State, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", "snaInformation"}} | T],
		undefined, State, Acc) ->
	% @todo snaInformation
	parse_tdd_lcr_attr1(T, undefined, State, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", "nsPlmnIdList"}} | T],
		undefined, State, Acc) ->
	% @todo NsPlmnIdListType
	parse_tdd_lcr_attr1(T, undefined, State, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", "cellCapabilityContainerTDD"}} | T],
		undefined, State, Acc) ->
	% @todo cellCapabilityContainerTDD 
	parse_tdd_lcr_attr1(T, undefined, State, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", "uarfcnLCRList"}} | T],
		undefined, State, Acc) ->
	% @todo uarfcnLCRList
	parse_tdd_lcr_attr1(T, undefined, State, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", "timeSlotLCRList"}} | T],
		undefined, State, Acc) ->
	% @todo timeSlotLCRList
	parse_tdd_lcr_attr1(T, undefined, State, Acc);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"userLabel" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"cId" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"localCellId" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"maximumTransmissionPower" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, "FDDMode"} | T],
		"cellMode" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "FDDMode"} | Acc]);
parse_tdd_lcr_attr1([{characters, "3-84McpsTDDMode"} | T],
		"cellMode" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "3-84McpsTDDMode"} | Acc]);
parse_tdd_lcr_attr1([{characters, "1-28McpsTDDMode"} | T],
		"cellMode" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "1-28McpsTDDMode"} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"pichPower" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"pchPower" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"fachPower" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"lac" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"rac" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"sac" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"utranCellIubLink" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_lcr_attr1([{characters, "enabled"} | T],
		"operationalState" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "enabled"} | Acc]);
parse_tdd_lcr_attr1([{characters, "disabled"} | T],
		"operationalState" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "disabled"} | Acc]);
parse_tdd_lcr_attr1([{characters, "0"} | T],
		"hsFlag" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = 0} | Acc]);
parse_tdd_lcr_attr1([{characters, "1"} | T],
		"hsFlag" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = 1} | Acc]);
parse_tdd_lcr_attr1([{characters, "1"} | T],
		"hsEnable" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = 1} | Acc]);
parse_tdd_lcr_attr1([{characters, "0"} | T],
		"hsEnable" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = 0} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"numOfHspdschs" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"numOfHsscchs" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"frameOffset" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"cellIndividualOffset" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"hcsPrio" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"maximumAllowedUlTxPower" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"qrxlevMin" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"deltaQrxlevmin" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"qhcs" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"penaltyTime" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"referenceTimeDifferenceToCell" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, "TRUE"} | T],
		"readSFNIndicator" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "TRUE"} | Acc]);
parse_tdd_lcr_attr1([{characters, "FALSE"} | T],
		"readSFNIndicator" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "FALSE"} | Acc]);
parse_tdd_lcr_attr1([{characters, "cellReservedForOperatorUse"} | T],
		"restrictionStateIndicator" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "cellReservedForOperatorUse"} | Acc]);
parse_tdd_lcr_attr1([{characters, "cellAccessible"} | T],
		"restrictionStateIndicator" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "cellAccessible"} | Acc]);
parse_tdd_lcr_attr1([{characters, "dpcModeChangeSupported"} | T],
		"dpcModechangeSupportIndicator" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "dpcModeChangeSupported"} | Acc]);
parse_tdd_lcr_attr1([{characters, "dpcModeChangeNotSupported"} | T],
		"dpcModechangeSupportIndicator" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "dpcModeChangeNotSupported"} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"relatedSectorEquipment" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"uarfcn" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"cellParameterId" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"primaryCcpchPower" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, "active"} | T],
		"sctdIndicator" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "active"} | Acc]);
parse_tdd_lcr_attr1([{characters, "inactive"} | T],
		"sctdIndicator" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "inactive"} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"dpchConstantValue" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"fpachPower" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		"dwPchPower" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = fraction1(Chars)} | Acc]);
parse_tdd_lcr_attr1([{characters, "active"} | T],
		"tstdIndicator" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "active"} | Acc]);
parse_tdd_lcr_attr1([{characters, "inactive"} | T],
		"tstdIndicator" = Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = "inactive"} | Acc]);
parse_tdd_lcr_attr1([{characters, Chars} | T],
		Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_tdd_lcr_attr1([{startElement, {"xn", "dn"}, _} | T],
		Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, Acc);
parse_tdd_lcr_attr1([{startElement, {"un", Attr}, _} | T],
		Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, undefined, State, Acc);
parse_tdd_lcr_attr1([{endElement, {"un", "attributes"}}],
		undefined, State, _Acc) ->
	State;
parse_tdd_lcr_attr1([{endElement, {"un", Attr}} | T],
		undefined, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, Acc);
parse_tdd_lcr_attr1([{endElement, {"xn", "dn"}} | T],
		Attr, State, Acc) ->
	parse_tdd_lcr_attr1(T, Attr, State, Acc);
parse_tdd_lcr_attr1([], undefined, #state{dn_prefix = [CurrentDn | _],
		parse_state = [#utran_state{rnc = RncId,
		tdd_lcr = TddLcrId} = UtranState | T],
		spec_cache = Cache} = State, _Acc) ->
	TddLcrDn = CurrentDn ++ RncId ++ TddLcrId,
	ClassType = "UtranCellTDDLcr",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = TddLcrDn,
			description = "UMTS Time Division Duplex Low Chip Rate",
			category = "RAN",
			class_type = ClassType,
			base_type = "SubNetwork",
			schema = "/resourceInventoryManagement/v3/schema/UtranCellTDDLcr",
			specification = Spec},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			State#state{parse_module = ?MODULE,
					parse_function = parse_rnc,
					parse_state = [UtranState#utran_state{tdd_lcr = TddLcrDn} | T],
					spec_cache = NewCache};
		{error, Reason} ->
			throw({add_resource, Reason})
	end.

%% @hidden
parse_iub({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_iub({startElement, _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_iub({endElement, _Uri, "IubLink", QName},
		#state{stack = Stack} = State) ->
	{[_ | T], NewStack} = pop(startElement, QName, Stack),
	parse_iub_attr(T, undefined, State#state{stack = NewStack}, []);
parse_iub({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

% @hidden
parse_iub_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
		undefined, State, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_iub_attr1(Attributes, undefined, State, Acc).
% @hidden
parse_iub_attr1([{endElement, {"un", "vnfParametersList" = Attr}} | T],
		undefined, State, Acc) ->
	% @todo vnfParametersListType
	parse_iub_attr1(T, Attr, State, Acc);
parse_iub_attr1([{endElement, {"un", "iubLinkUtranCell" = Attr}} | T],
		undefined, State, Acc) ->
	% @todo dnList
	parse_iub_attr1(T, Attr, State, Acc);
parse_iub_attr1([{characters, Chars} | T],
		"layerProtocolNameList" = Attr, State, Acc) ->
	parse_iub_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T],
		"aEnd" = Attr, State, Acc) ->
	parse_iub_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T],
		"zEnd" = Attr, State, Acc) ->
	parse_iub_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T],
		"linkType" = Attr, State, Acc) ->
	parse_iub_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T],
		"protocolVersion" = Attr, State, Acc) ->
	parse_iub_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T],
		"userLabel" = Attr, State, Acc) ->
	parse_iub_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T],
		"iubLinkATMChannelTerminationPoint" = Attr, State, Acc) ->
	parse_iub_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T],
		"iubLinkNodeBFunction" = Attr, State, Acc) ->
	parse_iub_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{characters, Chars} | T],
		Attr, State, Acc) ->
	parse_iub_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iub_attr1([{startElement, {"xn", "dn"}, _} | T],
		Attr, State, Acc) ->
	parse_iub_attr1(T, Attr, State, Acc);
parse_iub_attr1([{startElement, {"un", Attr}, _} | T],
		Attr, State, Acc) ->
	parse_iub_attr1(T, undefined, State, Acc);
parse_iub_attr1([{endElement, {"un", "attributes"}}],
		undefined, State, _Acc) ->
	State;
parse_iub_attr1([{endElement, {"un", Attr}} | T],
		undefined, State, Acc) ->
	parse_iub_attr1(T, Attr, State, Acc);
parse_iub_attr1([{endElement, {"xn", "dn"}} | T],
		Attr, State, Acc) ->
	parse_iub_attr1(T, Attr, State, Acc);
parse_iub_attr1([], undefined, #state{dn_prefix = [CurrentDn | _],
		parse_state = [#utran_state{rnc = RncId, iub = IubId} = UtranState | T],
		spec_cache = Cache} = State, _Acc) ->
	IubDn = CurrentDn ++ RncId ++ IubId,
	ClassType = "IubLink",
%	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = IubDn,
			description = "UMTS IUB interface",
			category = "RAN",
			class_type = ClassType,
			base_type = "SubNetwork",
			schema = "/resourceInventoryManagement/v3/schema/IubLink"},
%			specification = Spec},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			State#state{parse_module = ?MODULE,
					parse_function = parse_rnc,
					parse_state = [UtranState#utran_state{iub = IubDn} | T],
					spec_cache = Cache};
%					spec_cache = NewCache};
		{error, Reason} ->
			throw({add_resource, Reason})
	end.

%% @hidden
parse_iucs({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_iucs({startElement, _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_iucs({endElement, _Uri, "EP_IuCS", QName},
		#state{stack = Stack} = State) ->
	{[_ | T], NewStack} = pop(startElement, QName, Stack),
	parse_iucs_attr(T, undefined, State#state{stack = NewStack}, []);
parse_iucs({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

% @hidden
parse_iucs_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
		undefined, State, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_iucs_attr1(Attributes, undefined, State, Acc).
% @hidden
parse_iucs_attr1([{characters, Chars} | T],
		"userLabel" = Attr, State, Acc) ->
	parse_iucs_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iucs_attr1([{characters, Chars} | T],
		"farEndEntity" = Attr, State, Acc) ->
	parse_iucs_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iucs_attr1([{characters, Chars} | T],
		"connMscNumber" = Attr, State, Acc) ->
	parse_iucs_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_iucs_attr1([{characters, Chars} | T],
		Attr, State, Acc) ->
	parse_iucs_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iucs_attr1([{startElement, {"un", Attr}, _} | T],
		Attr, State, Acc) ->
	parse_iucs_attr1(T, undefined, State, Acc);
parse_iucs_attr1([{endElement, {"un", "attributes"}}],
		undefined, State, _Acc) ->
	State;
parse_iucs_attr1([{endElement, {"un", Attr}} | T],
		undefined, State, Acc) ->
	parse_iucs_attr1(T, Attr, State, Acc);
parse_iucs_attr1([], undefined, State, _Acc) ->
	State#state{parse_module = ?MODULE, parse_function = parse_rnc}.

%% @hidden
parse_iups({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_iups({startElement, _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_iups({endElement, _Uri, "EP_IuPS", QName},
		#state{stack = Stack} = State) ->
	{[_ | T], NewStack} = pop(startElement, QName, Stack),
	parse_iups_attr(T, undefined, State#state{stack = NewStack}, []);
parse_iups({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

% @hidden
parse_iups_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
		undefined, State, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_iups_attr1(Attributes, undefined, State, Acc).
% @hidden
parse_iups_attr1([{characters, Chars} | T],
		"userLabel" = Attr, State, Acc) ->
	parse_iups_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iups_attr1([{characters, Chars} | T],
		"farEndEntity" = Attr, State, Acc) ->
	parse_iups_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iups_attr1([{characters, Chars} | T],
		"connSgsnNumber" = Attr, State, Acc) ->
	parse_iups_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = list_to_integer(Chars)} | Acc]);
parse_iups_attr1([{characters, Chars} | T],
		Attr, State, Acc) ->
	parse_iups_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iups_attr1([{startElement, {"un", Attr}, _} | T],
		Attr, State, Acc) ->
	parse_iups_attr1(T, undefined, State, Acc);
parse_iups_attr1([{endElement, {"un", "attributes"}}],
		undefined, State, _Acc) ->
	State;
parse_iups_attr1([{endElement, {"un", Attr}} | T],
		undefined, State, Acc) ->
	parse_iups_attr1(T, Attr, State, Acc);
parse_iups_attr1([], undefined, State, _Acc) ->
	State#state{parse_module = ?MODULE, parse_function = parse_rnc}.

%% @hidden
parse_iur({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_iur({startElement, _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_iur({endElement, _Uri, "EP_Iur", QName},
		#state{stack = Stack} = State) ->
	{[_ | T], NewStack} = pop(startElement, QName, Stack),
	parse_iur_attr(T, undefined, State#state{stack = NewStack}, []);
parse_iur({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

% @hidden
parse_iur_attr([{startElement, {"un", "attributes"} = QName, []} | T1],
		undefined, State, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_iur_attr1(Attributes, undefined, State, Acc).
% @hidden
parse_iur_attr1([{characters, Chars} | T],
		"userLabel" = Attr, State, Acc) ->
	parse_iur_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iur_attr1([{characters, Chars} | T],
		"farEndEntity" = Attr, State, Acc) ->
	parse_iur_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iur_attr1([{characters, Chars} | T],
		"connectedRncId" = Attr, State, Acc) ->
	parse_iur_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iur_attr1([{characters, Chars} | T],
		Attr, State, Acc) ->
	parse_iur_attr1(T, Attr, State, [#resource_char{name = Attr,
			value = Chars} | Acc]);
parse_iur_attr1([{startElement, {"un", Attr}, _} | T],
		Attr, State, Acc) ->
	parse_iur_attr1(T, undefined, State, Acc);
parse_iur_attr1([{endElement, {"un", "attributes"}}],
		undefined, State, _Acc) ->
	State;
parse_iur_attr1([{endElement, {"un", Attr}} | T],
		undefined, State, Acc) ->
	parse_iur_attr1(T, Attr, State, Acc);
parse_iur_attr1([], undefined, State, _Acc) ->
	State#state{parse_module = ?MODULE, parse_function = parse_rnc}.

% @hidden
parse_fdd_rels([{startElement,
		{"gn", "GsmRelation"} = QName, _} | _] = Stack,
		#state{dn_prefix = [CurrentDn | _],
		parse_state = [#utran_state{rnc = RncId, fdd = FddId} | _T]} = State,
		Characteristics, #{gsmRel := GsmRels} = Acc) ->
	FddDn = CurrentDn ++ RncId ++ FddId,
	{Attributes, T} = pop(endElement, QName, Stack),
	Relation = gsm_relation(FddDn, Attributes),
	NewAcc = Acc#{gsmRel := [Relation | GsmRels]},
	parse_fdd_rels(T, State, Characteristics, NewAcc);
parse_fdd_rels([{startElement,
		{"un", "UtranRelation"} = QName, _} | _] = Stack,
		#state{dn_prefix = [CurrentDn | _],
		parse_state = [#utran_state{rnc = RncId, fdd = FddId} | _T]} = State,
		Characteristics, #{utranRel := UtranRels} = Acc) ->
	FddDn = CurrentDn ++ RncId ++ FddId,
	{Attributes, T} = pop(endElement, QName, Stack),
	Relation = utran_relation(FddDn, Attributes),
	NewAcc = Acc#{utranRel := [Relation | UtranRels]},
	parse_fdd_rels(T, State, Characteristics, NewAcc);
parse_fdd_rels(_FddStack, #state{dn_prefix = [CurrentDn | _],
		parse_state = [#utran_state{rnc = RncId, fdd = FddId,
		fdds = Fdds} = UtranState | T],
		spec_cache = Cache} = State, Characteristics, Acc) ->
	F1 = fun(gsmRel, [], Acc1) ->
				Acc1;
			(gsmRel, R, Acc1) ->
				[#resource_char{name = "gsmRelation",
						class_type = "GsmRelationList",
						schema = ?PathInventorySchema ++ "#definitions/GsmRelationList",
						value = lists:reverse(R)} | Acc1];
			(utranRel, [], Acc1) ->
				Acc1;
			(utranRel, R, Acc1) ->
				[#resource_char{name = "utranRelation",
						class_type = "UtranRelationList",
						schema = ?PathInventorySchema ++ "#definitions/UtranRelationList",
						value = lists:reverse(R)} | Acc1]
	end,
	NewCharacteristics = Characteristics ++ maps:fold(F1, [], Acc),
	FddDn = CurrentDn ++ RncId ++ FddId,
	ClassType = "UtranCellFDD",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = FddDn,
			description = "UMTS radio",
			category = "RAN",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/UtranCellFDD",
			specification = Spec,
			characteristic = NewCharacteristics},
	case im:add_resource(Resource) of
		{ok, #resource{}} ->
			State#state{parse_module = ?MODULE, parse_function = parse_rnc,
					parse_state = [UtranState#utran_state{fdds = [FddDn | Fdds]} | T],
					spec_cache = NewCache};
		{error, Reason} ->
			throw({add_resource, Reason})
	end.

%% @hidden
gsm_relation(DnPrefix, Stack) ->
	gsm_relation(Stack, [], DnPrefix, #{}).
%% @hidden
gsm_relation([{endElement, {"gn", "GsmRelation"} = QName} | T] = _Stack,
		[] = _State, DnPrefix, Acc) ->
	gsm_relation(T, [QName], DnPrefix, Acc);
gsm_relation([{endElement, {"xn", "VsDataContainer"} = QName} | _] = Stack,
		State, DnPrefix, Acc) ->
	{VsStack, T} = pop(startElement, QName, Stack),
	NewAcc = Acc#{"VsDataContainer" => vendor_specific(VsStack, DnPrefix)},
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{endElement, QName} | T] = _Stack, State, DnPrefix, Acc) ->
	gsm_relation(T, [QName | State], DnPrefix, Acc);
gsm_relation([{characters, Chars} | T],
		[{"gn", "adjacentCell"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("adjacentCell", Chars, Acc),
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{characters, Chars} | T],
		[{"gn", "bcchFrequency"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("bcchFrequency", Chars, Acc),
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{characters, Chars} | T],
		[{"gn", "ncc"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("ncc", Chars, Acc),
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{characters, Chars} | T],
		[{"gn", "bcc"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("bcc", Chars, Acc),
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{characters, Chars} | T],
		[{"gn", "lac"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("lac", Chars, Acc),
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{characters, Chars} | T],
		[{"gn", "isRemoveAllowed"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("isRemoveAllowed", Chars, Acc),
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{characters, Chars} | T],
		[{"gn", "isHOAllowed"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("isHOAllowed", Chars, Acc),
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{characters, Chars} | T],
		[{"gn", "isESCoveredBy"} | _] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("isESCoveredBy", Chars, Acc),
	gsm_relation(T, State, DnPrefix, NewAcc);
gsm_relation([{startElement, {"gn", "GsmRelation"} = QName, XmlAttr}],
		[QName], _DnPrefix, Acc) ->
	{_Uri, _Prefix, "id", RelId} = lists:keyfind("id", 3, XmlAttr),
	#{"@type" => "GsmRelation",
			"@schemaLocation" => ?PathInventorySchema ++ "#/definitions/GsmRelation",
			"value" => Acc#{"id" => RelId}};
gsm_relation([{startElement, QName, _} | T], [QName | State], DnPrefix, Acc) ->
	gsm_relation(T, State, DnPrefix, Acc).

%% @hidden
utran_relation(DnPrefix, Stack) ->
	utran_relation(Stack, [], DnPrefix, #{}).
%% @hidden
utran_relation([{endElement, {"un", "UtranRelation"} = QName} | T] = _Stack,
		[] = _State, DnPrefix, Acc) ->
	utran_relation(T, [QName], DnPrefix, Acc);
utran_relation([{endElement, {"xn", "VsDataContainer"} = QName} | _] = Stack,
		State, DnPrefix, Acc) ->
	{VsStack, T} = pop(startElement, QName, Stack),
	NewAcc = Acc#{"VsDataContainer" => vendor_specific(VsStack, DnPrefix)},
	utran_relation(T, State, DnPrefix, NewAcc);
utran_relation([{endElement, QName} | T] = _Stack, State, DnPrefix, Acc) ->
	utran_relation(T, [QName | State], DnPrefix, Acc);
utran_relation([{characters, Chars} | T],
		[{"un", "adjacentCell"}, {"un", "attributes"},
		{"un", "UtranRelation"}] = State, DnPrefix, Acc) ->
	NewAcc = attribute_add("adjacentCell", Chars, Acc),
	utran_relation(T, State, DnPrefix, NewAcc);
utran_relation([{startElement, {"un", "UtranRelation"} = QName, XmlAttr}],
		[QName], _DnPrefix, Acc) ->
	{_Uri, _Prefix, "id", RelId} = lists:keyfind("id", 3, XmlAttr),
	#{"@type" => "UtranRelation",
			"@schemaLocation" => ?PathInventorySchema ++ "#/definitions/UtranRelation",
			"value" => Acc#{"id" => RelId}};
utran_relation([{startElement, QName, _} | T], [QName | State], DnPrefix, Acc) ->
	utran_relation(T, State, DnPrefix, Acc).

% @hidden
vendor_specific(Stack, DnPrefix) ->
	vendor_specific(Stack, [], DnPrefix, #{}).
% @hidden
vendor_specific([{startElement, {"xn", "VsDataContainer"} = QName,
		XmlAttr} | T] = _Stack, [], DnPrefix, Acc) ->
	{_Uri, _Prefix, "id", ID} = lists:keyfind("id", 3, XmlAttr),
	vendor_specific(T, [QName], DnPrefix, Acc#{"id" => ID});
vendor_specific([{startElement, QName, _XmlAttr} | T] = _Stack,
		State, DnPrefix, Acc) ->
	vendor_specific(T, [QName | State], DnPrefix, Acc);
vendor_specific([{characters, Chars} | T],
		[{"xn", "vsDataType"}, {"xn", "attributes"},
		{"xn", "VsDataContainer"} | _] = State, DnPrefix, Acc) ->
	vendor_specific(T, State, DnPrefix, Acc#{"vsDataType" => Chars});
vendor_specific([{characters, Chars} | T],
		[{"xn", "vsDataFormatVersion"}, {"xn", "attributes"},
		{"xn", "VsDataContainer"} | _] = State,
		DnPrefix, Acc) ->
	vendor_specific(T, State, DnPrefix, Acc#{"vsDataFormatVersion" => Chars});
vendor_specific([{characters, _Chars} | T], [{"xn", "vsData"},
		{"xn", "attributes"}, {"xn", "VsDataContainer"} | _] = State,
		DnPrefix, Acc) ->
	% @todo handle vsData
	vendor_specific(T, State, DnPrefix, Acc);
vendor_specific([{endElement, {"xn", "VsDataContainer"} = QName}],
		[QName], _DnPrefix, Acc) ->
	Acc;
vendor_specific([{endElement, QName} | T],
		[QName | State], DnPrefix, Acc) ->
	vendor_specific(T, State, DnPrefix, Acc).

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

