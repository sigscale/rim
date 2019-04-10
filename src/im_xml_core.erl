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
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% export the im private API
-export([parse_msc/2, parse_iucs/2, parse_mgw/2, parse_ggsn/2, parse_sgsn/2,
		parse_iups/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

-define(PathCatalogSchema, "/resourceCatalogManagement/v3/resourceCatalogManagement").
-define(PathInventorySchema, "/resourceInventoryManagement/v3/resourceInventoryManagement").

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
			parse_state = #core_state{iucs = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_msc({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_msc({endElement, _Uri, "MscServerFunction", QName},
		[#state{dn_prefix = [MscDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	MscAttr = parse_msc_attr(T2, undefined, []),
	ClassType = "MscServerFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = MscDn,
			description = "Mobile Switch Center (MSC) Server",
			category = "RAN",
			class_type = ClassType,
			base_type = "SubNetwork",
			schema = "/resourceInventoryManagement/v3/schema/MscServerFunction",
			specification = Spec,
			characteristic = MscAttr},
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
parse_msc_attr([{startElement, {"cn", "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_msc_attr1(Attributes, undefined, Acc).
% @hidden
parse_msc_attr1([{endElement, {"cn", "vnfParametersList"}} | T],
		undefined, Acc) ->
	% @todo vnfParametersListType
	parse_msc_attr1(T, undefined, Acc);
parse_msc_attr1([{endElement, {"cn", "mccList"}} | T], undefined, Acc) ->
	% @todo longList
	parse_msc_attr1(T, undefined, Acc);
parse_msc_attr1([{endElement, {"cn", "mncList"}} | T], undefined, Acc) ->
	% @todo longList
	parse_msc_attr1(T, undefined, Acc);
parse_msc_attr1([{endElement, {"cn", "lacList"}} | T], undefined, Acc) ->
	% @todo longList
	parse_msc_attr1(T, undefined, Acc);
parse_msc_attr1([{endElement, {"cn", "sacList"}} | T], undefined, Acc) ->
	% @todo longList
	parse_msc_attr1(T, undefined, Acc);
parse_msc_attr1([{endElement, {"cn", "gcaList"}} | T], undefined, Acc) ->
	% @todo longList
	parse_msc_attr1(T, undefined, Acc);
parse_msc_attr1([{endElement, {"cn", "mscServerFunctionGsmCell"}} | T],
		undefined, Acc) ->
	% @todo dnList
	parse_msc_attr1(T, undefined, Acc);
parse_msc_attr1([{endElement, {"cn", "mscServerFunctionExternalGsmCell"}} | T],
		undefined, Acc) ->
	% @todo dnList
	parse_msc_attr1(T, undefined, Acc);
parse_msc_attr1([{endElement, {"cn", "mscServerFunctionCsMgwFunction"}} | T],
		undefined, Acc) ->
	% @todo dnList
	parse_msc_attr1(T, undefined, Acc);
parse_msc_attr1([{endElement, {"cn", "mscServerFunctionMscPool"}} | T],
		undefined, Acc) ->
	% @todo dnList
	parse_msc_attr1(T, undefined, Acc);
parse_msc_attr1([{endElement, {"cn", "nriList"}} | T], undefined, Acc) ->
	% @todo longList
	parse_msc_attr1(T, undefined, Acc);
parse_msc_attr1([{endElement, {"cn", "em"}} | T], undefined, Acc) ->
	parse_msc_attr1(T, undefined, Acc);
parse_msc_attr1([{endElement, {"xn", "dn"}} | T], undefined, Acc) ->
	parse_msc_attr1(T, undefined, Acc);
parse_msc_attr1([{endElement, {"cn", Attr}} | T], undefined, Acc) ->
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
parse_msc_attr1([{characters, _Chars} | T], undefined, Acc) ->
	parse_msc_attr1(T, undefined, Acc);
parse_msc_attr1([{startElement, {"cn", _Attr}, _} | T], undefined, Acc) ->
	parse_msc_attr1(T, undefined, Acc);
parse_msc_attr1([{startElement, {"xn", _Attr}, _} | T], undefined, Acc) ->
	parse_msc_attr1(T, undefined, Acc);
parse_msc_attr1([{startElement, {"cn", Attr}, _} | T], Attr, Acc) ->
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
parse_iucs({endElement, _Uri, "IucsLink", _QName},
		[_State, PrevState | T]) ->
	[PrevState | T];
parse_iucs({endElement, _Uri, _LocalName, QName},
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
			category = "RAN",
			class_type = ClassType,
			base_type = "SubNetwork",
			schema = "/resourceInventoryManagement/v3/schema/CsMgwFunction",
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
parse_mgw_attr([{startElement, {"cn", "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_mgw_attr1(Attributes, undefined, Acc).
% @hidden
parse_mgw_attr1([{endElement, {"cn", "vnfParametersList"}} | T],
		undefined, Acc) ->
	% @todo vnfParametersListType
	parse_mgw_attr1(T, undefined, Acc);
parse_mgw_attr1([{endElement, {"cn", "csMgwFunctionIucsLink"}} | T],
		undefined, Acc) ->
	% @todo dnList
	parse_mgw_attr1(T, undefined, Acc);
parse_mgw_attr1([{endElement, {"cn", "csMgwFunctionALink"}} | T],
		undefined, Acc) ->
	% @todo dnList
	parse_mgw_attr1(T, undefined, Acc);
parse_mgw_attr1([{endElement, {"xn", "dn"}} | T], undefined, Acc) ->
	parse_mgw_attr1(T, undefined, Acc);
parse_mgw_attr1([{endElement, {"cn", Attr}} | T], undefined, Acc) ->
	parse_mgw_attr1(T, Attr, Acc);
parse_mgw_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_mgw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_mgw_attr1([{characters, Chars} | T],
		"csMgwFunctionMscServerFunction" = Attr, Acc) ->
	parse_mgw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_mgw_attr1([{characters, _Chars} | T], undefined, Acc) ->
	parse_mgw_attr1(T, undefined, Acc);
parse_mgw_attr1([{startElement, {"xn", _Attr}, _} | T], undefined, Acc) ->
	parse_mgw_attr1(T, undefined, Acc);
parse_mgw_attr1([{startElement, {"cn", _Attr}, _} | T], undefined, Acc) ->
	parse_mgw_attr1(T, undefined, Acc);
parse_mgw_attr1([{startElement, {"cn", Attr}, _} | T], Attr, Acc) ->
	parse_mgw_attr1(T, undefined, Acc);
parse_mgw_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_ggsn({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ggsn({startElement,  _Uri, "VsDataContainer", QName,
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
			category = "RAN",
			class_type = ClassType,
			base_type = "SubNetwork",
			schema = "/resourceInventoryManagement/v3/schema/GgsnFunction",
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
parse_ggsn_attr([{startElement, {"cn", "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_ggsn_attr1(Attributes, undefined, Acc).
% @hidden
parse_ggsn_attr1([{endElement, {"cn", "vnfParametersList"}} | T],
		undefined, Acc) ->
	% @todo vnfParametersListType
	parse_ggsn_attr1(T, undefined, Acc);
parse_ggsn_attr1([{endElement, {"cn", "proceduralStatus"}} | T],
		undefined, Acc) ->
	% @todo proceduralStatusType
	parse_ggsn_attr1(T, undefined, Acc);
parse_ggsn_attr1([{endElement, {"sm", _Attr}} | T], undefined, Acc) ->
	parse_ggsn_attr1(T, undefined, Acc);
parse_ggsn_attr1([{endElement, {"cn", Attr}} | T], undefined, Acc) ->
	parse_ggsn_attr1(T, Attr, Acc);
parse_ggsn_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_ggsn_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_ggsn_attr1([{characters, _Chars} | T], undefined, Acc) ->
	parse_ggsn_attr1(T, undefined, Acc);
parse_ggsn_attr1([{startElement, {"cn", _Attr}, _} | T], undefined, Acc) ->
	parse_ggsn_attr1(T, undefined, Acc);
parse_ggsn_attr1([{startElement, {"sm", _Attr}, _} | T], undefined, Acc) ->
	parse_ggsn_attr1(T, undefined, Acc);
parse_ggsn_attr1([{startElement, {"cn", Attr}, _} | T], Attr, Acc) ->
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
			parse_state = #core_state{iups = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_sgsn({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_sgsn({endElement, _Uri, "SgsnFunction", QName},
		[#state{dn_prefix = [SgsnDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	SgsnAttr = parse_sgsn_attr(T2, undefined, []),
	ClassType = "SgsnFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = SgsnDn,
			description = "Serving GPRS Support Node (SGSN)",
			category = "RAN",
			class_type = ClassType,
			base_type = "SubNetwork",
			schema = "/resourceInventoryManagement/v3/schema/SgsnFunction",
			specification = Spec,
			characteristic = SgsnAttr},
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
parse_sgsn_attr([{startElement, {"cn", "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_sgsn_attr1(Attributes, undefined, Acc).
% @hidden
parse_sgsn_attr1([{endElement, {"cn", "vnfParametersList"}} | T],
		undefined, Acc) ->
	% @todo vnfParametersListType
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([{endElement, {"cn", "mccList"}} | T], undefined, Acc) ->
	% @todo longList
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([{endElement, {"cn", "mncList"}} | T], undefined, Acc) ->
	% @todo longList
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([{endElement, {"cn", "lacList"}} | T], undefined, Acc) ->
	% @todo longList
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([{endElement, {"cn", "racList"}} | T], undefined, Acc) ->
	% @todo longList
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([{endElement, {"cn", "sacList"}} | T], undefined, Acc) ->
	% @todo longList
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([{endElement, {"cn", "sgsnFunctionGsmCell"}} | T],
		undefined, Acc) ->
	% @todo dnList
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([{endElement, {"cn", "sgsnFunctionExternalGsmCell"}} | T],
		undefined, Acc) ->
	% @todo dnList
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([{endElement, {"cn", "proceduralStatus"}} | T],
		undefined, Acc) ->
	% @todo proceduralStatusType
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([{endElement, {"cn", "nriList"}} | T], undefined, Acc) ->
	% @todo longList
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([{endElement, {"sm", _Attr}} | T], undefined, Acc) ->
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([{endElement, {"xn", _Attr}} | T], undefined, Acc) ->
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([{endElement, {"cn", "em"}} | T], undefined, Acc) ->
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([{endElement, {"cn", Attr}} | T], undefined, Acc) ->
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
parse_sgsn_attr1([{characters, _Chars} | T], undefined, Acc) ->
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([{startElement, {"sm", _Attr}, _} | T], undefined, Acc) ->
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([{startElement, {"xn", _Attr}, _} | T], undefined, Acc) ->
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([{startElement, {"cn", _Attr}, _} | T], undefined, Acc) ->
	parse_sgsn_attr1(T, undefined, Acc);
parse_sgsn_attr1([{startElement, {"cn", Attr}, _} | T], Attr, Acc) ->
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
parse_iups({endElement, _Uri, "IupsLink", _QName},
		[_State, PrevState | T]) ->
	[PrevState | T];
parse_iups({endElement, _Uri, _LocalName, QName},
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
				{ok, #specification{id = Id, href = Href, name = Name,
						version = Version}} ->
					SpecRef = #specification_ref{id = Id, href = Href, name = Name,
							version = Version},
					{SpecRef, [SpecRef | Cache]};
				{error, Reason} ->
					throw({get_specification_name, Reason})
			end
	end.
