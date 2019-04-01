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
			parse_managed_element/2, parse_vsdata/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").


%%----------------------------------------------------------------------
%%  The im public API
%%----------------------------------------------------------------------

parse_generic({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_generic({startElement, _Uri, "SubNetwork", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{parse_state = undefined, stack = Stack} = State | T]) ->
	DnComponent = ",SubNetwork=" ++ Id,
	[State#state{parse_function = parse_subnetwork,
			parse_state = #generic_state{subnet = [DnComponent]},
			stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_generic({startElement, _Uri, "SubNetwork", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{parse_state = ParseState, stack = Stack} = State | T]) ->
	DnComponent = ",SubNetwork=" ++ Id,
	[State#state{parse_function = parse_subnetwork,
			parse_state = ParseState#generic_state{subnet = [DnComponent]},
			stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_generic({startElement, _Uri, "MeContext", QName,
		[{[], [], "id", Id}] = Attributes} = _Event,
		[#state{parse_state = undefined, stack = Stack} = State | T]) ->
	DnComponent = ",MeContext=" ++ Id,
	[State#state{parse_function = parse_mecontext,
			parse_state = #generic_state{me_context = [DnComponent]},
			stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_generic({startElement, _Uri, "MeContext", QName,
		[{[], [], "id", Id}] = Attributes} = _Event,
		[#state{parse_state = ParseState, stack = Stack} = State | T]) ->
	DnComponent = ",MeContext=" ++ Id,
	[State#state{parse_function = parse_mecontext,
			parse_state = ParseState#generic_state{me_context = [DnComponent]},
			stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_generic({startElement, _Uri, "ManagedElement", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{parse_state = undefined, stack = Stack} = State | T]) ->
	DnComponent = ",ManagedElement=" ++ Id,
	[State#state{parse_function = parse_managed_element,
			parse_state = #generic_state{managed_element = [DnComponent]},
			stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_generic({startElement, _Uri, "ManagedElement", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{parse_state = ParseState, stack = Stack} = State | T]) ->
	DnComponent = ",ManagedElement=" ++ Id,
	[State#state{parse_function = parse_managed_element,
			parse_state = ParseState#generic_state{managed_element = [DnComponent]},
			stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_generic({startElement,  _, _, QName, Attributes},
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
		[#state{parse_state = #generic_state{subnet = SubNetwork},
		stack = Stack} = State | T]) ->
	GenericState = State#state.parse_state,
	[H | _] = SubNetwork,
	DnComponent = H ++ "," ++ Id,
	[State#state{parse_state = GenericState#generic_state{
			subnet = [DnComponent | SubNetwork]},
			stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_subnetwork({startElement, _Uri, "MeContext", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{parse_state = #generic_state{me_context = MeContext},
		stack = Stack} = State | T]) ->
	GenericState = State#state.parse_state,
	DnComponent = ",MeContext=" ++ Id,
	[State#state{parse_function = parse_mecontext,
			parse_state = GenericState#generic_state{
			me_context = [DnComponent | MeContext]},
			stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_subnetwork({startElement, _Uri, "ManagedElement", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{parse_state = #generic_state{managed_element = MgdElement},
		stack = Stack} = State | T]) ->
	GenericState = State#state.parse_state,
	DnComponent = ",ManagedElement=" ++ Id,
	[State#state{parse_function = parse_managed_element,
			parse_state = GenericState#generic_state{
			managed_element = [DnComponent | MgdElement]},
			stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_subnetwork({startElement,  _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_subnetwork({endElement, _Uri, "SubNetwork", QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{parse_function = parse_generic,
			stack = [{endElement, QName} | Stack]} | T];
parse_subnetwork({endElement,  _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_mecontext({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_mecontext({startElement, _Uri, "ManagedElement", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{parse_state = #generic_state{managed_element = MgdEle},
		stack = Stack} = State | T]) ->
	GenericState = State#state.parse_state,
	DnComponent = ",ManagedElement=" ++ Id,
	[State#state{parse_function = parse_managed_element,
			parse_state = GenericState#generic_state{
			managed_element = [DnComponent | MgdEle]},
			stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_mecontext({startElement,  _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_mecontext({endElement, _Uri, "MeContext", QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{parse_function = parse_generic,
			stack = [{endElement, QName} | Stack]} | T];
parse_mecontext({endElement,  _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_managed_element({characters, Chars},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_managed_element({startElement, _, "BssFunction", QName,
		[{[], [], "id", Id}] = Attributes} = _Event,
		[#state{parse_state = #generic_state{subnet = [SubId | _],
		managed_element = [MeId | _]},
		dn_prefix = [DnPrefix | _]} | _T] = State) ->
	DnComponent = ",BssFunction=" ++ Id,
	CurrentDn = DnPrefix ++ SubId ++ MeId ++ DnComponent,
	[#state{parse_module = im_xml_geran, parse_function = parse_bss,
			dn_prefix = [CurrentDn],
			parse_state = #geran_state{bss = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "NodeBFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{parse_state = #generic_state{subnet = [SubId | _],
		managed_element = [MeId | _]},
		dn_prefix = [DnPrefix | _]} | _T] = State) ->
	DnComponent = ",NodeBFunction=" ++ Id,
	CurrentDn = DnPrefix ++ SubId ++ MeId ++ DnComponent,
	[#state{parse_module = im_xml_utran, parse_function = parse_nodeb,
			dn_prefix = [CurrentDn],
			parse_state = #utran_state{nodeb = #{"id" => CurrentDn}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "RncFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{parse_state = #generic_state{subnet = [SubId | _],
		managed_element = [MeId | _]}, dn_prefix = [DnPrefix | _]} | _T] = State) ->
	DnComponent = ",RncFunction=" ++ Id,
	CurrentDn = DnPrefix ++ SubId ++ MeId ++ DnComponent,
	[#state{parse_module = im_xml_utran, parse_function = parse_rnc,
			dn_prefix = [CurrentDn],
			parse_state = #utran_state{rnc = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes} = _Event, State) ->
% create new #state{} for VsDataContainer and push to state stack
% initialize #state.parse_state{vs_data = #{"id" => ID}
	[#state{parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = [#{"id" => Id}]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement,  _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_managed_element({endElement, _, "VsDataContainer", _QName} = _Event,
		[State1, #state{parse_state = #generic_state{vs_data = VsDataContainer}} = State2 | T]) ->
% pop the VsConatainer state
	#state{parse_state = #generic_state{vs_data = [VsData | _]}} = State1,
	NewState = State2#state{parse_state = #generic_state{vs_data = [VsData | VsDataContainer]}},
	[NewState | T];
parse_managed_element({endElement, _, "BssFunction", _QName},
		[#state{dn_prefix = [BssDn | _],
		parse_state = #geran_state{bss = #{"attributes" := BssAttr},
		btss = Btss}, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T]) ->
	BtsSiteMgr = #resource_char{name = "BtsSiteMgr", value = Btss},
	ClassType = "BssFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = BssDn,
			description = "GSM Base Station Subsystem (BSS)",
			category = "RAN",
			class_type = ClassType,
			base_type = "SubNetwork",
			schema = "/resourceInventoryManagement/v3/schema/BssFunction",
			specification = Spec,
			characteristic = lists:reverse([BtsSiteMgr | BssAttr])},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_managed_element({endElement, _, "NodeBFunction", _QName},
		[#state{dn_prefix = [NodebDn | _],
		parse_state = #utran_state{nodeb = #{"attributes" := NodeBAttr}},
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T]) ->
	ClassType = "NodeBFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = NodebDn,
			description = "UMTS Telecommunication Nodes",
			category = "RAN",
			class_type = ClassType,
			base_type = "SubNetwork",
			schema = "/resourceInventoryManagement/v3/schema/NodeBFunction",
			specification = Spec,
			characteristic = [NodeBAttr]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_managed_element({endElement, _, "RncFunction", _QName},
		[#state{dn_prefix = [RncDn | _],
		parse_state = #utran_state{rnc = #{"attributes" := RncAttr},
		fdds = Fdds}, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T]) ->
	UtranCellFDD = #resource_char{name = "UtranCellFDD", value = Fdds},
	ClassType = "RncFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = RncDn,
			description = "UMTS Radio Network Controller (RNC)",
			category = "RAN",
			class_type = ClassType,
			base_type = "SubNetwork",
			schema = "/resourceInventoryManagement/v3/schema/RncFunction",
			specification = Spec,
			characteristic = lists:reverse([UtranCellFDD | RncAttr])},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_managed_element({endElement, _Uri, "ManagedElement", QName},
%		[#state{parse_state = GenericState, stack = Stack} = State | T]) ->
		[#state{stack = Stack} = State | T]) ->
	[State#state{parse_function = parse_generic,
			stack = [{endElement, QName} | Stack]} | T];
parse_managed_element({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_vsdata({startElement, _, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes}, State) ->
	[#state{parse_state = #generic_state{vs_data = [#{"id" => Id}]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_vsdata({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_vsdata({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_vsdata({endElement, _Uri, "attributes", QName},
		[#state{parse_state = #generic_state{
		vs_data = [VsDataContainer | T2]}, stack = Stack} = State | T]) ->
	GenericState = State#state.parse_state,
	NewStack = [{endElement, QName} | Stack],
	{Attributes, _NextStack} = pop(startElement, QName, NewStack),
	VsAttr = parse_vsdata_attr(VsDataContainer, Attributes),
	[State#state{parse_state = GenericState#generic_state{
			vs_data = [VsDataContainer#{"attributes" => VsAttr} | T2]},
			stack = NewStack} | T];
parse_vsdata({endElement, _Uri, "VsDataContainer", _QName} = Event,
		[_State, #state{parse_module = M, parse_function = F} | _T] = State) ->
% peek into previous state for parse_function and call it
	M:F(Event, State);
parse_vsdata({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_vsdata_attr(VsDataContainer, Stack) ->
	parse_vsdata_attr(Stack, [], [], VsDataContainer).
%% @hidden
parse_vsdata_attr([{startElement, QName, _} = Event | T],
		State, OutStack, Acc) ->
	parse_vsdata_attr(T, [QName | State], [Event | OutStack], Acc);
parse_vsdata_attr([{characters, Chars} | T],
		[{"xn", "vsDataType"}, {"xn","attributes"}] = State, OutStack, Acc) ->
	NewAcc = Acc#{"vsDataType" => Chars},
	parse_vsdata_attr(T, State, OutStack, NewAcc);
parse_vsdata_attr([{characters, Chars} | T],
		[{"xn", "vsDataFormatVersion"}, {"xn", "vsDataType"},
		{"xn","attributes"}] = State, OutStack, Acc) ->
	NewAcc = Acc#{"vsDataFormatVersion" => Chars},
	parse_vsdata_attr(T, State, OutStack, NewAcc);
parse_vsdata_attr([{characters, _Chars} = Event | T],
		State, OutStack, Acc) ->
	parse_vsdata_attr(T, State, [Event | OutStack], Acc);
parse_vsdata_attr([], _State, OutStack,
		#{"vsDataFormatVersion" := "ZTESpecificAttributes"} = Acc) ->
	im_xml_zte:parse_zte_attr(OutStack, Acc);
parse_vsdata_attr([], _State, _OutStack, Acc) ->
	Acc;
parse_vsdata_attr([{endElement, _QName} = Event | T] = _InStack,
		State, OutStack, Acc) ->
	parse_vsdata_attr(T, State, [Event | OutStack], Acc).

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
