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
		[{[], [], "id", Id}] = Attributes}, [#state{dn_prefix = []} | _] = State) ->
	DnComponent = ",SubNetwork=" ++ Id,
	[#state{dn_prefix = [DnComponent],
			parse_module = im_xml_generic, parse_function = parse_subnetwork,
			parse_state = #generic_state{subnet = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_generic({startElement, _Uri, "SubNetwork", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",SubNetwork=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
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
parse_generic({endElement, _Uri, "SubNetwork", _QName},
		[_State, PrevState | T]) ->
	[PrevState | T];
parse_generic({endElement, _Uri, "MeContext", _QName},
		[_State, PrevState | T]) ->
	[PrevState | T];
parse_generic({endElement, _Uri, "ManagedElement", _QName},
		[_State, PrevState | T]) ->
	[PrevState | T];
parse_generic({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_subnetwork({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_subnetwork({startElement, _Uri, "SubNetwork", QName,
		[{[], [], "id", Id}] = Attributes}, State) ->
	DnComponent = ",SubNetwork=" ++ Id,
	[#state{parse_module = im_xml_generic, parse_function = parse_subnetwork,
			parse_state = #generic_state{subnet = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_subnetwork({startElement, _Uri, "meContext", QName,
		[{[], [], "id", Id}] = Attributes}, State) ->
% only for zte xml files
	DnComponent = ",meContext=" ++ Id,
	[#state{parse_module = im_xml_generic, parse_function = parse_mecontext,
			parse_state = #generic_state{me_context = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_subnetwork({startElement, _Uri, "MeContext", QName,
		[{[], [], "id", Id}] = Attributes}, State) ->
	DnComponent = ",MeContext=" ++ Id,
	[#state{parse_module = im_xml_generic, parse_function = parse_mecontext,
			parse_state = #generic_state{me_context = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_subnetwork({startElement, _Uri, "ManagedElement", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",ManagedElement=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_managed_element,
			parse_state = #generic_state{managed_element = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_subnetwork({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_subnetwork({endElement, _Uri, "meContext", _QName},
		[_State, PrevState | T]) ->
% only for zte xml files
	[PrevState | T];
parse_subnetwork({endElement, _Uri, "MeContext", _QName},
		[_State, PrevState | T]) ->
	[PrevState | T];
parse_subnetwork({endElement, _Uri, "ManagedElement", _QName},
		[_State, PrevState | T]) ->
	[PrevState | T];
parse_subnetwork({endElement, _Uri, "SubNetwork", _QName},
		[_State, #state{parse_module = ?MODULE,
		parse_function = parse_subnetwork} = PrevState | T]) ->
	[PrevState | T];
parse_subnetwork({endElement, _Uri, "SubNetwork", QName} = Event,
		[#state{stack = Stack} = State,
		#state{parse_module = M, parse_function = F} = PrevState | T]) ->
	StateStack = [State#state{stack = [{endElement, QName} | Stack]},
			PrevState | T],
	M:F(Event, StateStack);
parse_subnetwork({endElement,  _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_mecontext({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_mecontext({startElement, _Uri, "ManagedElement", QName,
		[{[], [], "id", Id}] = Attributes}, State) ->
	DnComponent = ",ManagedElement=" ++ Id,
	[#state{parse_module = im_xml_generic,
			parse_function = parse_managed_element,
			parse_state = #generic_state{managed_element = [DnComponent]},
			stack = [{startElement, QName, Attributes}]} | State];
parse_mecontext({startElement,  _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_mecontext({endElement, _Uri, "meContext", QName} = Event,
		[#state{stack = Stack} = State,
		#state{parse_module = M, parse_function = F} = PrevState | T]) ->
% only for zte xml files
	StateStack = [State#state{stack = [{endElement, QName} | Stack]},
			PrevState | T],
	M:F(Event, StateStack);
parse_mecontext({endElement, _Uri, "MeContext", QName} = Event,
		[#state{stack = Stack} = State,
		#state{parse_module = M, parse_function = F} = PrevState | T]) ->
	StateStack = [State#state{stack = [{endElement, QName} | Stack]},
			PrevState | T],
	M:F(Event, StateStack);
parse_mecontext({endElement, _Uri, "ManagedElement", _QName},
		[_State, PrevState | T]) ->
	[PrevState | T];
parse_mecontext({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_managed_element({characters, Chars},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_managed_element({startElement, _, "BssFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",BssFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_geran, parse_function = parse_bss,
			dn_prefix = [NewDn],
			parse_state = #geran_state{bss = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "NodeBFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",NodeBFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_utran, parse_function = parse_nodeb,
			dn_prefix = [NewDn],
			parse_state = #utran_state{nodeb = #{"id" => CurrentDn}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "RncFunction", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _T] = State) ->
	DnComponent = ",RncFunction=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{parse_module = im_xml_utran, parse_function = parse_rnc,
			dn_prefix = [NewDn],
			parse_state = #utran_state{rnc = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement, _, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes} = _Event, State) ->
	[#state{parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => Id}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement,  _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_managed_element({endElement, _, "VsDataContainer", _QName},
		[#state{parse_state = #generic_state{vs_data = VsData}},
		#state{parse_state = GenericState} = PrevState | T]) ->
	#generic_state{vs_data_container = VsDataContainer} = GenericState,
	[PrevState#state{parse_state = GenericState#generic_state{
			vs_data_container = [VsData | VsDataContainer]}} | T];
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
parse_managed_element({endElement, _Uri, "ManagedElement", QName} = Event,
		[#state{stack = Stack} = State,
		#state{parse_module = M, parse_function = F} = PrevState | T]) ->
	StateStack = [State#state{stack = [{endElement, QName} | Stack]},
			PrevState | T],
	M:F(Event, StateStack);
parse_managed_element({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

%% @hidden
parse_vsdata({startElement, _, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes}, State) ->
	[#state{parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => Id}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_vsdata({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_vsdata({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_vsdata({endElement, _Uri, "attributes", QName},
		[#state{parse_state = #generic_state{vs_data = VsData},
		stack = Stack} = State | T]) ->
	GenericState = State#state.parse_state,
	NewStack = [{endElement, QName} | Stack],
	{Attributes, _NextStack} = pop(startElement, QName, NewStack),
	NewVsData = parse_vsdata_attr(VsData, Attributes),
	[State#state{parse_state = GenericState#generic_state{
			vs_data = NewVsData},
			stack = NewStack} | T];
parse_vsdata({endElement, _, "VsDataContainer", _QName},
		[#state{parse_state = #generic_state{vs_data = VsData}},
		#state{parse_module = ?MODULE, parse_function = parse_vsdata,
		parse_state = GenericState} = PrevState | T]) ->
	#generic_state{vs_data_container = VsDataContainer} = GenericState,
	[PrevState#state{parse_state = GenericState#generic_state{
			vs_data_container = [VsData | VsDataContainer]}} | T];
parse_vsdata({endElement, _Uri, "VsDataContainer", _QName} = Event,
		[_State, #state{parse_module = M, parse_function = F} | _T] = StateStack) ->
	M:F(Event, StateStack);
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
