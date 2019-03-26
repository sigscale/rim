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

-record(generic_state,
		{subnet = [] :: [string()],
		me_context = [] :: [string()],
		managed_element = [] :: [string()],
		vs_data = [] :: [string()]}).

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
			parse_state = [GenericState#generic_state{
			managed_element = [DnComponent | MgdElement]} | T],
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
parse_managed_element({startElement, _, "BssFunction", _, _Attributes} = Event,
		[#state{parse_state = #generic_state{subnet = [SubId | _],
		managed_element = [MeId | _]},
		dn_prefix = [DnPrefix | _]} | _T] = State) ->
	Mod = im_xml_geran,
	F = parse_bss,
	CurrentDn = DnPrefix ++ SubId ++ MeId,
	NewState = #state{parse_module = Mod, parse_function = F,
			dn_prefix = [CurrentDn]},
	Mod:F(Event, [NewState | State]);
parse_managed_element({startElement, _, "NodeBFunction", _, _Attributes} = Event,
		[#state{parse_state = #generic_state{subnet = [SubId | _],
		managed_element = [MeId | _]},
		dn_prefix = [DnPrefix | _]} | _T] = State) ->
	Mod = im_xml_utran,
	F = parse_nodeb,
	CurrentDn = DnPrefix ++ SubId ++ MeId,
	NewState = #state{parse_module = Mod, parse_function = F,
			dn_prefix = [CurrentDn]},
	Mod:F(Event, [NewState | State]);
parse_managed_element({startElement, _, "RncFunction", _, _Attributes} = Event,
		[#state{parse_state = #generic_state{subnet = [SubId | _],
		managed_element = [MeId | _]}, dn_prefix = [DnPrefix | _]} | _T] = State) ->
	Mod = im_xml_utran,
	F = parse_rnc,
	CurrentDn = DnPrefix ++ SubId ++ MeId,
	NewState = State#state{parse_module = Mod, parse_function = F,
			dn_prefix = [CurrentDn]},
	Mod:F(Event, [NewState | State]);
parse_managed_element({startElement, _, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes} = _Event, State) ->
% create new #state{} for VsDataContainer and push to state stack
% initialize #state.parse_state{vs_data = #{"id" => ID}
	[#state{parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => Id}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_managed_element({startElement,  _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_managed_element({endElement, _, "VsDataContainer", _QName} = _Event,
		[State1, #state{parse_state = #generic_state{vs_data = VsDataContainer}} = State2 | T]) ->
% pop the VsConatainer state
	#state{parse_state = #generic_state{vs_data = VsData}} = State1,
	NewState = State2#state{parse_state = #generic_state{vs_data = [VsData | VsDataContainer]}},
	[NewState | T];
parse_managed_element({endElement, _Uri, "ManagedElement", QName},
		[#state{parse_state = GenericState, stack = Stack} = State | T]) ->
erlang:display({?MODULE, ?LINE, GenericState#generic_state.vs_data}),
	[State#state{parse_function = parse_generic,
			stack = [{endElement, QName} | Stack]} | T];
parse_managed_element({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%% @hidden
parse_vsdata({startElement, _, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes}, State) ->
	[#state{parse_state = #generic_state{vs_data = #{"id" => Id}},
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
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

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
