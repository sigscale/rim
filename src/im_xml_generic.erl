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

parse_generic({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_generic({startElement, _Uri, "SubNetwork", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{parse_state = StateStack, stack = Stack} = State) ->
	DnComponent = ",SubNetwork=" ++ Id,
	State#state{parse_function = parse_subnetwork,
			parse_state = [#generic_state{subnet = [DnComponent]} | StateStack],
			stack = [{startElement, QName, Attributes} | Stack]};
parse_generic({startElement, _Uri, "MeContext", QName,
		[{[], [], "id", Id}] = Attributes} = _Event,
		#state{parse_state = StateStack, stack = Stack} = State) ->
	DnComponent = ",MeContext=" ++ Id,
	State#state{parse_function = parse_mecontext,
			parse_state = [#generic_state{me_context = [DnComponent]} | StateStack],
			stack = [{startElement, QName, Attributes} | Stack]};
parse_generic({startElement, _Uri, "ManagedElement", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{parse_state = StateStack, stack = Stack} = State) ->
	DnComponent = ",ManagedElement=" ++ Id,
	State#state{parse_function = parse_managed_element,
			parse_state = [#generic_state{managed_element = [DnComponent]} | StateStack],
			stack = [{startElement, QName, Attributes} | Stack]};
parse_generic({startElement,  _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_generic({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%% @hidden
parse_subnetwork({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_subnetwork({startElement, _Uri, "SubNetwork", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{parse_state = [#generic_state{subnet = SubNetwork} = GenericState | T],
		stack = Stack} = State) ->
	DnComponent = SubNetwork ++ "," ++ Id,
	State#state{parse_state = [GenericState#generic_state{subnet = DnComponent} | T],
			stack = [{startElement, QName, Attributes} | Stack]};
parse_subnetwork({startElement, _Uri, "MeContext", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{parse_state = [#generic_state{me_context = MeContext} = GenericState | T],
		stack = Stack} = State) ->
	DnComponent = ",MeContext=" ++ Id,
	State#state{parse_function = parse_mecontext,
			parse_state = [GenericState#generic_state{
			me_context = [DnComponent | MeContext]} | T],
			stack = [{startElement, QName, Attributes} | Stack]};
parse_subnetwork({startElement, _Uri, "ManagedElement", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{parse_state = [#generic_state{
		managed_element = MgdElement} = GenericState | T],
		stack = Stack} = State) ->
	DnComponent = ",ManagedElement=" ++ Id,
	State#state{parse_function = parse_managed_element,
			parse_state = [GenericState#generic_state{
			managed_element = [DnComponent | MgdElement]} | T],
			stack = [{startElement, QName, Attributes} | Stack]};
parse_subnetwork({startElement,  _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_subnetwork({endElement, _Uri, "SubNetwork", _QName}, State) ->
	State#state{parse_function = parse_generic};
parse_subnetwork({endElement,  _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%% @hidden
parse_mecontext({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_mecontext({startElement, _Uri, "ManagedElement", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{parse_state = [#generic_state{managed_element = MgdEle} = GenericState | T],
		stack = Stack} = State) ->
	DnComponent = ",ManagedElement=" ++ Id,
	State#state{parse_function = parse_managed_element,
			parse_state = [GenericState#generic_state{
			managed_element = [DnComponent | MgdEle]} | T],
			stack = [{startElement, QName, Attributes} | Stack]};
parse_mecontext({startElement,  _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_mecontext({endElement, _Uri, "MeContext", _QName}, State) ->
	State#state{parse_function = parse_generic};
parse_mecontext({endElement,  _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%% @hidden
parse_managed_element({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_managed_element({startElement, _, "BssFunction", _, _Attributes} = Event,
		#state{parse_state = [#generic_state{subnet = [SubId | _],
		managed_element = [MeId | _]} | _], dn_prefix = [DnPrefix]} = State) ->
	Mod = im_xml_geran,
	F = parse_bss,
	CurrentDn = DnPrefix ++ SubId ++ MeId,
	NewState = State#state{parse_module = Mod, parse_function = F,
			dn_prefix = [CurrentDn | State#state.dn_prefix]},
	Mod:F(Event, NewState);
parse_managed_element({startElement, _, "NodeBFunction", _, _Attributes} = Event,
		#state{parse_state = [#generic_state{subnet = [SubId | _],
		managed_element = [MeId | _]} | _], dn_prefix = [DnPrefix]} = State) ->
	Mod = im_xml_utran,
	F = parse_nodeb,
	CurrentDn = DnPrefix ++ SubId ++ MeId,
	NewState = State#state{parse_module = Mod, parse_function = F,
			dn_prefix = [CurrentDn | State#state.dn_prefix]},
	Mod:F(Event, NewState);
parse_managed_element({startElement, _, "NodeBFunction", _, _Attributes} = Event,
		State) ->
	Mod = im_xml_utran,
	F = parse_nodeb,
	NewState = State#state{parse_module = Mod, parse_function = F},
	Mod:F(Event, NewState);
parse_managed_element({startElement, _, "RncFunction", _, _Attributes} = Event,
		#state{parse_state = [#generic_state{subnet = [SubId | _],
		managed_element = [MeId | _]} | _], dn_prefix = [DnPrefix]} = State) ->
	Mod = im_xml_utran,
	F = parse_rnc,
	CurrentDn = DnPrefix ++ SubId ++ MeId,
	NewState = State#state{parse_module = Mod, parse_function = F,
			dn_prefix = [CurrentDn | State#state.dn_prefix]},
	Mod:F(Event, NewState);
parse_managed_element({startElement, _, "RncFunction", _, _Attributes} = Event,
		State) ->
	Mod = im_xml_utran,
	F = parse_rnc,
	NewState = State#state{parse_module = Mod, parse_function = F},
	Mod:F(Event, NewState);
parse_managed_element({startElement, _, "VsDataContainer", _QName,
		_Attributes} = Event, State) ->
	NewState = State#state{parse_function = parse_vsdata},
	parse_vsdata(Event, NewState);
parse_managed_element({endElement, _, "VsDataContainer", _QName} = Event, State) ->
	NewState = State#state{parse_function = parse_vsdata},
	parse_vsdata(Event, NewState);
parse_managed_element({startElement,  _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_managed_element({endElement, _Uri, "ManagedElement", _QName},
		#state{parse_state = [GenericState | _]} = State) ->
erlang:display({?MODULE, ?LINE, GenericState#generic_state.vs_data}),
	State#state{parse_function = parse_generic};
parse_managed_element({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%% @hidden
parse_vsdata({startElement, _, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{parse_state = ParseState, stack = Stack} = State) ->
	VsDataContainer = #{"id" => Id},
	GenericState = #generic_state{vs_data = [VsDataContainer]},
	State#state{parse_state = [GenericState | ParseState],
			stack = [{startElement, QName, Attributes} | Stack]};
parse_vsdata({startElement, _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_vsdata({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_vsdata({endElement, _Uri, "attributes", QName},
		#state{parse_state = [#generic_state{
		vs_data = [VsDataContainer | T2]} = GenericState | T1],
		stack = Stack} = State) ->
	NewStack = [{endElement, QName} | Stack],
	{Attributes, _NextStack} = pop(startElement, QName, NewStack),
	VsAttr = parse_vsdata_attr(VsDataContainer, Attributes),
	State#state{parse_state = [GenericState#generic_state{
			vs_data = [VsDataContainer#{"attributes" => VsAttr} | T2]} | T1],
			stack = NewStack};
parse_vsdata({endElement, _Uri, "VsDataContainer", QName},
		#state{parse_state = [GenericState1, GenericState2 | ParseState],
		stack = Stack} = State) ->
	#generic_state{vs_data = [VsDataContainer | T]} = GenericState2,
	State#state{parse_state = [GenericState2#generic_state{
			vs_data = [VsDataContainer#{container => GenericState1} | T]} | ParseState],
			parse_function = parse_managed_element,
			stack = [{endElement, QName} | Stack]};
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
