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
			parse_managed_element/2, parse_vsdata/2, parse_vsdata1/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").

-record(state,
		{parse_module :: atom(),
		parse_function :: atom(),
		parse_state :: term(),
		dn_prefix = [] :: string(),
		subnet = [] :: string(),
		me_context = [] :: string(),
		managed_element = [] :: string(),
		vs_data = [] :: string(),
		stack = [] :: list(),
		spec_cache = [] :: [specification_ref()]}).
-type state() :: #state{}.

%-record(generic_state,
%		{subnet = [] :: string(),
%		me_context = [] :: string(),
%		managed_element = [] :: string(),
%		vs_data = [] :: [string()]}).

%%----------------------------------------------------------------------
%%  The im public API
%%----------------------------------------------------------------------

parse_generic({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_generic({startElement, _Uri, "SubNetwork", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{subnet = [], stack = Stack} = State) ->
	DnComponent = ",SubNetwork=" ++ Id,
	State#state{parse_function = parse_subnetwork, subnet = DnComponent,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_generic({startElement, _Uri, "MeContext", _QName,
		_Attributes} = _Event, State) ->
	State#state{parse_function = parse_mecontext};
parse_generic({startElement, _Uri, "ManagedElement", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{managed_element = [], stack = Stack} = State) ->
	DnComponent = ",ManagedElement=" ++ Id,
	State#state{parse_function = parse_managed_element,
			managed_element = DnComponent,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_generic({startElement,  _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_generic({endElement,  _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%% @hidden
parse_subnetwork({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_subnetwork({startElement, _Uri, "SubNetwork", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{subnet = SubNetwork, stack = Stack} = State) ->
	DnComponent = SubNetwork ++ "," ++ Id,
	State#state{subnet = DnComponent,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_subnetwork({startElement, _Uri, "MeContext", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{me_context = [], stack = Stack} = State) ->
	DnComponent = ",MeContext=" ++ Id,
	State#state{parse_function = parse_mecontext, me_context = DnComponent,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_subnetwork({startElement, _Uri, "ManagedElement", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{managed_element = [], stack = Stack} = State) ->
	DnComponent = ",ManagedElement=" ++ Id,
	State#state{parse_function = parse_managed_element,
			managed_element = DnComponent,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_subnetwork({startElement,  _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_subnetwork({endElement, _Uri, "SubNetwork", _QName}, State) ->
	State;
parse_subnetwork({endElement,  _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%% @hidden
parse_mecontext({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_mecontext({startElement, _Uri, "ManagedElement", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{managed_element = [], stack = Stack} = State) ->
	DnComponent = ",ManagedElement=" ++ Id,
	State#state{parse_function = parse_managed_element,
			managed_element = DnComponent,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_mecontext({startElement,  _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_mecontext({endElement, _Uri, "MeContext", _QName}, State) ->
	State;
parse_mecontext({endElement,  _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%% @hidden
parse_managed_element({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_managed_element({startElement, _, "BssFunction", _, _Attributes} = Event, State) ->
	Mod = im_xml_geran,
	F = parse_bss,
	NewState = State#state{parse_module = Mod, parse_function = F},
	Mod:F(Event, NewState);
parse_managed_element({startElement, _, "NodeBFunction", _, _Attributes} = Event, State) ->
	Mod = im_xml_utran,
	F = parse_nodeb,
	NewState = State#state{parse_module = Mod, parse_function = F},
	Mod:F(Event, NewState);
parse_managed_element({startElement, _, "RncFunction", _, _Attributes} = Event, State) ->
	Mod = im_xml_utran,
	F = parse_rnc,
	NewState = State#state{parse_module = Mod, parse_function = F},
	Mod:F(Event, NewState);
parse_managed_element({startElement, _, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{vs_data = [], stack = Stack} = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	State#state{parse_function = parse_vsdata, parse_state = #{vsData => []},
			vs_data = DnComponent,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_managed_element({startElement,  _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_managed_element({endElement, _Uri, "ManagedElement", _QName}, State) ->
	State;
parse_managed_element({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%% @hidden
parse_vsdata({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_vsdata({startElement, _, "VsDataContainer", QName,
		[{[], [], "id", Id}]},
		#state{parse_state = #{vsData := VsContainers}, vs_data = VsId,
		stack = [{endElement, {"xn","attributes"}} | Stack]} = State) ->
	DnComponent = VsId ++ "." ++ Id,
	#state{stack = NewStack} = State,
	{Attributes, _Rest} = pop(startElement, QName, NewStack),
	VsData = parse_vsdata_attr(DnComponent, Attributes),
	State#state{parse_function = parse_vsdata1,
			parse_state = #{vsData => [VsData | VsContainers]},
			vs_data = DnComponent,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_vsdata({startElement, _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_vsdata({endElement, _Uri, "VsDataContainer", QName},
		#state{parse_state = #{vsData := VsContainers},
		dn_prefix = DnPrefix, subnet = SubId, me_context = MeId,
		managed_element = MgdId, vs_data = VsId,
		stack = [QName | Stack]} = State) ->
	VsDn = DnPrefix ++ SubId ++ MeId ++ MgdId ++ VsId,
	NewStack = [{endElement, QName}| Stack],
	{Attributes, _NextStack} = pop(startElement, QName, NewStack),
	VsData = parse_vsdata_attr(VsDn, Attributes),
	State#state{parse_state = #{vsData => [VsData | VsContainers]},
			stack = NewStack};
parse_vsdata({endElement, _Uri, _LocalName, QName},
		#state{parse_state = #{vsData := VsContainers}, dn_prefix = DnPrefix,
		subnet = SubId, me_context = MeId, managed_element = MgdId, vs_data = VsId,
		stack = [{endElement, {"xn","VsDataContainer"}} | _Stack]} = State) ->
	VsDn = DnPrefix ++ SubId ++ MeId ++ MgdId ++ VsId,
	#state{stack = NewStack} = State,
	{Attributes, _NextStack} = pop(startElement, {"xn","VsDataContainer"},
			NewStack),
	VsData = parse_vsdata_attr(VsDn, Attributes),
	State#state{parse_state = #{vsData => [VsData | VsContainers]},
			stack = [{endElement, QName} | NewStack]};
parse_vsdata({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%% @hidden
parse_vsdata1({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_vsdata1({startElement, _, "VsDataContainer", QName,
		[{[], [], "id", Id}]},
		#state{parse_state = #{vsData := VsContainers}, vs_data = VsId,
		stack = [{endElement, {"xn","attributes"}} | Stack]} = State) ->
	DnComponent = VsId ++ "." ++ Id,
	#state{stack = NewStack} = State,
	{Attributes, _Rest} = pop(startElement, QName, NewStack),
	VsData = parse_vsdata_attr(DnComponent, Attributes),
	State#state{parse_function = parse_vsdata, vs_data = DnComponent,
			parse_state = #{vsData => [VsData | VsContainers]},
			stack = [{startElement, QName, Attributes} | Stack]};
parse_vsdata1({startElement,  _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_vsdata1({endElement, _Uri, "VsDataContainer", QName},
		#state{parse_state = #{vsData := VsContainers}, dn_prefix = DnPrefix,
		subnet = SubId, me_context = MeId, managed_element = MgdId,
		vs_data = VsId, stack = [QName | Stack]} = State) ->
	VsDn = DnPrefix ++ SubId ++ MeId ++ MgdId ++ VsId,
	NewStack = [{endElement, QName} | Stack],
	{Attributes, _NextStack} = pop(startElement, QName, NewStack),
	VsData = parse_vsdata_attr(VsDn, Attributes),
	State#state{parse_state = #{vsData => [VsData | VsContainers]},
			stack = NewStack};
parse_vsdata1({endElement, _Uri, _LocalName, QName},
		#state{parse_state = #{vsData := VsContainers}, dn_prefix = DnPrefix,
		subnet = SubId, me_context = MeId, managed_element = MgdId, vs_data = VsId,
		stack = [{endElement, {"xn","VsDataContainer"}} | _Stack]} = State) ->
	VsDn = DnPrefix ++ SubId ++ MeId ++ MgdId ++ VsId,
	#state{stack = NewStack} = State,
	{Attributes, _NextStack} = pop(startElement, {"xn","VsDataContainer"},
			NewStack),
	VsData = parse_vsdata_attr(VsDn, Attributes),
	State#state{parse_state = #{vsData => [VsData | VsContainers]},
			stack = [{endElement, QName} | NewStack]};
parse_vsdata1({endElement, _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

% @hidden
parse_vsdata_attr(DnPrefix, Stack) ->
	parse_vsdata_attr(Stack, [], DnPrefix, [], #{}).
% @hidden
parse_vsdata_attr([{startElement, QName, _} = Event | T],
		State, DnPrefix, OutStack, Acc) ->
	parse_vsdata_attr(T, [QName | State], DnPrefix, [Event | OutStack], Acc);
parse_vsdata_attr([{characters, Chars} | T],
		[{"xn", "vsDataType"}, {"xn","attributes"},
		{"xn", "VsDataContainer"}] = State, DnPrefix, OutStack, Acc) ->
	NewAcc = attribute_add("vsDataType", Chars, Acc),
	parse_vsdata_attr(T, State, DnPrefix, OutStack, NewAcc);
parse_vsdata_attr([{characters, Chars} | T],
		[{"xn", "vsDataFormatVersion"}, {"xn", "vsDataType"},
		{"xn","attributes"}, {"xn", "VsDataContainer"}] = State,
		DnPrefix, OutStack, Acc) ->
	NewAcc = attribute_add("vsDataFormatVersion", Chars, Acc),
	parse_vsdata_attr(T, State, DnPrefix, OutStack, NewAcc);
parse_vsdata_attr([{characters, _Chars} = Event | T],
		State, DnPrefix, OutStack, Acc) ->
	parse_vsdata_attr(T, State, DnPrefix, [Event | OutStack], Acc);
parse_vsdata_attr([], _State, DnPrefix, OutStack,
		#{"attributes" := #{"vsDataFormatVersion" := "ZTESpecificAttributes"}} = Acc) ->
	im_xml_zte:parse_zte_attr(OutStack, DnPrefix, Acc);
parse_vsdata_attr([{endElement, _, "VsDataContainer", _QName}],
		_State, DnPrefix, OutStack,
		#{"attributes" := #{"vsDataFormatVersion" := "ZTESpecificAttributes"}} = Acc) ->
	im_xml_zte:parse_zte_attr(OutStack, DnPrefix, Acc);
%parse_vsdata_attr([{endElement, {"xn", "VsDataContainer"}}],
%		[{"xn", "VsDataContainer"} | _], DnPrefix, OutStack,
%		#{"vsDataFormatVersion" := "ZTESpecificAttributes"} = Acc) ->
%	im_xml_zte:parse_zte_attr(OutStack, DnPrefix, Acc);
parse_vsdata_attr([{endElement, _QName} = Event | T] = _InStack,
		State, DnPrefix, OutStack, Acc) ->
	parse_vsdata_attr(T, State, DnPrefix, [Event | OutStack], Acc).

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
