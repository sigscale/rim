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
-module(im_xml_pee).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% export the im private API
-export([parse_pee_me/2, parse_me_description/2, parse_me_config/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

-define(PathCatalogSchema, "/resourceCatalogManagement/v3/resourceCatalogManagement").
-define(PathInventorySchema, "/resourceInventoryManagement/v3/resourceInventoryManagement").

%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

parse_pee_me({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_pee_me({startElement,  _Uri, "PEEMEDescription", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",PEEMEDescription=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_pee, parse_function = parse_me_description,
			parse_state = #pee_state{me_description = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_pee_me({startElement,  _Uri, "PEEConfigInformation", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",PEEConfigInformation=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_pee, parse_function = parse_me_config,
			parse_state = #pee_state{me_config = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_pee_me({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_pee_me({endElement, _Uri, "PEEMonitoredEntity", QName},
		[#state{dn_prefix = [MeDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	MeAttr = parse_pee_me_attr(T2, undefined, []),
	ClassType = "PEEMonitoredEntity",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = MeDn,
			description = "PEE Monitored Entity (ME)",
			category = "PEE",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/PEEMonitoredEntity",
			specification = Spec,
			characteristic = lists:reverse(MeAttr)},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_pee_me({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

parse_pee_me_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_pee_me_attr1(Attributes, undefined, Acc).
%% @hidden
parse_pee_me_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_pee_me_attr1(T, Attr, Acc);
parse_pee_me_attr1([{characters, Chars} | T], "mEId" = Attr, Acc) ->
	parse_pee_me_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_pee_me_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_pee_me_attr1(T, undefined, Acc);
parse_pee_me_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_me_description({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_me_description({startElement, _Uri, _LocalName, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_me_description({endElement, _Uri, "PEEMEDescription", QName},
		[#state{stack = Stack, parse_state = #pee_state{me_description = MeDescription}},
		#state{parse_state = PeeState} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NewMeDescription = parse_me_description_attr(T2, undefined, MeDescription),
	#pee_state{me = MonitoredEntity} = PeeState,
	NewMe = choice_add(NewMeDescription, MonitoredEntity),
	[PrevState#state{parse_state = PeeState#pee_state{me = NewMe}} | T1];
parse_me_description({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_me_description_attr([{startElement, {_, "attributes"} = QName, []} | T],
		undefined, Acc) ->
	{[_ | Attributes], _Rest} = pop(endElement, QName, T),
	parse_me_description_attr1(Attributes, undefined, Acc).
% @hidden
parse_me_description_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_me_description_attr1(T, Attr, Acc);
parse_me_description_attr1([{characters, Chars} | T], "siteIdentification" = Attr, Acc) ->
	NewAcc = attribute_add("siteIdentification", Chars, Acc),
	parse_me_description_attr1(T, Attr, NewAcc);
parse_me_description_attr1([{characters, Chars} | T], "siteLatitude" = Attr, Acc) ->
	NewAcc = attribute_add("siteLatitude", Chars, Acc),
	parse_me_description_attr1(T, Attr, NewAcc);
parse_me_description_attr1([{characters, Chars} | T], "siteLongitude" = Attr, Acc) ->
	NewAcc = attribute_add("siteLongitude", Chars, Acc),
	parse_me_description_attr1(T, Attr, NewAcc);
parse_me_description_attr1([{characters, Chars} | T], "siteDescription" = Attr, Acc) ->
	NewAcc = attribute_add("siteDescription", Chars, Acc),
	parse_me_description_attr1(T, Attr, NewAcc);
parse_me_description_attr1([{characters, Chars} | T], "equipmentType" = Attr, Acc) ->
	NewAcc = attribute_add("equipmentType", Chars, Acc),
	parse_me_description_attr1(T, Attr, NewAcc);
parse_me_description_attr1([{characters, Chars} | T], "environmentType" = Attr, Acc) ->
	NewAcc = attribute_add("environmentType", Chars, Acc),
	parse_me_description_attr1(T, Attr, NewAcc);
parse_me_description_attr1([{characters, Chars} | T], "powerInterface" = Attr, Acc) ->
	NewAcc = attribute_add("powerInterface", Chars, Acc),
	parse_me_description_attr1(T, Attr, NewAcc);
parse_me_description_attr1([{characters, Chars} | T], "xcuDguDescription" = Attr, Acc) ->
	NewAcc = attribute_add("xcuDguDescription", Chars, Acc),
	parse_me_description_attr1(T, Attr, NewAcc);
parse_me_description_attr1([{characters, Chars} | T], "sensorDescription" = Attr, Acc) ->
	NewAcc = attribute_add("sensorDescription", Chars, Acc),
	parse_me_description_attr1(T, Attr, NewAcc);
parse_me_description_attr1([{characters, Chars} | T], "vSRmsDescription" = Attr, Acc) ->
	NewAcc = attribute_add("vSRmsDescription", Chars, Acc),
	parse_me_description_attr1(T, Attr, NewAcc);
parse_me_description_attr1([{startElement, {_, Attr}, []} | T], Attr, Acc) ->
	parse_me_description_attr1(T, undefined, Acc);
parse_me_description_attr1([],  _Attr, Acc) ->
	Acc.

%% @hidden
parse_me_config({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_me_config({startElement, _Uri, _LocalName, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_me_config({endElement, _Uri, "PEEConfigInformation", QName},
		[#state{stack = Stack, parse_state = #pee_state{me_config = MeConfig}},
		#state{parse_state = PeeState} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NewMeConfig = parse_me_config_attr(T2, undefined, MeConfig),
	#pee_state{me = Me} = PeeState,
	NewMe = choice_add(NewMeConfig, Me),
	[PrevState#state{parse_state = PeeState#pee_state{me = NewMe}} | T1];
parse_me_config({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_me_config_attr([{startElement, {_, "attributes"} = QName, []} | T],
		undefined, Acc) ->
	{[_ | Attributes], _Rest} = pop(endElement, QName, T),
	parse_me_config_attr1(Attributes, undefined, Acc).
% @hidden
parse_me_config_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_me_config_attr1(T, Attr, Acc);
parse_me_config_attr1([{characters, Chars} | T], "powerMinThreshold" = Attr, Acc) ->
	NewAcc = attribute_add("powerMinThreshold", Chars, Acc),
	parse_me_config_attr1(T, Attr, NewAcc);
parse_me_config_attr1([{characters, Chars} | T], "powerMaxThreshold" = Attr, Acc) ->
	NewAcc = attribute_add("powerMaxThreshold", Chars, Acc),
	parse_me_config_attr1(T, Attr, NewAcc);
parse_me_config_attr1([{characters, Chars} | T], "temperatureMinThreshold" = Attr, Acc) ->
	NewAcc = attribute_add("temperatureMinThreshold", Chars, Acc),
	parse_me_config_attr1(T, Attr, NewAcc);
parse_me_config_attr1([{characters, Chars} | T], "temperatureMaxThreshold" = Attr, Acc) ->
	NewAcc = attribute_add("temperatureMaxThreshold>", Chars, Acc),
	parse_me_config_attr1(T, Attr, NewAcc);
parse_me_config_attr1([{characters, Chars} | T], "voltageMinThreshold" = Attr, Acc) ->
	NewAcc = attribute_add("voltageMinThreshold", Chars, Acc),
	parse_me_config_attr1(T, Attr, NewAcc);
parse_me_config_attr1([{characters, Chars} | T], "voltageMaxThreshold" = Attr, Acc) ->
	NewAcc = attribute_add("voltageMaxThreshold", Chars, Acc),
	parse_me_config_attr1(T, Attr, NewAcc);
parse_me_config_attr1([{characters, Chars} | T], "currentMinThreshold" = Attr, Acc) ->
	NewAcc = attribute_add("currentMinThreshold", Chars, Acc),
	parse_me_config_attr1(T, Attr, NewAcc);
parse_me_config_attr1([{characters, Chars} | T], "currentMaxThreshold" = Attr, Acc) ->
	NewAcc = attribute_add("currentMaxThreshold", Chars, Acc),
	parse_me_config_attr1(T, Attr, NewAcc);
parse_me_config_attr1([{characters, Chars} | T], "humidityMinThreshold" = Attr, Acc) ->
	NewAcc = attribute_add("humidityMinThreshold", Chars, Acc),
	parse_me_config_attr1(T, Attr, NewAcc);
parse_me_config_attr1([{characters, Chars} | T], "humidityMaxThreshold" = Attr, Acc) ->
	NewAcc = attribute_add("humidityMaxThreshold", Chars, Acc),
	parse_me_config_attr1(T, Attr, NewAcc);
parse_me_config_attr1([{startElement, {_, Attr}, []} | T], Attr, Acc) ->
	parse_me_config_attr1(T, undefined, Acc);
parse_me_config_attr1([],  _Attr, Acc) ->
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

-spec choice_add(Choice, NrmMap) -> NrmMap
	when
		Choice :: map(),
		NrmMap :: map().
%% @doc Add `Choice' to possibly list of choices.
choice_add(Choice, #{"choice" := Choices} = NrmMap) ->
	NrmMap#{"choice" => [Choice | Choices]};
choice_add(Choice, #{} = NrmMap) ->
	NrmMap#{"choice" => [Choice]}.
