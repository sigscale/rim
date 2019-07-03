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
parse_pee_me({startElement,  _Uri, "PEEMEDescription", QName, Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	[#state{dn_prefix = [CurrentDn],
			parse_module = im_xml_pee, parse_function = parse_me_description,
			stack = [{startElement, QName, Attributes}]} | State];
parse_pee_me({startElement, _Uri, "PEEConfigInformation", QName, Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	[#state{dn_prefix = [CurrentDn],
			parse_module = im_xml_pee, parse_function = parse_me_config,
			stack = [{startElement, QName, Attributes}]} | State];
parse_pee_me({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_pee_me({endElement, _Uri, "PEEMonitoredEntity", QName},
		[#state{dn_prefix = [MeDn | _], stack = Stack, spec_cache = Cache,
		parse_state = #pee_state{me_description = MeDescription,
		me_config = MeConfig}},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	[#resource_char{name = "mEId", value = MeId} = MeAttr]
			= parse_pee_me_attr(T2, undefined, []),
	ClassType = "PEEMonitoredEntity",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	MeDResourceChar = #resource_char{name = "peeMeDescription",
		class_type = "PEEMEDescription", value = MeDescription,
		schema = "/resourceCatalogManagement/v3/schema/peeCmonNrm#/definitions/PEEMEDescription"},
	MeCResourceChar = #resource_char{name = "peeMeConfiguration",
		class_type = "PEEMEConfiguration", value = MeConfig,
		schema = "/resourceCatalogManagement/v3/schema/peeCmonNrm#/definitions/PEEMEConfiguration"},
	Resource = #resource{name = MeDn ++ "mEId=" ++ MeId,
			description = "PEE Monitored Entity (ME)",
			category = "PEE",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/PEEMonitoredEntity",
			specification = Spec,
			characteristic = [MeAttr, MeDResourceChar, MeCResourceChar]},
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
		[#state{stack = Stack}, #state{parse_state = PeeState} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	MeDescription = parse_me_description_attr(T2, undefined, #{}),
	[PrevState#state{parse_state
			= PeeState#pee_state{me_description = MeDescription}} | T1];
parse_me_description({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_me_description_attr([{startElement, {_, Attr}, []} | T], undefined, Acc) ->
	parse_me_description_attr(T, Attr, Acc);
parse_me_description_attr([{characters, Chars} | T],
		"siteIdentification" = Attr, Acc) ->
	parse_me_description_attr(T, Attr, Acc#{Attr => Chars});
parse_me_description_attr([{characters, Chars} | T],
		"siteLatitude" = Attr, Acc) ->
	parse_me_description_attr(T, Attr, Acc#{Attr => Chars});
parse_me_description_attr([{characters, Chars} | T],
		"siteLongitude" = Attr, Acc) ->
	parse_me_description_attr(T, Attr, Acc#{Attr => Chars});
parse_me_description_attr([{characters, Chars} | T],
		"siteDescription" = Attr, Acc) ->
	parse_me_description_attr(T, Attr, Acc#{Attr => Chars});
parse_me_description_attr([{characters, Chars} | T],
		"equipmentType" = Attr, Acc) ->
	parse_me_description_attr(T, Attr, Acc#{Attr => Chars});
parse_me_description_attr([{characters, Chars} | T],
		"environmentType" = Attr, Acc) ->
	parse_me_description_attr(T, Attr, Acc#{Attr => Chars});
parse_me_description_attr([{characters, Chars} | T],
		"powerInterface" = Attr, Acc) ->
	parse_me_description_attr(T, Attr, Acc#{Attr => Chars});
parse_me_description_attr([{characters, Chars} | T],
		"xcuDguDescription" = Attr, Acc) ->
	parse_me_description_attr(T, Attr, Acc#{Attr => Chars});
parse_me_description_attr([{characters, Chars} | T],
		"sensorDescription" = Attr, Acc) ->
	parse_me_description_attr(T, Attr, Acc#{Attr => Chars});
parse_me_description_attr([{characters, Chars} | T],
		"vSRmsDescription" = Attr, Acc) ->
	parse_me_description_attr(T, Attr, Acc#{Attr => Chars});
parse_me_description_attr([{endElement, {_, Attr}} | T], Attr, Acc) ->
	parse_me_description_attr(T, undefined, Acc);
parse_me_description_attr([],  _Attr, Acc) ->
	Acc.

%% @hidden
parse_me_config({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_me_config({startElement, _Uri, _LocalName, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_me_config({endElement, _Uri, "PEEConfigInformation", QName},
		[#state{stack = Stack},
		#state{parse_state = PeeState} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	MeConfig = parse_me_config_attr(T2, undefined, #{}),
	[PrevState#state{parse_state
			= PeeState#pee_state{me_config = MeConfig}} | T1];
parse_me_config({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_me_config_attr([{startElement, {_, Attr}, []} | T], undefined, Acc) ->
	parse_me_config_attr(T, Attr, Acc);
parse_me_config_attr([{characters, Chars} | T],
		"powerMinThreshold" = Attr, Acc) ->
	parse_me_config_attr(T, Attr, Acc#{Attr => Chars});
parse_me_config_attr([{characters, Chars} | T],
		"powerMaxThreshold" = Attr, Acc) ->
	parse_me_config_attr(T, Attr, Acc#{Attr => Chars});
parse_me_config_attr([{characters, Chars} | T],
		"temperatureMinThreshold" = Attr, Acc) ->
	parse_me_config_attr(T, Attr, Acc#{Attr => Chars});
parse_me_config_attr([{characters, Chars} | T],
		"temperatureMaxThreshold" = Attr, Acc) ->
	parse_me_config_attr(T, Attr, Acc#{Attr => Chars});
parse_me_config_attr([{characters, Chars} | T],
		"voltageMinThreshold" = Attr, Acc) ->
	parse_me_config_attr(T, Attr, Acc#{Attr => Chars});
parse_me_config_attr([{characters, Chars} | T],
		"voltageMaxThreshold" = Attr, Acc) ->
	parse_me_config_attr(T, Attr, Acc#{Attr => Chars});
parse_me_config_attr([{characters, Chars} | T],
		"currentMinThreshold" = Attr, Acc) ->
	parse_me_config_attr(T, Attr, Acc#{Attr => Chars});
parse_me_config_attr([{characters, Chars} | T],
		"currentMaxThreshold" = Attr, Acc) ->
	parse_me_config_attr(T, Attr, Acc#{Attr => Chars});
parse_me_config_attr([{characters, Chars} | T],
		"humidityMinThreshold" = Attr, Acc) ->
	parse_me_config_attr(T, Attr, Acc#{Attr => Chars});
parse_me_config_attr([{characters, Chars} | T],
		"humidityMaxThreshold" = Attr, Acc) ->
	parse_me_config_attr(T, Attr, Acc#{Attr => Chars});
parse_me_config_attr([{endElement, {_, Attr}} | T], Attr, Acc) ->
	parse_me_config_attr(T, undefined, Acc);
parse_me_config_attr([],  _Attr, Acc) ->
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
