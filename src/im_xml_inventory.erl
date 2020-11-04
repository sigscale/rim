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
-module(im_xml_inventory).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% export the im private API
-export([parse_iu/2, parse_tmaiu/2, parse_aiu/2]).

-export([fraction1/1]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

-define(PathInventorySchema, "/resourceInventoryManagement/v4/schema").

%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

%% @hidden
parse_iu({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_iu({startElement, _Uri, "InventoryUnit", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",InventoryUnit=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_inventory,
			parse_function = parse_iu,
			parse_state = #im1_state{iu = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_iu({startElement, _Uri, "TmaInventoryUnit", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",TmaInventoryUnit=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_inventory,
			parse_function = parse_tmaiu,
			parse_state = #im1_state{tmaiu = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_iu({startElement, _Uri, "AntennaInventoryUnit", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",AntennaInventoryUnit=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_inventory,
			parse_function = parse_aiu,
			parse_state = #im1_state{aiu = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_iu({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_iu({endElement, _Uri, "InventoryUnit", QName},
		[#state{parse_state =  #im1_state{ius = Ius,
		tmaius = TmaIus, aius = Aius},
		dn_prefix = [IuDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "InventoryUnit",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	IuAttr = parse_iu_attr(T2, undefined, []),
	Resource = #resource{name = IuDn,
			description = "IM Inventory Unit",
			category = "IM",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/InventoryUnit",
			specification = Spec,
			characteristic = IuAttr,
			related = Ius ++ TmaIus ++ Aius},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_iu({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_iu_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_iu_attr1(Attributes, undefined, Acc).
% @hidden
parse_iu_attr1([{characters, Chars} | T], "inventoryUnitType" = Attr, Acc) ->
	parse_iu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_iu_attr1([{characters, Chars} | T], "vendorUnitFamilyType" = Attr, Acc) ->
	parse_iu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_iu_attr1([{characters, Chars} | T], "vendorUnitTypeNumber" = Attr, Acc) ->
	parse_iu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_iu_attr1([{characters, Chars} | T], "vendorName" = Attr, Acc) ->
	parse_iu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_iu_attr1([{characters, Chars} | T], "serialNumber" = Attr, Acc) ->
	parse_iu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_iu_attr1([{characters, Chars} | T], "dateOfManufacture" = Attr, Acc) ->
	parse_iu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_iu_attr1([{characters, Chars} | T], "dateOfLastService" = Attr, Acc) ->
	parse_iu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_iu_attr1([{characters, Chars} | T], "unitPosition" = Attr, Acc) ->
	parse_iu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_iu_attr1([{characters, Chars} | T], "manufacturerData" = Attr, Acc) ->
	parse_iu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_iu_attr1([{characters, Chars} | T], "versionNumber" = Attr, Acc) ->
	parse_iu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_iu_attr1([{characters, Chars} | T], "relatedFunction" = Attr, Acc) ->
	parse_iu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_iu_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_iu_attr1(T, undefined, Acc);
parse_iu_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_iu_attr1(T, Attr, Acc);
parse_iu_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_tmaiu({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_tmaiu({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_tmaiu({endElement, _Uri, "TmaInventoryUnit", QName},
		[#state{dn_prefix = [TmaIuDn | _], stack = Stack,
		spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	TmaIuAttr = parse_tmaiu_attr(T2, undefined, []),
	ClassType = "TmaInventoryUnit",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = TmaIuDn,
			description = "IM Tower Mounted Amplifier (TMA) Inventory Unit",
			category = "IM",
			class_type = ClassType,
			base_type = "InventoryUnit",
			schema = ?PathInventorySchema ++ "/TmaInventoryUnit",
			specification = Spec,
			characteristic = TmaIuAttr},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_tmaiu({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_tmaiu_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_tmaiu_attr1(Attributes, undefined, Acc).
% @hidden
parse_tmaiu_attr1([{endElement,
		{_, "tmaBeamwidthForEachOpBandInBandOrder"} = QName} | T1],
		undefined, Acc) ->
	{[_ | _TmaBandOrder], T2} = pop(startElement, QName, T1),
	% @todo eightOctetsType
	parse_tmaiu_attr1(T2, undefined, Acc);
parse_tmaiu_attr1([{endElement,
		{_, "tmaGainForEachOpBandInBandOrder"} = QName} | T1], undefined, Acc) ->
	{[_ | _TmaBandOrder], T2} = pop(startElement, QName, T1),
	% @todo fourOctetsType
	parse_tmaiu_attr1(T2, undefined, Acc);
parse_tmaiu_attr1([{characters, Chars} | T], "inventoryUnitType" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_tmaiu_attr1([{characters, Chars} | T], "vendorUnitFamilyType" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_tmaiu_attr1([{characters, Chars} | T], "vendorUnitTypeNumber" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_tmaiu_attr1([{characters, Chars} | T], "vendorName" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_tmaiu_attr1([{characters, Chars} | T], "serialNumber" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_tmaiu_attr1([{characters, Chars} | T], "dateOfManufacture" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_tmaiu_attr1([{characters, Chars} | T], "dateOfLastService" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_tmaiu_attr1([{characters, Chars} | T], "unitPosition" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_tmaiu_attr1([{characters, Chars} | T], "manufacturerData" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_tmaiu_attr1([{characters, Chars} | T], "versionNumber" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_tmaiu_attr1([{characters, Chars} | T], "relatedFunction" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_tmaiu_attr1([{characters, Chars} | T],
		"tmaNumberOfNonLinearGainValues" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_tmaiu_attr1([{characters, Chars} | T],
		"tmaNonLinearGainValue" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_tmaiu_attr1([{characters, Chars} | T],
		"tmaAdditionalDataFieldNumber" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_tmaiu_attr1([{characters, Chars} | T],
		"tmaAntennaModelNumber" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_tmaiu_attr1([{characters, Chars} | T],
		"tmaAntennaOperatingBands" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_tmaiu_attr1([{characters,
		Chars} | T], "tmaInstallationDate" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_tmaiu_attr1([{characters, Chars} | T], "tmaInstallersId" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_tmaiu_attr1([{characters,
		Chars} | T], "tmaMaxSupportedGain" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_tmaiu_attr1([{characters,
		Chars} | T], "tmaMinSupportedGain" = Attr, Acc) ->
	parse_tmaiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_tmaiu_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_tmaiu_attr1(T, undefined, Acc);
parse_tmaiu_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_tmaiu_attr1(T, Attr, Acc);
parse_tmaiu_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_aiu({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_aiu({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_aiu({endElement, _Uri, "AntennaInventoryUnit", QName},
		[#state{dn_prefix = [AntennaIuDn | _], stack = Stack,
		spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	AntennaIuAttr = parse_aiu_attr(T2, undefined, []),
	ClassType = "AntennaInventoryUnit",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = AntennaIuDn,
			description = "IM Inventory Unit",
			category = "IM",
			class_type = ClassType,
			base_type = "InventoryUnit",
			schema = ?PathInventorySchema ++ "/AntennaInventoryUnit",
			specification = Spec,
			characteristic = AntennaIuAttr},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_aiu({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_aiu_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_aiu_attr1(Attributes, undefined, Acc).
% @hidden
parse_aiu_attr1([{endElement,
		{_, "tmaBeamwidthForEachOpBandInBandOrder"} = QName} | T1],
		undefined, Acc) ->
	{[_ | _TmaBandOrder], T2} = pop(startElement, QName, T1),
	% @todo eightOctetsType
	parse_aiu_attr1(T2, undefined, Acc);
parse_aiu_attr1([{endElement,
		{_, "tmaGainForEachOpBandInBandOrder"} = QName} | T1], undefined, Acc) ->
	{[_ | _TmaBandOrder], T2} = pop(startElement, QName, T1),
	% @todo fourOctetsType
	parse_aiu_attr1(T2, undefined, Acc);
parse_aiu_attr1([{characters, Chars} | T], "inventoryUnitType" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_aiu_attr1([{characters, Chars} | T], "vendorUnitFamilyType" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_aiu_attr1([{characters, Chars} | T], "vendorUnitTypeNumber" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_aiu_attr1([{characters, Chars} | T], "vendorName" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_aiu_attr1([{characters, Chars} | T], "serialNumber" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_aiu_attr1([{characters, Chars} | T], "dateOfManufacture" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_aiu_attr1([{characters, Chars} | T], "dateOfLastService" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_aiu_attr1([{characters, Chars} | T], "unitPosition" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_aiu_attr1([{characters, Chars} | T], "manufacturerData" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_aiu_attr1([{characters, Chars} | T], "versionNumber" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_aiu_attr1([{characters, Chars} | T], "relatedFunction" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_aiu_attr1([{characters, Chars} | T], "maxTiltValue" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_aiu_attr1([{characters, Chars} | T], "minTiltValue" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_aiu_attr1([{characters, Chars} | T], "mechanicalOffset" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_aiu_attr1([{characters, Chars} | T], "baseElevation" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_aiu_attr1([{characters, Chars} | T], "latitude" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
%			[#resource_char{name = Attr, value = fraction1(Chars)} | Acc]);
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_aiu_attr1([{characters, Chars} | T], "longitude" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
%			[#resource_char{name = Attr, value = fraction1(Chars)} | Acc]);
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_aiu_attr1([{characters, Chars} | T], "patternLabel" = Attr, Acc) ->
	parse_aiu_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_aiu_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_aiu_attr1(T, undefined, Acc);
parse_aiu_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_aiu_attr1(T, Attr, Acc);
parse_aiu_attr1([], undefined, Acc) ->
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
				{ok, #specification{id = Id, href = Href,
						name = Name, class_type = Type, version = Version}} ->
					SpecRef = #specification_ref{id = Id, href = Href,
							name = Name, ref_type = Type, version = Version},
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
