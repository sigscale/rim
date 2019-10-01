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
-module(im_xml_inventory2).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% export the im private API
-export([parse_ne/2, parse_hw/2, parse_sw/2, parse_lic/2]).

-export([fraction1/1]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

%% @hidden
parse_ne({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ne({startElement, _Uri, "InventoryUnitNE", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",InventoryUnitNE=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_inventory2,
			parse_function = parse_ne,
			parse_state = #im2_state{iu_ne = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_ne({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ne({endElement, _Uri, "InventoryUnitNE", QName},
		[#state{parse_state =  #im2_state{iu_nes = Nes},
		dn_prefix = [NEDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state =  #im2_state{iu_ne = #{"id" := Id}, iu_nes = NERels},
		spec_cache = PrevCache} = PrevState | T1]) when is_list(Id) ->
	IMState = PrevState#state.parse_state,
	ClassType = "InventoryUnitNE",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NEAttr = parse_ne_attr(T2, undefined, []),
	Resource = #resource{name = NEDn,
			description = "IM Inventory Unit Network Element",
			category = "IM",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/InventoryUnitNE",
			specification = Spec,
			characteristic = NEAttr,
			related = Nes},
	case im:add_resource(Resource) of
		{ok, #resource{id = ResourceId}} ->
			NERel = #resource_rel{id = Id, name = NEDn, type = "contains",
					href = "/resourceInventoryManagement/v3/resource/" ++ ResourceId},
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					parse_state = IMState#im2_state{iu_nes = [NERel | NERels]}} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ne({endElement, _Uri, "InventoryUnitNE", QName},
		[#state{parse_state =  #im2_state{iu_nes = NEs},
		dn_prefix = [NEDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "InventoryUnitNE",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NEAttr = parse_ne_attr(T2, undefined, []),
	Resource = #resource{name = NEDn,
			description = "IM Inventory Unit Network Element",
			category = "IM",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/InventoryUnitNE",
			specification = Spec,
			characteristic = NEAttr,
			related = NEs},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ne({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_ne_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_ne_attr1(Attributes, undefined, Acc).
% @hidden
parse_ne_attr1([{endElement, {_, "hWList"} = QName} | T1], undefined, Acc) ->
	{[_ | _hWList], T2} = pop(startElement, QName, T1),
	% @todo dnList
	parse_ne_attr1(T2, undefined, Acc);
parse_ne_attr1([{endElement, {_, "sWList"} = QName} | T1], undefined, Acc) ->
	{[_ | _sWList], T2} = pop(startElement, QName, T1),
	% @todo dnList
	parse_ne_attr1(T2, undefined, Acc);
parse_ne_attr1([{endElement, {_, "lICList"} = QName} | T1], undefined, Acc) ->
	{[_ | _lICList], T2} = pop(startElement, QName, T1),
	% @todo dnList
	parse_ne_attr1(T2, undefined, Acc);
parse_ne_attr1([{characters, Chars} | T], "neId" = Attr, Acc) ->
	parse_ne_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_ne_attr1([{characters, Chars} | T], "customerIdentifier" = Attr, Acc) ->
	parse_ne_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_ne_attr1([{characters, Chars} | T], "productName" = Attr, Acc) ->
	parse_ne_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_ne_attr1([{characters, Chars} | T], "vendorName" = Attr, Acc) ->
	parse_ne_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_ne_attr1([{characters, Chars} | T], "productType" = Attr, Acc) ->
	parse_ne_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_ne_attr1([{characters, Chars} | T], "salesUniqueId" = Attr, Acc) ->
	parse_ne_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_ne_attr1([{characters, Chars} | T], "operatorUniqueName" = Attr, Acc) ->
	parse_ne_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_ne_attr1([{characters, Chars} | T], "siteId" = Attr, Acc) ->
	parse_ne_attr1(T, Attr,
			[#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
parse_ne_attr1([{characters, Chars} | T], "additionalInformation" = Attr, Acc) ->
	parse_ne_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_ne_attr1([{characters, Chars} | T], "mFunction" = Attr, Acc) ->
	parse_ne_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_ne_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_ne_attr1(T, undefined, Acc);
parse_ne_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_ne_attr1(T, Attr, Acc);
parse_ne_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_hw({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_hw({startElement, _Uri, "InventoryUnitHw", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",InventoryUnitHw=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_inventory2,
			parse_function = parse_hw,
			parse_state = #im2_state{iu_hw = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_hw({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_hw({endElement, _Uri, "InventoryUnitHw", QName},
		[#state{parse_state =  #im2_state{iu_hws = Hws},
		dn_prefix = [HwDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state =  #im2_state{iu_hw = #{"id" := Id}, iu_hws = HwRels},
		spec_cache = PrevCache} = PrevState | T1]) when is_list(Id) ->
	IMState = PrevState#state.parse_state,
	ClassType = "InventoryUnitHw",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	HwAttr = parse_hw_attr(T2, undefined, []),
	Resource = #resource{name = HwDn,
			description = "IM Inventory Unit Hardware",
			category = "IM",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/InventoryUnitHw",
			specification = Spec,
			characteristic = HwAttr,
			related = Hws},
	case im:add_resource(Resource) of
		{ok, #resource{id = ResourceId}} ->
			HwRel = #resource_rel{id = Id, name = HwDn, type = "contains",
					href = "/resourceInventoryManagement/v3/resource/" ++ ResourceId},
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					parse_state = IMState#im2_state{iu_hws = [HwRel | HwRels]}} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_hw({endElement, _Uri, "InventoryUnitHw", QName},
		[#state{parse_state =  #im2_state{iu_hws = Hws},
		dn_prefix = [HwDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "InventoryUnitHw",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	HwAttr = parse_hw_attr(T2, undefined, []),
	Resource = #resource{name = HwDn,
			description = "IM Inventory Unit Hardware",
			category = "IM",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/InventoryUnitHw",
			specification = Spec,
			characteristic = HwAttr,
			related = Hws},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_hw({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_hw_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_hw_attr1(Attributes, undefined, Acc).
% @hidden
parse_hw_attr1([{endElement, {_, "nEList"} = QName} | T1], undefined, Acc) ->
	{[_ | _nEList], T2} = pop(startElement, QName, T1),
	% @todo dnList
	parse_hw_attr1(T2, undefined, Acc);
parse_hw_attr1([{endElement, {_, "sWList"} = QName} | T1], undefined, Acc) ->
	{[_ | _sWList], T2} = pop(startElement, QName, T1),
	% @todo dnList
	parse_hw_attr1(T2, undefined, Acc);
parse_hw_attr1([{endElement, {_, "lICList"} = QName} | T1], undefined, Acc) ->
	{[_ | _lICList], T2} = pop(startElement, QName, T1),
	% @todo dnList
	parse_hw_attr1(T2, undefined, Acc);
parse_hw_attr1([{characters, Chars} | T], "hwId" = Attr, Acc) ->
	parse_hw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_hw_attr1([{characters, Chars} | T], "hwType" = Attr, Acc) ->
	parse_hw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_hw_attr1([{characters, Chars} | T], "hwName" = Attr, Acc) ->
	parse_hw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_hw_attr1([{characters, Chars} | T], "vendorName" = Attr, Acc) ->
	parse_hw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_hw_attr1([{characters, Chars} | T], "hwVersion" = Attr, Acc) ->
	parse_hw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_hw_attr1([{characters, Chars} | T], "salesUniqueId" = Attr, Acc) ->
	parse_hw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_hw_attr1([{characters, Chars} | T], "hwUnitLocation" = Attr, Acc) ->
	parse_hw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_hw_attr1([{characters, Chars} | T], "model" = Attr, Acc) ->
	parse_hw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_hw_attr1([{characters, Chars} | T], "hwCapability" = Attr, Acc) ->
	parse_hw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_hw_attr1([{characters, Chars} | T], "modificationDate" = Attr, Acc) ->
	parse_hw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_hw_attr1([{characters, Chars} | T], "manualDataEntry" = Attr, Acc) ->
	parse_hw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_hw_attr1([{characters, Chars} | T], "additionalInformation" = Attr, Acc) ->
	parse_hw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_hw_attr1([{characters, Chars} | T], "mFunction" = Attr, Acc) ->
	parse_hw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_hw_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_hw_attr1(T, undefined, Acc);
parse_hw_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_hw_attr1(T, Attr, Acc);
parse_hw_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_sw({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_sw({startElement, _Uri, "InventoryUnitSw", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",InventoryUnitSw=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_inventory2,
			parse_function = parse_sw,
			parse_state = #im2_state{iu_sw = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_sw({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_sw({endElement, _Uri, "InventoryUnitSw", QName},
		[#state{parse_state =  #im2_state{iu_sws = Sws},
		dn_prefix = [SwDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state =  #im2_state{iu_sw = #{"id" := Id}, iu_sws = SwRels},
		spec_cache = PrevCache} = PrevState | T1]) when is_list(Id) ->
	IMState = PrevState#state.parse_state,
	ClassType = "InventoryUnitSw",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	SwAttr = parse_sw_attr(T2, undefined, []),
	Resource = #resource{name = SwDn,
			description = "IM Inventory Unit Software",
			category = "IM",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/InventoryUnitSw",
			specification = Spec,
			characteristic = SwAttr,
			related = Sws},
	case im:add_resource(Resource) of
		{ok, #resource{id = ResourceId}} ->
			SwRel = #resource_rel{id = Id, name = SwDn, type = "contains",
					href = "/resourceInventoryManagement/v3/resource/" ++ ResourceId},
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					parse_state = IMState#im2_state{iu_sws = [SwRel | SwRels]}} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_sw({endElement, _Uri, "InventoryUnitSw", QName},
		[#state{parse_state =  #im2_state{iu_sws = Sws},
		dn_prefix = [SwDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "InventoryUnitSw",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	SwAttr = parse_sw_attr(T2, undefined, []),
	Resource = #resource{name = SwDn,
			description = "IM Inventory Unit Software",
			category = "IM",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/InventoryUnitSw",
			specification = Spec,
			characteristic = SwAttr,
			related = Sws},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_sw({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_sw_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_sw_attr1(Attributes, undefined, Acc).
% @hidden
parse_sw_attr1([{endElement, {_, "nEList"} = QName} | T1], undefined, Acc) ->
	{[_ | _nEList], T2} = pop(startElement, QName, T1),
	% @todo dnList
	parse_sw_attr1(T2, undefined, Acc);
parse_sw_attr1([{endElement, {_, "hWList"} = QName} | T1], undefined, Acc) ->
	{[_ | _hWList], T2} = pop(startElement, QName, T1),
	% @todo dnList
	parse_sw_attr1(T2, undefined, Acc);
parse_sw_attr1([{endElement, {_, "lICList"} = QName} | T1], undefined, Acc) ->
	{[_ | _lICList], T2} = pop(startElement, QName, T1),
	% @todo dnList
	parse_sw_attr1(T2, undefined, Acc);
parse_sw_attr1([{characters, Chars} | T], "swId" = Attr, Acc) ->
	parse_sw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_sw_attr1([{characters, Chars} | T], "swName" = Attr, Acc) ->
	parse_sw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_sw_attr1([{characters, Chars} | T], "vendorName" = Attr, Acc) ->
	parse_sw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_sw_attr1([{characters, Chars} | T], "swVersion" = Attr, Acc) ->
	parse_sw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_sw_attr1([{characters, Chars} | T], "salesUniqueId" = Attr, Acc) ->
	parse_sw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_sw_attr1([{characters, Chars} | T], "classification" = Attr, Acc) ->
	parse_sw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_sw_attr1([{characters, Chars} | T], "swInstallationTime" = Attr, Acc) ->
	parse_sw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_sw_attr1([{characters, Chars} | T], "swActivationTime" = Attr, Acc) ->
	parse_sw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_sw_attr1([{characters, Chars} | T], "swStatus" = Attr, Acc) ->
	parse_sw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_sw_attr1([{characters, Chars} | T], "additionalInformation" = Attr, Acc) ->
	parse_sw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_sw_attr1([{characters, Chars} | T], "mFunction" = Attr, Acc) ->
	parse_sw_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_sw_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_sw_attr1(T, undefined, Acc);
parse_sw_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_sw_attr1(T, Attr, Acc);
parse_sw_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_lic({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_lic({startElement, _Uri, "InventoryUnitLic", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",InventoryUnitLic=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn], parse_module = im_xml_inventory2,
			parse_function = parse_lic,
			parse_state = #im2_state{iu_lic = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_lic({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_lic({endElement, _Uri, "InventoryUnitLic", QName},
		[#state{parse_state =  #im2_state{iu_lics = Lics},
		dn_prefix = [LicDn | _], stack = Stack, spec_cache = Cache},
		#state{parse_state =  #im2_state{iu_lic = #{"id" := Id}, iu_lics = LicRels},
		spec_cache = PrevCache} = PrevState | T1]) when is_list(Id) ->
	IMState = PrevState#state.parse_state,
	ClassType = "InventoryUnitLic",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	LicAttr = parse_lic_attr(T2, undefined, []),
	Resource = #resource{name = LicDn,
			description = "IM Inventory Unit Licence",
			category = "IM",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/InventoryUnitLic",
			specification = Spec,
			characteristic = LicAttr,
			related = Lics},
	case im:add_resource(Resource) of
		{ok, #resource{id = ResourceId}} ->
			LicRel = #resource_rel{id = Id, name = LicDn, type = "contains",
					href = "/resourceInventoryManagement/v3/resource/" ++ ResourceId},
			[PrevState#state{spec_cache = [NewCache | PrevCache],
					parse_state = IMState#im2_state{iu_lics = [LicRel | LicRels]}} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_lic({endElement, _Uri, "InventoryUnitLic", QName},
		[#state{parse_state =  #im2_state{iu_lics = Lics},
		dn_prefix = [LicDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "InventoryUnitLic",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	LicAttr = parse_lic_attr(T2, undefined, []),
	Resource = #resource{name = LicDn,
			description = "IM Inventory Unit Licence",
			category = "IM",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/InventoryUnitLic",
			specification = Spec,
			characteristic = LicAttr,
			related = Lics},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_lic({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_lic_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_lic_attr1(Attributes, undefined, Acc).
% @hidden
parse_lic_attr1([{endElement, {_, "nEList"} = QName} | T1], undefined, Acc) ->
	{[_ | _nEList], T2} = pop(startElement, QName, T1),
	% @todo dnList
	parse_lic_attr1(T2, undefined, Acc);
parse_lic_attr1([{endElement, {_, "hWList"} = QName} | T1], undefined, Acc) ->
	{[_ | _hWList], T2} = pop(startElement, QName, T1),
	% @todo dnList
	parse_lic_attr1(T2, undefined, Acc);
parse_lic_attr1([{endElement, {_, "sWList"} = QName} | T1], undefined, Acc) ->
	{[_ | _sWList], T2} = pop(startElement, QName, T1),
	% @todo dnList
	parse_lic_attr1(T2, undefined, Acc);
parse_lic_attr1([{characters, Chars} | T], "licId" = Attr, Acc) ->
	parse_lic_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_lic_attr1([{characters, Chars} | T], "licType" = Attr, Acc) ->
	parse_lic_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_lic_attr1([{characters, Chars} | T], "vendorName" = Attr, Acc) ->
	parse_lic_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_lic_attr1([{characters, Chars} | T], "validity" = Attr, Acc) ->
	parse_lic_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_lic_attr1([{characters, Chars} | T], "key" = Attr, Acc) ->
	parse_lic_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_lic_attr1([{characters, Chars} | T], "licActivationTime" = Attr, Acc) ->
	parse_lic_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_lic_attr1([{characters, Chars} | T], "licStatus" = Attr, Acc) ->
	parse_lic_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_lic_attr1([{characters, Chars} | T], "salesUniqueId" = Attr, Acc) ->
	parse_lic_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_lic_attr1([{characters, Chars} | T], "additionalInformation" = Attr, Acc) ->
	parse_lic_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_lic_attr1([{characters, Chars} | T], "mFunction" = Attr, Acc) ->
	parse_lic_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_lic_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_lic_attr1(T, undefined, Acc);
parse_lic_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_lic_attr1(T, Attr, Acc);
parse_lic_attr1([], undefined, Acc) ->
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
