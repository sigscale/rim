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
-module(im_xml_ims).
-copyright('Copyright (c) 2018 - 2021 SigScale Global Inc.').

%% export the im private API
-export([parse_as/2, parse_hss/2, parse_icscf/2, parse_pcscf/2, parse_scscf/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

-define(PathCatalogSchema, "/resourceCatalogManagement/v4/schema").
-define(PathInventorySchema, "/resourceInventoryManagement/v4/schema").

%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

parse_as({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_as({startElement,  _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_as({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_as({endElement, _Uri, "ASFunction", QName},
		[#state{dn_prefix = [AsDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	AsAttr = parse_as_attr(T2, undefined, []),
	ClassType = "ASFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = AsDn,
			description = "IMS Application Server (AS)",
			category = "IMS",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/ASFunction",
			specification = Spec,
			characteristic = AsAttr},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_as({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

parse_as_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_as_attr1(Attributes, undefined, Acc).
%% @hidden
parse_as_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfParameterList], T2} = pop(startElement, QName, T1),
	parse_as_attr1(T2, undefined, Acc);
parse_as_attr1([{endElement, {_, "linkList"} = QName} | T1],
		undefined, Acc) ->
	% @todo linkListType
	{[_ | _linkList], T2} = pop(startElement, QName, T1),
	parse_as_attr1(T2, undefined, Acc);
parse_as_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_as_attr1(T, Attr, Acc);
parse_as_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_as_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_as_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_as_attr1(T, undefined, Acc);
parse_as_attr1([], undefined, Acc) ->
	Acc.

parse_hss({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_hss({startElement,  _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_hss({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_hss({endElement, _Uri, "HSSFunction", QName},
		[#state{dn_prefix = [HssDn | _], stack = Stack, location = Location,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	HssAttr = parse_hss_attr(T2, undefined, []),
	ClassType = "HSSFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	PeeParam = #resource_char{name = "peeParametersList",
			class_type = "PeeParametersListType", value = Location,
			schema = ?PathCatalogSchema ++ "/genericNrm#/"
					"definitions/PeeParametersListType"},
	Resource = #resource{name = HssDn,
			description = "IMS Home Subscriber Server (HSS)",
			category = "IMS",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/HSSFunction",
			specification = Spec,
			characteristic = lists:reverse([PeeParam | HssAttr])},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_hss({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

parse_hss_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_hss_attr1(Attributes, undefined, Acc).
%% @hidden
parse_hss_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfParameterList], T2} = pop(startElement, QName, T1),
	parse_hss_attr1(T2, undefined, Acc);
parse_hss_attr1([{endElement, {_, "linkList"} = QName} | T1],
		undefined, Acc) ->
	% @todo linkListType
	{[_ | _linkList], T2} = pop(startElement, QName, T1),
	parse_hss_attr1(T2, undefined, Acc);
parse_hss_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_hss_attr1(T, Attr, Acc);
parse_hss_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_hss_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_hss_attr1([{characters, _Chars} | T], Attr, Acc) ->
	parse_hss_attr1(T, Attr, Acc);
parse_hss_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_hss_attr1(T, undefined, Acc);
parse_hss_attr1([], undefined, Acc) ->
	Acc.

parse_icscf({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_icscf({startElement,  _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_icscf({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_icscf({endElement, _Uri, "ICSCFFunction", QName},
		[#state{dn_prefix = [IcscfDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	IcscfAttr = parse_icscf_attr(T2, undefined, []),
	ClassType = "ICSCFFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = IcscfDn,
			description = "IMS Interrogating Call Session Control Function (CSCF)",
			category = "IMS",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/ICSFFunction",
			specification = Spec,
			characteristic = IcscfAttr},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_icscf({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

parse_icscf_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_icscf_attr1(Attributes, undefined, Acc).
%% @hidden
parse_icscf_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfParameterList], T2} = pop(startElement, QName, T1),
	parse_icscf_attr1(T2, undefined, Acc);
parse_icscf_attr1([{endElement, {_, "linkList"} = QName} | T1],
		undefined, Acc) ->
	% @todo linkListType
	{[_ | _linkList], T2} = pop(startElement, QName, T1),
	parse_icscf_attr1(T2, undefined, Acc);
parse_icscf_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_icscf_attr1(T, Attr, Acc);
parse_icscf_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_icscf_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_icscf_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_icscf_attr1(T, undefined, Acc);
parse_icscf_attr1([], undefined, Acc) ->
	Acc.

parse_pcscf({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_pcscf({startElement,  _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_pcscf({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_pcscf({endElement, _Uri, "PCSCFFunction", QName},
		[#state{dn_prefix = [PcscfDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	PcscfAttr = parse_pcscf_attr(T2, undefined, []),
	ClassType = "PCSCFFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = PcscfDn,
			description = "IMS Proxy Call Session Control Function (PCSCF)",
			category = "IMS",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/PCSFFunction",
			specification = Spec,
			characteristic = PcscfAttr},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_pcscf({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

parse_pcscf_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_pcscf_attr1(Attributes, undefined, Acc).
%% @hidden
parse_pcscf_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfParameterList], T2} = pop(startElement, QName, T1),
	parse_pcscf_attr1(T2, undefined, Acc);
parse_pcscf_attr1([{endElement, {_, "linkList"} = QName} | T1],
		undefined, Acc) ->
	% @todo linkListType
	{[_ | _linkList], T2} = pop(startElement, QName, T1),
	parse_pcscf_attr1(T2, undefined, Acc);
parse_pcscf_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_pcscf_attr1(T, Attr, Acc);
parse_pcscf_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_pcscf_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_pcscf_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_pcscf_attr1(T, undefined, Acc);
parse_pcscf_attr1([], undefined, Acc) ->
	Acc.

parse_scscf({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_scscf({startElement,  _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		[#state{dn_prefix = [CurrentDn | _]} | _] = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	NewDn = CurrentDn ++ DnComponent,
	[#state{dn_prefix = [NewDn],
			parse_module = im_xml_generic, parse_function = parse_vsdata,
			parse_state = #generic_state{vs_data = #{"id" => DnComponent}},
			stack = [{startElement, QName, Attributes}]} | State];
parse_scscf({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_scscf({endElement, _Uri, "SCSCFFunction", QName},
		[#state{dn_prefix = [ScscfDn | _], stack = Stack,
		spec_cache = Cache}, #state{spec_cache = PrevCache} = PrevState | T1]) ->
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	ScscfAttr = parse_scscf_attr(T2, undefined, []),
	ClassType = "SCSCFFunction",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	Resource = #resource{name = ScscfDn,
			description = "IMS Serving Call Session Control Function (SCSCF)",
			category = "IMS",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/SCSCFFunction",
			specification = Spec,
			characteristic = ScscfAttr},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_scscf({endElement, _Uri, _LocalName, QName} = _Event,
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

parse_scscf_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_scscf_attr1(Attributes, undefined, Acc).
%% @hidden
parse_scscf_attr1([{endElement, {_, "vnfParametersList"} = QName} | T1],
		undefined, Acc) ->
	% @todo vnfParametersListType
	{[_ | _VnfParameterList], T2} = pop(startElement, QName, T1),
	parse_scscf_attr1(T2, undefined, Acc);
parse_scscf_attr1([{endElement, {_, "linkList"} = QName} | T1],
		undefined, Acc) ->
	% @todo linkListType
	{[_ | _linkList], T2} = pop(startElement, QName, T1),
	parse_scscf_attr1(T2, undefined, Acc);
parse_scscf_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_scscf_attr1(T, Attr, Acc);
parse_scscf_attr1([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
	parse_scscf_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_scscf_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_scscf_attr1(T, undefined, Acc);
parse_scscf_attr1([], undefined, Acc) ->
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
		Reason :: term().
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
