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
-module(im_xml_slice).
-copyright('Copyright (c) 2018 - 2023 SigScale Global Inc.').

%% export the im private API
-export([parse_network_slice/2, parse_ns_subnet/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

-define(PathInventorySchema, "/resourceInventoryManagement/v4/schema").

%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

%% @hidden
parse_network_slice({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_network_slice({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_network_slice({endElement, _Uri, "NetworkSlice", QName},
		[#state{dn_prefix = [NSDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "NetworkSlice",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NSAttr = parse_network_slice_attr(T2, undefined, []),
	Resource = #resource{name = NSDn,
			description = "Network Slice",
			category = "Slice",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/NetworkSlice",
			specification = Spec,
			characteristic = NSAttr},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_network_slice({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_network_slice_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_network_slice_attr1(Attributes, undefined, Acc).
% @hidden
parse_network_slice_attr1([{endElement,
		{_, "serviceProfileList"} = QName} | T1], undefined, Acc) ->
	% @todo serviceProfileList
	{[_ | _ServiceProfileList], T2} = pop(startElement, QName, T1),
	parse_network_slice_attr1(T2, undefined, Acc);
parse_network_slice_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_network_slice_attr1(T, Attr, Acc);
parse_network_slice_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_network_slice_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_network_slice_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_network_slice_attr1(T, undefined, Acc);
parse_network_slice_attr1([], undefined, Acc) ->
	Acc.

%% @hidden
parse_ns_subnet({characters, Chars}, [#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{characters, Chars} | Stack]} | T];
parse_ns_subnet({startElement, _, _, QName, Attributes},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{startElement, QName, Attributes} | Stack]} | T];
parse_ns_subnet({endElement, _Uri, "NetworkSliceSubnet", QName},
		[#state{dn_prefix = [NSSDn | _], stack = Stack, spec_cache = Cache},
		#state{spec_cache = PrevCache} = PrevState | T1]) ->
	ClassType = "NetworkSliceSubnet",
	{Spec, NewCache} = get_specification_ref(ClassType, Cache),
	{[_ | T2], _NewStack} = pop(startElement, QName, Stack),
	NSSAttr = parse_ns_subnet_attr(T2, undefined, []),
	ResNameSuffixes = ["AMFFunction=1", "AMFFunction=2", "SMFFunction=1",
			"UPFFunction=1", "UPFFunction=2", "AUSFFunction=1", "NSSFFunction=1",
			"UDMFunction=1", "PCFFunction=1",
			"AMFFunction=1,EP_N2=1", "N3IWFFunction=1,EP_N2=1",
			"UPFFunction=1,EP_N3=1", "N3IWFFunction=1,EP_N3=1",
			"SMFFunction=1,EP_N4=1", "UPFFunction=1,EP_N4=1",
			"UPFFunction=1,EP_N6=1",
			"SMFFunction=1,EP_N7=1", "PCFFunction=1,EP_N7=1",
			"AMFFunction=1,EP_N8=1", "UDMFunction=1,EP_N8=1",
			"UPFFunction=1,EP_N9=1", "UPFFunction=2,EP_N9=1",
			"SMFFunction=1,EP_N10=1", "UDMFunction=1,EP_N10=1",
			"AMFFunction=1,EP_N11=1", "SMFFunction=1,EP_N11=1",
			"AMFFunction=1,EP_N12=1", "AUSFFunction=1,EP_N12=1",
			"AUSFFunction=1,EP_N13=1", "UDMFunction=1,EP_N13=1",
			"AMFFunction=1,EP_N14=1", "AMFFunction=2,EP_N14=1",
			"AMFFunction=1,EP_N15=1", "PCFFunction=1,EP_N15=1",
			"AMFFunction=1,EP_N22=1", "NSSFFunction=1,EP_N22=1"],
	NamePrefix = "DC=sigscale.net,SubNetwork=12,ManagedElement=1,",
	Fresrel = fun(NameSuffix, Acc) ->
			Name = NamePrefix ++ NameSuffix,
			case im:get_resource_name(Name) of
				{ok, #resource{id = ResId, href = ResHref, name = ResName,
						class_type = ResType}} ->
					[#resource_rel{id = ResId, href = ResHref, name = ResName,
							ref_type = ResType, rel_type = "contains"} | Acc];
				{error, Reason} ->
					{error, Reason}
			end
	end,
	ResourceRels = lists:foldl(Fresrel, [], ResNameSuffixes),
	CPNameSuffixes = ["AMFFunction=1,EP_N2=1", "N3IWFFunction=1,EP_N2=1",
			"UPFFunction=1,EP_N3=1", "N3IWFFunction=1,EP_N3=1",
			"SMFFunction=1,EP_N4=1", "UPFFunction=1,EP_N4=1",
			"UPFFunction=1,EP_N6=1",
			"SMFFunction=1,EP_N7=1", "PCFFunction=1,EP_N7=1",
			"AMFFunction=1,EP_N8=1", "UDMFunction=1,EP_N8=1",
			"UPFFunction=1,EP_N9=1", "UPFFunction=2,EP_N9=1",
			"SMFFunction=1,EP_N10=1", "UDMFunction=1,EP_N10=1",
			"AMFFunction=1,EP_N11=1", "SMFFunction=1,EP_N11=1",
			"AMFFunction=1,EP_N12=1", "AUSFFunction=1,EP_N12=1",
			"AUSFFunction=1,EP_N13=1", "UDMFunction=1,EP_N13=1",
			"AMFFunction=1,EP_N14=1", "AMFFunction=2,EP_N14=1",
			"AMFFunction=1,EP_N15=1", "PCFFunction=1,EP_N15=1",
			"AMFFunction=1,EP_N22=1", "NSSFFunction=1,EP_N22=1"],
	Fresconn = fun(NameSuffix, Acc) ->
			Name = NamePrefix ++ NameSuffix,
			case lists:keyfind(Name, #resource_rel.name, ResourceRels) of
				#resource_rel{id = ResId, href = ResHref, name = ResName,
						ref_type = RefType} ->
					[#resource_ref{id = ResId, href = ResHref, name = ResName,
							ref_type = RefType} | Acc];
				false ->
					error_logger:warning_report(["Error reading resource rel",
							{resource_rel, Name}, {error, not_found}]),
					Acc
			end
	end,
	ConnectivitySuffixes = [{"EP_N2=1", "AMFFunction=1", "N3IWFFunction=1"},
			{"EP_N3=1", "UPFFunction=1", "N3IWFFunction=1"},
			{"EP_N4=1", "SMFFunction=1", "UPFFunction=1"},
			{"EP_N7=1", "SMFFunction=1", "PCFFunction=1"},
			{"EP_N8=1", "AMFFunction=1", "UDMFunction=1"},
			{"EP_N9=1", "UPFFunction=1", "UPFFunction=2"},
			{"EP_N10=1", "SMFFunction=1", "UDMFunction=1"},
			{"EP_N11=1", "AMFFunction=1", "SMFFunction=1"},
			{"EP_N12=1", "AMFFunction=1", "AUSFFunction=1"},
			{"EP_N13=1", "AUSFFunction=1", "UDMFunction=1"},
			{"EP_N14=1", "AMFFunction=1", "AMFFunction=2"},
			{"EP_N15=1", "AMFFunction=1", "PCFFunction=1"},
			{"EP_N22=1", "AMFFunction=1", "NSSFFunction=1"}],
	Fep = fun(CpName, EpName) ->
			case lists:keyfind(CpName, #resource_rel.name, ResourceRels) of
				#resource_rel{id = CpId, href = CpHref, name = CpName,
						ref_type = CpType} ->
					ConnectionPoint = [#resource_ref{id = CpId, href = CpHref,
							name = CpName, ref_type = CpType}],
					case lists:keyfind(EpName, #resource_rel.name, ResourceRels) of
						#resource_rel{id = EpId, href = EpHref, name = EpName,
								ref_type = EpType} ->
							#endpoint_ref{id = EpId, href = EpHref, name = EpName,
								ref_type = EpType, connection_point = ConnectionPoint};
						false ->
							{error, not_found}
					end;
				false ->
					{error, not_found}
			end
	end,
	Fcon = fun({CpSuffix, EpSuffix1, EpSuffix2}) ->
			EpName1 = NamePrefix ++ EpSuffix1,
			EP1 = Fep(EpName1 ++ "," ++ CpSuffix, EpName1),
			EpName2 = NamePrefix ++ EpSuffix2,
			EP2 = Fep(EpName2 ++ "," ++ CpSuffix, EpName2),
			[_, ConName, _] = string:tokens(CpSuffix, "_="),
			#connection{name = ConName, ass_type = "pointtoPoint",
					endpoint = [EP1, EP2]}
	end,
	Connections = lists:map(Fcon, ConnectivitySuffixes),
	Resource = #resource{name = NSSDn,
			description = "Network Slice Subnet",
			category = "Slice",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/NetworkSliceSubnet",
			specification = Spec,
			characteristic = NSSAttr,
			related = ResourceRels,
			connection_point = lists:foldl(Fresconn, [], CPNameSuffixes),
			connectivity = [#resource_graph{
             name = "Adjacency Graph",
             connection = lists:reverse(Connections)}]},
	case im:add_resource(Resource) of
		{ok, #resource{} = _R} ->
			[PrevState#state{spec_cache = [NewCache | PrevCache]} | T1];
		{error, Reason} ->
			throw({add_resource, Reason})
	end;
parse_ns_subnet({endElement, _Uri, _LocalName, QName},
		[#state{stack = Stack} = State | T]) ->
	[State#state{stack = [{endElement, QName} | Stack]} | T].

% @hidden
parse_ns_subnet_attr([{startElement, {_, "attributes"} = QName, []} | T1],
		undefined, Acc) ->
	{[_ | Attributes], _T2} = pop(endElement, QName, T1),
	parse_ns_subnet_attr1(Attributes, undefined, Acc).
% @hidden
parse_ns_subnet_attr1([{endElement, {_, Attr} = QName} | T1], undefined, Acc)
		when Attr == "mFIdList"; Attr == "ConstituentNSSIIdList" ->
	% @todo dnList
	{[_ | _DnList], T2} = pop(startElement, QName, T1),
	parse_ns_subnet_attr1(T2, undefined, Acc);
parse_ns_subnet_attr1([{endElement, {_, "nsInfo"} = QName} | T1],
		undefined, Acc) ->
	% @todo nsInfo
	{[_ | _NsInfo], T2} = pop(startElement, QName, T1),
	parse_ns_subnet_attr1(T2, undefined, Acc);
parse_ns_subnet_attr1([{endElement, {_, "sliceProfileList"} = QName} | T1],
		undefined, Acc) ->
	% @todo sliceProfileList
	{[_ | _SliceProfileList], T2} = pop(startElement, QName, T1),
	parse_ns_subnet_attr1(T2, undefined, Acc);
parse_ns_subnet_attr1([{endElement, {_, Attr}} | T], undefined, Acc) ->
	parse_ns_subnet_attr1(T, Attr, Acc);
parse_ns_subnet_attr1([{characters, Chars} | T], Attr, Acc) ->
	parse_ns_subnet_attr1(T, Attr,
			[#resource_char{name = Attr, value = Chars} | Acc]);
parse_ns_subnet_attr1([{startElement, {_, Attr}, _} | T], Attr, Acc) ->
	parse_ns_subnet_attr1(T, undefined, Acc);
parse_ns_subnet_attr1([], undefined, Acc) ->
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

