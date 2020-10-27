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
-copyright('Copyright (c) 2020 SigScale Global Inc.').

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
	Resource = #resource{name = NSSDn,
			description = "Network Slice Subnet",
			category = "Slice",
			class_type = ClassType,
			base_type = "ResourceFunction",
			schema = ?PathInventorySchema ++ "/NetworkSliceSubnet",
			specification = Spec,
			characteristic = NSSAttr},
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
				{ok, #specification{id = Id, href = Href, name = Name,
						version = Version}} ->
					SpecRef = #specification_ref{id = Id, href = Href, name = Name,
							version = Version},
					{SpecRef, [SpecRef | Cache]};
				{error, Reason} ->
					throw({get_specification_name, Reason})
			end
	end.

