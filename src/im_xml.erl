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
-module(im_xml).
-copyright('Copyright (c) 2018 SigScale Global Inc.').

%% export the im public API
-export([import/1]).

%% export the im private API
-export([parse_bulk_cm/2, parse_generic/2, parse_geran/2, parse_gsm_cell/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
  
-record(state,
		{parseFunction :: atom(),
		dnPrefix = [] :: string(),
		subnet = []:: string(),
		bss = [] :: string(),
		btss = [] :: [string()],
		cells = [] :: [string()],
		stack = [] :: list()}).
-type state() :: #state{}.

%%----------------------------------------------------------------------
%%  The im public API
%%----------------------------------------------------------------------

-spec import(File) -> Result
	when
		File :: string(),
		Result :: ok | ignore | {error, Reason},
		Reason :: term().
%% @doc Import a file in the inventory table.
import(File) when is_list(File) ->
	Options = [{event_fun, fun parse_xml/3},
		{event_state, #state{}}],
	case xmerl_sax_parser:file(File, Options) of
		{ok, _EventState, _Rest} ->
			ok;
		{fatal_error, {CurrentLocation, EntityName, LineNo},
				Reason, EndTags, EventState} ->
			error_logger:error_report(["Error parsing import file",
					{file, File}, {location, CurrentLocation},
					{line, LineNo}, {entity, EntityName},
					{tags, EndTags}, {error, Reason}]),
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

-spec parse_xml(Event, Location, State) -> NewState
	when
		Event :: xmerl_sax_parser:event(),
		Location :: {CurrentLocation, Entityname, LineNo},
		CurrentLocation :: string(),
		Entityname :: string(),
		LineNo :: integer(),
		State :: state(),
		NewState :: state().
%% @doc Parse xml.
parse_xml(startDocument = _Event, _Location, State) ->
	State;
parse_xml({startElement, _, "bulkCmConfigDataFile", _, []}, _, #state{parseFunction = undefined} = State) ->
	 State#state{parseFunction = parse_bulk_cm};
parse_xml(endDocument = _Event, _Location, State) ->
	State;
parse_xml(_Event, _Location, #state{parseFunction = undefined} = State) ->
	State;
parse_xml({ignorableWhitespace, _}, _, State) ->
	State;
parse_xml(_Event, _Location, #state{parseFunction = F} = State) ->
	?MODULE:F(_Event, State).

%% @hidden
parse_bulk_cm({startElement, _, "fileHeader", _, _}, State) ->
	State;
parse_bulk_cm({startElement, _, "configData", _, Attributes},
		#state{parseFunction = _, dnPrefix = [], stack = Stack} = State) ->
	case lists:keyfind("dnPrefix", 3, Attributes) of
		{_Uri, _Prefix, "dnPrefix", Dn} ->
			State#state{parseFunction = parse_generic, dnPrefix = Dn, stack = [{startElement, "configData", Attributes} | Stack]};
		false ->
			State#state{parseFunction = parse_generic, dnPrefix = [], stack = [{startElement, "configData", Attributes} | Stack]}
	end;
parse_bulk_cm({startElement, _, "fileFooter", _, _}, State) ->
	State;
parse_bulk_cm({endElement, _, "configData", _}, State) ->
	State;
parse_bulk_cm({ignorableWhitespace, _}, State) ->
	State;
parse_bulk_cm(_Event, #state{parseFunction = parse_bulk_cm} = State) ->
	State.

%% @hidden
parse_generic({ignorableWhitespace, _}, State) ->
	State;
parse_generic({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_generic({comment, _Comment}, State) ->
	State;
parse_generic({startElement,  _Uri, "SubNetwork", QName,
		[{[], [], "id", Sub}] = _Attributes}, #state{subnet = [], stack = Stack} = State) ->
		SubId = ",SubNetwork=" ++ Sub,
	State#state{subnet = SubId, stack = [{startElement, QName, _Attributes} | Stack]};
parse_generic({startElement,  _Uri, "BssFunction", QName,
		[{[], [], "id", Bss}] = _Attributes}, #state{parseFunction = _F, bss = [], stack = Stack} =State) ->
	BssId = ",BssFunction=" ++ Bss,
	State#state{parseFunction = parse_geran, bss = BssId, stack = [{startElement, QName, _Attributes} | Stack]};
parse_generic({startElement,  _, _, QName, Attributes}, #state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_generic({endElement,  _Uri, "BssFunction", _QName}, State) ->
	State;
%	#state{parseFunction = F, dnPrefix = Dn, subnet = SubId, bss = BssId, stack = Stack};
parse_generic({endElement,  _Uri, _LocalName, QName}, #state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%% @hidden
parse_geran({ignorableWhitespace, _}, State) ->
	State;
parse_geran({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_geran({startPrefixMapping, _Prefix, _Uri}, State) ->
	State;
parse_geran({endPrefixMapping, _Prefix}, State) ->
	State;
parse_geran({comment, _Comment}, State) ->
	State;
parse_geran({startElement,  _Uri, "GsmCell", QName,
		Attributes}, #state{parseFunction = _F, stack = Stack} = State) ->
	State#state{parseFunction = parse_gsm_cell, stack = [{startElement, QName, Attributes} | Stack]};
parse_geran({startElement, _, _, QName, Attributes}, #state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_geran({endElement,  _Uri, _LocalName, QName}, #state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%% @hidden
parse_gsm_cell({ignorableWhitespace, _}, State) ->
	State;
parse_gsm_cell({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_gsm_cell({comment, _Comment}, State) ->
	State;
parse_gsm_cell({startElement, _Uri, _LocalName, QName, Attributes}, #state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_gsm_cell({endElement,  _Uri, "GsmCell", QName}, #state{stack = Stack} = State) ->
	{Value, NewStack} = pop(startElement, QName, Stack),
	[{startElement, {"gn", "GsmCell"}, Attributes1} | T1] = Value,
	{_Uri1, _Prefix, "id", ID} = lists:keyfind("id", 3, Attributes1),
	parse_gsm_cell_attr(ID, [], T1, State#state{stack = NewStack});
parse_gsm_cell({endElement, _Uri, _LocalName, QName}, #state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.
%parse_gsm_cell({endPrefixMapping, _Prefix}, State) ->
%	State.

% @hidden
parse_gsm_cell_attr(ID, Characteristics,
		[{startElement, {"gn", "attributes"}, []} | T1] = _CellStack, State) ->
	{[_ | Attributes], T2} = pop(endElement, {"gn", "attributes"}, T1),
	Fchars = fun Fchars([{endElement, {"gn", "hoppingSequenceList"}} | T], undefined, Acc) ->
				{[_ | _HsList], T3} = pop(startElement, {"gn", "hoppingSequenceList"}, T),
				% @todo Implement hoppingSequenceList
				Fchars(T3, undefined, Acc);
			Fchars([{endElement, {"gn", Attr}} | T], undefined, Acc) ->
				Fchars(T, Attr, Acc);
			Fchars([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
				Fchars(T, Attr, [#resource_char{name = Attr, value = Chars} | Acc]);
			Fchars([{characters, Chars} | T], "cellIdentity" = Attr, Acc) ->
				Fchars(T, Attr, [#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "cellAllocation" = Attr, Acc) ->
				CellAllocation = [list_to_integer(C) || C <- string:tokens(Chars, [$\s, $\t, $\n, $\r])],
				Fchars(T, Attr, [#resource_char{name = Attr, value = CellAllocation} | Acc]);
			Fchars([{characters, Chars} | T], "ncc" = Attr, Acc) ->
				Fchars(T, Attr, [#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "bcc" = Attr, Acc) ->
				Fchars(T, Attr, [#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "lac" = Attr, Acc) ->
				Fchars(T, Attr, [#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "mcc" = Attr, Acc) ->
				Fchars(T, Attr, [#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "mnc" = Attr, Acc) ->
				Fchars(T, Attr, [#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "rac" = Attr, Acc) ->
				Fchars(T, Attr, [#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "racc" = Attr, Acc) ->
				Fchars(T, Attr, [#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "tsc" = Attr, Acc) ->
				Fchars(T, Attr, [#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "rxLevAccessMin" = Attr, Acc) ->
				Fchars(T, Attr, [#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "msTxPwrMaxCCH" = Attr, Acc) ->
				Fchars(T, Attr, [#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
			Fchars([{characters, "false"} | T], "rfHoppingEnabled" = Attr, Acc) ->
				Fchars(T, Attr, [#resource_char{name = Attr, value = false} | Acc]);
			Fchars([{characters, "true"} | T], "rfHoppingEnabled" = Attr, Acc) ->
				Fchars(T, Attr, [#resource_char{name = Attr, value = true} | Acc]);
			Fchars([{characters, Chars} | T], "plmnPermitted" = Attr, Acc) ->
				Fchars(T, Attr, [#resource_char{name = Attr, value = list_to_integer(Chars)} | Acc]);
			Fchars([{startElement, {"gn", Attr}, _} | T], Attr, Acc) ->
				Fchars(T, undefined, Acc);
			Fchars([], _Attr, Acc) ->
				Acc
	end,
	NewCharacteristics = Fchars(Attributes, undefined, Characteristics),
	parse_gsm_cell_rels(ID, NewCharacteristics, T2, State, #{}).

% @hidden
parse_gsm_cell_rels(GsmCellID, Characteristics,
		[{startElement, {"gn", "GsmRelation"}, Attributes1} | T1] = _CellStack, State, Acc1) ->
	{_Uri, _Prefix, "id", RelID} = lists:keyfind("id", 3, Attributes1),
	{[_ | Attributes2], T2} = pop(endElement, {"gn", "GsmRelation"}, T1),
	Frels = fun Frels([{startElement, {"gn", "attributes"}, []}], _Attr, Acc) ->
				Acc;
			Frels([{endElement, {"gn", "attributes"}} | T], undefined, Acc) ->
				Frels(T, undefined, Acc);
			Frels([{endElement, {"gn", Attr}} | T], undefined, Acc) ->
				Frels(T, Attr, Acc);
			Frels([{characters, Chars} | T], "adjacentCell" = Attr, Acc) ->
				Frels(T, Attr, Acc#gsm_relation{adjacent_cell = Chars});
			Frels([{characters, Chars} | T], "bcch_frequency" = Attr, Acc) ->
				Frels(T, Attr, Acc#gsm_relation{bcch_frequency = list_to_integer(Chars)});
			Frels([{characters, Chars} | T], "ncc" = Attr, Acc) ->
				Frels(T, Attr, Acc#gsm_relation{ncc = list_to_integer(Chars)});
			Frels([{characters, Chars} | T], "bcc" = Attr, Acc) ->
				Frels(T, Attr, Acc#gsm_relation{bcc = list_to_integer(Chars)});
			Frels([{characters, Chars} | T], "lac" = Attr, Acc) ->
				Frels(T, Attr, Acc#gsm_relation{lac = list_to_integer(Chars)});
			Frels([{characters, Chars} | T], "is_remove_allowed" = Attr, Acc) ->
				Frels(T, Attr, Acc#gsm_relation{lac = list_to_atom(Chars)});
			Frels([{characters, Chars} | T], "is_hoa_allowed" = Attr, Acc) ->
				Frels(T, Attr, Acc#gsm_relation{is_hoa_allowed = list_to_atom(Chars)});
			Frels([{characters, Chars} | T], "is_covered_by" = Attr, Acc) ->
				Frels(T, Attr, Acc#gsm_relation{is_covered_by = list_to_atom(Chars)});
			Frels([{startElement, {"gn", _Attr}, _} | T], _Attr, Acc) ->
				Frels(T, undefined, Acc)
	end,
	R = Frels(Attributes2, undefined, #gsm_relation{id = RelID}),
   % maps:update_with/4 is a stdlib-3.0 feature
   % Fmap = fun(R1) -> [R | R1] end,
   % NewAcc = maps:update_with(gsmRelation, Fmap, [R], Acc1),
	R2 = case Acc1 of
		#{gsmRelation := R1} ->
			[R | R1];
		#{} ->
			[R]
	end,
	NewAcc = Acc1#{gsmRelation => R2},
	parse_gsm_cell_rels(GsmCellID, Characteristics, T2, State, NewAcc);
parse_gsm_cell_rels(GsmCellID, Characteristics,
		[{startElement, {"un", "UtranRelation"}, Attributes1} | T1] = _CellStack, State, Acc1) ->
	{_Uri, _Prefix, "id", RelID} = lists:keyfind("id", 3, Attributes1),
	{[_ | Attributes2], T2} = pop(endElement, {"un", "UtranRelation"}, T1),
	Frels = fun Frels([{startElement, {"un", "attributes"}, []}], _Attr, Acc) ->
				Acc;
			Frels([{endElement, {"un", "attributes"}} | T], undefined, Acc) ->
				Frels(T, undefined, Acc);
			Frels([{endElement, {"un", Attr}} | T], undefined, Acc) ->
				Frels(T, Attr, Acc);
			Frels([{characters, Chars} | T], "adjacentCell" = Attr, Acc) ->
				Frels(T, Attr, Acc#utran_relation{adjacent_cell = Chars});
			Frels([{startElement, {"un", _Attr}, _} | T], _Attr, Acc) ->
				Frels(T, undefined, Acc)
	end,
	R = Frels(Attributes2, undefined, #utran_relation{id = RelID}),
	R2 = case Acc1 of
		#{utranRelation := R1} ->
			[R | R1];
		#{} ->
			[R]
	end,
	NewAcc = Acc1#{utranRelation => R2},
	parse_gsm_cell_rels(GsmCellID, Characteristics, T2, State, NewAcc);
parse_gsm_cell_rels(GsmCellID, Characteristics,
		[{startElement, {"en", "EutranRelation"}, Attributes1} | T1] = _CellStack, State, Acc1) ->
	{_Uri, _Prefix, "id", RelID} = lists:keyfind("id", 3, Attributes1),
	{[_ | Attributes2], T2} = pop(endElement, {"en", "EutranRelation"}, T1),
	Frels = fun Frels([{startElement, {"en", "attributes"}, []}], _Attr, Acc) ->
            Acc;
		Frels([{endElement, {"en", "attributes"}} | T], undefined, Acc) ->
			Frels(T, undefined, Acc);
		Frels([{endElement, {"en", Attr}} | T], undefined, Acc) ->
			Frels(T, Attr, Acc);
		Frels([{characters, Chars} | T], "tci" = Attr, Acc) ->
			Frels(T, Attr, Acc#eutran_relation{tci = list_to_integer(Chars)});
		Frels([{characters, Chars} | T], "isRemoveAllowed" = Attr, Acc) ->
			Frels(T, Attr, Acc#eutran_relation{is_remove_allowed = list_to_atom(Chars)});
		Frels([{characters, Chars} | T], "isHoaAllowed" = Attr, Acc) ->
			Frels(T, Attr, Acc#eutran_relation{is_hoa_allowed = list_to_atom(Chars)});
		Frels([{characters, Chars} | T], "isIcicInformationSendAllowed" = Attr, Acc) ->
			Frels(T, Attr, Acc#eutran_relation{is_icic_information_send_allowed = list_to_atom(Chars)});
		Frels([{characters, Chars} | T], "isLbAllowed" = Attr, Acc) ->
			Frels(T, Attr, Acc#eutran_relation{is_lb_allowed = list_to_atom(Chars)});
		Frels([{characters, Chars} | T], "adjacentCell" = Attr, Acc) ->
			Frels(T, Attr, Acc#eutran_relation{adjacent_cell = Chars});
		Frels([{characters, Chars} | T], "isEsCoveredBy" = Attr, Acc) ->
			Frels(T, Attr, Acc#eutran_relation{is_es_covered_by = list_to_atom(Chars)});
		Frels([{characters, Chars} | T], "cellIndividualOffset" = Attr, Acc) ->
			Frels(T, Attr, Acc#eutran_relation{cell_individual_offset = Chars});
		Frels([{characters, Chars} | T], "qOffset" = Attr, Acc) ->
			Frels(T, Attr, Acc#eutran_relation{q_offset = Chars});
		Frels([{startElement, {"en", _Attr}, _} | T], _Attr, Acc) ->
			Frels(T, undefined, Acc)
	end,
	R = Frels(Attributes2, undefined, #eutran_relation{id = RelID}),
	R2 = case Acc1 of
		#{eutranRelation := R1} ->
			[R | R1];
		#{} ->
			[R]
	end,
	NewAcc = Acc1#{eutranRelation => R2},
	parse_gsm_cell_rels(GsmCellID, Characteristics, T2, State, NewAcc);
parse_gsm_cell_rels(GsmCellID, Characteristics, CellStack,
		#state{dnPrefix = Dn, subnet = SubId, bss = BssId, cells = Cells} = State, Acc1) ->
	GsmCellID1 = ",GsmCell=" ++ GsmCellID,
	F1 = fun(gsmRelation, R, Acc) ->
				[#resource_char{name = "gsmRelation", value = R} | Acc];
			(utranRelation, R, Acc) ->
				[#resource_char{name = "utranRelation", value = R} | Acc];
			(eUtranRelation, R, Acc) ->
				[#resource_char{name = "eUtranRelation", value = R} | Acc]
	end,
	NewCharacteristics = maps:fold(F1, Characteristics, Acc1), 
	Resource = #resource{name = Dn ++ SubId ++ BssId ++ GsmCellID1,
			description = "GSM radio",
			category = "RAN",
			class_type = "GsmCell",
			base_type = "ResourceFunction",
			schema = "/resourceInventoryManagement/v3/schema/GsmCell",
			specification = #specification_ref{},
			characteristic = NewCharacteristics},
	case im:add_resource(Resource) of
		{ok, #resource{id = ID}} ->
			NewState = State#state{cells = [ID | Cells]},
			parse_gsm_cell_pol(GsmCellID, NewCharacteristics, CellStack, NewState);
		{error, Reason} ->
			{error, Reason}
	end.

% @hidden
parse_gsm_cell_pol(_GsmCellID, _Characteristics,
		[{startElement, {"sp", "IneractEsPolicies"}, []} | T1] = _CellStack, #state{parseFunction = F} = State) ->
	{[_ | _Attributes], _T2} = pop(endElement, {"sp", "IneractEsPolicies"}, T1),
	State#state{parseFunction = parse_geran};
parse_gsm_cell_pol(_GsmCellID, _Characteristics, _CellStack, #state{parseFunction = F} = State) ->
	State#state{parseFunction = parse_geran}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-type event() :: {startElement, QName :: {Prefix :: string(), LocalName :: string()},
		Attributes :: [tuple()]} | {endElement, QName :: {Prefix :: string(), LocalName :: string()}}
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

