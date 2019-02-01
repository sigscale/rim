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
		{current :: string(),
		resource = #resource{} :: resource()}).

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
		{event_state, {undefined, []}}],
	xmerl_sax_parser:file(File, Options).


%%----------------------------------------------------------------------
%%  The im public API
%%----------------------------------------------------------------------

-spec parse_xml(Event, Location, State) -> NewState
	when
		Event :: xmerl_sax_parser:event(),
		Location :: {CurrentLocation, Entityname, LineNo},
		CurrentLocation :: string(),
		Entityname :: string(),
		LineNo :: integer(),
		State :: {ParseFunction, Stack},
		ParseFunction :: atom(),
		Stack :: list(),
		NewState :: {ParseFunction, Stack}.
%% @doc Parse xml.
parse_xml(startDocument = _Event, _Location, State) ->
	State;
parse_xml({startElement, _, "bulkCmConfigDataFile", _, []}, _, {undefined, Stack} = State) ->
	{parse_bulk_cm, Stack};
parse_xml(endDocument = _Event, _Location, {F, Stack} = State) ->
	State;
parse_xml(_Event, _Location, {undefined, _Stack} = State) ->
	State;
parse_xml(_Event, _Location, {F, _Stack} = State) ->
	?MODULE:F(_Event, State).

%% @hidden
parse_bulk_cm({startElement, _, "fileHeader", _, _}, State) ->
	State;
parse_bulk_cm({startElement, _, "configData", _, Attributes}, {_, Stack} = _State) ->
	case lists:keyfind("dnPrefix", 3, Attributes) of
		{_Uri, _Prefix, "dnPrefix", DnPrefix} ->
			{parse_generic, [{startElement, "configData", Attributes} | Stack]};
		false ->
			{parse_generic, [{startElement, "configData", Attributes} | Stack]}
	end;
parse_bulk_cm({startElement, _, "fileFooter", _, _}, State) ->
	State;
parse_bulk_cm({endElement, _, "configData", _}, State) ->
	State;
parse_bulk_cm({ignorableWhitespace, _}, State) ->
	State;
parse_bulk_cm(Event, {parse_bulk_cm, _Stack} = State) ->
	State.

%% @hidden
parse_generic({ignorableWhitespace, _}, State) ->
	State;
parse_generic({characters, Chars}, {F, Stack}) ->
	{F, [{characters, Chars} | Stack]};
parse_generic({comment, _Comment}, State) ->
	State;
parse_generic({startElement,  _Uri, "BssFunction", QName,
		Attributes}, {F, Stack}) ->
	{parse_geran, [{startElement, QName, Attributes} | Stack]};
parse_generic({startElement,  _, _, QName, Attributes}, {F, Stack}) ->
	{F, [{startElement, QName, Attributes} | Stack]};
parse_generic({endElement,  _Uri, "BssFunction", QName}, {F, Stack}) ->
%	{parse_geran, pop(QName, Stack)};
	{parse_geran, Stack};
parse_generic({endElement,  _Uri, _LocalName, QName}, {F, Stack}) ->
	{F, [{endElement, QName} | Stack]}.

%% @hidden
parse_geran({ignorableWhitespace, _}, State) ->
	State;
parse_geran({characters, Chars}, {F, Stack}) ->
	{F, [{characters, Chars} | Stack]};
parse_geran({startPrefixMapping, _Prefix, _Uri}, State) ->
	State;
parse_geran({endPrefixMapping, _Prefix}, State) ->
	State;
parse_geran({comment, _Comment}, State) ->
	State;
parse_geran({startElement,  _Uri, "GsmCell", QName,
		Attributes}, {F, Stack}) ->
	{parse_gsm_cell, [{startElement, QName, Attributes} | Stack]};
parse_geran({startElement, _, _, QName, Attributes}, {F, Stack}) ->
	{F, [{startElement, QName, Attributes} | Stack]};
parse_geran({endElement,  _Uri, _LocalName, QName}, {F, Stack}) ->
	{F, [{endElement, QName} | Stack]}.

%% @hidden
parse_gsm_cell({ignorableWhitespace, _}, State) ->
	State;
parse_gsm_cell({characters, Chars}, {F, Stack}) ->
	{F, [{characters, Chars} | Stack]};
parse_gsm_cell({comment, _Comment}, State) ->
	State;
parse_gsm_cell({startElement, _Uri, _LocalName, QName, Attributes}, {F, Stack}) ->
	{F, [{startElement, QName, Attributes} | Stack]};
parse_gsm_cell({endElement,  _Uri, "GsmCell", QName}, {F, Stack}) ->
	{Value, NewStack} = pop(startElement, QName, Stack),
	[{startElement, {"gn", "GsmCell"}, Attributes1} | T1] = Value,
	{_Uri1, _Prefix, "id", ID} = lists:keyfind("id", 3, Attributes1),
	parse_gsm_cell_attr(ID, [], T1, NewStack);
parse_gsm_cell({endElement, _Uri, _LocalName, QName}, {F, Stack}) ->
	{F, [{endElement, QName} | Stack]}.

% @hidden
parse_gsm_cell_attr(ID, Characteristics,
		[{startElement, {"gn", "attributes"}, []} | T1] = _CellStack, Stack) ->
	{[_ | Attributes], T2} = pop(endElement, {"gn", "attributes"}, T1),
	Fchars = fun Fchars([{endElement, {"gn", Attr}} | T], undefined, Acc) ->
				Fchars(T, Attr, Acc);
			Fchars([{characters, Chars} | T], "userLabel" = Attr, Acc) ->
				Fchars(T, Attr, [{Attr, Chars} | Acc]);
			Fchars([{characters, Chars} | T], "cellIdentity" = Attr, Acc) ->
				Fchars(T, Attr, [{Attr, list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "cellAllocation" = Attr, Acc) ->
				Fchars(T, Attr, [{Attr, list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "ncc" = Attr, Acc) ->
				Fchars(T, Attr, [{Attr, list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "bcc" = Attr, Acc) ->
				Fchars(T, Attr, [{Attr, list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "lac" = Attr, Acc) ->
				Fchars(T, Attr, [{Attr, list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "mcc" = Attr, Acc) ->
				Fchars(T, Attr, [{Attr, list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "mnc" = Attr, Acc) ->
				Fchars(T, Attr, [{Attr, list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "rxLevAccessMin" = Attr, Acc) ->
				Fchars(T, Attr, [{Attr, list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "msTxPwrMaxCCH" = Attr, Acc) ->
				Fchars(T, Attr, [{Attr, list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "rfHoppingEnabled" = Attr, Acc) ->
				Fchars(T, Attr, [{Attr, list_to_atom(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "hoppingSequenceList" = Attr, Acc) ->
				Fchars(T, Attr, [{Attr, list_to_integer(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], "plmnPermitted" = Attr, Acc) ->
				Fchars(T, Attr, [{Attr, list_to_atom(Chars)} | Acc]);
			Fchars([{characters, Chars} | T], Attr, Acc) ->
				Fchars(T, Attr, [{Attr, Chars} | Acc]);
			Fchars([{startElement, {"gn", Attr}, _} | T], Attr, Acc) ->
				Fchars(T, undefined, Acc);
			Fchars([], _Attr, Acc) ->
				Acc
	end,
	CharAttr = Fchars(Attributes, undefined, []),
	NewCharacteristics =[{"attributes", CharAttr} | Characteristics],
	parse_gsm_cell_rels(ID, NewCharacteristics, T2, Stack, #{}).

% @hidden
parse_gsm_cell_rels(GsmCellID, Characteristics,
		[{startElement, {"gn", "GsmRelation"}, Attributes1} | T1] = _CellStack, Stack, Acc1) ->
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
			Frels([{startElement, {"gn", Attr}, _} | T], _Attr, Acc) ->
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
	parse_gsm_cell_rels(GsmCellID, Characteristics, T2, Stack, NewAcc);
parse_gsm_cell_rels(GsmCellID, Characteristics,
		[{startElement, {"un", "UtranRelation"}, Attributes1} | T1] = _CellStack, Stack, Acc1) ->
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
			Frels([{startElement, {"un", Attr}, _} | T], _Attr, Acc) ->
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
	parse_gsm_cell_rels(GsmCellID, Characteristics, T2, Stack, NewAcc);
parse_gsm_cell_rels(GsmCellID, Characteristics,
		[{startElement, {"en", "EutranRelation"}, Attributes1} | T1] = _CellStack, Stack, Acc1) ->
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
		Frels([{startElement, {"en", Attr}, _} | T], _Attr, Acc) ->
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
	parse_gsm_cell_rels(GsmCellID, Characteristics, T2, Stack, NewAcc);
parse_gsm_cell_rels(GsmCellID, Characteristics, CellStack, Stack, Acc) ->
	NewCharacteristics = maps:to_list(Acc) ++ Characteristics,
%	F1 = fun() ->
%		ResChars = #resource{characteristic = NewCharacteristics},
%		mnesia:write(inventory, ResChars, write),
%		ResChars
%	end,
%	case mnesia:transaction(F1) of
%		{atomic, Resource} ->
%			{ok, Resource};
%		{aborted, Reason} ->
%			exit(Reason)
%	end,
	parse_gsm_cell_pol(GsmCellID, NewCharacteristics, CellStack, Stack).

% @hidden
parse_gsm_cell_pol(GsmCellID, Characteristics,
		[{startElement, {"sp", "IneractEsPolicies"}, []} | T1] = _CellStack, Stack) ->
	{[_ | Attributes], T2} = pop(endElement, {"sp", "IneractEsPolicies"}, T1),
	{parse_geran, Stack};
parse_gsm_cell_pol(GsmCellID, Characteristics, _CellStack, Stack) ->
	{parse_geran, Stack}.

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

