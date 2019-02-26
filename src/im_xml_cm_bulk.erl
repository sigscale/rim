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
-module(im_xml_cm_bulk).
-copyright('Copyright (c) 2018 SigScale Global Inc.').

%% export the im public API
-export([import/1]).

%% export the im private API
-export([parse_bulk_cm/2, parse_generic/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").

-record(state,
		{parse_module :: atom(),
		parse_function :: atom(),
		parse_state :: term(),
		dn_prefix = [] :: string(),
		subnet = []:: string(),
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
				Reason, EndTags, _EventState} ->
			error_logger:error_report(["Error parsing import file",
					{file, File}, {location, CurrentLocation},
					{line, LineNo}, {entity, EntityName},
					{tags, EndTags}, {error, Reason}]),
			{error, Reason};
		{fatal_error, Reason} ->
			error_logger:error_report(["Error parsing import file",
					{file, File}, {error, Reason}]),
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
parse_xml({startElement, _, "bulkCmConfigDataFile", _, []}, _,
		#state{parse_function = undefined} = State) ->
	 State#state{parse_module = ?MODULE,
			parse_function = parse_bulk_cm};
parse_xml(endDocument = _Event, _Location, State) ->
	State;
parse_xml(_Event, _Location, #state{parse_function = undefined} = State) ->
	State;
parse_xml({startPrefixMapping, _Prefix, _Uri}, _, State) ->
	State;
parse_xml({endPrefixMapping, _Prefix}, _, State) ->
	State;
parse_xml({ignorableWhitespace, _}, _, State) ->
	State;
parse_xml({comment, _Comment}, _, State) ->
	State;
parse_xml(_Event, _Location, #state{parse_module = Mod, parse_function = F} = State) ->
	Mod:F(_Event, State).

%% @hidden
parse_bulk_cm({startElement, _, "fileHeader", _, _}, State) ->
	State;
parse_bulk_cm({startElement, _, "configData", _, Attributes},
		#state{dn_prefix = [], stack = Stack} = State) ->
	case lists:keyfind("dnPrefix", 3, Attributes) of
		{_Uri, _Prefix, "dnPrefix", Dn} ->
			State#state{parse_module = ?MODULE,
					parse_function = parse_generic, dn_prefix = Dn,
					stack = [{startElement, "configData", Attributes} | Stack]};
		false ->
			State#state{parse_module = ?MODULE,
					parse_function = parse_generic, dn_prefix = [],
					stack = [{startElement, "configData", Attributes} | Stack]}
	end;
parse_bulk_cm({startElement, _, "fileFooter", _, _}, State) ->
	State;
parse_bulk_cm({endElement, _, "configData", _}, State) ->
	State;
parse_bulk_cm(_Event, #state{parse_module = ?MODULE,
		parse_function = parse_bulk_cm} = State) ->
	State.

%% @hidden
parse_generic({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_generic({startElement, _Uri, "SubNetwork", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{subnet = [], stack = Stack} = State) ->
	DnComponent = ",SubNetwork=" ++ Id,
	State#state{subnet = DnComponent,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_generic({startElement, _, "BssFunction", _, _Attributes} = Event, State) ->
	Mod = im_xml_geran,
	F = parse_bss,
	NewState = State#state{parse_module = Mod, parse_function = F},
	Mod:F(Event, NewState);
parse_generic({startElement,  _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_generic({endElement, _Uri, "SubNetwork", _QName}, State) ->
	State;
parse_generic({endElement,  _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

