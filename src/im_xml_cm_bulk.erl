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
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% export the im public API
-export([import/1]).

%% export the im private API
-export([parse_bulk_cm/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").
-include("im_xml.hrl").

%%----------------------------------------------------------------------
%%  The im public API
%%----------------------------------------------------------------------

-spec import(File) -> Result
	when
		File :: string(),
		Result :: ok | ignore | {error, Reason},
		Reason :: term().
%% @doc Import a file into the inventory table.
import(File) when is_list(File) ->
	Options = [{event_fun, fun parse_xml/3},
		{event_state, #state{}}],
	case xmerl_sax_parser:file(File, Options) of
		{ok, _EventState, _Rest} ->
			ok;
		{Tag, {CurrentLocation, EntityName, LineNo},
				Reason, EndTags, _EventState} ->
			Message = case Tag of
				get_specification_name ->
					"Error getting specification for resource";
				add_resource ->
					"Error adding resource";
				fatal_error ->
					"Error parsing import file"
			end,
			error_logger:error_report([Message,
					{file, File}, {location, CurrentLocation},
					{line, LineNo}, {entity, EntityName},
					{tags, EndTags}, {error, Reason}]),
			{error, Reason};
		{error, Reason} ->
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
parse_xml({startElement, _, "bulkCmConfigDataFile", _, _} = _Event, _Location,
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
parse_bulk_cm({startElement, _, "configData", _, Attributes} = Event,
		#state{dn_prefix = [], stack = Stack} = State) ->
	case lists:keyfind("dnPrefix", 3, Attributes) of
		{_Uri, _Prefix, "dnPrefix", Dn} ->
			M = im_xml_generic,
			F = parse_generic,
			NewState = State#state{parse_module = M,
					parse_function = F, dn_prefix = [Dn],
					stack = [{startElement, "configData", Attributes} | Stack]},
			M:F(Event, NewState);
		false ->
			M = im_xml_generic,
			F = parse_generic,
			NewState = State#state{parse_module = M,
					parse_function = F, dn_prefix = [],
					stack = [{startElement, "configData", Attributes} | Stack]},
			M:F(Event, NewState)
	end;
parse_bulk_cm({startElement, _, "fileFooter", _, _}, State) ->
	State;
parse_bulk_cm({endElement, _, "configData", _}, State) ->
	State;
parse_bulk_cm(_Event, #state{parse_module = ?MODULE,
		parse_function = parse_bulk_cm} = State) ->
	State.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

