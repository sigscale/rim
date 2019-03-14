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
-export([parse_bulk_cm/2, parse_generic/2, parse_subnetwork/2,
		parse_mecontext/2, parse_managed_element/2, parse_vsdata/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").

-record(state,
		{parse_module :: atom(),
		parse_function :: atom(),
		parse_state :: term(),
		dn_prefix = [] :: string(),
		subnet = [] :: string(),
		me_context = [] :: string(),
		managed_element = [] :: string(),
		vs_data = [] :: string(),
		stack = [] :: list(),
		specs = [] :: [specification_ref()]}).
-type state() :: #state{}.

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
	State#state{parse_function = parse_subnetwork, subnet = DnComponent,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_generic({startElement, _Uri, "MeContext", _QName,
		_Attributes} = _Event, State) ->
	State#state{parse_function = parse_mecontext};
parse_generic({startElement, _Uri, "ManagedElement", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{managed_element = [], stack = Stack} = State) ->
	DnComponent = ",ManagedElement=" ++ Id,
	State#state{parse_function = parse_managed_element,
			managed_element = DnComponent,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_generic({startElement,  _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_generic({endElement,  _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%% @hidden
parse_subnetwork({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_subnetwork({startElement, _Uri, "SubNetwork", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{subnet = SubNetwork, stack = Stack} = State) ->
	DnComponent = SubNetwork ++ "," ++ Id,
	State#state{subnet = DnComponent,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_subnetwork({startElement, _Uri, "MeContext", _QName,
		_Attributes} = _Event, State) ->
	State#state{parse_function = parse_mecontext};
parse_subnetwork({startElement, _Uri, "ManagedElement", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{managed_element = [], stack = Stack} = State) ->
	DnComponent = ",ManagedElement=" ++ Id,
	State#state{parse_function = parse_managed_element,
			managed_element = DnComponent,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_subnetwork({startElement,  _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_subnetwork({endElement, _Uri, "SubNetwork", _QName}, State) ->
	State;
parse_subnetwork({endElement,  _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%% @hidden
parse_mecontext({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_mecontext({startElement, _Uri, "MeContext", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{me_context = [], stack = Stack} = State) ->
	DnComponent = ",MeContext=" ++ Id,
	State#state{me_context = DnComponent,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_mecontext({startElement, _Uri, "ManagedElement", _QName,
		_Attributes} = _Event, State) ->
	State#state{parse_function = parse_managed_element};
parse_mecontext({startElement,  _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_mecontext({endElement, _Uri, "MeContext", _QName}, State) ->
	State;
parse_mecontext({endElement,  _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%% @hidden
parse_managed_element({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_managed_element({startElement, _, "BssFunction", _, _Attributes} = Event, State) ->
	Mod = im_xml_geran,
	F = parse_bss,
	NewState = State#state{parse_module = Mod, parse_function = F},
	Mod:F(Event, NewState);
parse_managed_element({startElement, _, "NodeBFunction", _, _Attributes} = Event, State) ->
	Mod = im_xml_utran,
	F = parse_nodeb,
	NewState = State#state{parse_module = Mod, parse_function = F},
	Mod:F(Event, NewState);
parse_managed_element({startElement, _, "RncFunction", _, _Attributes} = Event, State) ->
	Mod = im_xml_utran,
	F = parse_rnc,
	NewState = State#state{parse_module = Mod, parse_function = F},
	Mod:F(Event, NewState);
parse_managed_element({startElement, _, "VsDataContainer", _, _Attributes} = _Event, State) ->
	State#state{parse_function = parse_vsdata};
parse_managed_element({startElement,  _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_managed_element({endElement, _Uri, "ManagedElement", _QName}, State) ->
	State;
parse_managed_element({endElement,  _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%% @hidden
parse_vsdata({characters, Chars}, #state{stack = Stack} = State) ->
	State#state{stack = [{characters, Chars} | Stack]};
parse_vsdata({startElement, _Uri, "VsDataContainer", QName,
		[{[], [], "id", Id}] = Attributes},
		#state{vs_data = [], stack = Stack} = State) ->
	DnComponent = ",VsDataContainer=" ++ Id,
	State#state{vs_data = DnComponent,
			stack = [{startElement, QName, Attributes} | Stack]};
parse_vsdata({startElement,  _, _, QName, Attributes},
		#state{stack = Stack} = State) ->
	State#state{stack = [{startElement, QName, Attributes} | Stack]};
parse_vsdata({endElement, _Uri, "VsDataContainer", _QName}, State) ->
	State;
parse_vsdata({endElement,  _Uri, _LocalName, QName},
		#state{stack = Stack} = State) ->
	State#state{stack = [{endElement, QName} | Stack]}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

