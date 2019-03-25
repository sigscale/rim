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
-module(im_xml_zte).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

%% export the im private API
-export([parse_zte_attr/2]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").

-define(PathCatalogSchema, "/resourceCatalogManagement/v3/resourceCatalogManagement").
-define(PathInventorySchema, "/resourceInventoryManagement/v3/resourceInventoryManagement").

%%----------------------------------------------------------------------
%%  The im private API
%%----------------------------------------------------------------------

parse_zte_attr(Stack, Acc) ->
	parse_zte_attr(Stack, [], Acc).
%% @hidden
parse_zte_attr([{endElement, {"zs", "vsDataBtsFunction"} = QName} | T] = _Stack,
		[] = _State, Acc) ->
	parse_zte_attr(T, [QName], Acc);
parse_zte_attr([{endElement, QName} | T] = _Stack, State, Acc) ->
	parse_zte_attr(T, [QName | State], Acc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "usedPwrCountSwitch"} | _] = State, Acc) ->
	NewAcc = attribute_add("usedPwrCountSwitch", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "isSupportLinkBackup"} | _] = State, Acc) ->
	NewAcc = attribute_add("isSupportLinkBackup", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "CpuThreshold"} | _] = State, Acc) ->
	NewAcc = attribute_add("CpuThreshold", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "isAssignedByTrxPriority"} | _] = State, Acc) ->
	NewAcc = attribute_add("isAssignedByTrxPriority", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "backupLinkSwitch"} | _] = State, Acc) ->
	NewAcc = attribute_add("backupLinkSwitch", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "operState"} | _] = State, Acc) ->
	NewAcc = attribute_add("operState", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "productStatus"} | _] = State, Acc) ->
	NewAcc = attribute_add("productStatus", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "AirSoftSyncAjustTime"} | _] = State, Acc) ->
	NewAcc = attribute_add("AirSoftSyncAjustTime", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "description"} | _] = State, Acc) ->
	NewAcc = attribute_add("description", Chars, Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "switchBackMode"} | _] = State, Acc) ->
	NewAcc = attribute_add("switchBackMode", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "autoSwitchTimer"} | _] = State, Acc) ->
	NewAcc = attribute_add("autoSwitchTimer", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "availStatus"} | _] = State, Acc) ->
	NewAcc = attribute_add("availStatus", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "adminState"} | _] = State, Acc) ->
	NewAcc = attribute_add("adminState", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "isSwitchBackOnlyIdle"} | _] = State, Acc) ->
	NewAcc = attribute_add("isSwitchBackOnlyIdle", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "siteId"} | _] = State, Acc) ->
	NewAcc = attribute_add("siteId", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "switchMode"} | _] = State, Acc) ->
	NewAcc = attribute_add("switchMode", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "isProtectTrxMax"} | _] = State, Acc) ->
	NewAcc = attribute_add("isProtectTrxMax", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "isTrafficControlByCpu"} | _] = State, Acc) ->
	NewAcc = attribute_add("isTrafficControlByCpu", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "isEnableEUIPBackup"} | _] = State, Acc) ->
	NewAcc = attribute_add("isEnableEUIPBackup", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "isAntPrimSecMeas"} | _] = State, Acc) ->
	NewAcc = attribute_add("isAntPrimSecMeas", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "isRUMeasStart"} | _] = State, Acc) ->
	NewAcc = attribute_add("isRUMeasStart", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "autoSwitchBackTimer"} | _] = State, Acc) ->
	NewAcc = attribute_add("autoSwitchBackTimer", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "bpSavingElectricitySwitch"} | _] = State, Acc) ->
	NewAcc = attribute_add("bpSavingElectricitySwitch", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", "bscMode"} | _] = State, Acc) ->
	NewAcc = attribute_add("bscMode", list_to_integer(Chars), Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([{characters, Chars} | T],
		[{"zs", Attr} | _] = State, Acc) ->
%default character handler
	NewAcc = attribute_add(Attr, Chars, Acc),
	parse_zte_attr(T, State, NewAcc);
parse_zte_attr([], _State, Acc) ->
	Acc;
parse_zte_attr([{startElement, _QName, _} | T], State, Acc) ->
	parse_zte_attr(T, State, Acc).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec attribute_add(Attribute, Value, NrmMap) -> NrmMap
	when
		Attribute :: string(),
		Value :: term(),
		NrmMap :: map().
%% @doc Add `Attribute' and `Value' to possibly missing attribute.
attribute_add(Attribute, Value, #{"attributes" := Attributes} = NrmMap) ->
%attribute_add(Attribute, Value, #{"attributes" := Attributes} = NrmMap) ->
	NrmMap#{"attributes" => Attributes#{Attribute => Value}};
attribute_add(Attribute, Value, NrmMap) ->
	NrmMap#{"attributes" => #{Attribute => Value}}.
