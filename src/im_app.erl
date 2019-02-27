%%% im_app.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018-2019 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%    http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This {@link //stdlib/application. application} behaviour callback
%%% 	module starts and stops the {@link //sigcale_im. sigscale_im} application.
%%%
-module(im_app).
-copyright('Copyright (c) 2018-2019 SigScale Global Inc.').

-behaviour(application).

%% callbacks needed for application behaviour
-export([start/2, stop/1, config_change/3]).
%% optional callbacks for application behaviour
-export([prep_stop/1, start_phase/3]).
%% export the im_app private API for installation
-export([install/0, install/1]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").

-record(state, {}).

-define(WAITFORSCHEMA, 11000).
-define(WAITFORTABLES, 11000).

%%----------------------------------------------------------------------
%% The im_app aplication callbacks
%%----------------------------------------------------------------------

-type start_type() :: normal | {takeover, node()} | {failover, node()}.
-spec start(StartType, StartArgs) -> Result
	when
		StartType :: start_type(),
		StartArgs :: term(),
		Result :: {'ok', pid()} | {'ok', pid(), State} | {'error', Reason},
		State :: #state{},
		Reason :: term().
%% @doc Starts the application processes.
%% @see //kernel/application:start/1
%% @see //kernel/application:start/2
%%
start(normal = _StartType, _Args) ->
	Tables = [catalog, category, candidate, specification, resource],
	case mnesia:wait_for_tables(Tables, 60000) of
		ok ->
			supervisor:start_link(im_sup, []);
		{timeout, BadTabList} ->
			case force(BadTabList) of
				ok ->
					supervisor:start_link(im_sup, []);
				{error, Reason} ->
					error_logger:error_report(["sigscale_im application failed to start",
							{reason, Reason}, {module, ?MODULE}]),
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%% The im_app private API
%%----------------------------------------------------------------------

-spec install() -> Result
	when
		Result :: {ok, Tables},
		Tables :: [atom()].
%% @equiv install([node() | nodes()])
install() ->
	Nodes = [node() | nodes()],
	install(Nodes).

-spec install(Nodes) -> Result
	when
		Nodes :: [node()],
		Result :: {ok, Tables},
		Tables :: [atom()].
%% @doc Initialize SigScale Fault Management (FM) tables.
%% 	`Nodes' is a list of the nodes where
%% 	{@link //sigscale_im. sigscale_im} tables will be replicated.
%%
%% 	If {@link //mnesia. mnesia} is not running an attempt
%% 	will be made to create a schema on all available nodes.
%% 	If a schema already exists on any node
%% 	{@link //mnesia. mnesia} will be started on all nodes
%% 	using the existing schema.
%%
%% @private
%%
install(Nodes) when is_list(Nodes) ->
	case mnesia:system_info(is_running) of
		no ->
			case mnesia:create_schema(Nodes) of
				ok ->
					error_logger:info_report("Created mnesia schema",
							[{nodes, Nodes}]),
					install1(Nodes);
				{error, Reason} ->
					error_logger:info_msg("Found existing schema.~n"),
					error_logger:error_report(["Failed to create schema",
							mnesia:error_description(Reason),
							{nodes, Nodes}, {error, Reason}]),
					{error, Reason}
			end;
		_ ->
			install2(Nodes)
	end.
%% @hidden
install1([Node] = Nodes) when Node == node() ->
	case mnesia:start() of
		ok ->
			error_logger:info_msg("Started mnesia~n"),
			install2(Nodes);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
					{error, Reason}]),
			{error, Reason}
	end;
install1(Nodes) ->
	case rpc:multicall(Nodes, mnesia, start, [], 60000) of
		{Results, []} ->
			F = fun(ok) ->
						false;
					(_) ->
						true
			end,
			case lists:filter(F, Results) of
				[] ->
					error_logger:info_report(["Started mnesia on all nodes",
							{nodes, Nodes}]),
					install2(Nodes);
				NotOKs ->
					error_logger:error_report(["Failed to start mnesia"
							" on all nodes", {nodes, Nodes}, {errors, NotOKs}]),
					{error, NotOKs}
			end;
		{Results, BadNodes} ->
			error_logger:error_report(["Failed to start mnesia"
					" on all nodes", {nodes, Nodes}, {results, Results},
					{badnodes, BadNodes}]),
			{error, {Results, BadNodes}}
	end.
%% @hidden
install2(Nodes) ->
	case mnesia:wait_for_tables([schema], ?WAITFORSCHEMA) of
		ok ->
			install3(Nodes, []);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason};
		{timeout, Tables} ->
			error_logger:error_report(["Timeout waiting for tables",
					{tables, Tables}]),
			{error, timeout}
	end.
%% @hidden
install3(Nodes, Acc) ->
	case mnesia:create_table(catalog, [{disc_copies, Nodes},
			{attributes, record_info(fields, catalog)}, {index, [name]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new resource catalog table.~n"),
			install4(Nodes, [catalog | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, catalog}} ->
			error_logger:info_msg("Found existing resource catalog table.~n"),
			install4(Nodes, [catalog | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install4(Nodes, Acc) ->
	case mnesia:create_table(category, [{disc_copies, Nodes},
			{attributes, record_info(fields, category)}, {index, [name]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new resource category table.~n"),
			install5(Nodes, [category | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, category}} ->
			error_logger:info_msg("Found existing resource category table.~n"),
			install5(Nodes, [category | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install5(Nodes, Acc) ->
	case mnesia:create_table(candidate, [{disc_copies, Nodes},
			{attributes, record_info(fields, candidate)}, {index, [name]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new resource candidate table.~n"),
			install6(Nodes, [candidate | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, candidate}} ->
			error_logger:info_msg("Found existing resource candidate table.~n"),
			install6(Nodes, [candidate | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install6(Nodes, Acc) ->
	case mnesia:create_table(specification, [{disc_copies, Nodes},
			{attributes, record_info(fields, specification)}, {index, [name]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new resource specification table.~n"),
			install7(Nodes, [specification | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, specification}} ->
			error_logger:info_msg("Found existing resource specification table.~n"),
			install7(Nodes, [specification | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install7(Nodes, Acc) ->
	case mnesia:create_table(resource, [{disc_copies, Nodes},
			{attributes, record_info(fields, resource)}, {index, [name]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new resource inventory table.~n"),
			install8(Nodes, [resource | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, resource}} ->
			error_logger:info_msg("Found existing resource inventory table.~n"),
			install8(Nodes, [resource | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install8(Nodes, Acc) ->
	case add_specifications() of
		ok ->
			error_logger:info_msg("Added 3GPP NRM resource specifications.~n"),
			install9(Nodes, Acc);
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install9(Nodes, Acc) ->
	case application:load(inets) of
		ok ->
			error_logger:info_msg("Loaded inets.~n"),
			install10(Nodes, Acc);
		{error, {already_loaded, inets}} ->
			install10(Nodes, Acc)
	end.
%% @hidden
install10(Nodes, Acc) ->
	case application:get_env(inets, services) of
		{ok, InetsServices} ->
			install11(Nodes, Acc, InetsServices);
		undefined ->
			error_logger:info_msg("Inets services not defined. "
					"User table not created~n"),
			install15(Nodes, Acc)
	end.
%% @hidden
install11(Nodes, Acc, InetsServices) ->
	case lists:keyfind(httpd, 1, InetsServices) of
		{httpd, HttpdInfo} ->
			install12(Nodes, Acc, lists:keyfind(directory, 1, HttpdInfo));
		false ->
			error_logger:info_msg("Httpd service not defined. "
					"User table not created~n"),
			install15(Nodes, Acc)
	end.
%% @hidden
install12(Nodes, Acc, {directory, {_, DirectoryInfo}}) ->
	case lists:keyfind(auth_type, 1, DirectoryInfo) of
		{auth_type, mnesia} ->
			install13(Nodes, Acc);
		_ ->
			error_logger:info_msg("Auth type not mnesia. "
					"User table not created~n"),
			install15(Nodes, Acc)
	end;
install12(Nodes, Acc, false) ->
	error_logger:info_msg("Auth directory not defined. "
			"User table not created~n"),
	install15(Nodes, Acc).
%% @hidden
install13(Nodes, Acc) ->
	case mnesia:create_table(httpd_user, [{type, bag}, {disc_copies, Nodes},
			{attributes, record_info(fields, httpd_user)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new httpd_user table.~n"),
			install14(Nodes, [httpd_user | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, httpd_user}} ->
			error_logger:info_msg("Found existing httpd_user table.~n"),
			install14(Nodes, [httpd_user | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install14(Nodes, Acc) ->
	case mnesia:create_table(httpd_group, [{type, bag}, {disc_copies, Nodes},
			{attributes, record_info(fields, httpd_group)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new httpd_group table.~n"),
			install15(Nodes, [httpd_group | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, httpd_group}} ->
			error_logger:info_msg("Found existing httpd_group table.~n"),
			install15(Nodes, [httpd_group | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install15(_Nodes, Tables) ->
	case mnesia:wait_for_tables(Tables, ?WAITFORTABLES) of
		ok ->
			install16(Tables, lists:member(httpd_user, Tables));
		{timeout, Tables} ->
			error_logger:error_report(["Timeout waiting for tables",
					{tables, Tables}]),
			{error, timeout};
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
					{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install16(Tables, true) ->
	case inets:start() of
		ok ->
			error_logger:info_msg("Started inets.~n"),
			install17(Tables);
		{error, {already_started, inets}} ->
			install17(Tables);
		{error, Reason} ->
			error_logger:error_msg("Failed to start inets~n"),
			{error, Reason}
	end;
install16(Tables, false) ->
	{ok, Tables}.
%% @hidden
install17(Tables) ->
	case im:get_user() of
		{ok, []} ->
			case im:add_user("admin", "admin", "en") of
				{ok, _LastModified} ->
					error_logger:info_report(["Created a default user",
							{username, "admin"}, {password, "admin"},
							{locale, "en"}]),
					{ok, Tables};
				{error, Reason} ->
					error_logger:error_report(["Failed to creat default user",
							{username, "admin"}, {password, "admin"},
							{locale, "en"}]),
					{error, Reason}
			end;
		{ok, Users} ->
			error_logger:info_report(["Found existing http users",
					{users, Users}]),
			{ok, Tables};
		{error, Reason} ->
			error_logger:error_report(["Failed to list http users",
				{error, Reason}]),
			{error, Reason}
	end.

-spec start_phase(Phase, StartType, PhaseArgs) -> Result
	when
		Phase :: atom(),
		StartType :: start_type(),
		PhaseArgs :: term(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Called for each start phase in the application and included
%% 	applications.
%% @see //kernel/app
%%
start_phase(_Phase, _StartType, _PhaseArgs) ->
	ok.

-spec prep_stop(State) -> #state{}
	when
		State :: #state{}.
%% @doc Called when the application is about to be shut down,
%% 	before any processes are terminated.
%% @see //kernel/application:stop/1
%%
prep_stop(State) ->
	State.

-spec stop(State) -> any()
	when
		State :: #state{}.
%% @doc Called after the application has stopped to clean up.
%%
stop(_State) ->
	ok.

-spec config_change(Changed, New, Removed) -> ok
	when
		Changed:: [{Par, Val}],
		New :: [{Par, Val}],
		Removed :: [Par],
		Par :: atom(),
		Val :: atom().
%% @doc Called after a code replacement, if there are any 
%% 	changes to the configuration parameters.
%%
config_change(_Changed, _New, _Removed) ->
	ok.

%%----------------------------------------------------------------------
%% internal functions
%%----------------------------------------------------------------------

-spec force(Tables) -> Result
	when
		Tables :: [TableName],
		Result :: ok | {error, Reason},
		TableName :: atom(),
		Reason :: term().
%% @doc Try to force load bad tables.
force([H | T]) ->
	case mnesia:force_load_table(H) of
		yes ->
			force(T);
		ErrorDescription ->
			{error, ErrorDescription}
	end;
force([]) ->
	ok.

-spec add_specifications() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Add 3GPP NRM `ResourceFunctionSpecification's to resource table.
add_specifications() ->
	add_bss([]).
%% @hidden
add_bss(NrmClasses) ->
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VnfParametersList"},
	BtsSiteMgr = #specification_char{name = "BtsSiteMgr",
			description = "Base Tranceiver Station (BTS) Sites",
			value_type = "BtsSiteMgrList",
			value_schema = "/resourceCatalogManagement/v3/schema/geranNrm#/definitions/BtsSiteMgrList"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VsDataContainerList"},
	Chars = [UserLabel, VnfParametersList, BtsSiteMgr, VsDataContainer],
	BssFunctionSpecification = #specification{name = "BssFunction",
			description = "GSM Base Station Subsystem (BSS)",
			class_type = "BssFunctionSpecification",
			schema = "/resourceCatalogManagement/v3/schema/BssFunctionSpecification",
			base_type = "ResourceFunctionSpecification",
			status = "Active",
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "BssFunction",
					schema = "/resourceInventoryManagement/v3/schema/BssFunction"},
			characteristic = Chars},
	case im:add_specification(BssFunctionSpecification) of
		{ok, _} ->
			add_bts(["BssFunction" | NrmClasses]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_bts(NrmClasses) ->
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VnfParametersList"},
	Latitude = #specification_char{name = "latitude",
			description = "Latitude of the site manager location based on (WGS 84) global reference frame",
			value_type = "latitude",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/latitude"},
	Longitude = #specification_char{name = "longitude",
			description = "Longitude of the site manager location based on (WGS 84) global reference frame",
			value_type = "longitude",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/longitude"},
	OperationalState = #specification_char{name = "operationalState",
			description = "Indicates the operational state of the object instance",
			value_type = "operationalStateType",
			value_schema = "/resourceCatalogManagement/v3/schema/stateManagementIRPNrm#/definitions/operationalStateType"},
	GsmCell = #specification_char{name = "GsmCell",
			description = "GSM Radio Cell",
			value_type = "GsmCellList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/GsmCellList"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VsDataContainerList"},
	InterRatEsPolicies = #specification_char{name = "InterRatEsPolicies",
			description = "Inter-RAT energy saving policies information.",
			value_type = "InterRatEsPoliciesList",
			value_schema = "/resourceCatalogManagement/v3/schema/sonPolicyNrm#/definitions/InterRatEsPoliciesList"},
	Chars = [UserLabel, VnfParametersList, Latitude, Longitude,
			OperationalState, GsmCell, VsDataContainer, InterRatEsPolicies],
	BtsSiteMgrSpecification = #specification{name = "BtsSiteMgr",
			description = "GSM Base Transceiver Station (BTS)",
			class_type = "BtsSiteMgrSpecification",
			schema = "/resourceCatalogManagement/v3/schema/BtsSiteMgrSpecification",
			base_type = "ResourceFunctionSpecification",
			status = "Active",
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "BtsSiteManager",
					schema = "/resourceInventoryManagement/v3/schema/BtsSiteManager"},
			characteristic = Chars},
	case im:add_specification(BtsSiteMgrSpecification) of
		{ok, _} ->
			add_gsmcell(["BtsSiteMgr" | NrmClasses]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_gsmcell(NrmClasses) ->
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VnfParametersList"},
	CellIdentity = #specification_char{name = "cellIdentity",
			description = "Cell Identity (3GPP 24.008)",
			value_type = "integer",
			char_value = [#spec_char_value{from = 0, to = 65535}]},
	CellAllocation = #specification_char{name = "cellAllocation",
			description = "The set of Absolute Radio Frequency Channel Number (ARFCN) (3GPP 44.018)",
			value_type = "array"},
	Ncc = #specification_char{name = "ncc",
			description = "Network Colour Code (NCC) (3GPP 44.018)",
			value_type = "integer",
			char_value = [#spec_char_value{from = 0, to = 7}]},
	Bcc = #specification_char{name = "bcc",
			description = "Base Station Colour Code (BCC) (3GPP 44.018)",
			value_type = "integer",
			char_value = [#spec_char_value{from = 0, to = 7}]},
	Lac = #specification_char{name = "lac",
			description = "Location Area Code (LAC) (3GPP 24.008)",
			value_type = "integer",
			char_value = [#spec_char_value{from = 1, to = 65533}]},
	Mcc = #specification_char{name = "mcc",
			description = "Mobile Country Code (MCC) (3GPP 23.003)",
			value_type = "integer",
			char_value = [#spec_char_value{from = 1, to = 999}]},
	Mnc = #specification_char{name = "mnc",
			description = "Mobile Network Code (MNC) (3GPP 23.003)",
			value_type = "integer",
			char_value = [#spec_char_value{from = 1, to = 999}]},
	Rac = #specification_char{name = "rac",
			description = "Routing Area Code (RAC) (3GPP 44.018)",
			value_type = "integer",
			char_value = [#spec_char_value{from = 0, to = 255}]},
	Racc = #specification_char{name = "racc",
			description = "Routing Area Colour Code (RACC) (3GPP 44.018)",
			value_type = "integer",
			char_value = [#spec_char_value{from = 0, to = 7}]},
	Tsc = #specification_char{name = "tsc",
			description = "Training Sequence Code (TSC) (3GPP 44.018)",
			value_type = "integer",
			char_value = [#spec_char_value{from = 0, to = 7}]},
	RxrLevAccessMinM = #specification_char{name = "rxrLevAccessMinM",
			description = "Minimum Access Level (RXLEV_ACCESS_MIN) (3GPP TS 45.008)",
			value_type = "integer",
			char_value = [#spec_char_value{from = 0, to = 63}]},
	MsTxPwrMaxCCH = #specification_char{name = "msTxPwrMaxCCH",
			description = "Maximum Transmission Power (MS_TXPWR_MAX_CCH) (3GPP 45.008)",
			value_type = "integer",
			char_value = [#spec_char_value{from = 0, to = 31}]},
	RfHoppingEnabled = #specification_char{name = "rfHoppingEnabled",
			description = "Indicates if frequency hopping is enabled",
			value_type = "boolean"},
	HoppingSequenceList = #specification_char{name = "hoppingSequenceList",
			description = "List of hopping sequence: MA (3GPP 44.018) and HSN (3GPP 45.002)",
			value_type = "HoppingSequenceList",
			value_schema = "/resourceCatalogManagement/v3/schema/geranNrm#/definitions/HoppingSequenceList"},
	PlmnPermitted = #specification_char{name = "plmnPermitted",
			description = "Network Colour Code (NCC) Permitted (NCC_PERMITTED) (3GPP 45.008)",
			value_type = "integer",
			char_value = [#spec_char_value{from = 0, to = 255}]},
	GsmRelation = #specification_char{name = "GsmRelation",
			description = "Neighbour cell Relation (NR) from a source cell to a target GsmCell",
			value_type = "GsmRelationList",
			value_schema = "/resourceCatalogManagement/v3/schema/geranNrm#/definitions/GsmRelationList"},
	UtranRelation = #specification_char{name = "UtranRelation",
			description = "Neighbour cell Relation (NR) from a source cell to a target UtranCell",
			value_type = "UtranRelationList",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/UtranRelationList"},
	EUtranRelation = #specification_char{name = "EUtranRelation",
			description = "Neighbour cell Relation (NR) from a source cell to a target EUtranCell",
			value_type = "EUtranRelationList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/EUtranRelationList"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VsDataContainerList"},
	InterRatEsPolicies = #specification_char{name = "InterRatEsPolicies",
			description = "Inter-RAT energy saving policies information",
			value_type = "InterRatEsPoliciesList",
			value_schema = "/resourceCatalogManagement/v3/schema/sonPolicyNrm#/definitions/InterRatEsPoliciesList"},
	Chars = [UserLabel, VnfParametersList, CellIdentity, CellAllocation, Ncc,
			Bcc, Lac, Mcc, Mnc, Rac, Racc, Tsc, RxrLevAccessMinM, MsTxPwrMaxCCH,
			RfHoppingEnabled, HoppingSequenceList, PlmnPermitted, GsmRelation,
			UtranRelation, EUtranRelation, VsDataContainer, InterRatEsPolicies],
	GsmCellSpecification = #specification{name = "GsmCell",
			description = "GSM Radio Cell",
			class_type = "GsmCellSpecification",
			schema = "/resourceCatalogManagement/v3/schema/GsmCellSpecification",
			base_type = "ResourceFunctionSpecification",
			status = "Active",
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "GsmCell",
					schema = "/resourceInventoryManagement/v3/schema/GsmCell"},
			characteristic = Chars},
	case im:add_specification(GsmCellSpecification) of
		{ok, _} ->
			add_rnc(["GsmCell" | NrmClasses]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_rnc(NrmClasses) ->
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VnfParametersList"},
	PeeParametersList = #specification_char{name = "peeParametersList",
			description = "Parameter list for the control and monitoring of power, energy and environment",
			value_type = "peeParametersListType",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/peeParametersListType"},
	Mcc = #specification_char{name = "mcc",
			description = "Mobile Country Code (MCC) (3GPP 23.003)",
			value_type = "integer",
			char_value = [#spec_char_value{from = 1, to = 999}]},
	Mnc = #specification_char{name = "mnc",
			description = "Mobile Network Code (MNC) (3GPP 23.003)",
			value_type = "integer",
			char_value = [#spec_char_value{from = 1, to = 999}]},
	RncId = #specification_char{name = "rncId",
			description = "Unique RNC ID for the associated RNC (3GPP 23.003)",
			value_type = "integer",
			char_value = [#spec_char_value{from = 0, to = 4095}]},
	SiptoSupported = #specification_char{name = "siptoSupported",
			description = "Indicates whether the RNC supports SIPTO function",
			value_type = "boolean"},
	TceIDMappingInfoList = #specification_char{name = "tceIDMappingInfoList",
			description = "List of Trace Collection Entity (TCE) ID and IP Address (3GPP 32.422)",
			value_type = "TceIDMappingInfoList",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/TceIDMappingInfoList"},
	SharNetTceMappingInfoList = #specification_char{name = "sharNetTceMappingInfoList",
			description = "List of shared PLMN ID, Trace Collection Entity (TCE) ID and IP Address (3GPP 32.422)",
			value_type = "SharNetTceMappingInfoList",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/SharNetTceMappingInfoList"},
	UtranCellFDD = #specification_char{name = "UtranCellFDD",
			description = "Frequency Division Duplex (FDD) radio cells controlled by an RNC",
			value_type = "UtranCellFDDList",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/UtranCellFDDList"},
	UtranCellTDDLcr = #specification_char{name = "UtranCellTDDLcr",
			description = "Time Division Duplex (TDD) low chip rate (LCR) radio cell controlled by an RNC",
			value_type = "UtranCellTDDLcrList",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/UtranCellTDDLcrList"},
	UtranCellTDDHcr = #specification_char{name = "UtranCellTDDHcr",
			description = "Time Division Duplex (TDD) high chip rate (HCR) radio cell controlled by an RNC",
			value_type = "UtranCellTDDHcrList",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/UtranCellTDDHcrList"},
	IubLink = #specification_char{name = "IubLink",
			description = "Logical link to a NodeB as seen from the RNC",
			value_type = "IubLinkList",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/IubLinkList"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VsDataContainerList"},
	Chars = [UserLabel, VnfParametersList, PeeParametersList, Mcc, Mnc, RncId,
			SiptoSupported, TceIDMappingInfoList, SharNetTceMappingInfoList,
			UtranCellFDD, UtranCellTDDLcr, UtranCellTDDHcr, IubLink,
			VsDataContainer],
	RncFunctionSpecification = #specification{name = "RncFunction",
			description = "UMTS Radio Network Controller (RNC)",
			class_type = "RncFunctionSpecification",
			schema = "/resourceCatalogManagement/v3/schema/RncFunctionSpecification",
			base_type = "ResourceFunctionSpecification",
			status = "Active",
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "RncFunction",
					schema = "/resourceInventoryManagement/v3/schema/RncFunction"},
			characteristic = Chars},
	case im:add_specification(RncFunctionSpecification) of
		{ok, _} ->
			add_nodeb(["RncFunction" | NrmClasses]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_nodeb(NrmClasses) ->
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VnfParametersList"},
	PeeParametersList = #specification_char{name = "peeParametersList",
			description = "Parameter list for the control and monitoring of power, energy and environment",
			value_type = "peeParametersListType",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/peeParametersListType"},
	NodeBFunctionIubLink = #specification_char{name = "nodeBFunctionIubLink",
			description = "DN of a logical link to an RNC as seen by a NodeB",
			value_type = "dn",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dn"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VsDataContainerList"},
	Chars = [UserLabel, VnfParametersList, PeeParametersList,
			NodeBFunctionIubLink, VsDataContainer],
	NodeBFunctionSpecification = #specification{name = "NodeBFunction",
			description = "UMTS NodeB",
			class_type = "NodeBFunctionSpecification",
			schema = "/resourceCatalogManagement/v3/schema/NodeBFunctionSpecification",
			base_type = "ResourceFunctionSpecification",
			status = "Active",
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "NodeBFunction",
					schema = "/resourceInventoryManagement/v3/schema/NodeBFunction"},
			characteristic = Chars},
	case im:add_specification(NodeBFunctionSpecification) of
		{ok, _} ->
			add_utrancellfdd(["NodeBFunction" | NrmClasses]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_utrancellfdd(NrmClasses) ->
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VnfParametersList"},
	CId = #specification_char{name = "cId",
			description = "Identifier of a cell in one RNC ('C-ID' in 3GPP 25.433)",
			value_type = "cId",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/cId"},
	LocalCellId = #specification_char{name = "localCellId",
			description = "Uniquely identify a cell in a Node B ('Local Cell ID' in 3GPP 25.433)",
			value_type = "localCellId",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/localCellId"},
	MaximumTransmissionPower = #specification_char{name = "maximumTransmissionPower",
			description = "Maximum power for all downlink channels ('Maximum Transmission Power' in 3GPP 25.433)",
			value_type = "maximumTransmissionPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/maximumTransmissionPower"},
	CellMode = #specification_char{name = "cellMode",
			description = "Identifies the cell mode",
			value_type = "cellMode",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/cellMode"},
	PichPower = #specification_char{name = "pichPower",
			description = "Power of the PICH channel in the cell ('PICH Power' in 3GPP 25.433)",
			value_type = "pichPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/pichPower"},
	PchPower = #specification_char{name = "pchPower",
			description = "PCH transport channel in the cell ('PCH Power' in 3GPP 25.433)",
			value_type = "pchPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/pchPower"},
	FachPower = #specification_char{name = "fachPower",
			description = "Maximum power of FACH transport channel ('Max FACH Power' in 3GPP 25.433)",
			value_type = "fachPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/fachPower"},
	Lac = #specification_char{name = "lac",
			description = "Location Area Code (LAC) (3GPP 23.003)",
			value_type = "lac",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/lac"},
	Rac = #specification_char{name = "rac",
			description = "Routing Area Code (RAC) (3GPP 23.003)",
			value_type = "rac",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/rac"},
	Sac = #specification_char{name = "sac",
			description = "Service Area Code (SAC) (3GPP 23.003)",
			value_type = "sac",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/sac"},
	UraList = #specification_char{name = "uraList",
			description = "UTRAN Registration Area identities ('URA identity' in 3GPP 25.331)",
			value_type = "uraList",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/uraList"},
	UtranCellIubLink = #specification_char{name = "utranCellIubLink",
			description = "Distinguished Name (DN) of IubLink",
			value_type = "dn",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dn"},
	RelatedAntennaList = #specification_char{name = "relatedAntennaList",
			description = "Distinguished Names (DN) of AntennaFunction(s) (3GPP 28.662)",
			value_type = "dnList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dnList"},
	RelatedTmaList = #specification_char{name = "relatedTmaList",
			description = "Distinguished Names (DN) of ('TmaFunction' in 3GPP 28.662)",
			value_type = "dnList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dnList"},
	OperationalState = #specification_char{name = "operationalState",
			description = "Operational state describes whether physically installed and working or not (ITU-T X.731)",
			value_type = "operationalStateType",
			value_schema = "/resourceCatalogManagement/v3/schema/stateManagementIRP#/definitions/operationalStateType"},
	HsFlag = #specification_char{name = "hsFlag",
			description = "High-Speed Downlink Packet Access (HSDPA) supported flag",
			value_type = "hsFlag",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/hsFlag"},
	HsEnable = #specification_char{name = "hsEnable",
			description = "High-Speed Downlink Packet Access (HSDPA) enabled flag",
			value_type = "hsEnable",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/hsEnable"},
	NumOfHspdschs = #specification_char{name = "numOfHspdschs",
			description = "In FDD the number of codes, in TDD the number of HS-PDSCHs (3GPP 25.433)",
			value_type = "numOfHspdschs",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/numOfHspdschs"},
	NumOfHsscchs = #specification_char{name = "numOfHsscchs",
			description = "Number of HS-SCCHs for one cell (3GPP 25.433)",
			value_type = "numOfHsscchs",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/numOfHsscchs"},
	FrameOffset = #specification_char{name = "frameOffset",
			description = "Required offset in neighbouring cells monitoring (3GPP 25.423)",
			value_type = "frameOffset",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/frameOffset"},
	CellIndividualOffset = #specification_char{name = "cellIndividualOffset",
			description = "Relevant for hand over (HO) decision (3GPP 25.331)",
			value_type = "cellIndividualOffset",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/cellIndividualOffset"},
	HcsPrio = #specification_char{name = "hcsPrio",
			description = "Hierarchical cell structure (HCS) priority in cell (re)selection (3GPP 25.331)",
			value_type = "hcsPrio",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/hcsPrio"},
	MaximumAllowedUlTxPower = #specification_char{name = "maximumAllowedUlTxPower",
			description = "Maximum allowed uplink transmit power in cell (re)selection (3GPP 25.331)",
			value_type = "maximumAllowedUlTxPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/maximumAllowedUlTxPower"},
	SnaInformation = #specification_char{name = "snaInformation",
			description = "List of Shared Networks Access Control (SNAC) (3GPP 25.423)",
			value_type = "snaInformation",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/snaInformation"},
	QrxlevMin = #specification_char{name = "qrxlevMin",
			description = "Minimum required RX level in cell (re)selection ('QrxlevMin' in 3GPP 25.331)",
			value_type = "qrxlevMin",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/qrxlevMin"},
	DeltaQrxlevmin = #specification_char{name = "deltaQrxlevmin",
			description = "Delta required RX level in cell (re)selection ('DeltaQrxlevmin' in 3GPP 25.331)",
			value_type = "deltaQrxlevmin",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/deltaQrxlevmin"},
	Qhcs = #specification_char{name = "qhcs",
			description = "Quality threshold levels in cell (re)selection ('Qhcs' in 3GPP 25.331)",
			value_type = "qhcs",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/qhcs"},
	PenaltyTime = #specification_char{name = "penaltyTime",
			description = "Penalty time duration in cell (re)selection ('Penalty_time' in 3GPP 25.331)",
			value_type = "penaltyTime",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/penaltyTime"},
	ReferenceTimeDifferenceToCell = #specification_char{name = "referenceTimeDifferenceToCell",
			description = "Reference time difference to cell in neighbouring cells monitoring (3GPP 25.331)",
			value_type = "referenceTimeDifferenceToCell",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/referenceTimeDifferenceToCell"},
	ReadSFNIndicator = #specification_char{name = "readSFNIndicator",
			description = "Read SFN indicator in neighbouring cells monitoring (3GPP 25.331)",
			value_type = "readSFNIndicator",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/readSFNIndicator"},
	NsPlmnIdList = #specification_char{name = "nsPlmnIdList",
			description = "List of unique identities for PLMN",
			value_type = "NsPlmnIdListType",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/NsPlmnIdListType"},
	RestrictionStateIndicator = #specification_char{name = "restrictionStateIndicator",
			description = "Indicates if cell reserved for operator use in Cell Access Control (3GPP 25.423)",
			value_type = "restrictionStateIndicator",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/restrictionStateIndicator"},
	DpcModechangeSupportIndicator = #specification_char{name = "dpcModechangeSupportIndicator",
			description = "Indicates support for DPC mode change in Power Control (3GPP 25.423)",
			value_type = "dpcModeChangeSupport",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/dpcModeChangeSupport"},
	RelatedSectorEquipment = #specification_char{name = "relatedSectorEquipment",
			description = "Distinguished Name (DN) of sector equipment ('relatedSectorEquipment' in 3GPP 28.662)",
			value_type = "dn",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dn"},
	UarfcnUl = #specification_char{name = "uarfcnUl",
			description = "Uplink (UL) UTRA absolute radio frequency channel number (UARFCN) (3GPP 25.433)",
			value_type = "uarfcnAnyMode",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/uarfcnAnyMode"},
	UarfcnDl = #specification_char{name = "uarfcnDl",
			description = "Downlink (DL) UTRA absolute radio frequency channel number (UARFCN) (3GPP 25.433)",
			value_type = "uarfcnAnyMode",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/uarfcnAnyMode"},
	PrimaryScramblingCode = #specification_char{name = "primaryScramblingCode",
			description = "Primary downlink (DL) scrambling code (3GPP 25.433)",
			value_type = "primaryScramblingCode",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/primaryScramblingCode"},
	PrimaryCpichPower = #specification_char{name = "primaryCpichPower",
			description = "Power of the primary CPICH channel (3GPP 25.433)",
			value_type = "primaryCpichPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/primaryCpichPower"},
	PrimarySchPower = #specification_char{name = "primarySchPower",
			description = "Power of the primary synchronisation channel (3GPP 25.433)",
			value_type = "primarySchPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/primarySchPower"},
	SecondarySchPower = #specification_char{name = "secondarySchPower",
			description = "Power of the secondary synchronisation channel (3GPP 25.433)",
			value_type = "secondarySchPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/secondarySchPower"},
	BchPower = #specification_char{name = "bchPower",
			description = "Power of the broadcast channel (3GPP 25.433)",
			value_type = "bchPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/bchPower"},
	AichPower = #specification_char{name = "aichPower",
			description = "Power of the the AICH channel (3GPP 25.433)",
			value_type = "aichPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/aichPower"},
	QqualMin = #specification_char{name = "qqualMin",
			description = "Minimum required quality level in cell (re)selection (3GPP 25.331)",
			value_type = "qqualMin",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/qqualMin"},
	CellCapabilityContainerFDD = #specification_char{name = "cellCapabilityContainerFDD",
			description = "Bitfield indicating support of particular functionality (3GPP 25.423)",
			value_type = "cellCapabilityContainerFDD",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/cellCapabilityContainerFDD"},
	TxDiversityIndicator = #specification_char{name = "txDiversityIndicator",
			description = "TX Diversity Indicator (3GPP 25.331)",
			value_type = "txDiversityIndicator",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/txDiversityIndicator"},
	TemporaryOffset1 = #specification_char{name = "temporaryOffset1",
			description = "Offset applied to hierarchical cell structure (HCS) in cell (re)selection (3GPP 25.331)",
			value_type = "temporaryOffset1",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/temporaryOffset1"},
	TemporaryOffset2 = #specification_char{name = "temporaryOffset2",
			description = "Offset applied to hierarchical cell structure (HCS) in cell (re)selection (3GPP 25.331)",
			value_type = "temporaryOffset2",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/temporaryOffset2"},
	SttdSupportIndicator = #specification_char{name = "sttdSupportIndicator",
			description = "STTD Support Indicator in power control (3GPP 25.423)",
			value_type = "sttdSupport",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/sttdSupport"},
	ClosedLoopModelSupportIndicator = #specification_char{name = "closedLoopModelSupportIndicator",
			description = "Closed Loop Mode1 Support Indicator in power control (3GPP 25.423)",
			value_type = "closedLoopMode1",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/closedLoopMode1"},
	UtranRelation = #specification_char{name = "UtranRelation",
			description = "Neighbour cell Relation (NR) from a source cell to a target UtranGenericCell",
			value_type = "UtranRelationList",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/UtranRelationList"},
	GsmRelation = #specification_char{name = "GsmRelation",
			description = "Neighbour cell Relation (NR) from a source cell to a target GsmCell",
			value_type = "GsmRelationList",
			value_schema = "/resourceCatalogManagement/v3/schema/geranNrm#/definitions/GsmRelationList"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VsDataContainerList"},
	Chars = [UserLabel, VnfParametersList, CId, LocalCellId,
			MaximumTransmissionPower, CellMode, PichPower, PchPower,
			FachPower, Lac, Rac, Sac, UraList, UtranCellIubLink,
			RelatedAntennaList, RelatedTmaList, OperationalState,
			HsFlag, HsEnable, NumOfHspdschs, NumOfHsscchs, FrameOffset,
			CellIndividualOffset, HcsPrio, MaximumAllowedUlTxPower,
			SnaInformation, QrxlevMin, DeltaQrxlevmin, Qhcs, PenaltyTime,
			ReferenceTimeDifferenceToCell, ReadSFNIndicator, NsPlmnIdList,
			RestrictionStateIndicator, DpcModechangeSupportIndicator,
			RelatedSectorEquipment, UarfcnUl, UarfcnDl, PrimaryScramblingCode,
			PrimaryCpichPower, PrimarySchPower, SecondarySchPower, BchPower,
			AichPower, QqualMin, CellCapabilityContainerFDD,
			CellCapabilityContainerFDD, TxDiversityIndicator,
			TemporaryOffset1, TemporaryOffset2, SttdSupportIndicator,
			ClosedLoopModelSupportIndicator, UtranRelation, GsmRelation,
			VsDataContainer],
	UtranCellFDDSpecification = #specification{name = "UtranCellFDD",
			description = "UMTS Frequency Division Duplex (FDD) radio cell",
			class_type = "UtranCellFDDSpecification",
			schema = "/resourceCatalogManagement/v3/schema/UtranCellFDDSpecification",
			base_type = "ResourceFunctionSpecification",
			status = "Active",
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "UtranCellFDD",
					schema = "/resourceInventoryManagement/v3/schema/UtranCellFDD"},
			characteristic = Chars},
	case im:add_specification(UtranCellFDDSpecification) of
		{ok, _} ->
			add_utrancelltddlcr(["UtranCellFDD" | NrmClasses]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_utrancelltddlcr(NrmClasses) ->
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VnfParametersList"},
	CId = #specification_char{name = "cId",
			description = "Identifier of a cell in one RNC ('C-ID' in 3GPP 25.433)",
			value_type = "cId",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/cId"},
	LocalCellId = #specification_char{name = "localCellId",
			description = "Uniquely identify a cell in a Node B ('Local Cell ID' in 3GPP 25.433)",
			value_type = "localCellId",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/localCellId"},
	MaximumTransmissionPower = #specification_char{name = "maximumTransmissionPower",
			description = "Maximum power for all downlink channels ('Maximum Transmission Power' in 3GPP 25.433)",
			value_type = "maximumTransmissionPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/maximumTransmissionPower"},
	CellMode = #specification_char{name = "cellMode",
			description = "Identifies the cell mode",
			value_type = "cellMode",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/cellMode"},
	PichPower = #specification_char{name = "pichPower",
			description = "Power of the PICH channel in the cell ('PICH Power' in 3GPP 25.433)",
			value_type = "pichPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/pichPower"},
	PchPower = #specification_char{name = "pchPower",
			description = "PCH transport channel in the cell ('PCH Power' in 3GPP 25.433)",
			value_type = "pchPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/pchPower"},
	FachPower = #specification_char{name = "fachPower",
			description = "Maximum power of FACH transport channel ('Max FACH Power' in 3GPP 25.433)",
			value_type = "fachPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/fachPower"},
	Lac = #specification_char{name = "lac",
			description = "Location Area Code (LAC) (3GPP 23.003)",
			value_type = "lac",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/lac"},
	Rac = #specification_char{name = "rac",
			description = "Routing Area Code (RAC) (3GPP 23.003)",
			value_type = "rac",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/rac"},
	Sac = #specification_char{name = "sac",
			description = "Service Area Code (SAC) (3GPP 23.003)",
			value_type = "sac",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/sac"},
	UraList = #specification_char{name = "uraList",
			description = "UTRAN Registration Area identities ('URA identity' in 3GPP 25.331)",
			value_type = "uraList",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/uraList"},
	UtranCellIubLink = #specification_char{name = "utranCellIubLink",
			description = "Distinguished Name (DN) of IubLink",
			value_type = "dn",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dn"},
	RelatedAntennaList = #specification_char{name = "relatedAntennaList",
			description = "Distinguished Names (DN) of AntennaFunction(s) (3GPP 28.662)",
			value_type = "dnList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dnList"},
	RelatedTmaList = #specification_char{name = "relatedTmaList",
			description = "Distinguished Names (DN) of ('TmaFunction' in 3GPP 28.662)",
			value_type = "dnList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dnList"},
	OperationalState = #specification_char{name = "operationalState",
			description = "Operational state describes whether physically installed and working or not (ITU-T X.731)",
			value_type = "operationalStateType",
			value_schema = "/resourceCatalogManagement/v3/schema/stateManagementIRP#/definitions/operationalStateType"},
	HsFlag = #specification_char{name = "hsFlag",
			description = "High-Speed Downlink Packet Access (HSDPA) supported flag",
			value_type = "hsFlag",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/hsFlag"},
	HsEnable = #specification_char{name = "hsEnable",
			description = "High-Speed Downlink Packet Access (HSDPA) enabled flag",
			value_type = "hsEnable",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/hsEnable"},
	NumOfHspdschs = #specification_char{name = "numOfHspdschs",
			description = "In FDD the number of codes, in TDD the number of HS-PDSCHs (3GPP 25.433)",
			value_type = "numOfHspdschs",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/numOfHspdschs"},
	NumOfHsscchs = #specification_char{name = "numOfHsscchs",
			description = "Number of HS-SCCHs for one cell (3GPP 25.433)",
			value_type = "numOfHsscchs",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/numOfHsscchs"},
	FrameOffset = #specification_char{name = "frameOffset",
			description = "Required offset in neighbouring cells monitoring (3GPP 25.423)",
			value_type = "frameOffset",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/frameOffset"},
	CellIndividualOffset = #specification_char{name = "cellIndividualOffset",
			description = "Relevant for hand over (HO) decision (3GPP 25.331)",
			value_type = "cellIndividualOffset",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/cellIndividualOffset"},
	HcsPrio = #specification_char{name = "hcsPrio",
			description = "Hierarchical cell structure (HCS) priority in cell (re)selection (3GPP 25.331)",
			value_type = "hcsPrio",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/hcsPrio"},
	MaximumAllowedUlTxPower = #specification_char{name = "maximumAllowedUlTxPower",
			description = "Maximum allowed uplink transmit power in cell (re)selection (3GPP 25.331)",
			value_type = "maximumAllowedUlTxPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/maximumAllowedUlTxPower"},
	SnaInformation = #specification_char{name = "snaInformation",
			description = "List of Shared Networks Access Control (SNAC) (3GPP 25.423)",
			value_type = "snaInformation",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/snaInformation"},
	QrxlevMin = #specification_char{name = "qrxlevMin",
			description = "Minimum required RX level in cell (re)selection ('QrxlevMin' in 3GPP 25.331)",
			value_type = "qrxlevMin",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/qrxlevMin"},
	DeltaQrxlevmin = #specification_char{name = "deltaQrxlevmin",
			description = "Delta required RX level in cell (re)selection ('DeltaQrxlevmin' in 3GPP 25.331)",
			value_type = "deltaQrxlevmin",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/deltaQrxlevmin"},
	Qhcs = #specification_char{name = "qhcs",
			description = "Quality threshold levels in cell (re)selection ('Qhcs' in 3GPP 25.331)",
			value_type = "qhcs",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/qhcs"},
	PenaltyTime = #specification_char{name = "penaltyTime",
			description = "Penalty time duration in cell (re)selection ('Penalty_time' in 3GPP 25.331)",
			value_type = "penaltyTime",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/penaltyTime"},
	ReferenceTimeDifferenceToCell = #specification_char{name = "referenceTimeDifferenceToCell",
			description = "Reference time difference to cell in neighbouring cells monitoring (3GPP 25.331)",
			value_type = "referenceTimeDifferenceToCell",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/referenceTimeDifferenceToCell"},
	ReadSFNIndicator = #specification_char{name = "readSFNIndicator",
			description = "Read SFN indicator in neighbouring cells monitoring (3GPP 25.331)",
			value_type = "readSFNIndicator",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/readSFNIndicator"},
	NsPlmnIdList = #specification_char{name = "nsPlmnIdList",
			description = "List of unique identities for PLMN",
			value_type = "NsPlmnIdListType",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/NsPlmnIdListType"},
	RestrictionStateIndicator = #specification_char{name = "restrictionStateIndicator",
			description = "Indicates if cell reserved for operator use in Cell Access Control (3GPP 25.423)",
			value_type = "restrictionStateIndicator",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/restrictionStateIndicator"},
	DpcModechangeSupportIndicator = #specification_char{name = "dpcModechangeSupportIndicator",
			description = "Indicates support for DPC mode change in Power Control (3GPP 25.423)",
			value_type = "dpcModeChangeSupport",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/dpcModeChangeSupport"},
	RelatedSectorEquipment = #specification_char{name = "relatedSectorEquipment",
			description = "Distinguished Name (DN) of sector equipment ('relatedSectorEquipment' in 3GPP 28.662)",
			value_type = "dn",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dn"},
	Uarfcn = #specification_char{name = "uarfcn",
			description = "UTRA absolute radio frequency channel number (UARFCN) (3GPP 25.433)",
			value_type = "uarfcnAnyMode",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/uarfcnAnyMode"},
	CellParameterId = #specification_char{name = "cellParameterId",
			description = "Unambiguously identifies the cell ('Cell Parameter ID' in TS 25.433)",
			value_type = "cellParameterId",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/cellParameterId"},
	PrimaryCcpchPower = #specification_char{name = "primaryCcpchPower",
			description = "Power of the primary CCPCH channel ('PCCPCH Power' in 3GPP 25.433)",
			value_type = "primaryCcpchPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/primaryCcpchPower"},
	CellCapabilityContainerTDD = #specification_char{name = "cellCapabilityContainerTDD",
			description = "Bitfield indicating support of particular functionality (3GPP 25.423)",
			value_type = "cellCapabilityContainerTDD",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/cellCapabilityContainerTDD"},
	SctdIndicator = #specification_char{name = "sctdIndicator",
			description = "Indicates whether SCTD is used ('SCDT Indicator' in 3GPP 25.433)",
			value_type = "sctdIndicator",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/sctdIndicator"},
	DpchConstantValue = #specification_char{name = "dpchConstantValue",
			description = "Power margin used by a UE ('DPCH Constant Valuer' in 3GPP 25.433)",
			value_type = "dpchConstantValue",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/dpchConstantValue"},
	UarfcnLCRList = #specification_char{name = "uarfcnLCRList",
			description = "List of UARFCN and time slot LCR, Direction, Status (3GPP 25.433)",
			value_type = "uarfcnLCRList",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/uarfcnLCRList"},
	FpachPower = #specification_char{name = "fpachPower",
			description = "Maximum power of the FPACH channel ('FPACH Power' in 3GPP 25.433)",
			value_type = "fpachPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/fpachPower"},
	DwPchPower = #specification_char{name = "dwPchPower",
			description = "Power used for transmitting the DwPCH ('DwPCH Power' in 3GPP 25.433)",
			value_type = "dwPchPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/dwPchPower"},
	TstdIndicator = #specification_char{name = "tstdIndicator",
			description = "Indicates whether TSTD is used ('TSDT Indicator' in 3GPP 25.433)",
			value_type = "tstdIndicator",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/tstdIndicator"},
	TimeSlotLCRList = #specification_char{name = "timeSlotLCRList",
			description = "Defines the time slot configuration information (3GPP 25.433)",
			value_type = "timeSlotLCRList",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/timeSlotLCRList"},
	UtranRelation = #specification_char{name = "UtranRelation",
			description = "Neighbour cell Relation (NR) from a source cell to a target UtranGenericCell",
			value_type = "UtranRelationList",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/UtranRelationList"},
	GsmRelation = #specification_char{name = "GsmRelation",
			description = "Neighbour cell Relation (NR) from a source cell to a target GsmCell",
			value_type = "GsmRelationList",
			value_schema = "/resourceCatalogManagement/v3/schema/geranNrm#/definitions/GsmRelationList"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VsDataContainerList"},
	Chars = [UserLabel, VnfParametersList, CId, LocalCellId,
			MaximumTransmissionPower, CellMode, PichPower, PchPower,
			FachPower, Lac, Rac, Sac, UraList, UtranCellIubLink,
			RelatedAntennaList, RelatedTmaList, OperationalState,
			HsFlag, HsEnable, NumOfHspdschs, NumOfHsscchs, FrameOffset,
			CellIndividualOffset, HcsPrio, MaximumAllowedUlTxPower,
			SnaInformation, QrxlevMin, DeltaQrxlevmin, Qhcs, PenaltyTime,
			ReferenceTimeDifferenceToCell, ReadSFNIndicator, NsPlmnIdList,
			RestrictionStateIndicator, DpcModechangeSupportIndicator,
			RelatedSectorEquipment, Uarfcn, CellParameterId, CellParameterId,
			PrimaryCcpchPower, CellCapabilityContainerTDD, SctdIndicator,
			DpchConstantValue, UarfcnLCRList, FpachPower, DwPchPower,
			TstdIndicator, TimeSlotLCRList, UtranRelation, GsmRelation,
			VsDataContainer],
	UtranCellTDDLcrSpecification = #specification{name = "UtranCellTDDLcr",
			description = "UMTS Time Division Duplex (TDD) low chip rate (LCR) radio cell",
			class_type = "UtranCellTDDLcrSpecification",
			schema = "/resourceCatalogManagement/v3/schema/UtranCellTDDLcrSpecification",
			base_type = "UtranCellTDDSpecification",
			status = "Active",
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "UtranCellTDDLcr",
					schema = "/resourceInventoryManagement/v3/schema/UtranCellTDDLcr"},
			characteristic = Chars},
	case im:add_specification(UtranCellTDDLcrSpecification) of
		{ok, _} ->
			add_utrancelltddhcr(["UtranCellTDDLcr" | NrmClasses]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_utrancelltddhcr(NrmClasses) ->
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VnfParametersList"},
	CId = #specification_char{name = "cId",
			description = "Identifier of a cell in one RNC ('C-ID' in 3GPP 25.433)",
			value_type = "cId",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/cId"},
	LocalCellId = #specification_char{name = "localCellId",
			description = "Uniquely identify a cell in a Node B ('Local Cell ID' in 3GPP 25.433)",
			value_type = "localCellId",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/localCellId"},
	MaximumTransmissionPower = #specification_char{name = "maximumTransmissionPower",
			description = "Maximum power for all downlink channels ('Maximum Transmission Power' in 3GPP 25.433)",
			value_type = "maximumTransmissionPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/maximumTransmissionPower"},
	CellMode = #specification_char{name = "cellMode",
			description = "Identifies the cell mode",
			value_type = "cellMode",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/cellMode"},
	PichPower = #specification_char{name = "pichPower",
			description = "Power of the PICH channel in the cell ('PICH Power' in 3GPP 25.433)",
			value_type = "pichPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/pichPower"},
	PchPower = #specification_char{name = "pchPower",
			description = "PCH transport channel in the cell ('PCH Power' in 3GPP 25.433)",
			value_type = "pchPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/pchPower"},
	FachPower = #specification_char{name = "fachPower",
			description = "Maximum power of FACH transport channel ('Max FACH Power' in 3GPP 25.433)",
			value_type = "fachPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/fachPower"},
	Lac = #specification_char{name = "lac",
			description = "Location Area Code (LAC) (3GPP 23.003)",
			value_type = "lac",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/lac"},
	Rac = #specification_char{name = "rac",
			description = "Routing Area Code (RAC) (3GPP 23.003)",
			value_type = "rac",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/rac"},
	Sac = #specification_char{name = "sac",
			description = "Service Area Code (SAC) (3GPP 23.003)",
			value_type = "sac",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/sac"},
	UraList = #specification_char{name = "uraList",
			description = "UTRAN Registration Area identities ('URA identity' in 3GPP 25.331)",
			value_type = "uraList",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/uraList"},
	UtranCellIubLink = #specification_char{name = "utranCellIubLink",
			description = "Distinguished Name (DN) of IubLink",
			value_type = "dn",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dn"},
	RelatedAntennaList = #specification_char{name = "relatedAntennaList",
			description = "Distinguished Names (DN) of AntennaFunction(s) (3GPP 28.662)",
			value_type = "dnList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dnList"},
	RelatedTmaList = #specification_char{name = "relatedTmaList",
			description = "Distinguished Names (DN) of ('TmaFunction' in 3GPP 28.662)",
			value_type = "dnList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dnList"},
	OperationalState = #specification_char{name = "operationalState",
			description = "Operational state describes whether physically installed and working or not (ITU-T X.731)",
			value_type = "operationalStateType",
			value_schema = "/resourceCatalogManagement/v3/schema/stateManagementIRP#/definitions/operationalStateType"},
	HsFlag = #specification_char{name = "hsFlag",
			description = "High-Speed Downlink Packet Access (HSDPA) supported flag",
			value_type = "hsFlag",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/hsFlag"},
	HsEnable = #specification_char{name = "hsEnable",
			description = "High-Speed Downlink Packet Access (HSDPA) enabled flag",
			value_type = "hsEnable",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/hsEnable"},
	NumOfHspdschs = #specification_char{name = "numOfHspdschs",
			description = "In FDD the number of codes, in TDD the number of HS-PDSCHs (3GPP 25.433)",
			value_type = "numOfHspdschs",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/numOfHspdschs"},
	NumOfHsscchs = #specification_char{name = "numOfHsscchs",
			description = "Number of HS-SCCHs for one cell (3GPP 25.433)",
			value_type = "numOfHsscchs",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/numOfHsscchs"},
	FrameOffset = #specification_char{name = "frameOffset",
			description = "Required offset in neighbouring cells monitoring (3GPP 25.423)",
			value_type = "frameOffset",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/frameOffset"},
	CellIndividualOffset = #specification_char{name = "cellIndividualOffset",
			description = "Relevant for hand over (HO) decision (3GPP 25.331)",
			value_type = "cellIndividualOffset",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/cellIndividualOffset"},
	HcsPrio = #specification_char{name = "hcsPrio",
			description = "Hierarchical cell structure (HCS) priority in cell (re)selection (3GPP 25.331)",
			value_type = "hcsPrio",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/hcsPrio"},
	MaximumAllowedUlTxPower = #specification_char{name = "maximumAllowedUlTxPower",
			description = "Maximum allowed uplink transmit power in cell (re)selection (3GPP 25.331)",
			value_type = "maximumAllowedUlTxPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/maximumAllowedUlTxPower"},
	SnaInformation = #specification_char{name = "snaInformation",
			description = "List of Shared Networks Access Control (SNAC) (3GPP 25.423)",
			value_type = "snaInformation",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/snaInformation"},
	QrxlevMin = #specification_char{name = "qrxlevMin",
			description = "Minimum required RX level in cell (re)selection ('QrxlevMin' in 3GPP 25.331)",
			value_type = "qrxlevMin",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/qrxlevMin"},
	DeltaQrxlevmin = #specification_char{name = "deltaQrxlevmin",
			description = "Delta required RX level in cell (re)selection ('DeltaQrxlevmin' in 3GPP 25.331)",
			value_type = "deltaQrxlevmin",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/deltaQrxlevmin"},
	Qhcs = #specification_char{name = "qhcs",
			description = "Quality threshold levels in cell (re)selection ('Qhcs' in 3GPP 25.331)",
			value_type = "qhcs",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/qhcs"},
	PenaltyTime = #specification_char{name = "penaltyTime",
			description = "Penalty time duration in cell (re)selection ('Penalty_time' in 3GPP 25.331)",
			value_type = "penaltyTime",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/penaltyTime"},
	ReferenceTimeDifferenceToCell = #specification_char{name = "referenceTimeDifferenceToCell",
			description = "Reference time difference to cell in neighbouring cells monitoring (3GPP 25.331)",
			value_type = "referenceTimeDifferenceToCell",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/referenceTimeDifferenceToCell"},
	ReadSFNIndicator = #specification_char{name = "readSFNIndicator",
			description = "Read SFN indicator in neighbouring cells monitoring (3GPP 25.331)",
			value_type = "readSFNIndicator",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/readSFNIndicator"},
	NsPlmnIdList = #specification_char{name = "nsPlmnIdList",
			description = "List of unique identities for PLMN",
			value_type = "NsPlmnIdListType",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/NsPlmnIdListType"},
	RestrictionStateIndicator = #specification_char{name = "restrictionStateIndicator",
			description = "Indicates if cell reserved for operator use in Cell Access Control (3GPP 25.423)",
			value_type = "restrictionStateIndicator",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/restrictionStateIndicator"},
	DpcModechangeSupportIndicator = #specification_char{name = "dpcModechangeSupportIndicator",
			description = "Indicates support for DPC mode change in Power Control (3GPP 25.423)",
			value_type = "dpcModeChangeSupport",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/dpcModeChangeSupport"},
	RelatedSectorEquipment = #specification_char{name = "relatedSectorEquipment",
			description = "Distinguished Name (DN) of sector equipment ('relatedSectorEquipment' in 3GPP 28.662)",
			value_type = "dn",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dn"},
	Uarfcn = #specification_char{name = "uarfcn",
			description = "UTRA absolute radio frequency channel number (UARFCN) (3GPP 25.433)",
			value_type = "uarfcnAnyMode",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/uarfcnAnyMode"},
	CellParameterId = #specification_char{name = "cellParameterId",
			description = "Unambiguously identifies the cell ('Cell Parameter ID' in TS 25.433)",
			value_type = "cellParameterId",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/cellParameterId"},
	PrimaryCcpchPower = #specification_char{name = "primaryCcpchPower",
			description = "Power of the primary CCPCH channel ('PCCPCH Power' in 3GPP 25.433)",
			value_type = "primaryCcpchPower",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/primaryCcpchPower"},
	CellCapabilityContainerTDD = #specification_char{name = "cellCapabilityContainerTDD",
			description = "Bitfield indicating support of particular functionality (3GPP 25.423)",
			value_type = "cellCapabilityContainerTDD",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/cellCapabilityContainerTDD"},
	SctdIndicator = #specification_char{name = "sctdIndicator",
			description = "Indicates whether SCTD is used ('SCDT Indicator' in 3GPP 25.433)",
			value_type = "sctdIndicator",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/sctdIndicator"},
	DpchConstantValue = #specification_char{name = "dpchConstantValue",
			description = "Power margin used by a UE ('DPCH Constant Valuer' in 3GPP 25.433)",
			value_type = "dpchConstantValue",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/dpchConstantValue"},
	TemporaryOffset1 = #specification_char{name = "temporaryOffset1",
			description = "Offset applied to hierarchical cell structure (HCS) in cell (re)selection (3GPP 25.331)",
			value_type = "temporaryOffset1",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/temporaryOffset1"},
	SyncCase = #specification_char{name = "syncCase",
			description = "SCH and PCCPCH mapped on one or two downlink slots per frame ('Synch Case' in 3GPP 25.433)",
			value_type = "syncCase",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/syncCase"},
	TimeSlotForSch = #specification_char{name = "timeSlotForSch",
			description = "Time interval assigned to a physical channel for SCH ('SCH Time Slot' in 3GPP 25.433)",
			value_type = "timeSlotForSch",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/timeSlotForSch"},
	SchTimeSlot = #specification_char{name = "schTimeSlot",
			description = "First time slot assigned to the Physical Channel SCH ('SCH Time Slot' in TS 25.433 )",
			value_type = "schTimeSlot",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/schTimeSlot"},
	TimeSlotHCRList = #specification_char{name = "timeSlotHCRList",
			description = "Defines the time slot configuration information (3GPP 25.433)",
			value_type = "timeSlotHCRList",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/timeSlotHCRList"},
	UtranRelation = #specification_char{name = "UtranRelation",
			description = "Neighbour cell Relation (NR) from a source cell to a target UtranGenericCell",
			value_type = "UtranRelationList",
			value_schema = "/resourceCatalogManagement/v3/schema/utranNrm#/definitions/UtranRelationList"},
	GsmRelation = #specification_char{name = "GsmRelation",
			description = "Neighbour cell Relation (NR) from a source cell to a target GsmCell",
			value_type = "GsmRelationList",
			value_schema = "/resourceCatalogManagement/v3/schema/geranNrm#/definitions/GsmRelationList"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VsDataContainerList"},
	Chars = [UserLabel, VnfParametersList, CId, LocalCellId,
			MaximumTransmissionPower, CellMode, PichPower, PchPower,
			FachPower, Lac, Rac, Sac, UraList, UtranCellIubLink,
			RelatedAntennaList, RelatedTmaList, OperationalState,
			HsFlag, HsEnable, NumOfHspdschs, NumOfHsscchs, FrameOffset,
			CellIndividualOffset, HcsPrio, MaximumAllowedUlTxPower,
			SnaInformation, QrxlevMin, DeltaQrxlevmin, Qhcs, PenaltyTime,
			ReferenceTimeDifferenceToCell, ReadSFNIndicator, NsPlmnIdList,
			RestrictionStateIndicator, DpcModechangeSupportIndicator,
			RelatedSectorEquipment, Uarfcn, CellParameterId, CellParameterId,
			PrimaryCcpchPower, CellCapabilityContainerTDD, SctdIndicator,
			DpchConstantValue, TemporaryOffset1, SyncCase, TimeSlotForSch,
			SchTimeSlot, TimeSlotHCRList, UtranRelation, GsmRelation,
			VsDataContainer],
	UtranCellTDDHcrSpecification = #specification{name = "UtranCellTDDHcr",
			description = "UMTS Time Division Duplex (TDD) high chip rate (HCR) radio cell",
			class_type = "UtranCellTDDHcrSpecification",
			schema = "/resourceCatalogManagement/v3/schema/UtranCellTDDHcrSpecification",
			base_type = "UtranCellTDDSpecification",
			status = "Active",
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "UtranCellTDDHcr",
					schema = "/resourceInventoryManagement/v3/schema/UtranCellTDDHcr"},
			characteristic = Chars},
	case im:add_specification(UtranCellTDDHcrSpecification) of
		{ok, _} ->
			add_enb(["UtranCellTDDHcr" | NrmClasses]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_enb(NrmClasses) ->
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VnfParametersList"},
	PeeParametersList = #specification_char{name = "peeParametersList",
			description = "Parameter list for the control and monitoring of power, energy and environment",
			value_type = "peeParametersListType",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/peeParametersListType"},
	IntraANRSwitch = #specification_char{name = "intraANRSwitch",
			description = "Whether the intra E-UTRAN ANR function is activated or deactivated",
			value_type = "boolean"},
	IRATANRSwitch = #specification_char{name = "iRATANRSwitch",
			description = "Whether the IRAT ANR function is activated or deactivated",
			value_type = "boolean"},
	ENBId = #specification_char{name = "eNBId",
			description = "Unambiguously identifies an eNodeB within a PLMN",
			value_type = "ENBId",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/ENBId"},
	X2BlackList = #specification_char{name = "x2BlackList",
			description = "List of target nodes prohibited X2 connections",
			value_type = "dnList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dnList"},
	X2WhiteList = #specification_char{name = "x2WhiteList",
			description = "List of target nodes allowed X2 connections",
			value_type = "dnList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dnList"},
	X2HOBlackList = #specification_char{name = "x2HOBlackList",
			description = "List of target nodes prohibited handover",
			value_type = "dnList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dnList"},
	X2IpAddressList = #specification_char{name = "x2IpAddressList",
			description = "IP addresses used for this ENBFunction's X2 Interface",
			value_type = "string"},
	TceIDMappingInfoList = #specification_char{name = "tceIDMappingInfoList",
			description = "List of Trace Collection Entity (TCE) ID and IP Address (3GPP 32.422)",
			value_type = "TceIDMappingInfoList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/TceIDMappingInfoList"},
	SharNetTceMappingInfoList = #specification_char{name = "sharNetTceMappingInfoList",
			description = "List of shared PLMN ID, Trace Collection Entity (TCE) ID and IP Address (3GPP 32.422)",
			value_type = "SharNetTceMappingInfoList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/SharNetTceMappingInfoList"},
	NetListeningRSForRIBS = #specification_char{name = "netListeningRSForRIBS",
			description = "RS for RIBS ('Radio Interface based Synchronization' in 3GPP 36.300)",
			value_type = "NetListeningRSForRIBS",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/NetListeningRSForRIBS"},
	EUtranCellFDD = #specification_char{name = "EUtranCellFDD",
			description = "LTE Frequency Division Duplex (FDD) radio cells",
			value_type = "EUtranCellFDDList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/EUtranCellFDDList"},
	EUtranCellTDD = #specification_char{name = "EUtranCellTDD",
			description = "LTE Time Division Duplex (TDD) radio cells",
			value_type = "EUtranCellTDDList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/EUtranCellTDDList"},
	EP_RP_EPS = #specification_char{name = "EP_RP_EPS",
			description = "Endpoint of reference point in EPS (3GPP 23.401)",
			value_type = "EP_RP_EPSList",
			value_schema = "/resourceCatalogManagement/v3/schema/epcNrm#/definitions/EP_RP_EPSList"},
	EP_X2C = #specification_char{name = "EP_X2C",
			description = "Endpoint of X2-C logical link to neighbour eNB or en-gNB node (3GPP 36.423)",
			value_type = "EP_X2CList",
			value_schema = "/resourceCatalogManagement/v3/schema/nrNrm#/definitions/EP_X2CList"},
	EP_X2U = #specification_char{name = "EP_X2U",
			description = "Endpoint of X2 user plane (X2-U) logical link (3GPP 36.425)",
			value_type = "EP_X2UList",
			value_schema = "/resourceCatalogManagement/v3/schema/nrNrm#/definitionsEP_X2UList/"},
	EP_NgC = #specification_char{name = "EP_NgC",
			description = "Endpoint of NG control plane (NG-C) between gNB and NG-Core entity (3GPP 38.470)",
			value_type = "EP_NgCList",
			value_schema = "/resourceCatalogManagement/v3/schema/nrNrm#/definitions/EP_NgCList"},
	EP_NgU = #specification_char{name = "EP_NgU",
			description = "Endpoint of NG user plane (NG-U) between gNB and UPGW (3GPP 38.470)",
			value_type = "EP_NgUList",
			value_schema = "/resourceCatalogManagement/v3/schema/nrNrm#/definitions/EP_NgUList"},
	EP_XnC = #specification_char{name = "EP_XnC",
			description = "Endpoint of Xn Application protocols between neighbour gNB nodes (3GPP 38.420)",
			value_type = "EP_XnCList",
			value_schema = "/resourceCatalogManagement/v3/schema/nrNrm#/definitions/"},
	EP_XnU = #specification_char{name = "EP_XnU",
			description = "Endpoint of Xn user plane (Xn-U) logical link (3GPP 38.420)",
			value_type = "EP_XnUList",
			value_schema = "/resourceCatalogManagement/v3/schema/nrNrm#/definitions/EP_XnUList"},
	ContainedNrmClass = #specification_char{name = "ENBFunctionOptionallyContainedNrmClass",
			description = "List of optionally contained NRM Class objects",
			value_type = "ENBFunctionOptionallyContainedNrmClassList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/ENBFunctionOptionallyContainedNrmClassList"},
	DeNBCapability = #specification_char{name = "DeNBCapability",
			description = "Capability for eNodeB to act as Donor eNodeB (DeNB) (3GPP 36.300)",
			value_type = "DeNBCapabilityList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/DeNBCapabilityList"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VsDataContainerList"},
	ESPolicies = #specification_char{name = "ESPolicies",
			description = "Energy saving policies information",
			value_type = "ESPoliciesList",
			value_schema = "/resourceCatalogManagement/v3/schema/sonPolicyNrm#/definitions/ESPoliciesList"},
	Chars = [UserLabel, VnfParametersList, PeeParametersList, IntraANRSwitch,
			IRATANRSwitch, ENBId, X2BlackList, X2WhiteList, X2HOBlackList,
			X2IpAddressList, TceIDMappingInfoList, SharNetTceMappingInfoList,
			NetListeningRSForRIBS, EUtranCellFDD, EUtranCellTDD, EP_RP_EPS,
			EP_X2C, EP_X2U, EP_NgC, EP_NgU, EP_XnC, EP_XnU, ContainedNrmClass,
			DeNBCapability, VsDataContainer, ESPolicies],
	ENBFunctionSpecification = #specification{name = "ENBFunction",
			description = "LTE eNodeB",
			class_type = "ENBFunctionSpecification",
			schema = "/resourceCatalogManagement/v3/schema/ENBFunctionSpecification",
			base_type = "ResourceFunctionSpecification",
			status = "Active",
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "ENBFunction",
					schema = "/resourceInventoryManagement/v3/schema/ENBFunction"},
			characteristic = Chars},
	case im:add_specification(ENBFunctionSpecification) of
		{ok, _} ->
			add_ltecellfdd(["ENBFunction" | NrmClasses]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_ltecellfdd(NrmClasses) ->
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VnfParametersList"},
	PeeParametersList = #specification_char{name = "peeParametersList",
			description = "Parameter list for the control and monitoring of power, energy and environment",
			value_type = "peeParametersListType",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/peeParametersListType"},
	CellLocalId = #specification_char{name = "cellLocalId",
			description = "Unambiguously identify a cell within an eNodeB",
			value_type = "CellLocalId",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/CellLocalId"},
	CellLocalIdList = #specification_char{name = "cellLocalIdList",
			description = "List of cellLocalId for split or merged cells in Active Antenna System operations",
			value_type = "CellLocalIdList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/CellLocalIdList"},
	CellSize = #specification_char{name = "cellSize",
			description = "Cell-Size (3GPP 36.423)",
			value_type = "cellSize",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/cellSize"},
	PLMNIdList = #specification_char{name = "pLMNIdList",
			description = "List of unique identities for PLMN",
			value_type = "PLMNIdList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/PLMNIdList"},
	CellAccessInfoList = #specification_char{name = "cellAccessInfoList",
			description = "List of PLMN information for RAN sharing",
			value_type = "CellAccessInfoList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/CellAccessInfoList"},
	Tac = #specification_char{name = "tac",
			description = "Tracking Area Code (3GPP 23.003)",
			value_type = "integer",
			char_value = [#spec_char_value{from = -9223372036854775808, to = 9223372036854775808}]},
	Pci = #specification_char{name = "pci",
			description = "Physical Cell Identity (PCI) of the cell (3GPP 36.211)",
			value_type = "Pci",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/Pci"},
	PciList = #specification_char{name = "pciList",
			description = "List of Physical Cell Identities (PCI) which can be assigned (3GPP 32.500)",
			value_type = "PciList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/PciList"},
	MaximumTransmissionPower = #specification_char{name = "maximumTransmissionPower",
			description = "Maximum power for all downlink channels used simultaneously, added together",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	PartOfSectorPower = #specification_char{name = "partOfSectorPower",
			description = "Requested % power allocated to the cell",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	ReferenceSignalPower  = #specification_char{name = "referenceSignalPower",
			description = "Cell specific downlink reference signal transmit power (3GPP 36.213)",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	Pb = #specification_char{name = "pb",
			description = "Downlink power allocation ('PB' in 3GPP 36.213 Section 5.2 of 3GPP 36.213)",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	RelatedTmaList = #specification_char{name = "relatedTmaList",
			description = "List DNs of TmaFunction(s) (3GPP 28.662)",
			value_type = "dnList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dnList"},
	RelatedAntennaList = #specification_char{name = "relatedAntennaList",
			description = "List DNs of AntennaFunction(s) (3GPP 28.662)",
			value_type = "dnList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dnList"},
	RelatedSector = #specification_char{name = "relatedSector",
			description = "List DNs of SectorEquipmentFunction (3GPP 28.662)",
			value_type = "dnList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dnList"},
	OperationalState = #specification_char{name = "operationalState",
			description = "Indicates the operational state of the object instance (ITU-T X.731)",
			value_type = "operationalStateType",
			value_schema = "/resourceCatalogManagement/v3/schema/stateManagementIRP#/definitions/operationalStateType"},
	AdministrativeState = #specification_char{name = "administrativeState",
			description = "Indicates the administrative state of the object instance (ITU-T X.731)",
			value_type = "administrativeStateType",
			value_schema = "/resourceCatalogManagement/v3/schema/stateManagementIRP#/definitions/administrativeStateType"},
	AvailabilityStatus = #specification_char{name = "availabilityStatus",
			description = "Indicates the availability status of the object instance (ITU-T X.731)",
			value_type = "availabilityStatusType",
			value_schema = "/resourceCatalogManagement/v3/schema/stateManagementIRP#/definitions/availabilityStatusType"},
	AllowedAccessClasses = #specification_char{name = "allowedAccessClasses",
			description = "Holds information for access classes allowed (3GPP 22.011)",
			value_type = "allowedAccessClassesType",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/allowedAccessClassesType"},
	CellResvInfo = #specification_char{name = "cellResvInfo",
			description = "Represents whether the cell is MBSFN Area Reserved Cell (3GPP 36.00)",
			value_type = "cellResvInfoType",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/cellResvInfoType"},
	NbIoTcellFlag = #specification_char{name = "nbIoTcellFlag",
			description = "Represents whether the cell is supporting NB-IoT or not (3GPP 36.300)",
			value_type = "yesNoType",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/yesNoType"},
	IsChangeForEnergySavingAllowed = #specification_char{name = "isChangeForEnergySavingAllowed",
			description = "Prohibit or allow configuration changes of the cell for ESM purposes",
			value_type = "yesNoType",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/yesNoType"},
	NgranCellFlag = #specification_char{name = "ngranCellFlag",
			description = "Represents whether the cell is provided by ng-eNB or not (3GPP 38.300)",
			value_type = "yesNoType",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/yesNoType"},
	NSSAI = #specification_char{name = "nSSAI",
			description = "Network Slice Selection Assistance Information",
			value_type = "NssaiList",
			value_schema = "/resourceCatalogManagement/v3/schema/ngcNrm#/definitions/NssaiList"},
	EarfcnDl = #specification_char{name = "earfcnDl",
			description = "Channel number for central downlink (DL) frequency (3GPP 36.101)",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	EarfcnUl = #specification_char{name = "earfcnUl",
			description = "Channel number for central uplink (UL) frequency (3GPP 36.101)",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	EUtranRelation = #specification_char{name = "EUtranRelation",
			description = "Neighbour cell Relation (NR) from a source cell to a target EUtranGenericCell",
			value_type = "EUtranRelationList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/EUtranRelationList"},
	Cdma2000Relation = #specification_char{name = "Cdma2000Relation",
			description = "Neighbour cell Relation (NR) from a source cell to a target CDMA200 sector (3GPP2 TS S.S0028)",
			value_type = "Cdma2000RelationList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/Cdma2000RelationList"},
	GsmRelation = #specification_char{name = "GsmRelation",
			description = "Neighbour cell Relation (NR) from a source cell to a target GsmCell",
			value_type = "GsmRelationList",
			value_schema = "/resourceCatalogManagement/v3/schema/geranNrm#/definitions/GsmRelationList"},
	ContainedNrmClass = #specification_char{name = "EUtranCellFDDOptionallyContainedNrmClass",
			description = "List of optionally contained NRM Class objects",
			value_type = "ENBFunctionOptionallyContainedNrmClassList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/ENBFunctionOptionallyContainedNrmClassList"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VsDataContainerList"},
	EnergySavingProperties = #specification_char{name = "EnergySavingProperties",
			description = "Energy saving properties of a network element supporting Energy Saving Management",
			value_type = "EnergySavingPropertiesList",
			value_schema = "/resourceCatalogManagement/v3/schema/sonPolicyNrm#/definitions/EnergySavingPropertiesList"},
	ESPolicies = #specification_char{name = "ESPolicies",
			description = "Energy saving policies information",
			value_type = "ESPoliciesList",
			value_schema = "/resourceCatalogManagement/v3/schema/sonPolicyNrm#/definitions/ESPoliciesList"},
	Chars = [UserLabel, VnfParametersList, PeeParametersList, CellLocalId,
			CellLocalIdList, CellSize, PLMNIdList, CellAccessInfoList, Tac,
			Pci, PciList, MaximumTransmissionPower, PartOfSectorPower,
			ReferenceSignalPower , Pb, RelatedTmaList, RelatedAntennaList,
			RelatedSector, OperationalState, AdministrativeState,
			AvailabilityStatus, AllowedAccessClasses, CellResvInfo,
			NbIoTcellFlag, IsChangeForEnergySavingAllowed, NgranCellFlag,
			NSSAI, EarfcnDl, EarfcnUl, EUtranRelation, Cdma2000Relation,
			GsmRelation, ContainedNrmClass, VsDataContainer,
			EnergySavingProperties, ESPolicies],
	EUtranCellFDDSpecification = #specification{name = "EUtranCellFDD",
			description = "LTE Frequency Division Duplex (FDD) radio cell",
			class_type = "EUtranCellFDDSpecification",
			schema = "/resourceCatalogManagement/v3/schema/EUtranCellFDDSpecification",
			base_type = "EUtranGenericCell",
			status = "Active",
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "EUtranCellFDD",
					schema = "/resourceInventoryManagement/v3/schema/EUtranCellFDD"},
			characteristic = Chars},
	case im:add_specification(EUtranCellFDDSpecification) of
		{ok, _} ->
			add_ltecelltdd(["EUtranCellFDD" | NrmClasses]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_ltecelltdd(NrmClasses) ->
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VnfParametersList"},
	PeeParametersList = #specification_char{name = "peeParametersList",
			description = "Parameter list for the control and monitoring of power, energy and environment",
			value_type = "peeParametersListType",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/peeParametersListType"},
	CellLocalId = #specification_char{name = "cellLocalId",
			description = "Unambiguously identify a cell within an eNodeB",
			value_type = "CellLocalId",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/CellLocalId"},
	CellLocalIdList = #specification_char{name = "cellLocalIdList",
			description = "List of cellLocalId for split or merged cells in Active Antenna System operations",
			value_type = "CellLocalIdList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/CellLocalIdList"},
	CellSize = #specification_char{name = "cellSize",
			description = "Cell-Size (3GPP 36.423)",
			value_type = "cellSize",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/cellSize"},
	PLMNIdList = #specification_char{name = "pLMNIdList",
			description = "List of unique identities for PLMN",
			value_type = "PLMNIdList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/PLMNIdList"},
	CellAccessInfoList = #specification_char{name = "cellAccessInfoList",
			description = "List of PLMN information for RAN sharing",
			value_type = "CellAccessInfoList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/CellAccessInfoList"},
	Tac = #specification_char{name = "tac",
			description = "Tracking Area Code (3GPP 23.003)",
			value_type = "integer",
			char_value = [#spec_char_value{from = -9223372036854775808, to = 9223372036854775808}]},
	Pci = #specification_char{name = "pci",
			description = "Physical Cell Identity (PCI) of the cell (3GPP 36.211)",
			value_type = "Pci",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/Pci"},
	PciList = #specification_char{name = "pciList",
			description = "List of Physical Cell Identities (PCI) which can be assigned (3GPP 32.500)",
			value_type = "PciList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/PciList"},
	MaximumTransmissionPower = #specification_char{name = "maximumTransmissionPower",
			description = "Maximum power for all downlink channels used simultaneously, added together",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	PartOfSectorPower = #specification_char{name = "partOfSectorPower",
			description = "Requested % power allocated to the cell",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	ReferenceSignalPower  = #specification_char{name = "referenceSignalPower",
			description = "Cell specific downlink reference signal transmit power (3GPP 36.213)",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	Pb = #specification_char{name = "pb",
			description = "Downlink power allocation ('PB' in 3GPP 36.213 Section 5.2 of 3GPP 36.213)",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	RelatedTmaList = #specification_char{name = "relatedTmaList",
			description = "List DNs of TmaFunction(s) (3GPP 28.662)",
			value_type = "dnList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dnList"},
	RelatedAntennaList = #specification_char{name = "relatedAntennaList",
			description = "List DNs of AntennaFunction(s) (3GPP 28.662)",
			value_type = "dnList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dnList"},
	RelatedSector = #specification_char{name = "relatedSector",
			description = "List DNs of SectorEquipmentFunction (3GPP 28.662)",
			value_type = "dnList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/dnList"},
	OperationalState = #specification_char{name = "operationalState",
			description = "Indicates the operational state of the object instance (ITU-T X.731)",
			value_type = "operationalStateType",
			value_schema = "/resourceCatalogManagement/v3/schema/stateManagementIRP#/definitions/operationalStateType"},
	AdministrativeState = #specification_char{name = "administrativeState",
			description = "Indicates the administrative state of the object instance (ITU-T X.731)",
			value_type = "administrativeStateType",
			value_schema = "/resourceCatalogManagement/v3/schema/stateManagementIRP#/definitions/administrativeStateType"},
	AvailabilityStatus = #specification_char{name = "availabilityStatus",
			description = "Indicates the availability status of the object instance (ITU-T X.731)",
			value_type = "availabilityStatusType",
			value_schema = "/resourceCatalogManagement/v3/schema/stateManagementIRP#/definitions/availabilityStatusType"},
	AllowedAccessClasses = #specification_char{name = "allowedAccessClasses",
			description = "Holds information for access classes allowed (3GPP 22.011)",
			value_type = "allowedAccessClassesType",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/allowedAccessClassesType"},
	CellResvInfo = #specification_char{name = "cellResvInfo",
			description = "Represents whether the cell is MBSFN Area Reserved Cell (3GPP 36.00)",
			value_type = "cellResvInfoType",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/cellResvInfoType"},
	NbIoTcellFlag = #specification_char{name = "nbIoTcellFlag",
			description = "Represents whether the cell is supporting NB-IoT or not (3GPP 36.300)",
			value_type = "yesNoType",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/yesNoType"},
	IsChangeForEnergySavingAllowed = #specification_char{name = "isChangeForEnergySavingAllowed",
			description = "Prohibit or allow configuration changes of the cell for ESM purposes",
			value_type = "yesNoType",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/yesNoType"},
	NgranCellFlag = #specification_char{name = "ngranCellFlag",
			description = "Represents whether the cell is provided by ng-eNB or not (3GPP 38.300)",
			value_type = "yesNoType",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/yesNoType"},
	NSSAI = #specification_char{name = "nSSAI",
			description = "Network Slice Selection Assistance Information",
			value_type = "NssaiList",
			value_schema = "/resourceCatalogManagement/v3/schema/ngcNrm#/definitions/NssaiList"},
	Earfcn = #specification_char{name = "earfcn",
			description = "Frequency number for the central frequency (3GPP 36.104)",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	SfAssignment = #specification_char{name = "sfAssignment",
			description = "Uplink-downlink subframe configuration number of a TDD cell (3GPP 36.211)",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	SpecialSfPatterns = #specification_char{name = "specialSfPatterns",
			description = "Special subframe configuration number of a TDD cell (3GPP 36.211)",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	EUtranRelation = #specification_char{name = "EUtranRelation",
			description = "Neighbour cell Relation (NR) from a source cell to a target EUtranGenericCell",
			value_type = "EUtranRelationList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/EUtranRelationList"},
	Cdma2000Relation = #specification_char{name = "Cdma2000Relation",
			description = "Neighbour cell Relation (NR) from a source cell to a target CDMA200 sector (3GPP2 TS S.S0028)",
			value_type = "Cdma2000RelationList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/Cdma2000RelationList"},
	GsmRelation = #specification_char{name = "GsmRelation",
			description = "Neighbour cell Relation (NR) from a source cell to a target GsmCell",
			value_type = "GsmRelationList",
			value_schema = "/resourceCatalogManagement/v3/schema/geranNrm#/definitions/GsmRelationList"},
	ContainedNrmClass = #specification_char{name = "EUtranCellFDDOptionallyContainedNrmClass",
			description = "List of optionally contained NRM Class objects",
			value_type = "ENBFunctionOptionallyContainedNrmClassList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/ENBFunctionOptionallyContainedNrmClassList"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VsDataContainerList"},
	EnergySavingProperties = #specification_char{name = "EnergySavingProperties",
			description = "Energy saving properties of a network element supporting Energy Saving Management",
			value_type = "EnergySavingPropertiesList",
			value_schema = "/resourceCatalogManagement/v3/schema/sonPolicyNrm#/definitions/EnergySavingPropertiesList"},
	ESPolicies = #specification_char{name = "ESPolicies",
			description = "Energy saving policies information",
			value_type = "ESPoliciesList",
			value_schema = "/resourceCatalogManagement/v3/schema/sonPolicyNrm#/definitions/ESPoliciesList"},
	Chars = [UserLabel, VnfParametersList, PeeParametersList, CellLocalId,
			CellLocalIdList, CellSize, PLMNIdList, CellAccessInfoList, Tac,
			Pci, PciList, MaximumTransmissionPower, PartOfSectorPower,
			ReferenceSignalPower , Pb, RelatedTmaList, RelatedAntennaList,
			RelatedSector, OperationalState, AdministrativeState,
			AvailabilityStatus, AllowedAccessClasses, CellResvInfo,
			NbIoTcellFlag, IsChangeForEnergySavingAllowed, NgranCellFlag,
			NSSAI, Earfcn, SfAssignment, SpecialSfPatterns, EUtranRelation,
			Cdma2000Relation, GsmRelation, ContainedNrmClass, VsDataContainer,
			EnergySavingProperties, ESPolicies],
	EUtranCellTDDSpecification = #specification{name = "EUtranCellTDD",
			description = "LTE Time Division Duplex (TDD) radio cell",
			class_type = "EUtranCellTDDSpecification",
			schema = "/resourceCatalogManagement/v3/schema/EUtranCellTDDSpecification",
			base_type = "EUtranGenericCell",
			status = "Active",
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "EUtranCellTDD",
					schema = "/resourceInventoryManagement/v3/schema/EUtranCellTDD"},
			characteristic = Chars},
	case im:add_specification(EUtranCellTDDSpecification) of
		{ok, _} ->
			add_sgw(["EUtranCellTDD" | NrmClasses]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_sgw(NrmClasses) ->
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VnfParametersList"},
	PLMNIdList = #specification_char{name = "pLMNIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PLMNIdList",
			value_schema = "/resourceCatalogManagement/v3/schema/epcNrm#/definitions/PLMNIdList"},
	TACList = #specification_char{name = "tACList",
			description = "List of TAC of MMEPoolArea used for traffic handling (3GPP 36.413)",
			value_type = "TACList",
			value_schema = "/resourceCatalogManagement/v3/schema/epcNrm#/definitions/TACList"},
	EP_RP_EPS = #specification_char{name = "EP_RP_EPS",
			description = "Endpoint of reference point in EPS (3GPP 23.401)",
			value_type = "EP_RP_EPSList",
			value_schema = "/resourceCatalogManagement/v3/schema/epcNrm#/definitions/EP_RP_EPSList"},
	ContainedNrmClass = #specification_char{name = "ServingGWFunctionOptionallyContainedNrmClass",
			description = "List of optionally contained NRM Class objects",
			value_type = "ServingGWFunctionOptionallyContainedNrmClassList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/ServingGWFunctionOptionallyContainedNrmClassList"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VsDataContainerList"},
	Chars = [UserLabel, VnfParametersList, PLMNIdList, TACList,
			ContainedNrmClass, EP_RP_EPS, VsDataContainer],
	ServingGWFunctionSpecification= #specification{name = "ServingGWFunction",
			description = "EPC Serving Gateway (SGW)",
			class_type = "ServingGWFunctionSpecification",
			schema = "/resourceCatalogManagement/v3/schema/ServingGWFunctionSpecification",
			base_type = "ResourceFunctionSpecification",
			status = "Active",
			version = "1.0",
			category = "EPC",
			target_schema = #target_schema_ref{class_type = "ServingGWFunction",
					schema = "/resourceInventoryManagement/v3/schema/ServingGWFunction"},
			characteristic = Chars},
	case im:add_specification(ServingGWFunctionSpecification) of
		{ok, _} ->
			add_pgw(["ServingGWFunction" | NrmClasses]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
add_pgw(NrmClasses) ->
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VnfParametersList"},
	EP_RP_EPS = #specification_char{name = "EP_RP_EPS",
			description = "Endpoint of reference point in EPS (3GPP 23.401)",
			value_type = "EP_RP_EPSList",
			value_schema = "/resourceCatalogManagement/v3/schema/epcNrm#/definitions/EP_RP_EPSList"},
	ContainedNrmClass = #specification_char{name = "PGWFunctionOptionallyContainedNrmClass",
			description = "List of optionally contained NRM Class objects",
			value_type = "PGWFunctionOptionallyContainedNrmClassList",
			value_schema = "/resourceCatalogManagement/v3/schema/eutranNrm#/definitions/PGWFunctionOptionallyContainedNrmClassList"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = "/resourceCatalogManagement/v3/schema/genericNrm#/definitions/VsDataContainerList"},
	Chars = [UserLabel, VnfParametersList, ContainedNrmClass,
			EP_RP_EPS, VsDataContainer],
	PGWFunctionSpecification= #specification{name = "PGWFunction",
			description = "EPC PDN Gateway (PGW)",
			class_type = "PGWFunctionSpecification",
			schema = "/resourceCatalogManagement/v3/schema/PGWFunctionSpecification",
			base_type = "ResourceFunctionSpecification",
			status = "Active",
			version = "1.0",
			category = "EPC",
			target_schema = #target_schema_ref{class_type = "PGWFunction",
					schema = "/resourceInventoryManagement/v3/schema/PGWFunction"},
			characteristic = Chars},
	case im:add_specification(PGWFunctionSpecification) of
		{ok, _} ->
			error_logger:info_report(["Added 3GPP NRM Classes to specification table",
					{classes, lists:reverse(["PGWFunction" | NrmClasses])}]);
		{error, Reason} ->
			{error, Reason}
	end.

