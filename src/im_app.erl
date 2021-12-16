%%% im_app.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018 - 2021 SigScale Global Inc.
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
-copyright('Copyright (c) 2018 - 2021 SigScale Global Inc.').

-behaviour(application).

%% callbacks needed for application behaviour
-export([start/2, stop/1, config_change/3]).
%% optional callbacks for application behaviour
-export([prep_stop/1, start_phase/3]).
%% export the im_app private API for installation
-export([install/0, install/1, join/1]).

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
			start1();
		{timeout, BadTabList} ->
			case force(BadTabList) of
				ok ->
					start1();
				{error, Reason} ->
					error_logger:error_report(["sigscale_im application failed to start",
							{reason, Reason}, {module, ?MODULE}]),
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start1() ->
	Options = [set, public, named_table, {write_concurrency, true}],
	metrics = ets:new(metrics, Options),
	supervisor:start_link(im_sup, []).

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
		Result :: {ok, Tables} | {error, Reason},
		Tables :: [atom()],
		Reason :: term().
%% @doc Initialize SigScale Inventory Management (IM) tables.
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
					install1(Nodes, false);
				{error, {_, {already_exists, _}}} ->
					error_logger:info_report("mnesia schema already exists",
							[{nodes, Nodes}]),
					install1(Nodes, true);
				{error, Reason} ->
					error_logger:info_msg("Found existing schema.~n"),
					error_logger:error_report(["Failed to create schema",
							mnesia:error_description(Reason),
							{nodes, Nodes}, {error, Reason}]),
					{error, Reason}
			end;
		_ ->
			install2(Nodes, true)
	end.
%% @hidden
install1([Node] = Nodes, Exist) when Node == node() ->
	case mnesia:start() of
		ok ->
			error_logger:info_msg("Started mnesia~n"),
			install2(Nodes, Exist);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
					{error, Reason}]),
			{error, Reason}
	end;
install1(Nodes, Exist) ->
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
					install2(Nodes, Exist);
				NotOKs ->
					error_logger:error_report(["Failed to start mnesia"
							" on all nodes", {nodes, Nodes}, {errors, NotOKs}]),
					{error, NotOKs}
			end;
		{Results, BadNodes} = Reason ->
			error_logger:error_report(["Failed to start mnesia"
					" on all nodes", {nodes, Nodes}, {results, Results},
					{badnodes, BadNodes}]),
			{error, Reason}
	end.
%% @hidden
install2(Nodes, Exist) ->
	case mnesia:wait_for_tables([schema], ?WAITFORSCHEMA) of
		ok ->
			install3(Nodes, Exist, []);
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
install3(Nodes, Exist, Acc) ->
	case mnesia:create_table(catalog, [{disc_copies, Nodes},
			{attributes, record_info(fields, catalog)}, {index, [name]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new resource catalog table.~n"),
			install4(Nodes, Exist, [catalog | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, catalog}} ->
			error_logger:info_msg("Found existing resource catalog table.~n"),
			install4(Nodes, Exist, [catalog | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install4(Nodes, Exist, Acc) ->
	case mnesia:create_table(category, [{disc_copies, Nodes},
			{attributes, record_info(fields, category)}, {index, [name]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new resource category table.~n"),
			install5(Nodes, Exist, [category | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, category}} ->
			error_logger:info_msg("Found existing resource category table.~n"),
			install5(Nodes, Exist, [category | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install5(Nodes, Exist, Acc) ->
	case mnesia:create_table(candidate, [{disc_copies, Nodes},
			{attributes, record_info(fields, candidate)}, {index, [name]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new resource candidate table.~n"),
			install6(Nodes, Exist, [candidate | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, candidate}} ->
			error_logger:info_msg("Found existing resource candidate table.~n"),
			install6(Nodes, Exist, [candidate | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install6(Nodes, Exist, Acc) ->
	case mnesia:create_table(specification, [{disc_copies, Nodes},
			{attributes, record_info(fields, specification)}, {index, [name]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new resource specification table.~n"),
			install7(Nodes, Exist, [specification | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, specification}} ->
			error_logger:info_msg("Found existing resource specification table.~n"),
			install7(Nodes, Exist, [specification | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install7(Nodes, Exist, Acc) ->
	Tables = [resource | Acc],
	case mnesia:create_table(resource, [{disc_copies, Nodes},
			{attributes, record_info(fields, resource)}, {index, [name]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new resource inventory table.~n"),
			case Exist of
				true ->
					install16(Nodes, Exist, Tables);
				false ->
					install8(Nodes, Exist, Tables)
			end;
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, resource}} ->
			error_logger:info_msg("Found existing resource inventory table.~n"),
			case Exist of
				true ->
					install16(Nodes, Exist, Tables);
				false ->
					install8(Nodes, Exist, Tables)
			end;
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install8(Nodes, Exist, Acc) ->
	CategoryFuns = [ngc_category, nr_category, epc_category, lte_category,
			core_category, umts_category, gsm_category, ims_category,
			oda_category],
	install8(CategoryFuns, Nodes, Exist, Acc).
%% @hidden
install8([F | T], Nodes, Exist, Acc) ->
	case im:add_category(im_specification:F()) of
		{ok, #category{}} ->
			install8(T, Nodes, Exist, Acc);
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM categories.",
				{error, Reason}]),
			{error, Reason}
	end;
install8([], Nodes, Exist, Acc) ->
	error_logger:info_msg("Added 3GPP NRM resource categories.~n"),
	install9(Nodes, Exist, Acc).
%% @hidden
install9(Nodes, Exist, Acc) ->
	CatalogFuns = [ng_catalog, lte_catalog, umts_catalog, gsm_catalog,
			oda_catalog],
	install9(CatalogFuns, Nodes, Exist, Acc).
%% @hidden
install9([F | T], Nodes, Exist, Acc) ->
	case im:add_catalog(im_specification:F()) of
		{ok, #catalog{}} ->
			install9(T, Nodes, Exist, Acc);
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM catalogs.",
				{error, Reason}]),
			{error, Reason}
	end;
install9([], Nodes, Exist, Acc) ->
	error_logger:info_msg("Added 3GPP NRM resource catalogs.~n"),
	install10(Nodes, Exist, Acc).
%% @hidden
install10(Nodes, Exist, Acc) ->
	SpecFuns = [generic_me, generic_subnetwork,
		gsm_cell, gsm_bts, gsm_bss, gsm_abis_link,
		umts_nodeb, umts_cell_fdd, umts_cell_tdd_lcr, umts_cell_tdd_hcr,
		umts_iub_link, umts_rnc,
		nr_sector_carrier, nr_cell_cu, nr_cell_du, nr_ep_x2c, nr_ep_x2u,
		nr_ep_ngc, nr_ep_ngu, nr_ep_xnc, nr_ep_xnu, nr_ep_f1c, nr_ep_f1u,
		nr_ep_e1, nr_ep_s1u, nr_gnb_cu_up, nr_gnb_cu_cp, nr_gnb_du,
		ngc_ep_n2, ngc_ep_n3, ngc_ep_n4, ngc_ep_n5, ngc_ep_n6, ngc_ep_n7,
		ngc_ep_n8, ngc_ep_n9, ngc_ep_n10, ngc_ep_n11, ngc_ep_n12, ngc_ep_n13,
		ngc_ep_n14, ngc_ep_n15, ngc_ep_n16, ngc_ep_n17, ngc_ep_n20, ngc_ep_n21,
		ngc_ep_n22, ngc_ep_n26, ngc_ep_n27, ngc_ep_n31, ngc_ep_n32, ngc_ep_nls,
		ngc_ep_nlg, ngc_ep_sbi_x, ngc_ep_sbi_ipx, ngc_ep_s5c, ngc_ep_s5u,
		ngc_ep_rx, ngc_ep_map_smsc,
		ngc_amf, ngc_smf, ngc_upf, ngc_n3iwf, ngc_pcf, ngc_ausf, ngc_udm, ngc_udr,
		ngc_udsf, ngc_nrf, ngc_nssf, ngc_smsf, ngc_lmf, ngc_ngeir, ngc_sepp,
		ngc_nwdaf,
		epc_link_mme_mme, epc_link_hss_mme, epc_link_mme_sgsn,
		epc_link_mme_servinggw, epc_link_enb_mme, epc_ep_rp_eps,
		epc_sgw, epc_pgw, epc_mme, epc_pcrf, epc_epdg,
		lte_cell_fdd, lte_cell_tdd, lte_enb,
		core_iucs, core_iups, core_iubc, core_alink, core_gb_link,
		core_msc, core_mgw, core_sgsn, core_ggsn, core_auc, core_hlr, core_eir,
		core_mnpsrf, core_cgf, core_sgw, core_cbc,
		ims_as, ims_hss, ims_pcscf, ims_scscf, ims_icscf,
		pee_me, epcn3ai_proxy, epcn3ai_server,
		im_tmaiu, im_aiu, im_iu, im_iu_ne, im_iu_hw, im_iu_sw, im_iu_lic,
		huawei_usn, huawei_ugw, huawei_cgpomu, huawei_igwb,
		huawei_uscdb, huawei_spsv3, huawei_mscsiosp, huawei_mscso,
		mec_rnis, mec_ls, mec_tr, mec_dnsr,
		mec_meas, mec_meps, mec_mea, mec_mep, mec_mehf,
		network_slice, network_slice_subnet,
		im_erlang_spec, im_catalog_api_spec, im_inventory_api_spec, im_rpc_spec,
		im_net_kernel_spec, im_httpd_spec, im_catalog_spec, im_inventory_spec,
		im_kernel_spec, im_inets_spec, im_application_spec, im_erlang_node_spec,
		sigscale_rim_spec],
	install10(SpecFuns, [], Nodes, Exist, Acc).
%% @hidden
install10([generic_subnetwork | T], SpecAcc, Nodes, Exist, Acc) ->
	case im:add_specification(im_specification:generic_subnetwork()) of
		{ok, #specification{id = Sid, href = Shref, name = Sname,
				class_type = Stype, related = Srels} = IUSpec} ->
			MESpecRel = #specification_rel{id = Sid, href = Shref, name = Sname,
					ref_type = Stype, rel_type = "composedOf"},
			Ftrans = fun() ->
					mnesia:write(specification, IUSpec#specification{
							related = [MESpecRel] ++ Srels}, write)
			end,
			case mnesia:transaction(Ftrans) of
				{aborted, Reason} ->
					{error, Reason};
				{atomic, ok} ->
					install10(T, SpecAcc, Nodes, Exist, Acc)
			end;
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
				{error, Reason}]),
			{error, Reason}
	end;
install10([F | T], SpecAcc, Nodes, Exist, Acc)
		when F == im_iu_ne; F == im_iu_hw; F == im_iu_sw; F == im_iu_lic ->
	case im:add_specification(im_specification:F()) of
		{ok, #specification{id = Sid, href = Shref, name = Sname,
				class_type = Stype} = IUSpec} ->
			IUResRel = #specification_rel{id = Sid, href = Shref, name = Sname,
					ref_type = Stype, rel_type = "composedOf"},
			Ftrans = fun() ->
					mnesia:write(specification,
							IUSpec#specification{related = [IUResRel]}, write)
			end,
			case mnesia:transaction(Ftrans) of
				{aborted, Reason} ->
					{error, Reason};
				{atomic, ok} ->
					install10(T, SpecAcc, Nodes, Exist, Acc)
			end;
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
				{error, Reason}]),
			{error, Reason}
	end;
install10([im_iu | T], SpecAcc, Nodes, Exist, Acc) ->
	case im:add_specification(im_specification:im_iu()) of
		{ok, #specification{id = Sid, href = Shref, name = Sname,
				class_type = Stype, related = ResRels} = IUSpec} ->
			IUResRel = #specification_rel{id = Sid, href = Shref, name = Sname,
					ref_type = Stype, rel_type = "composedOf"},
			{#specification{} = TmaSpec, #specification{} = AntennaSpec}
					= tma_antenna_spec(),
			Ftrans = fun() ->
					ok = mnesia:write(specification,
							IUSpec#specification{related = [IUResRel] ++ ResRels},
							write),
					ok = mnesia:write(specification,
							TmaSpec#specification{related = [IUResRel]}, write),
					mnesia:write(specification,
							AntennaSpec#specification{related = [IUResRel]}, write)
			end,
			case mnesia:transaction(Ftrans) of
				{aborted, Reason} ->
					{error, Reason};
				{atomic, ok} ->
					install10(T, SpecAcc, Nodes, Exist, Acc)
			end;
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
					{error, Reason}]),
			{error, Reason}
	end;
install10([umts_rnc | T], SpecAcc, Nodes, Exist, Acc) ->
	CategoryName = category_name(atom_to_list(umts_rnc)),
	case im:add_specification(im_specification:umts_rnc()) of
		{ok, #specification{id = RncSpecId} = RncSpec} ->
			Connectivity = rnc_connectivity(RncSpec),
			Ftrans = fun() ->
					[S] = mnesia:read(specification, RncSpecId, write),
					mnesia:write(specification,
							S#specification{connectivity = [Connectivity]}, write)
			end,
			case mnesia:transaction(Ftrans) of
				{aborted, Reason} ->
					{error, Reason};
				{atomic, ok} ->
					ok = add_candidate(CategoryName, RncSpec),
					install10(T, SpecAcc, Nodes, Exist, Acc)
			end;
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
					{error, Reason}]),
			{error, Reason}
	end;
install10([im_kernel_spec = F | T], SpecAcc, Nodes, Exist, Acc) ->
	CategoryName = category_name(atom_to_list(F)),
	case im:add_specification(im_specification:F()) of
		{ok, #specification{id = Sid, href = Shref, name = Sname,
				class_type = Stype} = Spec} ->
			ok = add_candidate(CategoryName, Spec),
			KernelRel = #specification_rel{id = Sid,
					href = Shref, name = Sname, ref_type = Stype},
			{ok, NewSpecAcc1} = write_rel_in(["Erlang"],
					KernelRel#specification_rel{rel_type = "composedOf"}, SpecAcc),
			{ok, NewSpecAcc2} = write_rel_in(["net_kernel"],
					KernelRel#specification_rel{
					rel_type = "providedBy"}, NewSpecAcc1),
			install10(T, NewSpecAcc2, Nodes, Exist, Acc);
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
					{error, Reason}]),
			{error, Reason}
	end;
install10([im_inets_spec = F | T], SpecAcc, Nodes, Exist, Acc) ->
	CategoryName = category_name(atom_to_list(F)),
	case im:add_specification(im_specification:F()) of
		{ok, #specification{id = Sid, href = Shref, name = Sname,
				class_type = Stype} = Spec} ->
			ok = add_candidate(CategoryName, Spec),
			InetsRel = #specification_rel{id = Sid,
					href = Shref, name = Sname, ref_type = Stype},
			{ok, NewSpecAcc1} = write_rel_in(["Erlang"],
					InetsRel#specification_rel{rel_type = "composedOf"}, SpecAcc),
			{ok, NewSpecAcc2} = write_rel_in(["httpd"], InetsRel#specification_rel{
					rel_type = "providedBy"}, NewSpecAcc1),
			install10(T, NewSpecAcc2, Nodes, Exist, Acc);
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
					{error, Reason}]),
			{error, Reason}
	end;
install10([im_application_spec = F | T], SpecAcc, Nodes, Exist, Acc) ->
	CategoryName = category_name(atom_to_list(F)),
	case im:add_specification(im_specification:F()) of
		{ok, #specification{id = Sid, href = Shref, name = Sname,
				class_type = Stype} = Spec} ->
			ok = add_candidate(CategoryName, Spec),
			AppRel = #specification_rel{id = Sid,
					href = Shref, name = Sname, ref_type = Stype},
			{ok, NewSpecAcc1} = write_rel_in(["Erlang"], AppRel#specification_rel{
					rel_type = "composedOf"}, SpecAcc),
			{ok, NewSpecAcc2} = write_rel_in(["Resource Catalog",
					"Resource Inventory"], AppRel#specification_rel{
					rel_type = "providedBy"}, NewSpecAcc1),
			install10(T, NewSpecAcc2, Nodes, Exist, Acc);
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
					{error, Reason}]),
			{error, Reason}
	end;
install10([im_erlang_node_spec = F | T], SpecAcc, Nodes, Exist, Acc) ->
	CategoryName = category_name(atom_to_list(F)),
	case im:add_specification(im_specification:F()) of
		{ok, #specification{id = Sid, href = Shref, name = Sname,
				class_type = Stype} = Spec} ->
			ErlNodeRel = #specification_rel{id = Sid, href = Shref,
					name = Sname, ref_type = Stype, rel_type = "composedOf"},
			{ok, NewSpecAcc} = write_rel_in(["Erlang"], ErlNodeRel, SpecAcc),
			Fspecrel = fun(#specification{id = Cid, href = Chref, name = Cname,
							class_type = Ctype}) when Cname == "net_kernel";
							Cname == "httpd"; Cname == "Resource Catalog";
							Cname == "Resource Inventory" ->
						{true, #specification_rel{id = Cid, href = Chref,
								name = Cname, ref_type = Ctype,
								rel_type = "composedOf"}};
					(_) ->
						false
			end,
			case lists:keyfind("Erlang", #specification.name, NewSpecAcc) of
				#specification{id = Pid, href = Phref,
						name = Pname, class_type = Ptype} ->
					ParentRel = #specification_rel{id = Pid, href = Phref,
							name = Pname, ref_type = Ptype, rel_type = "providedBy"},
					ChildRels = lists:filtermap(Fspecrel, SpecAcc),
					NewSpec = Spec#specification{related = [ParentRel | ChildRels]},
					ok = write_spec(NewSpec),
					ok = add_candidate(CategoryName, NewSpec),
					install10(T, [NewSpec | NewSpecAcc], Nodes, Exist, Acc);
				false ->
					{error, not_found}
			end;
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
				{error, Reason}]),
			{error, Reason}
	end;
install10([F | T], SpecAcc, Nodes, Exist, Acc) ->
	case im:add_specification(im_specification:F()) of
		{ok, #specification{} = Spec} ->
			case category_name(atom_to_list(F)) of
				"ODA" ->
					ok = add_candidate("ODA", Spec),
					install10(T, [Spec | SpecAcc], Nodes, Exist, Acc);
				CategoryName ->
					ok = add_candidate(CategoryName, Spec),
					install10(T, SpecAcc, Nodes, Exist, Acc)
			end;
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
					{error, Reason}]),
			{error, Reason}
	end;
install10([], _SpecAcc, Nodes, Exist, Acc) ->
	error_logger:info_msg("Added 3GPP NRM resource specifications.~n"),
	install11(Nodes, Exist, Acc).
%% @hidden
install11(Nodes, Exist, Acc) ->
	case application:load(inets) of
		ok ->
			error_logger:info_msg("Loaded inets.~n"),
			install12(Nodes, Exist, Acc);
		{error, {already_loaded, inets}} ->
			install12(Nodes, Exist, Acc)
	end.
%% @hidden
install12(Nodes, Exist, Acc) ->
	case application:load(sigscale_im) of
		ok ->
			error_logger:info_msg("Loaded sigscale_im.~n"),
			install13(Nodes, Exist, Acc);
		{error, {already_loaded, sigscale_im}} ->
			install13(Nodes, Exist, Acc)
	end.
%% @hidden
install13(Nodes, Exist, Tables) ->
	case application:get_env(inets, services) of
		{ok, InetsServices} ->
			install14(Nodes, Exist, Tables, InetsServices);
		undefined ->
			error_logger:info_msg("Inets services not defined. "
					"User table not created~n"),
			install20(Tables)
	end.
%% @hidden
install14(Nodes, Exist, Tables, InetsServices) ->
	case lists:keyfind(httpd, 1, InetsServices) of
		{httpd, HttpdInfo} ->
			F = fun({directory, _}) ->
						true;
					(_) ->
						false
			end,
			install15(Nodes, Exist, Tables, lists:filter(F, HttpdInfo));
		false ->
			error_logger:info_msg("Httpd service not defined. "
					"User table not created~n"),
			install20(Tables)
	end.
%% @hidden
install15(Nodes, Exist, Tables, [{directory, {_Dir, []}} | T]) ->
	install15(Nodes, Exist, Tables, T);
install15(Nodes, Exist, Tables, [{directory, {_, DirectoryInfo}} | _]) ->
	case lists:keyfind(auth_type, 1, DirectoryInfo) of
		{auth_type, mnesia} ->
			install16(Nodes, Exist, Tables);
		_ ->
			error_logger:info_msg("Auth type not mnesia. "
					"User table not created~n"),
			install20(Tables)
	end;
install15(_Nodes, _Exist, Tables, []) ->
	error_logger:info_msg("Auth directory not defined. "
			"User table not created~n"),
	install20(Tables).
%% @hidden
install16(Nodes, Exist, Acc) ->
	case mnesia:create_table(httpd_user, [{type, bag}, {disc_copies, Nodes},
			{attributes, record_info(fields, httpd_user)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new httpd_user table.~n"),
			install17(Nodes, Exist, [httpd_user | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, httpd_user}} ->
			error_logger:info_msg("Found existing httpd_user table.~n"),
			install17(Nodes, Exist, [httpd_user | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install17(Nodes, Exist, Acc) ->
	case mnesia:create_table(httpd_group, [{type, bag}, {disc_copies, Nodes},
			{attributes, record_info(fields, httpd_group)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new httpd_group table.~n"),
			install18(Nodes, Exist, [httpd_group | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, httpd_group}} ->
			error_logger:info_msg("Found existing httpd_group table.~n"),
			install18(Nodes, Exist, [httpd_group | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install18(Nodes, Exist, Acc) ->
	case mnesia:create_table(pee_rule, [{disc_copies, Nodes},
			{attributes, record_info(fields, pee_rule)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new pee rule table.~n"),
			case Exist of
				true ->
					install20([pee_rule | Acc]);
				false ->
					install19(Nodes, [pee_rule | Acc], [])
			end;
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, pee_rule}} ->
			error_logger:info_msg("Found existing pee rule table.~n"),
			case Exist of
				true ->
					install20([pee_rule | Acc]);
				false ->
					install19(Nodes, [pee_rule | Acc], [])
			end;
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install19([], Tables, ErlNodeRelAcc) ->
	Res = im_specification:sigscale_rim_res(),
	case im:add_resource(Res#resource{related = ErlNodeRelAcc}) of
		{ok, #resource{}} ->
			install20(Tables);
		{error, Reason} ->
			error_logger:error_report(["Failed to add SigScale RIM resource.",
					{error, Reason}]),
			{error, Reason}
	end;
install19(Nodes, Acc, ErlNodeRelAcc) ->
	ResourceFuns = [im_erlang_res, im_catalog_api_res, im_inventory_api_res,
			im_rpc_res, im_net_kernel_res, im_httpd_res, im_catalog_res,
			im_inventory_res, im_kernel_res, im_inets_res,
			im_application_res, im_erlang_node_res],
	install19(ResourceFuns, Nodes, Acc, ErlNodeRelAcc, []).
%% @hidden
install19([im_kernel_res = F | T],
		[Node | _] = Nodes, Acc, ErlNodeRelAcc, ResAcc) ->
	case im:add_resource(im_specification:F(Node)) of
		{ok, #resource{id = ResId, href = ResHref, name = ResName,
				class_type = ResType}} ->
			KernelRel = #resource_rel{id = ResId,
					href = ResHref, name = ResName, ref_type = ResType},
			{ok, NewResAcc1} = write_res_rel_in(["Erlang"],
					KernelRel#resource_rel{rel_type = "composedOf"}, ResAcc),
			{ok, NewResAcc2} = write_res_rel_in(["net_kernel"],
					KernelRel#resource_rel{rel_type = "providedBy"}, NewResAcc1),
			install19(T, Nodes, Acc, ErlNodeRelAcc, NewResAcc2);
		{error, Reason} ->
			error_logger:error_report(["Failed to add ODA Component resources.",
					{error, Reason}]),
			{error, Reason}
	end;
install19([im_inets_res = F | T],
		[Node | _] = Nodes, Acc, ErlNodeRelAcc, ResAcc) ->
	case im:add_resource(im_specification:F(Node)) of
		{ok, #resource{id = ResId, href = ResHref, name = ResName,
				class_type = ResType}} ->
			InetsRel = #resource_rel{id = ResId,
					href = ResHref, name = ResName, ref_type = ResType},
			{ok, NewResAcc1} = write_res_rel_in(["Erlang"],
					InetsRel#resource_rel{rel_type = "composedOf"}, ResAcc),
			{ok, NewResAcc2} = write_res_rel_in(["httpd"],
					InetsRel#resource_rel{rel_type = "providedBy"}, NewResAcc1),
			install19(T, Nodes, Acc, ErlNodeRelAcc, NewResAcc2);
		{error, Reason} ->
			error_logger:error_report(["Failed to add ODA Component resources.",
					{error, Reason}]),
			{error, Reason}
	end;
install19([im_application_res = F | T],
		[Node | _] = Nodes, Acc, ErlNodeRelAcc, ResAcc) ->
	case im:add_resource(im_specification:F(Node)) of
		{ok, #resource{id = ResId, href = ResHref, name = ResName,
				class_type = ResType}} ->
			AppRel = #resource_rel{id = ResId, href = ResHref,
					name = ResName, ref_type = ResType},
			{ok, NewResAcc1} = write_res_rel_in(["Erlang"],
					AppRel#resource_rel{rel_type = "composedOf"}, ResAcc),
			{ok, NewResAcc2} = write_res_rel_in(["Resource Catalog",
					"Resource Inventory"],
					AppRel#resource_rel{rel_type = "providedBy"}, NewResAcc1),
			install19(T, Nodes, Acc, ErlNodeRelAcc, NewResAcc2);
		{error, Reason} ->
			error_logger:error_report(["Failed to add ODA Component resources.",
					{error, Reason}]),
			{error, Reason}
	end;
install19([im_erlang_node_res = F | T],
		[Node | _] = Nodes, Acc, ErlNodeRelAcc, ResAcc) ->
	case im:add_resource(im_specification:F(Node)) of
		{ok, #resource{id = ResId, href = ResHref, name = ResName,
				class_type = ResType} = Res} ->
			ErlNodeRel = #resource_rel{id = ResId, href = ResHref,
					name = ResName, ref_type = ResType, rel_type = "composedOf"},
			{ok, NewResAcc} = write_res_rel_in(["Erlang"], ErlNodeRel, ResAcc),
			Fresrel = fun(#resource{id = Cid, href = Chref, name = Cname,
							class_type = Ctype}) when Cname == "net_kernel";
							Cname == "httpd"; Cname == "Resource Catalog";
							Cname == "Resource Inventory" ->
						{true, #resource_rel{id = Cid, href = Chref,
								name = Cname, ref_type = Ctype,
								rel_type = "composedOf"}};
					(_) ->
						false
			end,
			case lists:keyfind("Erlang", #resource.name, NewResAcc) of
				#resource{id = Pid, href = Phref,
						name = Pname, class_type = Ptype} ->
					ParentRel = #resource_rel{id = Pid, href = Phref,
							name = Pname, ref_type = Ptype, rel_type = "providedBy"},
					ChildRels = lists:filtermap(Fresrel, ResAcc),
					NewRes = Res#resource{related = [ParentRel | ChildRels]},
					ok = write_resource(NewRes),
					install19(T, Nodes, Acc,
							[ErlNodeRel | ErlNodeRelAcc], [NewRes | NewResAcc]);
				false ->
					error_logger:error_report(["Failed to find Erlang resource."]),
					{error, not_found}
			end;
		{error, Reason} ->
			error_logger:error_report(["Failed to add ODA Component resources.",
					{error, Reason}]),
			{error, Reason}
	end;
install19([F | T], [Node | _] = Nodes, Acc, ErlNodeRelAcc, ResAcc) ->
	case im:add_resource(im_specification:F(Node)) of
		{ok, #resource{} = Res} ->
			install19(T, Nodes, Acc, ErlNodeRelAcc, [Res | ResAcc]);
		{error, Reason} ->
			error_logger:error_report(["Failed to add ODA Component resources.",
				{error, Reason}]),
			{error, Reason}
	end;
install19([], [_Node | Nodes], Acc, ErlNodeRelAcc, _ResAcc) ->
	error_logger:info_msg("Added ODA Component resources.~n"),
	install19(Nodes, Acc, ErlNodeRelAcc).
%% @hidden
install20(Tables) ->
	case mnesia:wait_for_tables(Tables, ?WAITFORTABLES) of
		ok ->
			install21(Tables, lists:member(httpd_user, Tables));
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
install21(Tables, true) ->
	case inets:start() of
		ok ->
			error_logger:info_msg("Started inets.~n"),
			install22(Tables);
		{error, {already_started, inets}} ->
			install22(Tables);
		{error, Reason} ->
			error_logger:error_msg("Failed to start inets~n"),
			{error, Reason}
	end;
install21(Tables, false) ->
	{ok, Tables}.
%% @hidden
install22(Tables) ->
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

-spec join(Node) -> Result
	when
		Node :: atom(),
		Result :: {ok, Tables} | {error, Reason},
		Tables :: [atom()],
		Reason :: term().
%% @doc Join an existing cluster.
%%
%% 	Tables will be copied from a give `Node'.
%%
join(Node) when is_atom(Node) ->
	case mnesia:system_info(is_running) of
		no ->
			join1(Node);
		Running ->
			error_logger:error_report(["mnesia running", {is_running, Running}]),
			{error, Running}
	end.
%% @hidden
join1(Node) ->
	case net_kernel:connect_node(Node) of
		true ->
			join2(Node);
		Connect ->
			error_logger:error_report(["Failed to connect node", {result, Connect}]),
			{error, Connect}
	end.
%% @hidden
join2(Node) ->
	case rpc:call(Node, mnesia, add_table_copy, [schema, node(), ram_copies]) of
		{atomic, ok} ->
			join3(Node, false);
		{aborted, {already_exists, schema, _}} ->
			join3(Node, true);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join3(Node, Exist) ->
	case application:start(mnesia) of
		ok ->
			join4(Node, Exist);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join4(Node, Exist) ->
	case mnesia:change_config(extra_db_nodes, [Node]) of
		{ok, _Nodes} ->
			join5(Node, Exist);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join5(Node, Exist) ->
	case mnesia:change_table_copy_type(schema, node(), disc_copies) of
		{atomic, ok} ->
			error_logger:info_msg("Copied schema table from ~s.~n", [Node]),
			join6(Node, Exist, [schema]);
		{aborted, {already_exists, schema, _, disc_copies}} ->
			join6(Node, Exist, [schema]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join6(Node, Exist, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [catalog, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied catalog table from ~s.~n", [Node]),
			join7(Node, Exist, [catalog | Acc]);
		{aborted, {already_exists, catalog, _}} ->
			join7(Node, Exist, [catalog | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join7(Node, Exist, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [category, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied category table from ~s.~n", [Node]),
			join8(Node, Exist, [category | Acc]);
		{aborted, {already_exists, category, _}} ->
			error_logger:info_msg("Found existing category table on ~s.~n", [Node]),
			join8(Node, Exist, [category | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join8(Node, Exist, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [candidate, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied candidate table from ~s.~n", [Node]),
			join9(Node, Exist, [candidate | Acc]);
		{aborted, {already_exists, candidate, _}} ->
			error_logger:info_msg("Found existing candidate table on ~s.~n", [Node]),
			join9(Node, Exist, [candidate | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join9(Node, Exist, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [specification, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied specification table from ~s.~n", [Node]),
			join10(Node, Exist, [specification | Acc]);
		{aborted, {already_exists, specification, _}} ->
			error_logger:info_msg("Found existing specification table on ~s.~n", [Node]),
			join10(Node, Exist, [specification | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join10(Node, Exist, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [resource, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied resource table from ~s.~n", [Node]),
			join11(Node, Exist, [resource | Acc]);
		{aborted, {already_exists, resource, _}} ->
			error_logger:info_msg("Found existing resource table on ~s.~n", [Node]),
			join11(Node, Exist, [resource | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join11(Node, Exist, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [httpd_user, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied httpd_user table from ~s.~n", [Node]),
			join12(Node, Exist, [httpd_user | Acc]);
		{aborted, {already_exists, httpd_user, _}} ->
			error_logger:info_msg("Found existing httpd_user table on ~s.~n", [Node]),
			join12(Node, Exist, [httpd_user | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join12(Node, Exist, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [httpd_group, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied httpd_group table from ~s.~n", [Node]),
			join13(Node, Exist, [httpd_group | Acc]);
		{aborted, {already_exists, httpd_group, _}} ->
			error_logger:info_msg("Found existing httpd_group table on ~s.~n", [Node]),
			join13(Node, Exist, [httpd_group | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join13(Node, Exist, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [pee_rule, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied pee_rule table from ~s.~n", [Node]),
			join14(Node, Exist, [pee_rule | Acc]);
		{aborted, {already_exists, pee_rule, _}} ->
			error_logger:info_msg("Found existing pee_rule table on ~s.~n", [Node]),
			join14(Node, Exist, [pee_rule | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join14(_Node, Exist, Tables) ->
	case mnesia:wait_for_tables(lists:reverse(Tables), ?WAITFORTABLES) of
		ok when Exist == false ->
			{new_erl_node(), Tables};
		ok when Exist == true ->
			{ok, Tables};
		{timeout, BadTables} ->
			error_logger:error_report(["Timeout waiting for tables",
					{tables, BadTables}]),
			{error, BadTables};
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
					{error, Reason}]),
			{error, Reason}
	end.

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

%% @hidden
tma_antenna_spec() ->
	F = fun(SpecName) ->
			case im:get_specification_name(SpecName) of
				{ok, #specification{} = Spec} ->
					Spec;
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, SpecName}, {error, Reason}]),
					{error, Reason}
			end
	end,
	{F("TmaInventoryUnit"), F("AntennaInventoryUnit")}.

%% @hidden
rnc_connectivity(#specification{id = RncId, href = RncHref,
		name = RncName, class_type = Classtype}) ->
	RncEndPointSpec = #endpoint_spec_ref{id = RncId, href = RncHref,
			name = RncName, ref_type = Classtype},
	EndPointSpecNames = ["UtranCellFDD", "UtranCellTDDLcr", "UtranCellTDDHcr"],
	Fcon = fun(EndPointSpecName, Acc) ->
			case im:get_specification_name(EndPointSpecName) of
				{ok, #specification{id = SpecId, href = Spechref,
						name = EndPointSpecName, class_type = Spectype}} ->
					EndPointSpec = #endpoint_spec_ref{id = SpecId, href = Spechref,
							name = EndPointSpecName, ref_type = Spectype},
							[#connection_spec{name = "IubLink", ass_type = "pointtoPoint",
									description = "Edge between contained RFs",
									endpoint = [RncEndPointSpec, EndPointSpec]} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, EndPointSpecName}, {error, Reason}]),
					Acc
			end
	end,
	#resource_graph_spec{name = "Adjacency Graph",
			class_type = "ResourceGraphSpecification",
			description = "Topology of internal adjacency",
			connection = lists:reverse(lists:foldl(Fcon, [], EndPointSpecNames))}.

%% @hidden
category_name("gsm_" ++ _) ->
	"GSM";
category_name("umts_" ++ _) ->
	"UMTS";
category_name("lte_" ++ _) ->
	"LTE";
category_name("nr_" ++ _) ->
	"NR";
category_name("network_slice" ++ _) ->
	"5GC";
category_name("ngc_" ++ _) ->
	"5GC";
category_name("epc" ++ _) ->
	"EPC";
category_name("core_" ++ _) ->
	"Core";
category_name("ims_" ++ _) ->
	"IMS";
category_name("im_" ++ _) ->
	"ODA";
category_name("sigscale_" ++ _) ->
	"ODA";
category_name(_) ->
	[].

%% @hidden
add_candidate([], #specification{}) ->
	ok;
add_candidate(CategoryName, #specification{id = SpecId, href = SpecHref,
		name = SpecName, version = SpecVersion}) ->
	case im:get_category_name(CategoryName) of
		{ok, #category{id = CategoryId, href = CategoryHref,
				name = CategoryName, candidate = C,
				version = CategoryVersion} = Category} ->
			CategoryRef = #category_ref{id = CategoryId, href = CategoryHref,
					name = CategoryName, version = CategoryVersion},
			Candidate = #candidate{name = SpecName, version = "1.0",
					description = "candidate of " ++ SpecName, status = active,
					class_type = "ResourceCandidate", category = [CategoryRef],
					specification = #specification_ref{id = SpecId,
					href = SpecHref, name = SpecName, version = SpecVersion}},
			case im:add_candidate(Candidate) of
				{ok, #candidate{id = CandidateId, href = CandidateHref,
						name = SpecName, version = CandidateVersion}} ->
					CandidateRef = #candidate_ref{id = CandidateId,
							href = CandidateHref, name = SpecName,
							version = CandidateVersion},
					Ftrans = fun() ->
							mnesia:write(category, Category#category{
									candidate = C ++ [CandidateRef]}, write)
					end,
					case mnesia:transaction(Ftrans) of
						{aborted, Reason} ->
							{error, Reason};
						{atomic, ok} ->
							ok
					end;
				{error, Reason} ->
					error_logger:warning_report(["Error adding resource candidate",
							{candidate, SpecName}, {error, Reason}])
			end;
		{error, Reason} ->
			error_logger:warning_report(["Error reading resource category",
					{category, CategoryName}, {error, Reason}]),
			{error, Reason}
	end.

%% @hidden
write_spec(#specification{} = Spec) ->
	Ftrans = fun() ->
			mnesia:write(specification, Spec, write)
	end,
	case mnesia:transaction(Ftrans) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, ok} ->
			ok
	end.

%% @hidden
write_resource(#resource{} = Res) ->
	Ftrans = fun() ->
			mnesia:write(resource, Res, write)
	end,
	case mnesia:transaction(Ftrans) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, ok} ->
			ok
	end.

%% @hidden
write_rel_in([SpecName | T], Rel, SpecAcc) ->
	case lists:keytake(SpecName, #specification.name, SpecAcc) of
		{value, #specification{related = Rels} = Spec, RestAcc} ->
			NewSpec = Spec#specification{related = [Rel | Rels]},
			ok = write_spec(NewSpec),
			write_rel_in(T, Rel, [NewSpec | RestAcc]);
		false ->
			{error, false}
	end;
write_rel_in([], _Rel, SpecAcc) ->
	{ok, SpecAcc}.

%% @hidden
write_res_rel_in([ResName | T], Rel, ResAcc) ->
	case lists:keytake(ResName, #resource.name, ResAcc) of
		{value, #resource{related = Rels} = Res, RestAcc} ->
			NewRes = Res#resource{related = [Rel | Rels]},
			ok = write_resource(NewRes),
			write_res_rel_in(T, Rel, [NewRes | RestAcc]);
		false ->
			{error, false}
	end;
write_res_rel_in([], _Rel, ResAcc) ->
	{ok, ResAcc}.

-spec new_erl_node() -> Result
	when
		Result :: ok.
%% @doc Add new set of copy of example resources into the table
%% 	and update `SigScale RIM' resource function.
%% @hidden
new_erl_node() ->
	ResourceFuns = [im_erlang_res, im_catalog_api_res, im_inventory_api_res,
			im_rpc_res, im_net_kernel_res, im_httpd_res, im_catalog_res,
			im_inventory_res, im_kernel_res, im_inets_res,
			im_application_res, im_erlang_node_res],
	new_erl_node(ResourceFuns, node(), [], []).
%% @hidden
new_erl_node([im_kernel_res = F | T], Node, ErlNodeRelAcc, ResAcc) ->
	case im:add_resource(im_specification:F(Node)) of
		{ok, #resource{id = ResId, href = ResHref, name = ResName,
				class_type = ResType}} ->
			KernelRel = #resource_rel{id = ResId,
					href = ResHref, name = ResName, ref_type = ResType},
			{ok, NewResAcc1} = write_res_rel_in(["Erlang"],
					KernelRel#resource_rel{rel_type = "composedOf"}, ResAcc),
			{ok, NewResAcc2} = write_res_rel_in(["net_kernel"],
					KernelRel#resource_rel{rel_type = "providedBy"}, NewResAcc1),
			new_erl_node(T, Node, ErlNodeRelAcc, NewResAcc2);
		{error, Reason} ->
			error_logger:error_report(["Failed to add kernel resource.",
				{error, Reason}]),
			{error, Reason}
	end;
%% @hidden
new_erl_node([im_inets_res = F | T], Node, ErlNodeRelAcc, ResAcc) ->
	case im:add_resource(im_specification:F(Node)) of
		{ok, #resource{id = ResId, href = ResHref, name = ResName,
				class_type = ResType}} ->
			InetsRel = #resource_rel{id = ResId,
					href = ResHref, name = ResName, ref_type = ResType},
			{ok, NewResAcc1} = write_res_rel_in(["Erlang"],
					InetsRel#resource_rel{rel_type = "composedOf"}, ResAcc),
			{ok, NewResAcc2} = write_res_rel_in(["httpd"],
					InetsRel#resource_rel{rel_type = "providedBy"}, NewResAcc1),
			new_erl_node(T, Node, ErlNodeRelAcc, NewResAcc2);
		{error, Reason} ->
			error_logger:error_report(["Failed to add ODA Component resources.",
				{error, Reason}]),
			{error, Reason}
	end;
%% @hidden
new_erl_node([im_application_res = F | T], Node, ErlNodeRelAcc, ResAcc) ->
	case im:add_resource(im_specification:F(Node)) of
		{ok, #resource{id = ResId, href = ResHref, name = ResName,
				class_type = ResType}} ->
			AppRel = #resource_rel{id = ResId, href = ResHref,
					name = ResName, ref_type = ResType},
			{ok, NewResAcc1} = write_res_rel_in(["Erlang"],
					AppRel#resource_rel{rel_type = "composedOf"}, ResAcc),
			{ok, NewResAcc2} = write_res_rel_in(["Resource Catalog",
					"Resource Inventory"],
					AppRel#resource_rel{rel_type = "providedBy"}, NewResAcc1),
			new_erl_node(T, Node, ErlNodeRelAcc, NewResAcc2);
		{error, Reason} ->
			error_logger:error_report(["Failed to add sigscale_im resource.",
				{error, Reason}]),
			{error, Reason}
	end;
%% @hidden
new_erl_node([im_erlang_node_res = F | T], Node, ErlNodeRelAcc, ResAcc) ->
	case im:add_resource(im_specification:F(Node)) of
		{ok, #resource{id = ResId, href = ResHref, name = ResName,
				class_type = ResType} = Res} ->
			ErlNodeRel = #resource_rel{id = ResId, href = ResHref,
					name = ResName, ref_type = ResType, rel_type = "composedOf"},
			{ok, NewResAcc} = write_res_rel_in(["Erlang"], ErlNodeRel, ResAcc),
			Fresrel = fun(#resource{id = Cid, href = Chref, name = Cname,
							class_type = Ctype}) when Cname == "net_kernel";
							Cname == "httpd"; Cname == "Resource Catalog";
							Cname == "Resource Inventory" ->
						{true, #resource_rel{id = Cid, href = Chref,
								name = Cname, ref_type = Ctype,
								rel_type = "composedOf"}};
					(_) ->
						false
			end,
			case lists:keyfind("Erlang", #resource.name, NewResAcc) of
				#resource{id = Pid, href = Phref,
						name = Pname, class_type = Ptype} ->
					ParentRel = #resource_rel{id = Pid, href = Phref,
							name = Pname, ref_type = Ptype, rel_type = "providedBy"},
					ChildRels = lists:filtermap(Fresrel, ResAcc),
					NewRes = Res#resource{related = [ParentRel | ChildRels]},
					ok = write_resource(NewRes),
					new_erl_node(T, Node,
							[ErlNodeRel | ErlNodeRelAcc], [NewRes | NewResAcc]);
				false ->
					error_logger:error_report(["Failed to find Erlang resource."]),
					{error, false}
			end;
		{error, Reason} ->
			error_logger:error_report(["Failed to add ODA Component resources.",
				{error, Reason}]),
			{error, Reason}
	end;
new_erl_node([F | T], Node, ErlNodeRelAcc, ResAcc) ->
	case im:add_resource(im_specification:F(Node)) of
		{ok, #resource{} = Res} ->
			new_erl_node(T, Node, ErlNodeRelAcc, [Res | ResAcc]);
		{error, Reason} ->
			error_logger:error_report(["Failed to add ODA Component resources.",
				{error, Reason}]),
			{error, Reason}
	end;
new_erl_node([], _Node, [ErlNodeRel], _ResAcc) ->
	case im:get_resource_name("SigScale RIM") of
		{ok, #resource{related = Rels} = Res} ->
			error_logger:info_msg("Added SigScale RIM ODA Component resource.~n"),
			write_resource(Res#resource{related = [ErlNodeRel | Rels]});
		{error, Reason} ->
			error_logger:error_report(["Failed to get SigScale RIM resource.",
				{error, Reason}]),
			{error, Reason}
	end.

