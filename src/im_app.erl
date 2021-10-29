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
%%		If any errors are encountered calls
%%		{@link //erts/erlang:halt/1. erlang:halt(1)} which
%%		causes an exit status of failure in the operating system shell.
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
					erlang:halt(1)
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
			erlang:halt(1)
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
					erlang:halt(1)
			end;
		{Results, BadNodes} ->
			error_logger:error_report(["Failed to start mnesia"
					" on all nodes", {nodes, Nodes}, {results, Results},
					{badnodes, BadNodes}]),
			erlang:halt(1)
	end.
%% @hidden
install2(Nodes) ->
	case mnesia:wait_for_tables([schema], ?WAITFORSCHEMA) of
		ok ->
			install3(Nodes, []);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1);
		{timeout, Tables} ->
			error_logger:error_report(["Timeout waiting for tables",
					{tables, Tables}]),
			erlang:halt(1)
	end.
%% @hidden
install3(Nodes, Acc) ->
	case mnesia:create_table(catalog, [{disc_copies, Nodes},
			{attributes, record_info(fields, catalog)}, {index, [name]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new resource catalog table.~n"),
			install4(Nodes, [catalog | Acc]);
		{aborted, {not_active, _, Node}} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			erlang:halt(1);
		{aborted, {already_exists, catalog}} ->
			error_logger:info_msg("Found existing resource catalog table.~n"),
			install4(Nodes, [catalog | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
install4(Nodes, Acc) ->
	case mnesia:create_table(category, [{disc_copies, Nodes},
			{attributes, record_info(fields, category)}, {index, [name]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new resource category table.~n"),
			install5(Nodes, [category | Acc]);
		{aborted, {not_active, _, Node}} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			erlang:halt(1);
		{aborted, {already_exists, category}} ->
			error_logger:info_msg("Found existing resource category table.~n"),
			install5(Nodes, [category | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
install5(Nodes, Acc) ->
	case mnesia:create_table(candidate, [{disc_copies, Nodes},
			{attributes, record_info(fields, candidate)}, {index, [name]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new resource candidate table.~n"),
			install6(Nodes, [candidate | Acc]);
		{aborted, {not_active, _, Node} = _Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			erlang:halt(1);
		{aborted, {already_exists, candidate}} ->
			error_logger:info_msg("Found existing resource candidate table.~n"),
			install6(Nodes, [candidate | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
install6(Nodes, Acc) ->
	case mnesia:create_table(specification, [{disc_copies, Nodes},
			{attributes, record_info(fields, specification)}, {index, [name]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new resource specification table.~n"),
			install7(Nodes, [specification | Acc]);
		{aborted, {not_active, _, Node}} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			erlang:halt(1);
		{aborted, {already_exists, specification}} ->
			error_logger:info_msg("Found existing resource specification table.~n"),
			install7(Nodes, [specification | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
install7(Nodes, Acc) ->
	case mnesia:create_table(resource, [{disc_copies, Nodes},
			{attributes, record_info(fields, resource)}, {index, [name]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new resource inventory table.~n"),
			install8(Nodes, [resource | Acc]);
		{aborted, {not_active, _, Node}} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			erlang:halt(1);
		{aborted, {already_exists, resource}} ->
			error_logger:info_msg("Found existing resource inventory table.~n"),
			install8(Nodes, [resource | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
install8(Nodes, Acc) ->
	CategoryFuns = [ngc_category, nr_category, epc_category, lte_category,
			core_category, umts_category, gsm_category, ims_category,
			oda_category],
	install8(CategoryFuns, Nodes, Acc).
%% @hidden
install8([F | T], Nodes, Acc) ->
	case im:add_category(im_specification:F()) of
		{ok, #category{}} ->
			install8(T, Nodes, Acc);
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM categories.",
				{error, Reason}]),
			erlang:halt(1)
	end;
install8([], Nodes, Acc) ->
	error_logger:info_msg("Added 3GPP NRM resource categories.~n"),
	install9(Nodes, Acc).
%% @hidden
install9(Nodes, Acc) ->
	CatalogFuns = [ng_catalog, lte_catalog, umts_catalog, gsm_catalog,
			oda_catalog],
	install9(CatalogFuns, Nodes, Acc).
%% @hidden
install9([F | T], Nodes, Acc) ->
	case im:add_catalog(im_specification:F()) of
		{ok, #catalog{}} ->
			install9(T, Nodes, Acc);
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM catalogs.",
				{error, Reason}]),
			erlang:halt(1)
	end;
install9([], Nodes, Acc) ->
	error_logger:info_msg("Added 3GPP NRM resource catalogs.~n"),
	install10(Nodes, Acc).
%% @hidden
install10(Nodes, Acc) ->
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
		im_erlang_spec, im_catalog_api_spec, im_inventory_api_spec,
		im_net_kernel_spec, im_httpd_spec, im_catalog_spec, im_inventory_spec,
		im_kernel_spec, im_inets_spec, im_application_spec, im_erlang_node_spec],
	install10(SpecFuns, [], Nodes, Acc).
%% @hidden
install10([generic_subnetwork | T], SpecAcc, Nodes, Acc) ->
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
				{aborted, _Reason} ->
					erlang:halt(1);
				{atomic, ok} ->
					install10(T, SpecAcc, Nodes, Acc)
			end;
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
				{error, Reason}]),
			erlang:halt(1)
	end;
install10([F | T], SpecAcc, Nodes, Acc)
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
				{aborted, _Reason} ->
					erlang:halt(1);
				{atomic, ok} ->
					install10(T, SpecAcc, Nodes, Acc)
			end;
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
				{error, Reason}]),
			erlang:halt(1)
	end;
install10([im_iu | T], SpecAcc, Nodes, Acc) ->
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
				{aborted, _Reason} ->
					erlang:halt(1);
				{atomic, ok} ->
					install10(T, SpecAcc, Nodes, Acc)
			end;
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
				{error, Reason}]),
			erlang:halt(1)
	end;
install10([umts_rnc | T], SpecAcc, Nodes, Acc) ->
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
				{aborted, _Reason} ->
					erlang:halt(1);
				{atomic, ok} ->
					ok = add_candidate(CategoryName, RncSpec),
					install10(T, SpecAcc, Nodes, Acc)
			end;
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
				{error, Reason}]),
			erlang:halt(1)
	end;
install10([im_kernel_spec = F | T], SpecAcc, Nodes, Acc) ->
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
			install10(T, NewSpecAcc2, Nodes, Acc);
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
				{error, Reason}]),
			erlang:halt(1)
	end;
install10([im_inets_spec = F | T], SpecAcc, Nodes, Acc) ->
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
			install10(T, NewSpecAcc2, Nodes, Acc);
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
				{error, Reason}]),
			erlang:halt(1)
	end;
install10([im_application_spec = F | T], SpecAcc, Nodes, Acc) ->
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
			install10(T, NewSpecAcc2, Nodes, Acc);
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
				{error, Reason}]),
			erlang:halt(1)
	end;
install10([im_erlang_node_spec = F | T], SpecAcc, Nodes, Acc) ->
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
					install10(T, [NewSpec | NewSpecAcc], Nodes, Acc);
				false ->
					erlang:halt(1)
			end;
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
				{error, Reason}]),
			erlang:halt(1)
	end;
install10([F | T], SpecAcc, Nodes, Acc) ->
	case im:add_specification(im_specification:F()) of
		{ok, #specification{} = Spec} ->
			case category_name(atom_to_list(F)) of
				"ODA" ->
					ok = add_candidate("ODA", Spec),
					install10(T, [Spec | SpecAcc], Nodes, Acc);
				CategoryName ->
					ok = add_candidate(CategoryName, Spec),
					install10(T, SpecAcc, Nodes, Acc)
			end;
		{error, Reason} ->
			error_logger:error_report(["Failed to add 3GPP NRM specifications.",
				{error, Reason}]),
			erlang:halt(1)
	end;
install10([], _SpecAcc, Nodes, Acc) ->
	error_logger:info_msg("Added 3GPP NRM resource specifications.~n"),
	install11(Nodes, Acc).
%% @hidden
install11(Nodes, Acc) ->
	case application:load(inets) of
		ok ->
			error_logger:info_msg("Loaded inets.~n"),
			install12(Nodes, Acc);
		{error, {already_loaded, inets}} ->
			install12(Nodes, Acc)
	end.
%% @hidden
install12(Nodes, Acc) ->
	case application:load(sigscale_im) of
		ok ->
			error_logger:info_msg("Loaded sigscale_im.~n"),
			install13(Nodes, Acc);
		{error, {already_loaded, sigscale_im}} ->
			install13(Nodes, Acc)
	end.
%% @hidden
install13(Nodes, Acc) ->
	ResourceFuns = [im_catalog_api_res, im_catalog_res, im_inventory_api_res,
			im_inventory_res, im_application_res, im_inets_res, im_erlang_res,
			im_httpd_res],
	install13(ResourceFuns, [], Nodes, Acc).
%% @hidden
install13([im_application_res = F | T], ResAcc, Nodes, Acc) ->
	case im:add_resource(im_specification:F()) of
		{ok, #resource{id = ResId, href = ResHref, name = ResName,
				class_type = ResType} = Res} ->
			ManagerRel = #resource_rel{id = ResId, href = ResHref, name = ResName,
					ref_type = ResType, rel_type = "providedBy"},
			Fresrel = fun(#resource{id = Cid, href = Chref, name = Cname,
							class_type = Ctype} = ChildRes) when
							Cname == "Resource Catalog";
							Cname == "Resource Inventory" ->
						ok = write_resource(ChildRes#resource{related
								= [ManagerRel]}),
						{true, #resource_rel{id = Cid, href = Chref, name = Cname,
								ref_type = Ctype, rel_type = "composedOf"}};
					(_) ->
						false
			end,
			NewRes = Res#resource{related = lists:filtermap(Fresrel, ResAcc)},
			ok = write_resource(NewRes),
			install13(T, [NewRes | ResAcc], Nodes, Acc);
		{error, Reason} ->
			error_logger:error_report(["Failed to add ODA Component resources.",
				{error, Reason}]),
			erlang:halt(1)
	end;
install13([im_erlang_res = F | T], ResAcc, Nodes, Acc) ->
	case im:add_resource(im_specification:F()) of
		{ok, #resource{} = Res} ->
			Fresrel = fun(#resource{id = Cid, href = Chref, name = Cname,
							class_type = Ctype}) when Cname == "SigScale RIM";
							Cname == "inets" ->
						{true, #resource_rel{id = Cid, href = Chref, name = Cname,
								ref_type = Ctype, rel_type = "composedOf"}};
					(_) ->
						false
			end,
			NewRes = Res#resource{related = lists:filtermap(Fresrel, ResAcc)},
			ok = write_resource(NewRes),
			install13(T, [NewRes | ResAcc], Nodes, Acc);
		{error, Reason} ->
			error_logger:error_report(["Failed to add ODA Component resources.",
				{error, Reason}]),
			erlang:halt(1)
	end;
install13([im_httpd_res = F | T], ResAcc, Nodes, Acc) ->
	case im:add_resource(im_specification:F()) of
		{ok, #resource{id = ResId, href = ResHref, name = ResName,
				class_type = ResType} = HttpdRes} ->
			HttpdRel = #resource_rel{id = ResId, href = ResHref, name = ResName,
					ref_type = ResType, rel_type = "composedOf"},
			Fcon = fun(#resource{id = Cid, href = Chref,
							name = Cname, class_type = Ctype})
							when Cname == "TMF634"; Cname == "TMF639" ->
						{true, #resource_ref{id = Cid, href = Chref, name = Cname,
								class_type = "ConnectionPointRef", ref_type = Ctype}};
					(_) ->
						false
			end,
			case lists:keytake("inets", #resource.name, ResAcc) of
				{value, #resource{id = InetsId, href = InetsHref, name = InetsName,
						class_type = InetsType, related = Related}
						= InetsRes, Rest} ->
					NewInetRes = InetsRes#resource{related = [HttpdRel | Related]},
					ok = write_resource(NewInetRes),
					InetsRel = #resource_rel{id = InetsId, href = InetsHref,
							name = InetsName, ref_type = InetsType,
							rel_type = "providedBy"},
					NewHttpdRes = HttpdRes#resource{related = [InetsRel],
							connection_point = lists:filtermap(Fcon, ResAcc)},
					ok = write_resource(NewHttpdRes),
					install13(T, [NewHttpdRes, NewInetRes | Rest], Nodes, Acc);
				false ->
					error_logger:error_report(["Failed to find ODA"
							" inets resource.", {error, false}]),
					erlang:halt(1)
			end;
		{error, Reason} ->
			error_logger:error_report(["Failed to add ODA Component resources.",
				{error, Reason}]),
			erlang:halt(1)
	end;
install13([F | T], ResAcc, Nodes, Acc) ->
	case im:add_resource(im_specification:F()) of
		{ok, #resource{} = Res} ->
			install13(T, [Res | ResAcc], Nodes, Acc);
		{error, Reason} ->
			error_logger:error_report(["Failed to add ODA Component resources.",
				{error, Reason}]),
			erlang:halt(1)
	end;
install13([], _ResAcc, Nodes, Acc) ->
	error_logger:info_msg("Added ODA Component resources.~n"),
	install14(Nodes, Acc).
%% @hidden
install14(Nodes, Acc) ->
	case application:get_env(inets, services) of
		{ok, InetsServices} ->
			install15(Nodes, Acc, InetsServices);
		undefined ->
			error_logger:info_msg("Inets services not defined. "
					"User table not created~n"),
			install19(Nodes, Acc)
	end.
%% @hidden
install15(Nodes, Acc, InetsServices) ->
	case lists:keyfind(httpd, 1, InetsServices) of
		{httpd, HttpdInfo} ->
			F = fun({directory, _}) ->
						true;
					(_) ->
						false
			end,
			install16(Nodes, Acc, lists:filter(F, HttpdInfo));
		false ->
			error_logger:info_msg("Httpd service not defined. "
					"User table not created~n"),
			install19(Nodes, Acc)
	end.
%% @hidden
install16(Nodes, Acc, [{directory, {_Dir, []}} | T]) ->
	install16(Nodes, Acc, T);
install16(Nodes, Acc, [{directory, {_, DirectoryInfo}} | _]) ->
	case lists:keyfind(auth_type, 1, DirectoryInfo) of
		{auth_type, mnesia} ->
			install17(Nodes, Acc);
		_ ->
			error_logger:info_msg("Auth type not mnesia. "
					"User table not created~n"),
			install19(Nodes, Acc)
	end;
install16(Nodes, Acc, []) ->
	error_logger:info_msg("Auth directory not defined. "
			"User table not created~n"),
	install19(Nodes, Acc).
%% @hidden
install17(Nodes, Acc) ->
	case mnesia:create_table(httpd_user, [{type, bag}, {disc_copies, Nodes},
			{attributes, record_info(fields, httpd_user)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new httpd_user table.~n"),
			install18(Nodes, [httpd_user | Acc]);
		{aborted, {not_active, _, Node}} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			erlang:halt(1);
		{aborted, {already_exists, httpd_user}} ->
			error_logger:info_msg("Found existing httpd_user table.~n"),
			install18(Nodes, [httpd_user | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
install18(Nodes, Acc) ->
	case mnesia:create_table(httpd_group, [{type, bag}, {disc_copies, Nodes},
			{attributes, record_info(fields, httpd_group)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new httpd_group table.~n"),
			install19(Nodes, [httpd_group | Acc]);
		{aborted, {not_active, _, Node}} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			erlang:halt(1);
		{aborted, {already_exists, httpd_group}} ->
			error_logger:info_msg("Found existing httpd_group table.~n"),
			install19(Nodes, [httpd_group | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
install19(Nodes, Acc) ->
	case mnesia:create_table(pee_rule, [{disc_copies, Nodes},
			{attributes, record_info(fields, pee_rule)}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new pee rule table.~n"),
			install20(Nodes, [pee_rule | Acc]);
		{aborted, {not_active, _, Node}} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			erlang:halt(1);
		{aborted, {already_exists, pee_rule}} ->
			error_logger:info_msg("Found existing pee rule table.~n"),
			install20(Nodes, [pee_rule | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
install20(_Nodes, Tables) ->
	case mnesia:wait_for_tables(Tables, ?WAITFORTABLES) of
		ok ->
			install21(Tables, lists:member(httpd_user, Tables));
		{timeout, Tables} ->
			error_logger:error_report(["Timeout waiting for tables",
					{tables, Tables}]),
			erlang:halt(1);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
					{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
install21(Tables, true) ->
	case inets:start() of
		ok ->
			error_logger:info_msg("Started inets.~n"),
			install22(Tables);
		{error, {already_started, inets}} ->
			install22(Tables);
		{error, _Reason} ->
			error_logger:error_msg("Failed to start inets~n"),
			erlang:halt(1)
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
				{error, _Reason} ->
					error_logger:error_report(["Failed to creat default user",
							{username, "admin"}, {password, "admin"},
							{locale, "en"}]),
					erlang:halt(1)
			end;
		{ok, Users} ->
			error_logger:info_report(["Found existing http users",
					{users, Users}]),
			{ok, Tables};
		{error, Reason} ->
			error_logger:error_report(["Failed to list http users",
				{error, Reason}]),
			erlang:halt(1)
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

-spec join(Nodes) -> Result
	when
		Nodes :: [Node],
		Node :: atom(),
		Result :: {ok, Tables},
		Tables :: [atom()].
%% @doc Join an existing cluster.
%%
%% 	Tables will be copied from a randomly
%% 	selected `Node' in the `Nodes' list.
%%
%%		If any errors are encountered calls
%%		{@link //erts/erlang:halt/1. erlang:halt(1)} which
%%		causes an exit status of failure in the operating system shell.
%%
join(Nodes) when is_list(Nodes), is_atom(hd(Nodes))  ->
	N = rand:uniform(length(Nodes)),
	Node = lists:nth(N, Nodes),
	case mnesia:system_info(is_running) of
		no ->
			join1(Node);
		Running ->
			error_logger:error_report(["mnesia running", {is_running, Running}]),
			erlang:halt(1)
	end.
%% @hidden
join1(Node) ->
	case net_kernel:connect_node(Node) of
		true ->
			join2(Node);
		Connect ->
			error_logger:error_report(["Failed to connect node", {result, Connect}]),
			erlang:halt(1)
	end.
%% @hidden
join2(Node) ->
	case rpc:call(Node, mnesia, add_table_copy, [schema, node(), ram_copies]) of
		{atomic, ok} ->
			join3(Node);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
join3(Node) ->
	case application:start(mnesia) of
		ok ->
			join4(Node);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
join4(Node) ->
	case mnesia:change_config(extra_db_nodes, [Node]) of
		{ok, _Nodes} ->
			join5(Node);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
join5(Node) ->
	case mnesia:change_table_copy_type(schema, node(), disc_copies) of
		{atomic, ok} ->
			error_logger:info_msg("Copied schema table from ~s.~n", [Node]),
			join6(Node, [schema]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
join6(Node, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [catalog, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied catalog table from ~s.~n", [Node]),
			join7(Node, [resource | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
join7(Node, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [category, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied category table from ~s.~n", [Node]),
			join8(Node, [resource | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
join8(Node, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [candidate, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied candidate table from ~s.~n", [Node]),
			join9(Node, [resource | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
join9(Node, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [specification, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied specification table from ~s.~n", [Node]),
			join10(Node, [resource | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
join10(Node, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [resource, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied resource table from ~s.~n", [Node]),
			join11(Node, [resource | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
join11(Node, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [httpd_user, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied httpd_user table from ~s.~n", [Node]),
			join12(Node, [httpd_user | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
join12(Node, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [httpd_group, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied httpd_group table from ~s.~n", [Node]),
			join13(Node, [httpd_group | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			erlang:halt(1)
	end.
%% @hidden
join13(_Node, Tables) ->
	case mnesia:wait_for_tables(lists:reverse(Tables), ?WAITFORTABLES) of
		ok ->
			{ok, Tables};
		{timeout, BadTables} ->
			error_logger:error_report(["Timeout waiting for tables",
					{tables, BadTables}]),
			erlang:halt(1);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
					{error, Reason}]),
			erlang:halt(1)
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
					erlang:halt(1)
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
						{aborted, _Reason} ->
							erlang:halt(1);
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
			erlang:halt(1)
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
		{aborted, _Reason} ->
			erlang:halt(1);
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
			erlang:halt(1)
	end;
write_rel_in([], _Rel, SpecAcc) ->
	{ok, SpecAcc}.
