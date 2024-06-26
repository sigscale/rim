%%% im_specification.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018 - 2024 SigScale Global Inc.
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
%%% @doc This library module is used to create sample specifications for
%%% 	3GPP resource functions in the
%%% 	{@link //im. im} application.
%%%
-module(im_specification).
-copyright('Copyright (c) 2018 - 2024 SigScale Global Inc.').

-export([gsm_bss/0, gsm_bts/0, gsm_cell/0]).
-export([gsm_abis_link/0]).
-export([umts_rnc/0, umts_nodeb/0, umts_cell_fdd/0,
		umts_cell_tdd_lcr/0, umts_cell_tdd_hcr/0, umts_iub_link/0]).
-export([lte_enb/0, lte_cell_fdd/0, lte_cell_tdd/0]).
-export([nr_gnb_du/0, nr_gnb_cu_cp/0, nr_gnb_cu_up/0,
		nr_cell_cu/0, nr_cell_du/0, nr_sector_carrier/0,
		nr_ep_x2c/0, nr_ep_x2u/0, nr_ep_ngc/0, nr_ep_ngu/0, nr_ep_xnc/0,
		nr_ep_xnu/0, nr_ep_f1c/0, nr_ep_f1u/0, nr_ep_e1/0, nr_ep_s1u/0]).
-export([network_slice/0, network_slice_subnet/0, ngc_amf/0, ngc_smf/0, ngc_upf/0,
		ngc_n3iwf/0, ngc_pcf/0, ngc_ausf/0, ngc_udm/0, ngc_udr/0, ngc_udsf/0,
		ngc_nrf/0, ngc_nssf/0, ngc_smsf/0, ngc_lmf/0, ngc_ngeir/0, ngc_sepp/0,
		ngc_nwdaf/0, ngc_ep_n2/0, ngc_ep_n3/0, ngc_ep_n4/0, ngc_ep_n5/0,
		ngc_ep_n6/0, ngc_ep_n7/0, ngc_ep_n8/0, ngc_ep_n9/0,
		ngc_ep_n10/0, ngc_ep_n11/0, ngc_ep_n12/0, ngc_ep_n13/0, ngc_ep_n14/0,
		ngc_ep_n15/0, ngc_ep_n16/0, ngc_ep_n17/0,
		ngc_ep_n20/0, ngc_ep_n21/0, ngc_ep_n22/0, ngc_ep_n26/0, ngc_ep_n27/0,
		ngc_ep_n31/0, ngc_ep_n32/0,
		ngc_ep_nls/0, ngc_ep_nlg/0, ngc_ep_sbi_x/0, ngc_ep_sbi_ipx/0,
		ngc_ep_s5c/0, ngc_ep_s5u/0, ngc_ep_rx/0, ngc_ep_map_smsc/0]).
-export([epc_sgw/0, epc_pgw/0, epc_mme/0, epc_pcrf/0, epc_epdg/0,
		epc_link_mme_mme/0, epc_link_hss_mme/0, epc_link_mme_sgsn/0,
		epc_link_mme_servinggw/0, epc_link_enb_mme/0, epc_ep_rp_eps/0]).
-export([core_msc/0, core_mgw/0, core_sgsn/0, core_ggsn/0, core_auc/0,
		core_hlr/0, core_eir/0, core_mnpsrf/0, core_cgf/0, core_sgw/0, core_cbc/0,
		core_iucs/0, core_iups/0, core_iubc/0, core_alink/0, core_gb_link/0]).
-export([ims_as/0, ims_hss/0, ims_pcscf/0, ims_scscf/0, ims_icscf/0]).
-export([pee_me/0]).
-export([epcn3ai_proxy/0, epcn3ai_server/0]).
-export([im_iu/0, im_tmaiu/0, im_aiu/0, im_iu_ne/0, im_iu_hw/0, im_iu_sw/0,
		im_iu_lic/0]).
-export([generic_me/0, generic_subnetwork/0]).
-export([huawei_usn/0, huawei_ugw/0, huawei_cgpomu/0, huawei_igwb/0,
		huawei_uscdb/0, huawei_spsv3/0, huawei_mscsiosp/0, huawei_mscso/0]).
-export([mec_mehf/0, mec_mep/0, mec_mea/0, mec_meps/0, mec_meas/0, mec_rnis/0,
		mec_ls/0, mec_tr/0, mec_dnsr/0]).
-export([im_catalog_api_spec/0, im_catalog_spec/0, im_inventory_api_spec/0,
		im_inventory_spec/0, im_application_spec/0, im_erlang_spec/0,
		im_inets_spec/0, im_httpd_spec/0, im_erlang_node_spec/0,
		im_kernel_spec/0, im_net_kernel_spec/0, im_rpc_spec/0,
		sigscale_rim_spec/0]).

-export([ngc_category/0, nr_category/0, epc_category/0, lte_category/0,
		core_category/0, umts_category/0, gsm_category/0, ims_category/0,
		oda_category/0]).
-export([ng_catalog/0, lte_catalog/0, umts_catalog/0, gsm_catalog/0,
		oda_catalog/0]).

-export([im_catalog_api_res/1, im_catalog_res/1, im_inventory_api_res/1,
		im_inventory_res/1, im_application_res/1, im_inets_res/1,
		im_erlang_res/1, im_httpd_res/1, im_erlang_node_res/1,
		im_kernel_res/1, im_net_kernel_res/1, im_rpc_res/1, sigscale_rim_res/0]).

-include("im.hrl").

-define(PathCatalogSchema, "/resourceCatalogManagement/v4/schema").
-define(PathInventorySchema, "/resourceInventoryManagement/v4/schema").
-define(PathCatalogSpec, "/resourceCatalogManagement/v4/resourceSpecification/").
-define(PathPartySpec, "/partyManagement/v4/organization/").

%%----------------------------------------------------------------------
%% the im_specification public api
%%----------------------------------------------------------------------

-spec gsm_cell() -> specification().
%% @doc GSM radio cell resource specification.
gsm_cell() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
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
			value_schema = ?PathCatalogSchema ++ "/geranNrm#/definitions/HoppingSequenceList"},
	PlmnPermitted = #specification_char{name = "plmnPermitted",
			description = "Network Colour Code (NCC) Permitted (NCC_PERMITTED) (3GPP 45.008)",
			value_type = "integer",
			char_value = [#spec_char_value{from = 0, to = 255}]},
	Chars = [Id, UserLabel, VnfParametersList, CellIdentity, CellAllocation, Ncc,
			Bcc, Lac, Mcc, Mnc, Rac, Racc, Tsc, RxrLevAccessMinM, MsTxPwrMaxCCH,
			RfHoppingEnabled, HoppingSequenceList, PlmnPermitted],
	#specification{name = "GsmCell",
			description = "GSM Radio Cell",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "GsmCell",
					schema = ?PathCatalogSchema ++ "GsmCell"},
			characteristic = Chars}.

-spec gsm_bts() -> specification().
%% @doc GSM Base Transceive Station (BTS) resource specification.
gsm_bts() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	Latitude = #specification_char{name = "latitude",
			description = "Latitude of the site manager location based on (WGS 84) global reference frame",
			value_type = "latitude",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/latitude"},
	Longitude = #specification_char{name = "longitude",
			description = "Longitude of the site manager location based on (WGS 84) global reference frame",
			value_type = "longitude",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/longitude"},
	OperationalState = #specification_char{name = "operationalState",
			description = "Indicates the operational state of the object instance",
			value_type = "operationalStateType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRPNrm#/definitions/operationalStateType"},
	Chars = [Id, UserLabel, VnfParametersList, Latitude, Longitude,
			OperationalState],
	SRelName = "GsmCell",
	case im:get_specification_name(SRelName) of
		{ok, #specification{id = Sid, href = Shref,
				name = SRelName, class_type = Stype}} ->
			GsmCellRel = #specification_rel{id = Sid, href = Shref,
					name = SRelName, ref_type = Stype, rel_type = "composedOf"},
			#specification{name = "BtsSiteMgr",
					description = "GSM Base Transceiver Station (BTS)",
					class_type = "ResourceFunctionSpecification",
					status = active,
					version = "1.0",
					category = "RAN",
					target_schema = #target_schema_ref{class_type = "BtsSiteManager",
							schema = ?PathCatalogSchema ++ "BtsSiteManager"},
					characteristic = Chars,
					related = [GsmCellRel]};
		{error, Reason} ->
			error_logger:warning_report(["Error reading resource specification",
					{specification, SRelName}, {error, Reason}]),
			{error, Reason}
	end.

-spec gsm_bss() -> specification().
%% @doc GSM Base Station Subsystem (BSS) resource specification.
gsm_bss() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	Chars = [Id, UserLabel, VnfParametersList],
	SRelName = "BtsSiteMgr",
	case im:get_specification_name(SRelName) of
		{ok, #specification{id = Sid, href = Shref,
				name = SRelName, class_type = Stype}} ->
			BtsSiteMgrRel = #specification_rel{id = Sid, href = Shref,
					name = SRelName, ref_type = Stype, rel_type = "composedOf"},
			#specification{name = "BssFunction",
					description = "GSM Base Station Subsystem (BSS)",
					class_type = "ResourceFunctionSpecification",
					status = active,
					version = "1.0",
					category = "RAN",
					target_schema = #target_schema_ref{class_type = "BssFunction",
							schema = ?PathCatalogSchema ++ "BssFunction"},
					characteristic = Chars,
					related = [BtsSiteMgrRel]};
		{error, Reason} ->
			error_logger:warning_report(["Error reading resource specification",
					{specification, SRelName}, {error, Reason}]),
			{error, Reason}
	end.

-spec gsm_abis_link() -> specification().
%% @doc Generic Managed Element resource specification.
gsm_abis_link() ->
	AssociationName = #specification_char{name = "associationName",
			description = "",
			value_type = "string"},
	LogicalBCSUAddress = #specification_char{name = "logicalBCSUAddress",
			description = "",
			value_type = "string"},
	Name = #specification_char{name = "name",
			description = "",
			value_type = "string"},
	Sapi = #specification_char{name = "sapi",
			description = "",
			value_type = "string"},
	StreamId = #specification_char{name = "streamId",
			description = "",
			value_type = "string"},
	Tei = #specification_char{name = "tei",
			description = "",
			value_type = "string"},
	Chars = [AssociationName, LogicalBCSUAddress, Name, Sapi, StreamId, Tei],
	#specification{name = "AbisLink",
			description = "GSM Abis Link",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "AbisLink",
					schema = ?PathCatalogSchema ++ "AbisLink"},
			characteristic = Chars}.

-spec umts_nodeb() -> specification().
%% @doc UMTS NodeB resource specification.
umts_nodeb() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PeeParametersList = #specification_char{name = "peeParametersList",
			description = "Parameter list for the control and monitoring of power, energy and environment",
			value_type = "peeParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/peeParametersListType"},
	NodeBFunctionIubLink = #specification_char{name = "nodeBFunctionIubLink",
			description = "DN of a logical link to an RNC as seen by a NodeB",
			value_type = "dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dn"},
	Chars = [Id, UserLabel, VnfParametersList, PeeParametersList,
			NodeBFunctionIubLink],
	#specification{name = "NodeBFunction",
			description = "UMTS NodeB",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "NodeBFunction",
					schema = ?PathCatalogSchema ++ "NodeBFunction"},
			characteristic = Chars}.

-spec umts_cell_fdd() -> specification().
%% @doc UMTS Frequency Division Duplex (FDD) radio cell resource specification.
umts_cell_fdd() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	CId = #specification_char{name = "cId",
			description = "Identifier of a cell in one RNC ('C-ID' in 3GPP 25.433)",
			value_type = "cId",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/cId"},
	LocalCellId = #specification_char{name = "localCellId",
			description = "Uniquely identify a cell in a Node B ('Local Cell ID' in 3GPP 25.433)",
			value_type = "localCellId",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/localCellId"},
	MaximumTransmissionPower = #specification_char{name = "maximumTransmissionPower",
			description = "Maximum power for all downlink channels ('Maximum Transmission Power' in 3GPP 25.433)",
			value_type = "maximumTransmissionPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/maximumTransmissionPower"},
	CellMode = #specification_char{name = "cellMode",
			description = "Identifies the cell mode",
			value_type = "cellMode",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/cellMode"},
	PichPower = #specification_char{name = "pichPower",
			description = "Power of the PICH channel in the cell ('PICH Power' in 3GPP 25.433)",
			value_type = "pichPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/pichPower"},
	PchPower = #specification_char{name = "pchPower",
			description = "PCH transport channel in the cell ('PCH Power' in 3GPP 25.433)",
			value_type = "pchPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/pchPower"},
	FachPower = #specification_char{name = "fachPower",
			description = "Maximum power of FACH transport channel ('Max FACH Power' in 3GPP 25.433)",
			value_type = "fachPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/fachPower"},
	Lac = #specification_char{name = "lac",
			description = "Location Area Code (LAC) (3GPP 23.003)",
			value_type = "lac",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/lac"},
	Rac = #specification_char{name = "rac",
			description = "Routing Area Code (RAC) (3GPP 23.003)",
			value_type = "rac",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/rac"},
	Sac = #specification_char{name = "sac",
			description = "Service Area Code (SAC) (3GPP 23.003)",
			value_type = "sac",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/sac"},
	UraList = #specification_char{name = "uraList",
			description = "UTRAN Registration Area identities ('URA identity' in 3GPP 25.331)",
			value_type = "uraList",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/uraList"},
	UtranCellIubLink = #specification_char{name = "utranCellIubLink",
			description = "Distinguished Name (DN) of IubLink",
			value_type = "dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dn"},
	RelatedAntennaList = #specification_char{name = "relatedAntennaList",
			description = "Distinguished Names (DN) of AntennaFunction(s) (3GPP 28.662)",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	RelatedTmaList = #specification_char{name = "relatedTmaList",
			description = "Distinguished Names (DN) of ('TmaFunction' in 3GPP 28.662)",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	OperationalState = #specification_char{name = "operationalState",
			description = "Operational state describes whether physically installed and working or not (ITU-T X.731)",
			value_type = "operationalStateType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRP#/definitions/operationalStateType"},
	HsFlag = #specification_char{name = "hsFlag",
			description = "High-Speed Downlink Packet Access (HSDPA) supported flag",
			value_type = "hsFlag",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/hsFlag"},
	HsEnable = #specification_char{name = "hsEnable",
			description = "High-Speed Downlink Packet Access (HSDPA) enabled flag",
			value_type = "hsEnable",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/hsEnable"},
	NumOfHspdschs = #specification_char{name = "numOfHspdschs",
			description = "In FDD the number of codes, in TDD the number of HS-PDSCHs (3GPP 25.433)",
			value_type = "numOfHspdschs",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/numOfHspdschs"},
	NumOfHsscchs = #specification_char{name = "numOfHsscchs",
			description = "Number of HS-SCCHs for one cell (3GPP 25.433)",
			value_type = "numOfHsscchs",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/numOfHsscchs"},
	FrameOffset = #specification_char{name = "frameOffset",
			description = "Required offset in neighbouring cells monitoring (3GPP 25.423)",
			value_type = "frameOffset",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/frameOffset"},
	CellIndividualOffset = #specification_char{name = "cellIndividualOffset",
			description = "Relevant for hand over (HO) decision (3GPP 25.331)",
			value_type = "cellIndividualOffset",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/cellIndividualOffset"},
	HcsPrio = #specification_char{name = "hcsPrio",
			description = "Hierarchical cell structure (HCS) priority in cell (re)selection (3GPP 25.331)",
			value_type = "hcsPrio",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/hcsPrio"},
	MaximumAllowedUlTxPower = #specification_char{name = "maximumAllowedUlTxPower",
			description = "Maximum allowed uplink transmit power in cell (re)selection (3GPP 25.331)",
			value_type = "maximumAllowedUlTxPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/maximumAllowedUlTxPower"},
	SnaInformation = #specification_char{name = "snaInformation",
			description = "List of Shared Networks Access Control (SNAC) (3GPP 25.423)",
			value_type = "snaInformation",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/snaInformation"},
	QrxlevMin = #specification_char{name = "qrxlevMin",
			description = "Minimum required RX level in cell (re)selection ('QrxlevMin' in 3GPP 25.331)",
			value_type = "qrxlevMin",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/qrxlevMin"},
	DeltaQrxlevmin = #specification_char{name = "deltaQrxlevmin",
			description = "Delta required RX level in cell (re)selection ('DeltaQrxlevmin' in 3GPP 25.331)",
			value_type = "deltaQrxlevmin",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/deltaQrxlevmin"},
	Qhcs = #specification_char{name = "qhcs",
			description = "Quality threshold levels in cell (re)selection ('Qhcs' in 3GPP 25.331)",
			value_type = "qhcs",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/qhcs"},
	PenaltyTime = #specification_char{name = "penaltyTime",
			description = "Penalty time duration in cell (re)selection ('Penalty_time' in 3GPP 25.331)",
			value_type = "penaltyTime",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/penaltyTime"},
	ReferenceTimeDifferenceToCell = #specification_char{name = "referenceTimeDifferenceToCell",
			description = "Reference time difference to cell in neighbouring cells monitoring (3GPP 25.331)",
			value_type = "referenceTimeDifferenceToCell",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/referenceTimeDifferenceToCell"},
	ReadSFNIndicator = #specification_char{name = "readSFNIndicator",
			description = "Read SFN indicator in neighbouring cells monitoring (3GPP 25.331)",
			value_type = "readSFNIndicator",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/readSFNIndicator"},
	NsPlmnIdList = #specification_char{name = "nsPlmnIdList",
			description = "List of unique identities for PLMN",
			value_type = "NsPlmnIdListType",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/NsPlmnIdListType"},
	RestrictionStateIndicator = #specification_char{name = "restrictionStateIndicator",
			description = "Indicates if cell reserved for operator use in Cell Access Control (3GPP 25.423)",
			value_type = "restrictionStateIndicator",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/restrictionStateIndicator"},
	DpcModechangeSupportIndicator = #specification_char{name = "dpcModechangeSupportIndicator",
			description = "Indicates support for DPC mode change in Power Control (3GPP 25.423)",
			value_type = "dpcModeChangeSupport",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/dpcModeChangeSupport"},
	RelatedSectorEquipment = #specification_char{name = "relatedSectorEquipment",
			description = "Distinguished Name (DN) of sector equipment ('relatedSectorEquipment' in 3GPP 28.662)",
			value_type = "dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dn"},
	UarfcnUl = #specification_char{name = "uarfcnUl",
			description = "Uplink (UL) UTRA absolute radio frequency channel number (UARFCN) (3GPP 25.433)",
			value_type = "uarfcnAnyMode",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/uarfcnAnyMode"},
	UarfcnDl = #specification_char{name = "uarfcnDl",
			description = "Downlink (DL) UTRA absolute radio frequency channel number (UARFCN) (3GPP 25.433)",
			value_type = "uarfcnAnyMode",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/uarfcnAnyMode"},
	PrimaryScramblingCode = #specification_char{name = "primaryScramblingCode",
			description = "Primary downlink (DL) scrambling code (3GPP 25.433)",
			value_type = "primaryScramblingCode",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/primaryScramblingCode"},
	PrimaryCpichPower = #specification_char{name = "primaryCpichPower",
			description = "Power of the primary CPICH channel (3GPP 25.433)",
			value_type = "primaryCpichPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/primaryCpichPower"},
	PrimarySchPower = #specification_char{name = "primarySchPower",
			description = "Power of the primary synchronisation channel (3GPP 25.433)",
			value_type = "primarySchPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/primarySchPower"},
	SecondarySchPower = #specification_char{name = "secondarySchPower",
			description = "Power of the secondary synchronisation channel (3GPP 25.433)",
			value_type = "secondarySchPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/secondarySchPower"},
	BchPower = #specification_char{name = "bchPower",
			description = "Power of the broadcast channel (3GPP 25.433)",
			value_type = "bchPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/bchPower"},
	AichPower = #specification_char{name = "aichPower",
			description = "Power of the the AICH channel (3GPP 25.433)",
			value_type = "aichPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/aichPower"},
	QqualMin = #specification_char{name = "qqualMin",
			description = "Minimum required quality level in cell (re)selection (3GPP 25.331)",
			value_type = "qqualMin",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/qqualMin"},
	CellCapabilityContainerFDD = #specification_char{name = "cellCapabilityContainerFDD",
			description = "Bitfield indicating support of particular functionality (3GPP 25.423)",
			value_type = "cellCapabilityContainerFDD",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/cellCapabilityContainerFDD"},
	TxDiversityIndicator = #specification_char{name = "txDiversityIndicator",
			description = "TX Diversity Indicator (3GPP 25.331)",
			value_type = "txDiversityIndicator",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/txDiversityIndicator"},
	TemporaryOffset1 = #specification_char{name = "temporaryOffset1",
			description = "Offset applied to hierarchical cell structure (HCS) in cell (re)selection (3GPP 25.331)",
			value_type = "temporaryOffset1",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/temporaryOffset1"},
	TemporaryOffset2 = #specification_char{name = "temporaryOffset2",
			description = "Offset applied to hierarchical cell structure (HCS) in cell (re)selection (3GPP 25.331)",
			value_type = "temporaryOffset2",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/temporaryOffset2"},
	SttdSupportIndicator = #specification_char{name = "sttdSupportIndicator",
			description = "STTD Support Indicator in power control (3GPP 25.423)",
			value_type = "sttdSupport",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/sttdSupport"},
	ClosedLoopModelSupportIndicator = #specification_char{name = "closedLoopModelSupportIndicator",
			description = "Closed Loop Mode1 Support Indicator in power control (3GPP 25.423)",
			value_type = "closedLoopMode1",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/closedLoopMode1"},
	Chars = [Id, UserLabel, VnfParametersList, CId, LocalCellId,
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
			ClosedLoopModelSupportIndicator],
	#specification{name = "UtranCellFDD",
			description = "UMTS Frequency Division Duplex (FDD) radio cell",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "UtranCellFDD",
					schema = ?PathCatalogSchema ++ "UtranCellFDD"},
			characteristic = Chars}.

-spec umts_cell_tdd_lcr() -> specification().
%% @doc UMTS Time Division Duplex(TDD) low cell rate (LCR) radio cell resource specification.
umts_cell_tdd_lcr() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	CId = #specification_char{name = "cId",
			description = "Identifier of a cell in one RNC ('C-ID' in 3GPP 25.433)",
			value_type = "cId",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/cId"},
	LocalCellId = #specification_char{name = "localCellId",
			description = "Uniquely identify a cell in a Node B ('Local Cell ID' in 3GPP 25.433)",
			value_type = "localCellId",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/localCellId"},
	MaximumTransmissionPower = #specification_char{name = "maximumTransmissionPower",
			description = "Maximum power for all downlink channels ('Maximum Transmission Power' in 3GPP 25.433)",
			value_type = "maximumTransmissionPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/maximumTransmissionPower"},
	CellMode = #specification_char{name = "cellMode",
			description = "Identifies the cell mode",
			value_type = "cellMode",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/cellMode"},
	PichPower = #specification_char{name = "pichPower",
			description = "Power of the PICH channel in the cell ('PICH Power' in 3GPP 25.433)",
			value_type = "pichPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/pichPower"},
	PchPower = #specification_char{name = "pchPower",
			description = "PCH transport channel in the cell ('PCH Power' in 3GPP 25.433)",
			value_type = "pchPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/pchPower"},
	FachPower = #specification_char{name = "fachPower",
			description = "Maximum power of FACH transport channel ('Max FACH Power' in 3GPP 25.433)",
			value_type = "fachPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/fachPower"},
	Lac = #specification_char{name = "lac",
			description = "Location Area Code (LAC) (3GPP 23.003)",
			value_type = "lac",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/lac"},
	Rac = #specification_char{name = "rac",
			description = "Routing Area Code (RAC) (3GPP 23.003)",
			value_type = "rac",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/rac"},
	Sac = #specification_char{name = "sac",
			description = "Service Area Code (SAC) (3GPP 23.003)",
			value_type = "sac",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/sac"},
	UraList = #specification_char{name = "uraList",
			description = "UTRAN Registration Area identities ('URA identity' in 3GPP 25.331)",
			value_type = "uraList",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/uraList"},
	UtranCellIubLink = #specification_char{name = "utranCellIubLink",
			description = "Distinguished Name (DN) of IubLink",
			value_type = "dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dn"},
	RelatedAntennaList = #specification_char{name = "relatedAntennaList",
			description = "Distinguished Names (DN) of AntennaFunction(s) (3GPP 28.662)",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	RelatedTmaList = #specification_char{name = "relatedTmaList",
			description = "Distinguished Names (DN) of ('TmaFunction' in 3GPP 28.662)",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	OperationalState = #specification_char{name = "operationalState",
			description = "Operational state describes whether physically installed and working or not (ITU-T X.731)",
			value_type = "operationalStateType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRP#/definitions/operationalStateType"},
	HsFlag = #specification_char{name = "hsFlag",
			description = "High-Speed Downlink Packet Access (HSDPA) supported flag",
			value_type = "hsFlag",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/hsFlag"},
	HsEnable = #specification_char{name = "hsEnable",
			description = "High-Speed Downlink Packet Access (HSDPA) enabled flag",
			value_type = "hsEnable",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/hsEnable"},
	NumOfHspdschs = #specification_char{name = "numOfHspdschs",
			description = "In FDD the number of codes, in TDD the number of HS-PDSCHs (3GPP 25.433)",
			value_type = "numOfHspdschs",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/numOfHspdschs"},
	NumOfHsscchs = #specification_char{name = "numOfHsscchs",
			description = "Number of HS-SCCHs for one cell (3GPP 25.433)",
			value_type = "numOfHsscchs",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/numOfHsscchs"},
	FrameOffset = #specification_char{name = "frameOffset",
			description = "Required offset in neighbouring cells monitoring (3GPP 25.423)",
			value_type = "frameOffset",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/frameOffset"},
	CellIndividualOffset = #specification_char{name = "cellIndividualOffset",
			description = "Relevant for hand over (HO) decision (3GPP 25.331)",
			value_type = "cellIndividualOffset",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/cellIndividualOffset"},
	HcsPrio = #specification_char{name = "hcsPrio",
			description = "Hierarchical cell structure (HCS) priority in cell (re)selection (3GPP 25.331)",
			value_type = "hcsPrio",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/hcsPrio"},
	MaximumAllowedUlTxPower = #specification_char{name = "maximumAllowedUlTxPower",
			description = "Maximum allowed uplink transmit power in cell (re)selection (3GPP 25.331)",
			value_type = "maximumAllowedUlTxPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/maximumAllowedUlTxPower"},
	SnaInformation = #specification_char{name = "snaInformation",
			description = "List of Shared Networks Access Control (SNAC) (3GPP 25.423)",
			value_type = "snaInformation",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/snaInformation"},
	QrxlevMin = #specification_char{name = "qrxlevMin",
			description = "Minimum required RX level in cell (re)selection ('QrxlevMin' in 3GPP 25.331)",
			value_type = "qrxlevMin",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/qrxlevMin"},
	DeltaQrxlevmin = #specification_char{name = "deltaQrxlevmin",
			description = "Delta required RX level in cell (re)selection ('DeltaQrxlevmin' in 3GPP 25.331)",
			value_type = "deltaQrxlevmin",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/deltaQrxlevmin"},
	Qhcs = #specification_char{name = "qhcs",
			description = "Quality threshold levels in cell (re)selection ('Qhcs' in 3GPP 25.331)",
			value_type = "qhcs",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/qhcs"},
	PenaltyTime = #specification_char{name = "penaltyTime",
			description = "Penalty time duration in cell (re)selection ('Penalty_time' in 3GPP 25.331)",
			value_type = "penaltyTime",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/penaltyTime"},
	ReferenceTimeDifferenceToCell = #specification_char{name = "referenceTimeDifferenceToCell",
			description = "Reference time difference to cell in neighbouring cells monitoring (3GPP 25.331)",
			value_type = "referenceTimeDifferenceToCell",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/referenceTimeDifferenceToCell"},
	ReadSFNIndicator = #specification_char{name = "readSFNIndicator",
			description = "Read SFN indicator in neighbouring cells monitoring (3GPP 25.331)",
			value_type = "readSFNIndicator",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/readSFNIndicator"},
	NsPlmnIdList = #specification_char{name = "nsPlmnIdList",
			description = "List of unique identities for PLMN",
			value_type = "NsPlmnIdListType",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/NsPlmnIdListType"},
	RestrictionStateIndicator = #specification_char{name = "restrictionStateIndicator",
			description = "Indicates if cell reserved for operator use in Cell Access Control (3GPP 25.423)",
			value_type = "restrictionStateIndicator",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/restrictionStateIndicator"},
	DpcModechangeSupportIndicator = #specification_char{name = "dpcModechangeSupportIndicator",
			description = "Indicates support for DPC mode change in Power Control (3GPP 25.423)",
			value_type = "dpcModeChangeSupport",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/dpcModeChangeSupport"},
	RelatedSectorEquipment = #specification_char{name = "relatedSectorEquipment",
			description = "Distinguished Name (DN) of sector equipment ('relatedSectorEquipment' in 3GPP 28.662)",
			value_type = "dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dn"},
	Uarfcn = #specification_char{name = "uarfcn",
			description = "UTRA absolute radio frequency channel number (UARFCN) (3GPP 25.433)",
			value_type = "uarfcnAnyMode",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/uarfcnAnyMode"},
	CellParameterId = #specification_char{name = "cellParameterId",
			description = "Unambiguously identifies the cell ('Cell Parameter ID' in TS 25.433)",
			value_type = "cellParameterId",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/cellParameterId"},
	PrimaryCcpchPower = #specification_char{name = "primaryCcpchPower",
			description = "Power of the primary CCPCH channel ('PCCPCH Power' in 3GPP 25.433)",
			value_type = "primaryCcpchPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/primaryCcpchPower"},
	CellCapabilityContainerTDD = #specification_char{name = "cellCapabilityContainerTDD",
			description = "Bitfield indicating support of particular functionality (3GPP 25.423)",
			value_type = "cellCapabilityContainerTDD",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/cellCapabilityContainerTDD"},
	SctdIndicator = #specification_char{name = "sctdIndicator",
			description = "Indicates whether SCTD is used ('SCDT Indicator' in 3GPP 25.433)",
			value_type = "sctdIndicator",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/sctdIndicator"},
	DpchConstantValue = #specification_char{name = "dpchConstantValue",
			description = "Power margin used by a UE ('DPCH Constant Valuer' in 3GPP 25.433)",
			value_type = "dpchConstantValue",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/dpchConstantValue"},
	UarfcnLCRList = #specification_char{name = "uarfcnLCRList",
			description = "List of UARFCN and time slot LCR, Direction, Status (3GPP 25.433)",
			value_type = "uarfcnLCRList",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/uarfcnLCRList"},
	FpachPower = #specification_char{name = "fpachPower",
			description = "Maximum power of the FPACH channel ('FPACH Power' in 3GPP 25.433)",
			value_type = "fpachPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/fpachPower"},
	DwPchPower = #specification_char{name = "dwPchPower",
			description = "Power used for transmitting the DwPCH ('DwPCH Power' in 3GPP 25.433)",
			value_type = "dwPchPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/dwPchPower"},
	TstdIndicator = #specification_char{name = "tstdIndicator",
			description = "Indicates whether TSTD is used ('TSDT Indicator' in 3GPP 25.433)",
			value_type = "tstdIndicator",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/tstdIndicator"},
	TimeSlotLCRList = #specification_char{name = "timeSlotLCRList",
			description = "Defines the time slot configuration information (3GPP 25.433)",
			value_type = "timeSlotLCRList",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/timeSlotLCRList"},
	Chars = [Id, UserLabel, VnfParametersList, CId, LocalCellId,
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
			TstdIndicator, TimeSlotLCRList],
	#specification{name = "UtranCellTDDLcr",
			description = "UMTS Time Division Duplex (TDD) low chip rate (LCR) radio cell",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "UtranCellTDDLcr",
					schema = ?PathCatalogSchema ++ "UtranCellTDDLcr"},
			characteristic = Chars}.

-spec umts_cell_tdd_hcr() -> specification().
%% @doc UMTS Time Division Duplex(TDD) high cell rate (HCR) radio cell resource specification.
umts_cell_tdd_hcr() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	CId = #specification_char{name = "cId",
			description = "Identifier of a cell in one RNC ('C-ID' in 3GPP 25.433)",
			value_type = "cId",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/cId"},
	LocalCellId = #specification_char{name = "localCellId",
			description = "Uniquely identify a cell in a Node B ('Local Cell ID' in 3GPP 25.433)",
			value_type = "localCellId",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/localCellId"},
	MaximumTransmissionPower = #specification_char{name = "maximumTransmissionPower",
			description = "Maximum power for all downlink channels ('Maximum Transmission Power' in 3GPP 25.433)",
			value_type = "maximumTransmissionPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/maximumTransmissionPower"},
	CellMode = #specification_char{name = "cellMode",
			description = "Identifies the cell mode",
			value_type = "cellMode",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/cellMode"},
	PichPower = #specification_char{name = "pichPower",
			description = "Power of the PICH channel in the cell ('PICH Power' in 3GPP 25.433)",
			value_type = "pichPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/pichPower"},
	PchPower = #specification_char{name = "pchPower",
			description = "PCH transport channel in the cell ('PCH Power' in 3GPP 25.433)",
			value_type = "pchPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/pchPower"},
	FachPower = #specification_char{name = "fachPower",
			description = "Maximum power of FACH transport channel ('Max FACH Power' in 3GPP 25.433)",
			value_type = "fachPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/fachPower"},
	Lac = #specification_char{name = "lac",
			description = "Location Area Code (LAC) (3GPP 23.003)",
			value_type = "lac",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/lac"},
	Rac = #specification_char{name = "rac",
			description = "Routing Area Code (RAC) (3GPP 23.003)",
			value_type = "rac",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/rac"},
	Sac = #specification_char{name = "sac",
			description = "Service Area Code (SAC) (3GPP 23.003)",
			value_type = "sac",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/sac"},
	UraList = #specification_char{name = "uraList",
			description = "UTRAN Registration Area identities ('URA identity' in 3GPP 25.331)",
			value_type = "uraList",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/uraList"},
	UtranCellIubLink = #specification_char{name = "utranCellIubLink",
			description = "Distinguished Name (DN) of IubLink",
			value_type = "dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dn"},
	RelatedAntennaList = #specification_char{name = "relatedAntennaList",
			description = "Distinguished Names (DN) of AntennaFunction(s) (3GPP 28.662)",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	RelatedTmaList = #specification_char{name = "relatedTmaList",
			description = "Distinguished Names (DN) of ('TmaFunction' in 3GPP 28.662)",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	OperationalState = #specification_char{name = "operationalState",
			description = "Operational state describes whether physically installed and working or not (ITU-T X.731)",
			value_type = "operationalStateType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRP#/definitions/operationalStateType"},
	HsFlag = #specification_char{name = "hsFlag",
			description = "High-Speed Downlink Packet Access (HSDPA) supported flag",
			value_type = "hsFlag",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/hsFlag"},
	HsEnable = #specification_char{name = "hsEnable",
			description = "High-Speed Downlink Packet Access (HSDPA) enabled flag",
			value_type = "hsEnable",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/hsEnable"},
	NumOfHspdschs = #specification_char{name = "numOfHspdschs",
			description = "In FDD the number of codes, in TDD the number of HS-PDSCHs (3GPP 25.433)",
			value_type = "numOfHspdschs",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/numOfHspdschs"},
	NumOfHsscchs = #specification_char{name = "numOfHsscchs",
			description = "Number of HS-SCCHs for one cell (3GPP 25.433)",
			value_type = "numOfHsscchs",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/numOfHsscchs"},
	FrameOffset = #specification_char{name = "frameOffset",
			description = "Required offset in neighbouring cells monitoring (3GPP 25.423)",
			value_type = "frameOffset",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/frameOffset"},
	CellIndividualOffset = #specification_char{name = "cellIndividualOffset",
			description = "Relevant for hand over (HO) decision (3GPP 25.331)",
			value_type = "cellIndividualOffset",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/cellIndividualOffset"},
	HcsPrio = #specification_char{name = "hcsPrio",
			description = "Hierarchical cell structure (HCS) priority in cell (re)selection (3GPP 25.331)",
			value_type = "hcsPrio",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/hcsPrio"},
	MaximumAllowedUlTxPower = #specification_char{name = "maximumAllowedUlTxPower",
			description = "Maximum allowed uplink transmit power in cell (re)selection (3GPP 25.331)",
			value_type = "maximumAllowedUlTxPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/maximumAllowedUlTxPower"},
	SnaInformation = #specification_char{name = "snaInformation",
			description = "List of Shared Networks Access Control (SNAC) (3GPP 25.423)",
			value_type = "snaInformation",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/snaInformation"},
	QrxlevMin = #specification_char{name = "qrxlevMin",
			description = "Minimum required RX level in cell (re)selection ('QrxlevMin' in 3GPP 25.331)",
			value_type = "qrxlevMin",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/qrxlevMin"},
	DeltaQrxlevmin = #specification_char{name = "deltaQrxlevmin",
			description = "Delta required RX level in cell (re)selection ('DeltaQrxlevmin' in 3GPP 25.331)",
			value_type = "deltaQrxlevmin",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/deltaQrxlevmin"},
	Qhcs = #specification_char{name = "qhcs",
			description = "Quality threshold levels in cell (re)selection ('Qhcs' in 3GPP 25.331)",
			value_type = "qhcs",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/qhcs"},
	PenaltyTime = #specification_char{name = "penaltyTime",
			description = "Penalty time duration in cell (re)selection ('Penalty_time' in 3GPP 25.331)",
			value_type = "penaltyTime",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/penaltyTime"},
	ReferenceTimeDifferenceToCell = #specification_char{name = "referenceTimeDifferenceToCell",
			description = "Reference time difference to cell in neighbouring cells monitoring (3GPP 25.331)",
			value_type = "referenceTimeDifferenceToCell",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/referenceTimeDifferenceToCell"},
	ReadSFNIndicator = #specification_char{name = "readSFNIndicator",
			description = "Read SFN indicator in neighbouring cells monitoring (3GPP 25.331)",
			value_type = "readSFNIndicator",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/readSFNIndicator"},
	NsPlmnIdList = #specification_char{name = "nsPlmnIdList",
			description = "List of unique identities for PLMN",
			value_type = "NsPlmnIdListType",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/NsPlmnIdListType"},
	RestrictionStateIndicator = #specification_char{name = "restrictionStateIndicator",
			description = "Indicates if cell reserved for operator use in Cell Access Control (3GPP 25.423)",
			value_type = "restrictionStateIndicator",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/restrictionStateIndicator"},
	DpcModechangeSupportIndicator = #specification_char{name = "dpcModechangeSupportIndicator",
			description = "Indicates support for DPC mode change in Power Control (3GPP 25.423)",
			value_type = "dpcModeChangeSupport",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/dpcModeChangeSupport"},
	RelatedSectorEquipment = #specification_char{name = "relatedSectorEquipment",
			description = "Distinguished Name (DN) of sector equipment ('relatedSectorEquipment' in 3GPP 28.662)",
			value_type = "dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dn"},
	Uarfcn = #specification_char{name = "uarfcn",
			description = "UTRA absolute radio frequency channel number (UARFCN) (3GPP 25.433)",
			value_type = "uarfcnAnyMode",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/uarfcnAnyMode"},
	CellParameterId = #specification_char{name = "cellParameterId",
			description = "Unambiguously identifies the cell ('Cell Parameter ID' in TS 25.433)",
			value_type = "cellParameterId",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/cellParameterId"},
	PrimaryCcpchPower = #specification_char{name = "primaryCcpchPower",
			description = "Power of the primary CCPCH channel ('PCCPCH Power' in 3GPP 25.433)",
			value_type = "primaryCcpchPower",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/primaryCcpchPower"},
	CellCapabilityContainerTDD = #specification_char{name = "cellCapabilityContainerTDD",
			description = "Bitfield indicating support of particular functionality (3GPP 25.423)",
			value_type = "cellCapabilityContainerTDD",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/cellCapabilityContainerTDD"},
	SctdIndicator = #specification_char{name = "sctdIndicator",
			description = "Indicates whether SCTD is used ('SCDT Indicator' in 3GPP 25.433)",
			value_type = "sctdIndicator",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/sctdIndicator"},
	DpchConstantValue = #specification_char{name = "dpchConstantValue",
			description = "Power margin used by a UE ('DPCH Constant Valuer' in 3GPP 25.433)",
			value_type = "dpchConstantValue",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/dpchConstantValue"},
	TemporaryOffset1 = #specification_char{name = "temporaryOffset1",
			description = "Offset applied to hierarchical cell structure (HCS) in cell (re)selection (3GPP 25.331)",
			value_type = "temporaryOffset1",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/temporaryOffset1"},
	SyncCase = #specification_char{name = "syncCase",
			description = "SCH and PCCPCH mapped on one or two downlink slots per frame ('Synch Case' in 3GPP 25.433)",
			value_type = "syncCase",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/syncCase"},
	TimeSlotForSch = #specification_char{name = "timeSlotForSch",
			description = "Time interval assigned to a physical channel for SCH ('SCH Time Slot' in 3GPP 25.433)",
			value_type = "timeSlotForSch",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/timeSlotForSch"},
	SchTimeSlot = #specification_char{name = "schTimeSlot",
			description = "First time slot assigned to the Physical Channel SCH ('SCH Time Slot' in TS 25.433 )",
			value_type = "schTimeSlot",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/schTimeSlot"},
	TimeSlotHCRList = #specification_char{name = "timeSlotHCRList",
			description = "Defines the time slot configuration information (3GPP 25.433)",
			value_type = "timeSlotHCRList",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/timeSlotHCRList"},
	Chars = [Id, UserLabel, VnfParametersList, CId, LocalCellId,
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
			SchTimeSlot, TimeSlotHCRList],
	#specification{name = "UtranCellTDDHcr",
			description = "UMTS Time Division Duplex (TDD) high chip rate (HCR) radio cell",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "UtranCellTDDHcr",
					schema = ?PathCatalogSchema ++ "UtranCellTDDHcr"},
			characteristic = Chars}.

-spec umts_iub_link() -> specification().
%% @doc UMTS Iub Link resource function specification.
umts_iub_link() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	IubLinkUtranCell = #specification_char{name = "iubLinkUtranCell",
			description = "Associated UTRAN radio cells",
			value_type = "DnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/DnList"},
	IubLinkNodeBFunction = #specification_char{name = "iubLinkNodeBFunction",
			description = "Connected UTRAN NodeB",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	IubLinkATMChannelTerminationPoint = #specification_char{name = "iubLinkATMChannelTerminationPoint",
			description = "Associated ATM termination point",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	LayerProtocolNameList = #specification_char{name = "layerProtocolNameList",
			description = "",
			value_type = "string"},
	AEnd = #specification_char{name = "aEnd",
			description = "",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	ZEnd = #specification_char{name = "zEnd",
			description = "",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	LinkType = #specification_char{name = "linkType",
			description = "",
			value_type = "string"},
	ProtocolVersion = #specification_char{name = "protocolVersion",
			description = "",
			value_type = "string"},
	Chars = [Id, UserLabel, VnfParametersList, IubLinkUtranCell, IubLinkNodeBFunction,
			IubLinkATMChannelTerminationPoint, LayerProtocolNameList, AEnd, ZEnd, LinkType,
			ProtocolVersion],
	#specification{name = "IubLink",
			description = "UMTS Iub Link",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "IubLink",
					schema = ?PathCatalogSchema ++ "IubLink"},
			characteristic = Chars}.

-spec umts_rnc() -> specification().
%% @doc UMTS Radio Network Controller (RNC) resource specification.
umts_rnc() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PeeParametersList = #specification_char{name = "peeParametersList",
			description = "Parameter list for the control and monitoring of power, energy and environment",
			value_type = "peeParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/peeParametersListType"},
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
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/TceIDMappingInfoList"},
	SharNetTceMappingInfoList = #specification_char{name = "sharNetTceMappingInfoList",
			description = "List of shared PLMN ID, Trace Collection Entity (TCE) ID and IP Address (3GPP 32.422)",
			value_type = "SharNetTceMappingInfoList",
			value_schema = ?PathCatalogSchema ++ "/utranNrm#/definitions/SharNetTceMappingInfoList"},
	Chars = [Id, UserLabel, VnfParametersList, PeeParametersList, Mcc, Mnc, RncId,
			SiptoSupported, TceIDMappingInfoList, SharNetTceMappingInfoList],
	SRelNames = ["UtranCellFDD", "UtranCellTDDLcr", "UtranCellTDDHcr",
			"IubLink"],
	Fspecrel = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{id = Sid, href = Shref,
						name = Name, class_type = Stype}} ->
					[#specification_rel{id = Sid, href = Shref, name = Name,
							ref_type = Stype, rel_type = "composedOf"} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ResSpecRels = lists:foldl(Fspecrel, [], SRelNames),
	#specification{name = "RncFunction",
			description = "UMTS Radio Network Controller (RNC)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "RncFunction",
					schema = ?PathCatalogSchema ++ "RncFunction"},
			characteristic = Chars,
			related = ResSpecRels}.

-spec lte_enb() -> specification().
%% @doc LTE eNodeB resource specification.
lte_enb() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PeeParametersList = #specification_char{name = "peeParametersList",
			description = "Parameter list for the control and monitoring of power, energy and environment",
			value_type = "peeParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/peeParametersListType"},
	IntraANRSwitch = #specification_char{name = "intraANRSwitch",
			description = "Whether the intra E-UTRAN ANR function is activated or deactivated",
			value_type = "boolean"},
	IRATANRSwitch = #specification_char{name = "iRATANRSwitch",
			description = "Whether the IRAT ANR function is activated or deactivated",
			value_type = "boolean"},
	ENBId = #specification_char{name = "eNBId",
			description = "Unambiguously identifies an eNodeB within a PLMN",
			value_type = "ENBId",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/ENBId"},
	X2BlackList = #specification_char{name = "x2BlackList",
			description = "List of target nodes prohibited X2 connections",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	X2WhiteList = #specification_char{name = "x2WhiteList",
			description = "List of target nodes allowed X2 connections",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	X2HOBlackList = #specification_char{name = "x2HOBlackList",
			description = "List of target nodes prohibited handover",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	X2IpAddressList = #specification_char{name = "x2IpAddressList",
			description = "IP addresses used for this ENBFunction's X2 Interface",
			value_type = "string"},
	TceIDMappingInfoList = #specification_char{name = "tceIDMappingInfoList",
			description = "List of Trace Collection Entity (TCE) ID and IP Address (3GPP 32.422)",
			value_type = "TceIDMappingInfoList",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/TceIDMappingInfoList"},
	SharNetTceMappingInfoList = #specification_char{name = "sharNetTceMappingInfoList",
			description = "List of shared PLMN ID, Trace Collection Entity (TCE) ID and IP Address (3GPP 32.422)",
			value_type = "SharNetTceMappingInfoList",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/SharNetTceMappingInfoList"},
	NetListeningRSForRIBS = #specification_char{name = "netListeningRSForRIBS",
			description = "RS for RIBS ('Radio Interface based Synchronization' in 3GPP 36.300)",
			value_type = "NetListeningRSForRIBS",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/NetListeningRSForRIBS"},
	Chars = [Id, UserLabel, VnfParametersList, PeeParametersList, IntraANRSwitch,
			IRATANRSwitch, ENBId, X2BlackList, X2WhiteList, X2HOBlackList,
			X2IpAddressList, TceIDMappingInfoList, SharNetTceMappingInfoList,
			NetListeningRSForRIBS],
	SRelNames = ["EUtranCellFDD", "EUtranCellTDD", "EP_RP_EPS", "EP_X2C",
			"EP_X2U", "EP_NgC", "EP_NgU", "EP_XnC", "EP_XnU"],
	Fspecrel = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{id = Sid, href = Shref,
						name = Name, class_type = Stype}} ->
					[#specification_rel{id = Sid, href = Shref, name = Name,
							ref_type = Stype, rel_type = "composedOf"} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ResSpecRels = lists:foldl(Fspecrel, [], SRelNames),
	ConPointNames = ["EP_RP_EPS", "EP_X2C", "EP_X2U", "EP_NgC", "EP_NgU",
			"EP_XnC", "EP_XnU"],
	Fcp = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{id = CPid, href = CPhref,
						name = CPname, class_type = CPtype}} ->
					[#specification_ref{id = CPid, href = CPhref, name = CPname,
							class_type = "ConnectionPointRef", ref_type = CPtype} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ConnectionPoint = lists:foldl(Fcp, [], ConPointNames),
	#specification{name = "ENBFunction",
			description = "LTE eNodeB",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "ENBFunction",
					schema = ?PathCatalogSchema ++ "ENBFunction"},
			characteristic = Chars,
			related = ResSpecRels,
			connection_point = ConnectionPoint}.

-spec lte_cell_fdd() -> specification().
%% @doc LTE Frequency Division Duplex (FDD) radio cell resource specification.
lte_cell_fdd() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PeeParametersList = #specification_char{name = "peeParametersList",
			description = "Parameter list for the control and monitoring of power, energy and environment",
			value_type = "peeParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/peeParametersListType"},
	CellLocalId = #specification_char{name = "cellLocalId",
			description = "Unambiguously identify a cell within an eNodeB",
			value_type = "CellLocalId",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/CellLocalId"},
	CellLocalIdList = #specification_char{name = "cellLocalIdList",
			description = "List of cellLocalId for split or merged cells in Active Antenna System operations",
			value_type = "CellLocalIdList",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/CellLocalIdList"},
	CellSize = #specification_char{name = "cellSize",
			description = "Cell-Size (3GPP 36.423)",
			value_type = "cellSize",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/cellSize"},
	PLMNIdList = #specification_char{name = "pLMNIdList",
			description = "List of unique identities for PLMN",
			value_type = "PLMNIdList",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/PLMNIdList"},
	CellAccessInfoList = #specification_char{name = "cellAccessInfoList",
			description = "List of PLMN information for RAN sharing",
			value_type = "CellAccessInfoList",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/CellAccessInfoList"},
	Tac = #specification_char{name = "tac",
			description = "Tracking Area Code (3GPP 23.003)",
			value_type = "integer",
			char_value = [#spec_char_value{from = -9223372036854775808, to = 9223372036854775808}]},
	Pci = #specification_char{name = "pci",
			description = "Physical Cell Identity (PCI) of the cell (3GPP 36.211)",
			value_type = "Pci",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/Pci"},
	PciList = #specification_char{name = "pciList",
			description = "List of Physical Cell Identities (PCI) which can be assigned (3GPP 32.500)",
			value_type = "PciList",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/PciList"},
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
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	RelatedAntennaList = #specification_char{name = "relatedAntennaList",
			description = "List DNs of AntennaFunction(s) (3GPP 28.662)",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	RelatedSector = #specification_char{name = "relatedSector",
			description = "List DNs of SectorEquipmentFunction (3GPP 28.662)",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	OperationalState = #specification_char{name = "operationalState",
			description = "Indicates the operational state of the object instance (ITU-T X.731)",
			value_type = "operationalStateType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRP#/definitions/operationalStateType"},
	AdministrativeState = #specification_char{name = "administrativeState",
			description = "Indicates the administrative state of the object instance (ITU-T X.731)",
			value_type = "administrativeStateType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRP#/definitions/administrativeStateType"},
	AvailabilityStatus = #specification_char{name = "availabilityStatus",
			description = "Indicates the availability status of the object instance (ITU-T X.731)",
			value_type = "availabilityStatusType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRP#/definitions/availabilityStatusType"},
	AllowedAccessClasses = #specification_char{name = "allowedAccessClasses",
			description = "Holds information for access classes allowed (3GPP 22.011)",
			value_type = "allowedAccessClassesType",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/allowedAccessClassesType"},
	CellResvInfo = #specification_char{name = "cellResvInfo",
			description = "Represents whether the cell is MBSFN Area Reserved Cell (3GPP 36.00)",
			value_type = "cellResvInfoType",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/cellResvInfoType"},
	NbIoTcellFlag = #specification_char{name = "nbIoTcellFlag",
			description = "Represents whether the cell is supporting NB-IoT or not (3GPP 36.300)",
			value_type = "yesNoType",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/yesNoType"},
	IsChangeForEnergySavingAllowed = #specification_char{name = "isChangeForEnergySavingAllowed",
			description = "Prohibit or allow configuration changes of the cell for ESM purposes",
			value_type = "yesNoType",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/yesNoType"},
	NgranCellFlag = #specification_char{name = "ngranCellFlag",
			description = "Represents whether the cell is provided by ng-eNB or not (3GPP 38.300)",
			value_type = "yesNoType",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/yesNoType"},
	NSSAI = #specification_char{name = "nSSAI",
			description = "Network Slice Selection Assistance Information",
			value_type = "NssaiList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/NssaiList"},
	EarfcnDl = #specification_char{name = "earfcnDl",
			description = "Channel number for central downlink (DL) frequency (3GPP 36.101)",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	EarfcnUl = #specification_char{name = "earfcnUl",
			description = "Channel number for central uplink (UL) frequency (3GPP 36.101)",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	Chars = [Id, UserLabel, VnfParametersList, PeeParametersList, CellLocalId,
			CellLocalIdList, CellSize, PLMNIdList, CellAccessInfoList, Tac,
			Pci, PciList, MaximumTransmissionPower, PartOfSectorPower,
			ReferenceSignalPower , Pb, RelatedTmaList, RelatedAntennaList,
			RelatedSector, OperationalState, AdministrativeState,
			AvailabilityStatus, AllowedAccessClasses, CellResvInfo,
			NbIoTcellFlag, IsChangeForEnergySavingAllowed, NgranCellFlag,
			NSSAI, EarfcnDl, EarfcnUl],
	#specification{name = "EUtranCellFDD",
			description = "LTE Frequency Division Duplex (FDD) radio cell",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "EUtranCellFDD",
					schema = ?PathCatalogSchema ++ "EUtranCellFDD"},
			characteristic = Chars}.

-spec lte_cell_tdd() -> specification().
%% @doc LTE Time Division Duplex (TDD) radio cell resource specification.
lte_cell_tdd() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PeeParametersList = #specification_char{name = "peeParametersList",
			description = "Parameter list for the control and monitoring of power, energy and environment",
			value_type = "peeParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/peeParametersListType"},
	CellLocalId = #specification_char{name = "cellLocalId",
			description = "Unambiguously identify a cell within an eNodeB",
			value_type = "CellLocalId",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/CellLocalId"},
	CellLocalIdList = #specification_char{name = "cellLocalIdList",
			description = "List of cellLocalId for split or merged cells in Active Antenna System operations",
			value_type = "CellLocalIdList",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/CellLocalIdList"},
	CellSize = #specification_char{name = "cellSize",
			description = "Cell-Size (3GPP 36.423)",
			value_type = "cellSize",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/cellSize"},
	PLMNIdList = #specification_char{name = "pLMNIdList",
			description = "List of unique identities for PLMN",
			value_type = "PLMNIdList",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/PLMNIdList"},
	CellAccessInfoList = #specification_char{name = "cellAccessInfoList",
			description = "List of PLMN information for RAN sharing",
			value_type = "CellAccessInfoList",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/CellAccessInfoList"},
	Tac = #specification_char{name = "tac",
			description = "Tracking Area Code (3GPP 23.003)",
			value_type = "integer",
			char_value = [#spec_char_value{from = -9223372036854775808, to = 9223372036854775808}]},
	Pci = #specification_char{name = "pci",
			description = "Physical Cell Identity (PCI) of the cell (3GPP 36.211)",
			value_type = "Pci",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/Pci"},
	PciList = #specification_char{name = "pciList",
			description = "List of Physical Cell Identities (PCI) which can be assigned (3GPP 32.500)",
			value_type = "PciList",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/PciList"},
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
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	RelatedAntennaList = #specification_char{name = "relatedAntennaList",
			description = "List DNs of AntennaFunction(s) (3GPP 28.662)",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	RelatedSector = #specification_char{name = "relatedSector",
			description = "List DNs of SectorEquipmentFunction (3GPP 28.662)",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	OperationalState = #specification_char{name = "operationalState",
			description = "Indicates the operational state of the object instance (ITU-T X.731)",
			value_type = "operationalStateType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRP#/definitions/operationalStateType"},
	AdministrativeState = #specification_char{name = "administrativeState",
			description = "Indicates the administrative state of the object instance (ITU-T X.731)",
			value_type = "administrativeStateType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRP#/definitions/administrativeStateType"},
	AvailabilityStatus = #specification_char{name = "availabilityStatus",
			description = "Indicates the availability status of the object instance (ITU-T X.731)",
			value_type = "availabilityStatusType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRP#/definitions/availabilityStatusType"},
	AllowedAccessClasses = #specification_char{name = "allowedAccessClasses",
			description = "Holds information for access classes allowed (3GPP 22.011)",
			value_type = "allowedAccessClassesType",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/allowedAccessClassesType"},
	CellResvInfo = #specification_char{name = "cellResvInfo",
			description = "Represents whether the cell is MBSFN Area Reserved Cell (3GPP 36.00)",
			value_type = "cellResvInfoType",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/cellResvInfoType"},
	NbIoTcellFlag = #specification_char{name = "nbIoTcellFlag",
			description = "Represents whether the cell is supporting NB-IoT or not (3GPP 36.300)",
			value_type = "yesNoType",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/yesNoType"},
	IsChangeForEnergySavingAllowed = #specification_char{name = "isChangeForEnergySavingAllowed",
			description = "Prohibit or allow configuration changes of the cell for ESM purposes",
			value_type = "yesNoType",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/yesNoType"},
	NgranCellFlag = #specification_char{name = "ngranCellFlag",
			description = "Represents whether the cell is provided by ng-eNB or not (3GPP 38.300)",
			value_type = "yesNoType",
			value_schema = ?PathCatalogSchema ++ "/eutranNrm#/definitions/yesNoType"},
	NSSAI = #specification_char{name = "nSSAI",
			description = "Network Slice Selection Assistance Information",
			value_type = "NssaiList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/NssaiList"},
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
	Chars = [Id, UserLabel, VnfParametersList, PeeParametersList, CellLocalId,
			CellLocalIdList, CellSize, PLMNIdList, CellAccessInfoList, Tac,
			Pci, PciList, MaximumTransmissionPower, PartOfSectorPower,
			ReferenceSignalPower , Pb, RelatedTmaList, RelatedAntennaList,
			RelatedSector, OperationalState, AdministrativeState,
			AvailabilityStatus, AllowedAccessClasses, CellResvInfo,
			NbIoTcellFlag, IsChangeForEnergySavingAllowed, NgranCellFlag,
			NSSAI, Earfcn, SfAssignment, SpecialSfPatterns],
	#specification{name = "EUtranCellTDD",
			description = "LTE Time Division Duplex (TDD) radio cell",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "EUtranCellTDD",
					schema = ?PathCatalogSchema ++ "EUtranCellTDD"},
			characteristic = Chars}.

-spec nr_gnb_du() -> specification().
%% @doc NR gNB Distributed Unit (DU) resource function specification.
nr_gnb_du() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PeeParametersList = #specification_char{name = "peeParametersList",
			description = "Parameter list for the control and monitoring of power, energy and environment",
			value_type = "peeParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/peeParametersListType"},
	GnbId = #specification_char{name = "gnbId",
			description = "Identifies a gNB within a PLMN (3GPP 38.300)",
			value_type = "GnbId",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/GnbId"},
	GnbIdLength = #specification_char{name = "gnbIdLength",
			description = "Indicates number of bits encoding Global gNB ID  (3GPP TS 38.413 clause 9.3.1.6)",
			value_type = "GnbIdLength",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/GnbIdLength"},
	GnbDUId = #specification_char{name = "gnbDuId",
			description = "Identifies the DU at least within a gNB (3GPP 38.473)",
			value_type = "GnbDuId",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/GnbDuId"},
	GnbDuName = #specification_char{name = "gnbDuName",
			description = "Identifies the Distributed Entity of an NR node (3GPP 38.473)",
			value_type = "GnbName",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/GnbName"},
	Chars = [Id, UserLabel, VnfParametersList, PeeParametersList,
				GnbId, GnbIdLength, GnbDUId, GnbDuName],
	SRelNames = ["NRCellDU", "NRSectorCarrier", "EP_F1C", "EP_F1U"],
	Fspecrel = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{id = Sid, href = Shref,
						name = Name, class_type = Stype}} ->
					[#specification_rel{id = Sid, href = Shref, name = Name,
							ref_type = Stype, rel_type = "composedOf"} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ResSpecRels = lists:foldl(Fspecrel, [], SRelNames),
	ConPointNames = ["EP_F1C", "EP_F1U"],
	Fcp = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{id = CPid, href = CPhref,
						name = CPname, class_type = CPtype}} ->
					[#specification_ref{id = CPid, href = CPhref, name = CPname,
							class_type = "ConnectionPointRef", ref_type = CPtype} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ConnectionPoint = lists:foldl(Fcp, [], ConPointNames),
	#specification{name = "GNBDUFunction",
			description = "5G NR gNB Distributed Unit (DU)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "GNBDUFunction",
					schema = ?PathCatalogSchema ++ "GNBDUFunction"},
			characteristic = Chars,
			related = ResSpecRels,
			connection_point = ConnectionPoint}.

-spec nr_gnb_cu_cp() -> specification().
%% @doc NR gNB Central Unit (CU) Control Plane (CP) resource specification.
nr_gnb_cu_cp() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PeeParametersList = #specification_char{name = "peeParametersList",
			description = "Parameter list for the control and monitoring of power, energy and environment",
			value_type = "peeParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/peeParametersListType"},
	GnbId = #specification_char{name = "gnbId",
			description = "Identifies a gNB within a PLMN (3GPP 38.300)",
			value_type = "GnbId",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/GnbId"},
	GnbIdLength = #specification_char{name = "gnbIdLength",
			description = "Indicates number of bits encoding Global gNB ID  (3GPP TS 38.413 clause 9.3.1.6)",
			value_type = "GnbIdLength",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/GnbIdLength"},
	GnbCuName = #specification_char{name = "gnbCuName",
			description = "Identifies the Central Entity of an NR node (3GPP 38.473)",
			value_type = "GnbName",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/GnbName"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	Chars = [Id, UserLabel, VnfParametersList, PeeParametersList,
			GnbId, GnbIdLength, GnbCuName, PLMNIdList],
	SRelNames = ["NRCellCU", "EP_F1C", "EP_E1", "EP_XnC", "EP_X2C", "EP_NgC"],
	Fspecrel = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{id = Sid, href = Shref,
						name = Name, class_type = Stype}} ->
					[#specification_rel{id = Sid, href = Shref, name = Name,
							ref_type = Stype, rel_type = "composedOf"} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ResSpecRels = lists:foldl(Fspecrel, [], SRelNames),
	ConPointNames = ["EP_F1C", "EP_E1", "EP_XnC", "EP_X2C", "EP_NgC"],
	Fcp = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{id = CPid, href = CPhref,
						name = CPname, class_type = CPtype}} ->
					[#specification_ref{id = CPid, href = CPhref, name = CPname,
							class_type = "ConnectionPointRef", ref_type = CPtype} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ConnectionPoint = lists:foldl(Fcp, [], ConPointNames),
	#specification{name = "GNBCUCPFunction",
			description = "5G NR gNB Central Unit (CU) Control Plane (CP)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "GNBCUCPFunction",
					schema = ?PathCatalogSchema ++ "GNBCUCPFunction"},
			characteristic = Chars,
			related = ResSpecRels,
			connection_point = ConnectionPoint}.

-spec nr_gnb_cu_up() -> specification().
%% @doc NR gNB Central Unit (CU) User Plane (UP) resource specification.
nr_gnb_cu_up() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PeeParametersList = #specification_char{name = "peeParametersList",
			description = "Parameter list for the control and monitoring of power, energy and environment",
			value_type = "peeParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/peeParametersListType"},
	GnbId = #specification_char{name = "gnbId",
			description = "Identifies a gNB within a PLMN (3GPP 38.300)",
			value_type = "GnbId",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/GnbId"},
	GnbIdLength = #specification_char{name = "gnbIdLength",
			description = "Indicates number of bits encoding Global gNB ID  (3GPP TS 38.413 clause 9.3.1.6)",
			value_type = "GnbIdLength",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/GnbIdLength"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	Chars = [Id, UserLabel, VnfParametersList, PeeParametersList,
			GnbId, GnbIdLength, PLMNIdList],
	SRelNames = ["EP_E1", "EP_F1U", "EP_XnU", "EP_NgU", "EP_X2U", "EP_S1U"],
	Fspecrel = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{id = Sid, href = Shref,
						name = Name, class_type = Stype}} ->
					[#specification_rel{id = Sid, href = Shref, name = Name,
							ref_type = Stype, rel_type = "composedOf"} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ResSpecRels = lists:foldl(Fspecrel, [], SRelNames),
	ConPointNames = ["EP_E1", "EP_F1U", "EP_XnU", "EP_NgU", "EP_X2U", "EP_S1U"],
	Fcp = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{id = CPid, href = CPhref,
						name = CPname, class_type = CPtype}} ->
					[#specification_ref{id = CPid, href = CPhref, name = CPname,
							class_type = "ConnectionPointRef", ref_type = CPtype} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ConnectionPoint = lists:foldl(Fcp, [], ConPointNames),
	#specification{name = "GNBCUUPFunction",
			description = "5G NR gNB Central Unit (CU) User Plane (UP)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "GNBCUUPFunction",
					schema = ?PathCatalogSchema ++ "GNBCUUPFunction"},
			characteristic = Chars,
			related = ResSpecRels,
			connection_point = ConnectionPoint}.

-spec nr_cell_cu() -> specification().
%% @doc NR Cell Central Unit (CU) resource function specification.
nr_cell_cu() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PeeParametersList = #specification_char{name = "peeParametersList",
			description = "Parameter list for the control and monitoring of power, energy and environment",
			value_type = "peeParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/peeParametersListType"},
	NCI = #specification_char{name = "nCi",
			description = "Uniquely identifies a NR cell within a PLMN (3GPP 38.300)",
			value_type = "Nci",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/Nci"},
	PLMNIdList = #specification_char{name = "pplmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/epcNrm#/definitions/PlmnIdList"},
	SNSSAIList = #specification_char{name = "snssaiList",
			description = "Single Network Slice Selection Assistance Information (S-NSSAI) (3GPP TS 23.003)",
			value_type = "SnssaiList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/SnssaiList"},
	RRMPolicyType  = #specification_char{name = "rrmPolicyType",
			description = "Radio Resource Management (RRM) policy type",
			value_type = "integer"},
	RRMPolicy = #specification_char{name = "rrmPolicy",
			description = "Radio Resources Management (RRM) policy",
			value_type = "RrmPolicy",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/RrmPolicy"},
	RRMPolicyNSSIId = #specification_char{name = "rrmPolicyNSSIId",
			description = "S-NSSAIs for which a rrmPolicyRatio value is specified",
			value_type = "DnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/DnList"},
	RRMPolicyRatio = #specification_char{name = "rrmPolicyRatio",
			description = "Ratio for the split between the supported S-NSSAI lists",
			value_type = "integer"},
	RRMPolicyRatio2 = #specification_char{name = "rrmPolicyRatio2",
			description = "Ratio for the split between the supported S-NSSAI lists",
			value_type = "RrmPolicyRatio2",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/RrmPolicyRatio2"},
	Chars = [Id, UserLabel, VnfParametersList, PeeParametersList,
			NCI, PLMNIdList, SNSSAIList, RRMPolicyType,
			RRMPolicy, RRMPolicyNSSIId, RRMPolicyRatio, RRMPolicyRatio2],
	#specification{name = "NRCellCU",
			description = "5G NR Cell Central Unit (CU)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "NRCellCU",
					schema = ?PathCatalogSchema ++ "NRCellCU"},
			characteristic = Chars}.

-spec nr_cell_du() -> specification().
%% @doc NR Cell Distributed Unit (DU) resource function specification.
nr_cell_du() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	OperationalState = #specification_char{name = "operationalState",
			description = "Operational state describes whether physically installed and working or not (ITU-T X.731)",
			value_type = "operationalStateType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRP#/definitions/operationalStateType"},
	AdministrativeState = #specification_char{name = "administrativeState",
			description = "Indicates the administrative state of the object instance (ITU-T X.731)",
			value_type = "administrativeStateType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRP#/definitions/administrativeStateType"},
	AvailabilityStatus = #specification_char{name = "availabilityStatus",
			description = "Indicates the availability status of the object instance (ITU-T X.731)",
			value_type = "availabilityStatusType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRP#/definitions/availabilityStatusType"},
	NCI = #specification_char{name = "nCi",
			description = "Uniquely identifies a NR cell within a PLMN (3GPP 38.300)",
			value_type = "Nci",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/Nci"},
	PLMNIdList = #specification_char{name = "pplmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/epcNrm#/definitions/PlmnIdList"},
	SNSSAIList = #specification_char{name = "snssaiList",
			description = "Single Network Slice Selection Assistance Information (S-NSSAI) (3GPP TS 23.003)",
			value_type = "SnssaiList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/SnssaiList"},
	NRpci = #specification_char{name = "nrPci",
			description = "Physical Cell Identity (PCI) of NR cell (3GPP 36.211)",
			value_type = "Pci",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/Pci"},
	NRTac = #specification_char{name = "nrTac",
			description = "Identity of the common Tracking Area Code (TAC) for the PLMNs.",
			value_type = "NrTac",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/NrTac"},
	ARFCNUL = #specification_char{name = "arfcnUL",
			description = "NR Absolute Radio Frequency Channel Number (NR-ARFCN) for uplink",
			value_type = "integer"},
	ARFCNDL = #specification_char{name = "arfcnDL",
			description = "NR Absolute Radio Frequency Channel Number (NR-ARFCN) for downlink",
			value_type = "integer"},
	ARFCNSUL = #specification_char{name = "arfcnSUL",
			description = "NR Absolute Radio Frequency Channel Number (NR-ARFCN) for supplementary uplink",
			value_type = "integer"},
	BSChannelBwUL = #specification_char{name = "bSChannelBwUL",
			description = "BS Channel bandwidth in MHz for uplink (3GPP 38.104 clause 5.3)",
			value_type = "integer"},
	BSChannelBwDL = #specification_char{name = "bSChannelBwDL",
			description = "BS Channel bandwidth in MHz for downlink (3GPP 38.104 clause 5.3)",
			value_type = "integer"},
	BSChannelBwSUL = #specification_char{name = "bSChannelBwSUL",
			description = "BS Channel bandwidth in MHz for supplementary uplink (3GPP 38.104 clause 5.3)",
			value_type = "integer"},
	Chars = [Id, UserLabel, VnfParametersList, OperationalState,
			AdministrativeState, AvailabilityStatus, NCI, PLMNIdList,
			SNSSAIList, NRpci, NRTac, ARFCNUL, ARFCNDL, ARFCNSUL,
			BSChannelBwUL, BSChannelBwDL, BSChannelBwSUL],
	#specification{name = "NRCellDU",
			description = "5G NR Cell Distributed Unit (DU)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "NRCellDU",
					schema = ?PathCatalogSchema ++ "NRCellDU"},
			characteristic = Chars}.

-spec nr_sector_carrier() -> specification().
%% @doc NR Sector Carrier resource function specification.
nr_sector_carrier() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	TxDirection = #specification_char{name = "txDirection",
			description = "Transmission direction",
			value_type = "TxDirection",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/TxDirection"},
	ConfiguredMaxTxPower = #specification_char{name = "configuredMaxTxPower",
			description = "Maximum transmit power for all downlink channels in a cell totaled together",
			value_type = "integer"},
	ARFCNUL = #specification_char{name = "arfcnUL",
			description = "NR Absolute Radio Frequency Channel Number (NR-ARFCN) for uplink",
			value_type = "integer"},
	ARFCNDL = #specification_char{name = "arfcnDL",
			description = "NR Absolute Radio Frequency Channel Number (NR-ARFCN) for downlink",
			value_type = "integer"},
	BSChannelBwUL = #specification_char{name = "bSChannelBwUL",
			description = "BS Channel bandwidth in MHz for uplink (3GPP 38.104 clause 5.3)",
			value_type = "integer"},
	BSChannelBwDL = #specification_char{name = "bSChannelBwDL",
			description = "BS Channel bandwidth in MHz for downlink (3GPP 38.104 clause 5.3)",
			value_type = "integer"},
	Chars = [Id, UserLabel, VnfParametersList, TxDirection,
			ConfiguredMaxTxPower, ARFCNUL, ARFCNDL, BSChannelBwUL, BSChannelBwDL],
	#specification{name = "NRSectorCarrier",
			description = "5G NR Sector Carrier",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "NRSectorCarrier",
					schema = ?PathCatalogSchema ++ "NRSectorCarrier"},
			characteristic = Chars}.

-spec nr_ep_x2c() -> specification().
%% @doc NR End Point X2C resource specification.
nr_ep_x2c() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_X2C",
			description = "NR End Point of the logical link, supporting X2-C application protocols",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "NR",
			target_schema = #target_schema_ref{class_type = "EP_X2C",
					schema = ?PathCatalogSchema ++ "EP_X2C"},
			characteristic = Chars}.

-spec nr_ep_x2u() -> specification().
%% @doc NR End Point X2U resource specification.
nr_ep_x2u() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_X2U",
			description = "NR End Point of the logical link, supporting the X2 user plane (X2-U) interface",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "NR",
			target_schema = #target_schema_ref{class_type = "EP_X2U",
					schema = ?PathCatalogSchema ++ "EP_X2U"},
			characteristic = Chars}.

-spec nr_ep_ngc() -> specification().
%% @doc NR End Point NgC resource specification.
nr_ep_ngc() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_NgC",
			description = "NR End Point of the control plane interface (NG-C) between the gNB and NG-Core entity",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "NR",
			target_schema = #target_schema_ref{class_type = "EP_NgC",
					schema = ?PathCatalogSchema ++ "EP_NgC"},
			characteristic = Chars}.

-spec nr_ep_ngu() -> specification().
%% @doc NR End Point NgU resource specification.
nr_ep_ngu() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_NgU",
			description = "NR End Point of the NG user plane (NG-U) interface between the gNB and the UPGW",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "NR",
			target_schema = #target_schema_ref{class_type = "EP_NgU",
					schema = ?PathCatalogSchema ++ "EP_NgU"},
			characteristic = Chars}.

-spec nr_ep_xnc() -> specification().
%% @doc NR End Point XnC resource specification.
nr_ep_xnc() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_XnC",
			description = "NR End Point of the logical link, supporting Xn Application protocols, to a neighbour gNB node",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "NR",
			target_schema = #target_schema_ref{class_type = "EP_XnC",
					schema = ?PathCatalogSchema ++ "EP_XnC"},
			characteristic = Chars}.

-spec nr_ep_xnu() -> specification().
%% @doc NR End Point XnU resource specification.
nr_ep_xnu() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_XnU",
			description = "NR End Point of a logical link supporting the Xn user plane (Xn-U) interface",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "NR",
			target_schema = #target_schema_ref{class_type = "EP_XnU",
					schema = ?PathCatalogSchema ++ "EP_XnU"},
			characteristic = Chars}.

-spec nr_ep_f1c() -> specification().
%% @doc NR End Point F1C resource specification.
nr_ep_f1c() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_F1C",
			description = "NR End Point of the control plane interface (F1-C) between the DU and CU or CUUP",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "NR",
			target_schema = #target_schema_ref{class_type = "EP_F1C",
					schema = ?PathCatalogSchema ++ "EP_F1C"},
			characteristic = Chars}.

-spec nr_ep_f1u() -> specification().
%% @doc NR End Point F1U resource specification.
nr_ep_f1u() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_F1U",
			description = "NR End Point of the user plane interface (F1-U) between the DU and CU or CUUP",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "NR",
			target_schema = #target_schema_ref{class_type = "EP_F1U",
					schema = ?PathCatalogSchema ++ "EP_F1U"},
			characteristic = Chars}.

-spec nr_ep_e1() -> specification().
%% @doc NR End Point E1 resource specification.
nr_ep_e1() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_E1",
			description = "NR End Point of the logical link, supporting E1 interface between gNB-CU-CP and gNB-CUUP",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "NR",
			target_schema = #target_schema_ref{class_type = "EP_E1",
					schema = ?PathCatalogSchema ++ "EP_E1"},
			characteristic = Chars}.

-spec nr_ep_s1u() -> specification().
%% @doc NR End Point S1U resource specification.
nr_ep_s1u() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_S1U",
			description = "NR End Point of the logical link, supporting S1-U interface towards a S-GW node",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "NR",
			target_schema = #target_schema_ref{class_type = "EP_S1U",
					schema = ?PathCatalogSchema ++ "EP_S1U"},
			characteristic = Chars}.

-spec network_slice() -> specification().
%% @doc Network Slice resource function specification.
network_slice() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	DnPrefix = #specification_char{name = "dnPrefix",
			description = "Distinguished Name (DN) prefix (3GPP 32.300 Annex C)",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	UserDefinedNetworkType = #specification_char{name = "userDefinedNetworkType",
			description = "User defined network type (3GPP 28.620)",
			value_type = "string"},
	SetOfMcc = #specification_char{name = "setOfMcc",
			description = "All Mobile Country Codes (MCC) included",
			value_type = "string"},
	NSSIId = #specification_char{name = "nSSIId",
			description = "This RDN uniquely identifies the NetworkSliceSubnet",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	OperationalState = #specification_char{name = "operationalState",
			description = "Operational state describes whether physically installed and working or not (ITU-T X.731)",
			value_type = "operationalStateType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRP#/definitions/operationalStateType"},
	AdministrativeState = #specification_char{name = "administrativeState",
			description = "Indicates the administrative state of the object instance (ITU-T X.731)",
			value_type = "administrativeStateType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRP#/"
					"definitions/administrativeStateType"},
	ServiceProfileList = #specification_char{name = "serviceProfileList",
			description = "Service Profiles (3GPP TS 28.541 clause 6.3.3)",
			value_type = "ServiceProfileList",
			value_schema = ?PathCatalogSchema ++ "/sliceNrm#/definitions/ServiceProfileList"},
	Chars = [Id, DnPrefix, UserLabel, UserDefinedNetworkType, SetOfMcc, NSSIId,
			OperationalState, AdministrativeState, ServiceProfileList, ServiceProfileList],
	#specification{name = "NetworkSlice",
			description = "Network Slice",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Slice",
			target_schema = #target_schema_ref{class_type = "NetworkSlice",
					schema = ?PathCatalogSchema ++ "NetworkSlice"},
			characteristic = Chars}.

-spec network_slice_subnet() -> specification().
%% @doc Network Slice Subnet resource function specification.
network_slice_subnet() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	DnPrefix = #specification_char{name = "dnPrefix",
			description = "Distinguished Name (DN) prefix (3GPP 32.300 Annex C)",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	UserDefinedNetworkType = #specification_char{name = "userDefinedNetworkType",
			description = "User defined network type (3GPP 28.620)",
			value_type = "string"},
	SetOfMcc = #specification_char{name = "setOfMcc",
			description = "All Mobile Country Codes (MCC) included",
			value_type = "string"},
	MFIdList = #specification_char{name = "mFIdList",
			description = "Associated Network Functions (NF)",
			value_type = "DnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/DnList"},
	ConstituentNSSIIdList = #specification_char{name = "constituentNSSIIdList",
			description = "Constituent Network Slice SubNetwork Instance (NSSI) list",
			value_type = "DnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/DnList"},
	OperationalState = #specification_char{name = "operationalState",
			description = "Operational state describes whether physically installed and working or not (ITU-T X.731)",
			value_type = "operationalStateType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRP#/definitions/operationalStateType"},
	AdministrativeState = #specification_char{name = "administrativeState",
			description = "Indicates the administrative state of the object instance (ITU-T X.731)",
			value_type = "administrativeStateType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRP#"
					"/definitions/administrativeStateType"},
	NsInfo = #specification_char{name = "nsInfo",
			description = "Network Service (NS) info (ETSI GS NFV-IFA 013 clause 8.3.3.2.2)",
			value_type = "NsInfo",
			value_schema = ?PathCatalogSchema ++ "/sliceNrm#/definitions/NsInfo"},
	SliceProfileList = #specification_char{name = "sliceProfileList",
			description = "Slice Profiles (3GPP TS 28.541 clause 6.3.4)",
			value_type = "SliceProfileList",
			value_schema = ?PathCatalogSchema ++ "/sliceNrm#/definitions/SliceProfileList"},
	Chars = [Id, DnPrefix, UserLabel, UserDefinedNetworkType,
			SetOfMcc, MFIdList, ConstituentNSSIIdList,
			OperationalState, AdministrativeState, NsInfo, SliceProfileList],
	SpecificationNames = ["AMFFunction", "SMFFunction", "UPFFunction",
			"AUSFFunction", "NSSFFunction", "UDMFunction", "PCFFunction",
			"EP_N2", "EP_N3", "EP_N4", "EP_N6", "EP_N7", "EP_N8",
			"EP_N9", "EP_N10", "EP_N11", "EP_N12", "EP_N13", "EP_N14",
			"EP_N15", "EP_N22"],
	Fspecrel = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{id = Sid, href = Shref,
						name = Sname, class_type = Stype}} ->
					case Name of
						"AMFFunction" ->
							[#specification_rel{id = Sid, href = Shref,
									name = Sname, ref_type = Stype,
									default = 1, min = 1, max = 6,
									rel_type = "composedOf"} | Acc];
						"SMFFunction" ->
							[#specification_rel{id = Sid, href = Shref,
									name = Sname, ref_type = Stype,
									default = 1, min = 1, max = 6,
									rel_type = "composedOf"} | Acc];
						"UPFFunction" ->
							[#specification_rel{id = Sid, href = Shref,
									name = Sname, ref_type = Stype,
									default = 1, min = 1, max = 6,
									rel_type = "composedOf"} | Acc];
						"EP_N2" ->
							[#specification_rel{id = Sid, href = Shref,
									name = Sname, ref_type = Stype,
									default = 1, min = 1, max = 6,
									rel_type = "composedOf"} | Acc];
						"EP_N3" ->
							[#specification_rel{id = Sid, href = Shref,
									name = Sname, ref_type = Stype,
									default = 1, min = 1, max = 6,
									rel_type = "composedOf"} | Acc];
						"EP_N6" ->
							[#specification_rel{id = Sid, href = Shref,
									name = Sname, ref_type = Stype,
									default = 1, min = 1, max = 6,
									rel_type = "composedOf"} | Acc];
						_ ->
							[#specification_rel{id = Sid, href = Shref,
									name = Sname, ref_type = Stype,
									rel_type = "composedOf"} | Acc]
					end;
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ResourceSpecRelationship = lists:foldl(Fspecrel, [], SpecificationNames),
	ConnectionPointNames = ["EP_N2", "EP_N3", "EP_N6"],
	Fcp = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{id = CPid, href = CPhref,
						name = CPname, class_type = CPtype}} ->
					[#specification_ref{id = CPid, href = CPhref, name = CPname,
							class_type = "ConnectionPointRef", ref_type = CPtype} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ConnectionPoint = lists:foldl(Fcp, [], ConnectionPointNames),
	ConnectionNames = [{"N2", "EP_N2", "AMFFunction"},
			{"N3", "EP_N3", "UPFFunction"},
			{"N4", "SMFFunction", "UPFFunction"},
			{"N6", "UPFFunction", "EP_N6"},
			{"N7", "SMFFunction", "PCFFunction"},
			{"N8", "AMFFunction", "UDMFunction"},
			{"N9", "UPFFunction", "UPFFunction"},
			{"N10", "SMFFunction", "UDMFunction"},
			{"N11", "AMFFunction", "SMFFunction"},
			{"N12", "AMFFunction", "AUSFFunction"},
			{"N13", "AUSFFunction", "UDMFunction"},
			{"N14", "AMFFunction", "AMFFunction"},
			{"N15", "AMFFunction", "PCFFunction"},
			{"N22", "AMFFunction", "NSSFFunction"}],
	Fcon = fun({ConName, EpName1, EpName2}, Acc) ->
			case im:get_specification_name(EpName1) of
				{ok, #specification{id = EP1id, href = EP1href,
						name = EP1name, class_type = EP1type}} ->
					EP1 = #endpoint_spec_ref{id = EP1id, href = EP1href,
							name = EP1name, ref_type = EP1type},
					case im:get_specification_name(EpName2) of
						{ok, #specification{id = EP2id, href = EP2href,
								name = EP2name, class_type = EP2type}} ->
							EP2 = #endpoint_spec_ref{id = EP2id, href = EP2href,
									name = EP2name, ref_type = EP2type},
							[#connection_spec{name = ConName,
									endpoint = [EP1, EP2]} | Acc];
						{error, Reason} ->
							error_logger:warning_report(["Error reading resource specification",
									{specification, EpName2}, {error, Reason}]),
							Acc
					end;
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, EpName1}, {error, Reason}]),
					Acc
			end
	end,
	Connections = lists:foldl(Fcon, [], ConnectionNames),
	#specification{name = "NetworkSliceSubnet",
			description = "Network Slice Subnet",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Slice",
			target_schema = #target_schema_ref{class_type = "NetworkSliceSubnet",
					schema = ?PathCatalogSchema ++ "NetworkSliceSubnet"},
			characteristic = Chars,
			related = lists:reverse(ResourceSpecRelationship),
			connection_point = lists:reverse(ConnectionPoint),
			connectivity = [#resource_graph_spec{
             name = "Adjacency Graph",
             connection = lists:reverse(Connections)}]}.

-spec ngc_amf() -> specification().
%% @doc 5GC Access and Mobility Management Function (AMF) resource function specification.
ngc_amf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	AmfIdentifier = #specification_char{name = "amfIdentifier",
			description = "Uniquely identify an AMF instance",
			value_type = "AMFIdentifier",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/AMFIdentifier"},
	SBIFqdn = #specification_char{name = "sBIFqdn",
			description = "FQDN of the registered NF instance in service-based interface",
			value_type = "SBIFqdn",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIFqdn"},
	SBIServiceList = #specification_char{name = "sBIServiceList",
			description = "All supported NF services registered on service-based interface",
			value_type = "SBIServiceList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIServiceList"},
	WeightFactor = #specification_char{name = "weightFactor",
			description = "Capacity of local node relative to other nodes in the same type (3GPP 23.501)",
			value_type = "WeightFactor",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/WeightFactor"},
	SNSSAIList = #specification_char{name = "snssaiList",
			description = "Single Network Slice Selection Assistance Information (S-NSSAI) (3GPP TS 23.003)",
			value_type = "SnssaiList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/SnssaiList"},
	AmfSet = #specification_char{name = "amfSet",
			description = "DN of an AMF set",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	Chars = [ID, UserLabel, VnfParametersList, PLMNIdList, AmfIdentifier,
			SBIFqdn, SBIServiceList, WeightFactor, SNSSAIList, AmfSet],
	SpecNames = ["EP_N2", "EP_N8", "EP_N11", "EP_N12", "EP_N14", "EP_N15",
			"EP_N17", "EP_N20", "EP_N22", "EP_N26", "EP_NLS", "EP_NLG", "EP_SBI_X"],
	Fspecs = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{} = Spec} ->
					[Spec | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ResSpecs = lists:foldl(Fspecs, [], SpecNames),
	Fspecrels = fun(#specification{id = Sid, href = Shref,
				name = Name, class_type = Stype}) ->
			#specification_rel{id = Sid, href = Shref, name = Name,
							ref_type = Stype, rel_type = "composedOf"}
	end,
	ResSpecRels = lists:map(Fspecrels, ResSpecs),
	Fspeccps = fun(#specification{id = CPid, href = CPhref,
				name = CPname, class_type = CPtype}) ->
			#specification_ref{id = CPid, href = CPhref, name = CPname,
					class_type = "ConnectionPointRef", ref_type = CPtype}
	end,
	ResSpecCPs = lists:map(Fspeccps, ResSpecs),
	#specification{name = "AMFFunction",
			description = "5G Core Access and Mobility Management Function (AMF)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "AMFFunction",
					schema = ?PathCatalogSchema ++ "AMFFunction"},
			characteristic = Chars,
			related = ResSpecRels,
			connection_point = ResSpecCPs}.

-spec ngc_smf() -> specification().
%% @doc 5GC Session Management Function (SMF) resource function specification.
ngc_smf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	TacList = #specification_char{name = "nrTacList",
			description = "Tracking Area Code (TAC) where management function serving (3GPP TS 38.413)",
			value_type = "TACList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/TACList"},
	SBIFqdn = #specification_char{name = "sBIFqdn",
			description = "FQDN of the registered NF instance in service-based interface",
			value_type = "SBIFqdn",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIFqdn"},
	SBIServiceList = #specification_char{name = "sBIServiceList",
			description = "All supported NF services registered on service-based interface",
			value_type = "SBIServiceList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIServiceList"},
	SNSSAIList = #specification_char{name = "snssaiList",
			description = "Single Network Slice Selection Assistance Information (S-NSSAI) (3GPP TS 23.003)",
			value_type = "SnssaiList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/SnssaiList"},
	Chars = [ID, UserLabel, VnfParametersList, PLMNIdList, TacList,
			SBIFqdn, SBIServiceList, SNSSAIList],
	SpecNames = ["EP_N4", "EP_N7", "EP_N10", "EP_N11", "EP_N16", "EP_S5C",
			"EP_SBI_X"],
	Fspecs = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{} = Spec} ->
					[Spec | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ResSpecs = lists:foldl(Fspecs, [], SpecNames),
	Fspecrels = fun(#specification{id = Sid, href = Shref,
				name = Name, class_type = Stype}) ->
			#specification_rel{id = Sid, href = Shref, name = Name,
							ref_type = Stype, rel_type = "composedOf"}
	end,
	ResSpecRels = lists:map(Fspecrels, ResSpecs),
	Fspeccps = fun(#specification{id = CPid, href = CPhref,
				name = CPname, class_type = CPtype}) ->
			#specification_ref{id = CPid, href = CPhref, name = CPname,
					class_type = "ConnectionPointRef", ref_type = CPtype}
	end,
	ResSpecCPs = lists:map(Fspeccps, ResSpecs),
	#specification{name = "SMFFunction",
			description = "5G Core Session Management Function (SMF)"
					"resource function specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "SMFFunction",
					schema = ?PathCatalogSchema ++ "SMFFunction"},
			characteristic = Chars,
			related = ResSpecRels,
			connection_point = ResSpecCPs}.

-spec ngc_upf() -> specification().
%% @doc 5GC User Plane Function (UPF) resource function specification.
ngc_upf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	TacList = #specification_char{name = "nrTacList",
			description = "Tracking Area Code (TAC) where management function serving (3GPP TS 38.413)",
			value_type = "TACList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/TACList"},
	SNSSAIList = #specification_char{name = "snssaiList",
			description = "Single Network Slice Selection Assistance Information (S-NSSAI) (3GPP TS 23.003)",
			value_type = "SnssaiList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/SnssaiList"},
	Chars = [ID, UserLabel, VnfParametersList, PLMNIdList, TacList, SNSSAIList],
	SpecNames = ["EP_N3", "EP_N4", "EP_N6", "EP_N9", "EP_S5U", "EP_SBI_X"],
	Fspecs = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{} = Spec} ->
					[Spec | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ResSpecs = lists:foldl(Fspecs, [], SpecNames),
	Fspecrels = fun(#specification{id = Sid, href = Shref,
				name = Name, class_type = Stype}) ->
			#specification_rel{id = Sid, href = Shref, name = Name,
							ref_type = Stype, rel_type = "composedOf"}
	end,
	ResSpecRels = lists:map(Fspecrels, ResSpecs),
	Fspeccps = fun(#specification{id = CPid, href = CPhref,
				name = CPname, class_type = CPtype}) ->
			#specification_ref{id = CPid, href = CPhref, name = CPname,
					class_type = "ConnectionPointRef", ref_type = CPtype}
	end,
	ResSpecCPs = lists:map(Fspeccps, ResSpecs),
	#specification{name = "UPFFunction",
			description = "5G Core User Plane Function (UPF)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "UPFFunction",
					schema = ?PathCatalogSchema ++ "UPFFunction"},
			characteristic = Chars,
			related = ResSpecRels,
			connection_point = ResSpecCPs}.

-spec ngc_n3iwf() -> specification().
%% @doc 5GC Non 3GPP Inter Working Function (N3IWF) resource function specification.
ngc_n3iwf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	Chars = [ID, UserLabel, VnfParametersList, PLMNIdList],
	SpecNames = ["EP_N2", "EP_N3"],
	Fspecs = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{} = Spec} ->
					[Spec | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ResSpecs = lists:foldl(Fspecs, [], SpecNames),
	Fspecrels = fun(#specification{id = Sid, href = Shref,
				name = Name, class_type = Stype}) ->
			#specification_rel{id = Sid, href = Shref, name = Name,
							ref_type = Stype, rel_type = "composedOf"}
	end,
	ResSpecRels = lists:map(Fspecrels, ResSpecs),
	Fspeccps = fun(#specification{id = CPid, href = CPhref,
				name = CPname, class_type = CPtype}) ->
			#specification_ref{id = CPid, href = CPhref, name = CPname,
					class_type = "ConnectionPointRef", ref_type = CPtype}
	end,
	ResSpecCPs = lists:map(Fspeccps, ResSpecs),
	#specification{name = "N3IWFFunction",
			description = "5G Core Non 3GPP Inter Working Function (N3IWF)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "N3IWFFunction",
					schema = ?PathCatalogSchema ++ "N3IWFFunction"},
			characteristic = Chars,
			related = ResSpecRels,
			connection_point = ResSpecCPs}.

-spec ngc_pcf() -> specification().
%% @doc 5G Policy Control Function (PCF) resource function specification.
ngc_pcf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	SBIFqdn = #specification_char{name = "sBIFqdn",
			description = "FQDN of the registered NF instance in service-based interface",
			value_type = "SBIFqdn",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIFqdn"},
	SBIServiceList = #specification_char{name = "sBIServiceList",
			description = "All supported NF services registered on service-based interface",
			value_type = "SBIServiceList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIServiceList"},
	SNSSAIList = #specification_char{name = "snssaiList",
			description = "Single Network Slice Selection Assistance Information (S-NSSAI) (3GPP TS 23.003)",
			value_type = "SnssaiList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/SnssaiList"},
	Chars = [ID, UserLabel, VnfParametersList, PLMNIdList, SBIFqdn,
			SBIServiceList, SNSSAIList],
	SpecNames = ["EP_N5", "EP_N7", "EP_N15", "EP_N16", "EP_Rx", "EP_SBI_X"],
	Fspecs = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{} = Spec} ->
					[Spec | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ResSpecs = lists:foldl(Fspecs, [], SpecNames),
	Fspecrels = fun(#specification{id = Sid, href = Shref,
				name = Name, class_type = Stype}) ->
			#specification_rel{id = Sid, href = Shref, name = Name,
							ref_type = Stype, rel_type = "composedOf"}
	end,
	ResSpecRels = lists:map(Fspecrels, ResSpecs),
	Fspeccps = fun(#specification{id = CPid, href = CPhref,
				name = CPname, class_type = CPtype}) ->
			#specification_ref{id = CPid, href = CPhref, name = CPname,
					class_type = "ConnectionPointRef", ref_type = CPtype}
	end,
	ResSpecCPs = lists:map(Fspeccps, ResSpecs),
	#specification{name = "PCFFunction",
			description = "5G Policy Control Function (PCF)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "PCFFunction",
					schema = ?PathCatalogSchema ++ "PCFFunction"},
			characteristic = Chars,
			related = ResSpecRels,
			connection_point = ResSpecCPs}.

-spec ngc_ausf() -> specification().
%% @doc 5GC Authentication Server Function (AUSF) resource function specification.
ngc_ausf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	SBIFqdn = #specification_char{name = "sBIFqdn",
			description = "FQDN of the registered NF instance in service-based interface",
			value_type = "SBIFqdn",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIFqdn"},
	SBIServiceList = #specification_char{name = "sBIServiceList",
			description = "All supported NF services registered on service-based interface",
			value_type = "SBIServiceList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIServiceList"},
	SNSSAIList = #specification_char{name = "snssaiList",
			description = "Single Network Slice Selection Assistance Information (S-NSSAI) (3GPP TS 23.003)",
			value_type = "SnssaiList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/SnssaiList"},
	Chars = [ID, UserLabel, VnfParametersList, PLMNIdList, SBIFqdn,
			SBIServiceList, SNSSAIList],
	SpecNames = ["EP_N12", "EP_N13", "EP_SBI_X"],
	Fspecs = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{} = Spec} ->
					[Spec | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ResSpecs = lists:foldl(Fspecs, [], SpecNames),
	Fspecrels = fun(#specification{id = Sid, href = Shref,
				name = Name, class_type = Stype}) ->
			#specification_rel{id = Sid, href = Shref, name = Name,
							ref_type = Stype, rel_type = "composedOf"}
	end,
	ResSpecRels = lists:map(Fspecrels, ResSpecs),
	Fspeccps = fun(#specification{id = CPid, href = CPhref,
				name = CPname, class_type = CPtype}) ->
			#specification_ref{id = CPid, href = CPhref, name = CPname,
					class_type = "ConnectionPointRef", ref_type = CPtype}
	end,
	ResSpecCPs = lists:map(Fspeccps, ResSpecs),
	#specification{name = "AUSFFunction",
			description = "5G Core Authentication Server Function (AUSF)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "AUSFFunction",
					schema = ?PathCatalogSchema ++ "AUSFFunction"},
			characteristic = Chars,
			related = ResSpecRels,
			connection_point = ResSpecCPs}.

-spec ngc_udm() -> specification().
%% @doc 5GC Unified Data Management (UDM) resource function specification.
ngc_udm() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	SBIFqdn = #specification_char{name = "sBIFqdn",
			description = "FQDN of the registered NF instance in service-based interface",
			value_type = "SBIFqdn",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIFqdn"},
	SBIServiceList = #specification_char{name = "sBIServiceList",
			description = "All supported NF services registered on service-based interface",
			value_type = "SBIServiceList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIServiceList"},
	SNSSAIList = #specification_char{name = "snssaiList",
			description = "Single Network Slice Selection Assistance Information (S-NSSAI) (3GPP TS 23.003)",
			value_type = "SnssaiList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/SnssaiList"},
	Chars = [ID, UserLabel, VnfParametersList, PLMNIdList, SBIFqdn,
			SBIServiceList, SNSSAIList],
	SpecNames = ["EP_N8", "EP_N10", "EP_N13", "EP_SBI_X"],
	Fspecs = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{} = Spec} ->
					[Spec | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ResSpecs = lists:foldl(Fspecs, [], SpecNames),
	Fspecrels = fun(#specification{id = Sid, href = Shref,
				name = Name, class_type = Stype}) ->
			#specification_rel{id = Sid, href = Shref, name = Name,
							ref_type = Stype, rel_type = "composedOf"}
	end,
	ResSpecRels = lists:map(Fspecrels, ResSpecs),
	Fspeccps = fun(#specification{id = CPid, href = CPhref,
				name = CPname, class_type = CPtype}) ->
			#specification_ref{id = CPid, href = CPhref, name = CPname,
					class_type = "ConnectionPointRef", ref_type = CPtype}
	end,
	ResSpecCPs = lists:map(Fspeccps, ResSpecs),
	#specification{name = "UDMFunction",
			description = "5G Core Unified Data Management (UDM)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "UDMFunction",
					schema = ?PathCatalogSchema ++ "UDMFunction"},
			characteristic = Chars,
			related = ResSpecRels,
			connection_point = ResSpecCPs}.

-spec ngc_udr() -> specification().
%% @doc 5GC Unified Data Repository (UDR) resource function specification.
ngc_udr() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	SBIFqdn = #specification_char{name = "sBIFqdn",
			description = "FQDN of the registered NF instance in service-based interface",
			value_type = "SBIFqdn",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIFqdn"},
	SBIServiceList = #specification_char{name = "sBIServiceList",
			description = "All supported NF services registered on service-based interface",
			value_type = "SBIServiceList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIServiceList"},
	SNSSAIList = #specification_char{name = "snssaiList",
			description = "Single Network Slice Selection Assistance Information (S-NSSAI) (3GPP TS 23.003)",
			value_type = "SnssaiList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/SnssaiList"},
	Chars = [ID, UserLabel, VnfParametersList, PLMNIdList, SBIFqdn,
			SBIServiceList, SNSSAIList],
	Name = "EP_SBI_X",
	case im:get_specification_name(Name) of
		{ok, #specification{id = Sid, href = Shref, name = Name,
				class_type = Stype}} ->
			EpSbiXRel = #specification_rel{id = Sid, href = Shref, name = Name,
					ref_type = Stype, rel_type = "composedOf"},
			EpSbiXCP = #specification_ref{id = Sid, href = Shref, name = Name,
					class_type = "ConnectionPointRef", ref_type = Stype},
			#specification{name = "UDRFunction",
					description = "5G Core Unified Data Repository (UDR)",
					class_type = "ResourceFunctionSpecification",
					status = active,
					version = "1.0",
					category = "Core",
					target_schema = #target_schema_ref{class_type = "UDRFunction",
							schema = ?PathCatalogSchema ++ "UDRFunction"},
					characteristic = Chars,
					related = [EpSbiXRel],
					connection_point = [EpSbiXCP]};
		{error, Reason} ->
			error_logger:warning_report(["Error reading resource specification",
					{specification, Name}, {error, Reason}]),
			{error, Reason}
	end.

-spec ngc_udsf() -> specification().
%% @doc 5GC Unified Data Storage Function (UDSF) resource function specification.
ngc_udsf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	SBIFqdn = #specification_char{name = "sBIFqdn",
			description = "FQDN of the registered NF instance in service-based interface",
			value_type = "SBIFqdn",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIFqdn"},
	SBIServiceList = #specification_char{name = "sBIServiceList",
			description = "All supported NF services registered on service-based interface",
			value_type = "SBIServiceList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIServiceList"},
	SNSSAIList = #specification_char{name = "snssaiList",
			description = "Single Network Slice Selection Assistance Information (S-NSSAI) (3GPP TS 23.003)",
			value_type = "SnssaiList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/SnssaiList"},
	Chars = [ID, UserLabel, VnfParametersList, PLMNIdList, SBIFqdn,
			SBIServiceList, SNSSAIList],
	Name = "EP_SBI_X",
	case im:get_specification_name(Name) of
		{ok, #specification{id = Sid, href = Shref, name = Name,
				class_type = Stype}} ->
			EpSbiXRel = #specification_rel{id = Sid, href = Shref, name = Name,
					ref_type = Stype, rel_type = "composedOf"},
			EpSbiXCP = #specification_ref{id = Sid, href = Shref, name = Name,
					class_type = "ConnectionPointRef", ref_type = Stype},
			#specification{name = "UDSFFunction",
					description = "5G Core Unified Data Storage Function (UDSF)",
					class_type = "ResourceFunctionSpecification",
					status = active,
					version = "1.0",
					category = "Core",
					target_schema = #target_schema_ref{class_type = "UDSFFunction",
							schema = ?PathCatalogSchema ++ "UDSFFunction"},
					characteristic = Chars,
					related = [EpSbiXRel],
					connection_point = [EpSbiXCP]};
		{error, Reason} ->
			error_logger:warning_report(["Error reading resource specification",
					{specification, Name}, {error, Reason}]),
			{error, Reason}
	end.

-spec ngc_nrf() -> specification().
%% @doc 5GC Network Repository Function (NRF) resource function specification.
ngc_nrf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	SBIFqdn = #specification_char{name = "sBIFqdn",
			description = "FQDN of the registered NF instance in service-based interface",
			value_type = "SBIFqdn",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIFqdn"},
	NSIIdList = #specification_char{name = "nSIIdList",
			description = "Set of Network Slice Instance (NSI) dentities (3GPP TS 29.531 clause 6.1.6.2.8)",
			value_type = "NSIIdList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/NSIIdList"},
	NFProfileList = #specification_char{name = "nFProfileList",
			description = "Set of Network Function (NF) Profiles to be registered (3GPP TS 29.510)",
			value_type = "NFProfileList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/NFProfileList"},
	SNSSAIList = #specification_char{name = "snssaiList",
			description = "Single Network Slice Selection Assistance Information (S-NSSAI) (3GPP TS 23.003)",
			value_type = "SnssaiList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/SnssaiList"},
	Chars = [ID, UserLabel, VnfParametersList, PLMNIdList, SBIFqdn,
			NSIIdList, NFProfileList, SNSSAIList],
	SpecNames = ["EP_N27", "EP_SBI_X"],
	Fspecs = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{} = Spec} ->
					[Spec | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ResSpecs = lists:foldl(Fspecs, [], SpecNames),
	Fspecrels = fun(#specification{id = Sid, href = Shref,
				name = Name, class_type = Stype}) ->
			#specification_rel{id = Sid, href = Shref, name = Name,
							ref_type = Stype, rel_type = "composedOf"}
	end,
	ResSpecRels = lists:map(Fspecrels, ResSpecs),
	Fspeccps = fun(#specification{id = CPid, href = CPhref,
				name = CPname, class_type = CPtype}) ->
			#specification_ref{id = CPid, href = CPhref, name = CPname,
					class_type = "ConnectionPointRef", ref_type = CPtype}
	end,
	ResSpecCPs = lists:map(Fspeccps, ResSpecs),
	#specification{name = "NRFFunction",
			description = "5G Core Network Repository Function (NRF)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "NRFFunction",
					schema = ?PathCatalogSchema ++ "NRFFunction"},
			characteristic = Chars,
			related = ResSpecRels,
			connection_point = ResSpecCPs}.

-spec ngc_nssf() -> specification().
%% @doc 5GC Network Slice Selection Function (NSSF) resource function specification.
ngc_nssf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	SBIFqdn = #specification_char{name = "sBIFqdn",
			description = "FQDN of the registered NF instance in service-based interface",
			value_type = "SBIFqdn",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIFqdn"},
	NSIIdList = #specification_char{name = "nSIIdList",
			description = "Set of Network Slice Instance (NSI) dentities (3GPP TS 29.531 clause 6.1.6.2.8)",
			value_type = "NSIIdList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/NSIIdList"},
	NFProfileList = #specification_char{name = "nFProfileList",
			description = "Set of Network Function (NF) Profiles to be registered (3GPP TS 29.510)",
			value_type = "NFProfileList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/NFProfileList"},
	SNSSAIList = #specification_char{name = "snssaiList",
			description = "Single Network Slice Selection Assistance Information (S-NSSAI) (3GPP TS 23.003)",
			value_type = "SnssaiList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/SnssaiList"},
	Chars = [ID, UserLabel, VnfParametersList, PLMNIdList, SBIFqdn,
			NSIIdList, NFProfileList, SNSSAIList],
	SpecNames = ["EP_N22", "EP_N27", "EP_N31", "EP_SBI_X"],
	Fspecs = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{} = Spec} ->
					[Spec | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ResSpecs = lists:foldl(Fspecs, [], SpecNames),
	Fspecrels = fun(#specification{id = Sid, href = Shref,
				name = Name, class_type = Stype}) ->
			#specification_rel{id = Sid, href = Shref, name = Name,
							ref_type = Stype, rel_type = "composedOf"}
	end,
	ResSpecRels = lists:map(Fspecrels, ResSpecs),
	Fspeccps = fun(#specification{id = CPid, href = CPhref,
				name = CPname, class_type = CPtype}) ->
			#specification_ref{id = CPid, href = CPhref, name = CPname,
					class_type = "ConnectionPointRef", ref_type = CPtype}
	end,
	ResSpecCPs = lists:map(Fspeccps, ResSpecs),
	#specification{name = "NSSFFunction",
			description = "5G Core Network Slice Selection Function (NSSF)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "NSSFFunction",
					schema = ?PathCatalogSchema ++ "NSSFFunction"},
			characteristic = Chars,
			related = ResSpecRels,
			connection_point = ResSpecCPs}.

-spec ngc_smsf() -> specification().
%% @doc 5GC Short Message Service Function (SMSF) resource function specification.
ngc_smsf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	SBIFqdn = #specification_char{name = "sBIFqdn",
			description = "FQDN of the registered NF instance in service-based interface",
			value_type = "SBIFqdn",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIFqdn"},
	SBIServiceList = #specification_char{name = "sBIServiceList",
			description = "All supported NF services registered on service-based interface",
			value_type = "SBIServiceList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIServiceList"},
	Chars = [ID, UserLabel, VnfParametersList, PLMNIdList, SBIFqdn,
			SBIServiceList],
	SpecNames = ["EP_N20", "EP_N21", "EP_MAP_SMSC"],
	Fspecs = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{} = Spec} ->
					[Spec | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ResSpecs = lists:foldl(Fspecs, [], SpecNames),
	Fspecrels = fun(#specification{id = Sid, href = Shref,
				name = Name, class_type = Stype}) ->
			#specification_rel{id = Sid, href = Shref, name = Name,
							ref_type = Stype, rel_type = "composedOf"}
	end,
	ResSpecRels = lists:map(Fspecrels, ResSpecs),
	Fspeccps = fun(#specification{id = CPid, href = CPhref,
				name = CPname, class_type = CPtype}) ->
			#specification_ref{id = CPid, href = CPhref, name = CPname,
					class_type = "ConnectionPointRef", ref_type = CPtype}
	end,
	ResSpecCPs = lists:map(Fspeccps, ResSpecs),
	#specification{name = "SMSFFunction",
			description = "5G Core Short Message Service Function (SMSF)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "SMSFFunction",
					schema = ?PathCatalogSchema ++ "SMSFFunction"},
			characteristic = Chars,
			related = ResSpecRels,
			connection_point = ResSpecCPs}.

-spec ngc_lmf() -> specification().
%% @doc 5GC Location Management Function (LMF) resource function specification.
ngc_lmf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	Chars = [ID, UserLabel, VnfParametersList, PLMNIdList],
	Name = "EP_NLS",
	case im:get_specification_name(Name) of
		{ok, #specification{id = Sid, href = Shref, name = Name,
				class_type = Stype}} ->
			EpNlsRel = #specification_rel{id = Sid, href = Shref, name = Name,
					ref_type = Stype, rel_type = "composedOf"},
			EpNlsCP = #specification_ref{id = Sid, href = Shref, name = Name,
					class_type = "ConnectionPointRef", ref_type = Stype},
			#specification{name = "LMFFunction",
					description = "5G Core Location Management Function (LMF)",
					class_type = "ResourceFunctionSpecification",
					status = active,
					version = "1.0",
					category = "Core",
					target_schema = #target_schema_ref{class_type = "LMFFunction",
							schema = ?PathCatalogSchema ++ "LMFFunction"},
					characteristic = Chars,
					related = [EpNlsRel],
					connection_point = [EpNlsCP]};
		{error, Reason} ->
			error_logger:warning_report(["Error reading resource specification",
					{specification, Name}, {error, Reason}]),
			{error, Reason}
	end.

-spec ngc_ngeir() -> specification().
%% @doc 5GC NG Equipment Identity Register (ngEIR) resource function specification.
ngc_ngeir() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	SBIFqdn = #specification_char{name = "sBIFqdn",
			description = "FQDN of the registered NF instance in service-based interface",
			value_type = "SBIFqdn",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIFqdn"},
	SBIServiceList = #specification_char{name = "sBIServiceList",
			description = "All supported NF services registered on service-based interface",
			value_type = "SBIServiceList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIServiceList"},
	SNSSAIList = #specification_char{name = "snssaiList",
			description = "Single Network Slice Selection Assistance Information (S-NSSAI) (3GPP TS 23.003)",
			value_type = "SnssaiList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/SnssaiList"},
	Chars = [ID, UserLabel, VnfParametersList, PLMNIdList, SBIFqdn,
			SBIServiceList, SNSSAIList],
	Name = "EP_N17",
	case im:get_specification_name(Name) of
		{ok, #specification{id = Sid, href = Shref, name = Name,
				class_type = Stype}} ->
			EpN17Rel = #specification_rel{id = Sid, href = Shref, name = Name,
					ref_type = Stype, rel_type = "composedOf"},
			EpN17CP = #specification_ref{id = Sid, href = Shref, name = Name,
					class_type = "ConnectionPointRef", ref_type = Stype},
			#specification{name = "NGEIRFunction",
					description = "5G Core NG Equipment Identity Register (ngEIR)",
					class_type = "ResourceFunctionSpecification",
					status = active,
					version = "1.0",
					category = "Core",
					target_schema = #target_schema_ref{class_type = "NGEIRFunction",
							schema = ?PathCatalogSchema ++ "NGEIRFunction"},
					characteristic = Chars,
					related = [EpN17Rel],
					connection_point = [EpN17CP]};
		{error, Reason} ->
			error_logger:warning_report(["Error reading resource specification",
					{specification, Name}, {error, Reason}]),
			{error, Reason}
	end.

-spec ngc_sepp() -> specification().
%% @doc 5GC Security Edge Protection Proxy (SEPP) resource function specification.
ngc_sepp() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	Chars = [ID, UserLabel, VnfParametersList, PLMNIdList],
	SpecNames = ["EP_N32", "EP_SBI_IPX"],
	Fspecs = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{} = Spec} ->
					[Spec | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	ResSpecs = lists:foldl(Fspecs, [], SpecNames),
	Fspecrels = fun(#specification{id = Sid, href = Shref,
				name = Name, class_type = Stype}) ->
			#specification_rel{id = Sid, href = Shref, name = Name,
							ref_type = Stype, rel_type = "composedOf"}
	end,
	ResSpecRels = lists:map(Fspecrels, ResSpecs),
	Fspeccps = fun(#specification{id = CPid, href = CPhref,
				name = CPname, class_type = CPtype}) ->
			#specification_ref{id = CPid, href = CPhref, name = CPname,
					class_type = "ConnectionPointRef", ref_type = CPtype}
	end,
	ResSpecCPs = lists:map(Fspeccps, ResSpecs),
	#specification{name = "SEPPFunction",
			description = "5G Core Security Edge Protection Proxy (SEPP)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "SEPPFunction",
					schema = ?PathCatalogSchema ++ "SEPPFunction"},
			characteristic = Chars,
			related = ResSpecRels,
			connection_point = ResSpecCPs}.

-spec ngc_nwdaf() -> specification().
%% @doc 5GC Network Data Analytics Function (NWDAF) resource function specification.
ngc_nwdaf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PLMNIdList = #specification_char{name = "plmnIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PlmnIdList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/PlmnIdList"},
	SBIFqdn = #specification_char{name = "sBIFqdn",
			description = "FQDN of the registered NF instance in service-based interface",
			value_type = "SBIFqdn",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIFqdn"},
	SBIServiceList = #specification_char{name = "sBIServiceList",
			description = "All supported NF services registered on service-based interface",
			value_type = "SBIServiceList",
			value_schema = ?PathCatalogSchema ++ "/ngcNrm#/definitions/SBIServiceList"},
	SNSSAIList = #specification_char{name = "snssaiList",
			description = "Single Network Slice Selection Assistance Information (S-NSSAI) (3GPP TS 23.003)",
			value_type = "SnssaiList",
			value_schema = ?PathCatalogSchema ++ "/nrNrm#/definitions/SnssaiList"},
	Chars = [ID, UserLabel, VnfParametersList, PLMNIdList, SBIFqdn,
			SBIServiceList, SNSSAIList],
	Name = "EP_SBI_X",
	case im:get_specification_name(Name) of
		{ok, #specification{id = Sid, href = Shref, name = Name,
				class_type = Stype}} ->
			EpSbiXRel = #specification_rel{id = Sid, href = Shref, name = Name,
					ref_type = Stype, rel_type = "composedOf"},
			EpSbiXCP = #specification_ref{id = Sid, href = Shref, name = Name,
					class_type = "ConnectionPointRef", ref_type = Stype},
			#specification{name = "NWDAFFunction",
					description = "5G Core Network Data Analytics Function (NWDAF)",
					class_type = "ResourceFunctionSpecification",
					status = active,
					version = "1.0",
					category = "Core",
					target_schema = #target_schema_ref{class_type = "NWDAFFunction",
							schema = ?PathCatalogSchema ++ "NWDAFFunction"},
					characteristic = Chars,
					related = [EpSbiXRel],
					connection_point = [EpSbiXCP]};
		{error, Reason} ->
			error_logger:warning_report(["Error reading resource specification",
					{specification, Name}, {error, Reason}])
	end.

-spec ngc_ep_n2() -> specification().
%% @doc 5GC End Point N2 resource specification.
ngc_ep_n2() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N2",
			description = "5G Core End Point of N2 interface (between (R)AN and AMF)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N2",
					schema = ?PathCatalogSchema ++ "EP_N2"},
			characteristic = Chars}.

-spec ngc_ep_n3() -> specification().
%% @doc 5GC End Point N3 resource specification.
ngc_ep_n3() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress],
	#specification{name = "EP_N3",
			description = "5G Core End Point of N3 interface (between (R)AN and UPF)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N3",
					schema = ?PathCatalogSchema ++ "EP_N3"},
			characteristic = Chars}.

-spec ngc_ep_n4() -> specification().
%% @doc 5GC End Point N4 resource specification.
ngc_ep_n4() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N4",
			description = "5G Core End Point of N4 interface (between SMF and UPF)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N4",
					schema = ?PathCatalogSchema ++ "EP_N4"},
			characteristic = Chars}.

-spec ngc_ep_n5() -> specification().
%% @doc 5GC End Point N5 resource specification.
ngc_ep_n5() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N5",
			description = "5G Core End Point of N5 interface (between PCF and AF)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N5",
					schema = ?PathCatalogSchema ++ "EP_N5"},
			characteristic = Chars}.

-spec ngc_ep_n6() -> specification().
%% @doc 5GC End Point N6 resource specification.
ngc_ep_n6() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N6",
			description = "5G Core End Point of N6 interface (between UPF and DN)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N6",
					schema = ?PathCatalogSchema ++ "EP_N6"},
			characteristic = Chars}.

-spec ngc_ep_n7() -> specification().
%% @doc 5GC End Point N7 resource specification.
ngc_ep_n7() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N7",
			description = "5G Core End Point of N7 interface (between SMF and PCF)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N7",
					schema = ?PathCatalogSchema ++ "EP_N7"},
			characteristic = Chars}.

-spec ngc_ep_n8() -> specification().
%% @doc 5GC End Point N8 resource specification.
ngc_ep_n8() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N8",
			description = "5G Core End Point of N8 interface (between AMF and UDM)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N8",
					schema = ?PathCatalogSchema ++ "EP_N8"},
			characteristic = Chars}.

-spec ngc_ep_n9() -> specification().
%% @doc 5GC End Point N9 resource specification.
ngc_ep_n9() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress],
	#specification{name = "EP_N9",
			description = "5G Core End Point of N9 interface (between two UPFs)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N9",
					schema = ?PathCatalogSchema ++ "EP_N9"},
			characteristic = Chars}.

-spec ngc_ep_n10() -> specification().
%% @doc 5GC End Point N10 resource specification.
ngc_ep_n10() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N10",
			description = "5G Core End Point of N10 interface (between SMF and UDM)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N10",
					schema = ?PathCatalogSchema ++ "EP_N10"},
			characteristic = Chars}.

-spec ngc_ep_n11() -> specification().
%% @doc 5GC End Point N11 resource specification.
ngc_ep_n11() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N11",
			description = "5G Core End Point of N11 interface (between AMF and SMF)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N11",
					schema = ?PathCatalogSchema ++ "EP_N11"},
			characteristic = Chars}.

-spec ngc_ep_n12() -> specification().
%% @doc 5GC End Point N12 resource specification.
ngc_ep_n12() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N12",
			description = "5G Core End Point of N12 interface (between AMF and AUSF)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N12",
					schema = ?PathCatalogSchema ++ "EP_N12"},
			characteristic = Chars}.

-spec ngc_ep_n13() -> specification().
%% @doc 5GC End Point N13 resource specification.
ngc_ep_n13() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N13",
			description = "5G Core End Point of N13 interface (between AUSF and UDM)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N13",
					schema = ?PathCatalogSchema ++ "EP_N13"},
			characteristic = Chars}.

-spec ngc_ep_n14() -> specification().
%% @doc 5GC End Point N14 resource specification.
ngc_ep_n14() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N14",
			description = "5G Core End Point of N14 interface (between two AMFs)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N14",
					schema = ?PathCatalogSchema ++ "EP_N14"},
			characteristic = Chars}.

-spec ngc_ep_n15() -> specification().
%% @doc 5GC End Point N15 resource specification.
ngc_ep_n15() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N15",
			description = "5G Core End Point of N15 interface (between AMF and PCF)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N15",
					schema = ?PathCatalogSchema ++ "EP_N15"},
			characteristic = Chars}.

-spec ngc_ep_n16() -> specification().
%% @doc 5GC End Point N16 resource specification.
ngc_ep_n16() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N16",
			description = "5G Core End Point of N16 interface (between two SMFs)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N16",
					schema = ?PathCatalogSchema ++ "EP_N16"},
			characteristic = Chars}.

-spec ngc_ep_n17() -> specification().
%% @doc 5GC End Point N17 resource specification.
ngc_ep_n17() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N17",
			description = "5G Core End Point of N17 interface (between AMF and 5G-EIR)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N17",
					schema = ?PathCatalogSchema ++ "EP_N17"},
			characteristic = Chars}.

-spec ngc_ep_n20() -> specification().
%% @doc 5GC End Point N20 resource specification.
ngc_ep_n20() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N20",
			description = "5G Core End Point of N20 interface (between AMF and SMSF)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N20",
					schema = ?PathCatalogSchema ++ "EP_N20"},
			characteristic = Chars}.

-spec ngc_ep_n21() -> specification().
%% @doc 5GC End Point N21 resource specification.
ngc_ep_n21() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N21",
			description = "5G Core End Point of N21 interface (between SMSF and UDM)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N21",
					schema = ?PathCatalogSchema ++ "EP_N21"},
			characteristic = Chars}.

-spec ngc_ep_n22() -> specification().
%% @doc 5GC End Point N22 resource specification.
ngc_ep_n22() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N22",
			description = "5G Core End Point of N22 interface (between AMF and NSSF)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N22",
					schema = ?PathCatalogSchema ++ "EP_N22"},
			characteristic = Chars}.

-spec ngc_ep_n26() -> specification().
%% @doc 5GC End Point N26 resource specification.
ngc_ep_n26() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N26",
			description = "5G Core End Point of N26 interface (between AMF and MME)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N26",
					schema = ?PathCatalogSchema ++ "EP_N26"},
			characteristic = Chars}.

-spec ngc_ep_n27() -> specification().
%% @doc 5GC End Point N27 resource specification.
ngc_ep_n27() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N27",
			description = "5G Core End Point of N27 interface"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N27",
					schema = ?PathCatalogSchema ++ "EP_N27"},
			characteristic = Chars}.

-spec ngc_ep_n31() -> specification().
%% @doc 5GC End Point N31 resource specification.
ngc_ep_n31() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N31",
			description = "5G Core End Point of N31 interface"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N31",
					schema = ?PathCatalogSchema ++ "EP_N31"},
			characteristic = Chars}.

-spec ngc_ep_n32() -> specification().
%% @doc 5GC End Point N32 resource specification.
ngc_ep_n32() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_N32",
			description = "5G Core End Point of N32 interface"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_N32",
					schema = ?PathCatalogSchema ++ "EP_N32"},
			characteristic = Chars}.

-spec ngc_ep_nls() -> specification().
%% @doc 5GC End Point NLS resource specification.
ngc_ep_nls() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_NLS",
			description = "5G Core End Point of NLs interface (between AMF and LMF)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_NLS",
					schema = ?PathCatalogSchema ++ "EP_NLS"},
			characteristic = Chars}.

-spec ngc_ep_nlg() -> specification().
%% @doc 5GC End Point NLG resource specification.
ngc_ep_nlg() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_NLG",
			description = "5G Core End Point of NLg interface (between AMF and GMLC)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_NLG",
					schema = ?PathCatalogSchema ++ "EP_NLG"},
			characteristic = Chars}.

-spec ngc_ep_sbi_x() -> specification().
%% @doc 5GC End Point Service Based Interface X resource specification.
ngc_ep_sbi_x() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_SBI_X",
			description = "5G Core End Point (EP) of Service Based Interface (SBI) X"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_SBI_X",
					schema = ?PathCatalogSchema ++ "EP_SBI_X"},
			characteristic = Chars}.

-spec ngc_ep_sbi_ipx() -> specification().
%% @doc 5GC End Point Service Based Interface IPX resource specification.
ngc_ep_sbi_ipx() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_SBI_IPX",
			description = "5G Core End Point (EP) of Service Based Interface (SBI) IPX"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_SBI_IPX",
					schema = ?PathCatalogSchema ++ "EP_SBI_IPX"},
			characteristic = Chars}.

-spec ngc_ep_s5c() -> specification().
%% @doc 5GC End Point S5-C resource specification.
ngc_ep_s5c() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_S5C",
			description = "5G Core End Point (EP) of S5-C interface"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_S5C",
					schema = ?PathCatalogSchema ++ "EP_S5C"},
			characteristic = Chars}.

-spec ngc_ep_s5u() -> specification().
%% @doc 5GC End Point S5-U resource specification.
ngc_ep_s5u() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress],
	#specification{name = "EP_S5U",
			description = "5G Core End Point (EP) of S5-U interface"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_S5U",
					schema = ?PathCatalogSchema ++ "EP_S5U"},
			characteristic = Chars}.

-spec ngc_ep_rx() -> specification().
%% @doc 5GC End Point Rx resource specification.
ngc_ep_rx() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_Rx",
			description = "5G Core End Point of Rx interface (between PCF and AF)"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_Rx",
					schema = ?PathCatalogSchema ++ "EP_Rx"},
			characteristic = Chars}.

-spec ngc_ep_map_smsc() -> specification().
%% @doc 5GC End Point MAP SMSC resource specification.
ngc_ep_map_smsc() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	LocalAddress = #specification_char{name = "localAddress",
			description = "Includes IP address and VLAN ID used for initialization"
					"of the underlying transport",
			value_type = "string"},
	RemoteAddress = #specification_char{name = "remoteAddress",
			description = "IP address used for initialization of the"
					"underlying transport",
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, LocalAddress, RemoteAddress],
	#specification{name = "EP_MAP_SMSC",
			description = "5G Core End Point of MAP interface"
					"resource specification",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EP_MAP_SMSC",
					schema = ?PathCatalogSchema ++ "EP_MAP_SMSC"},
			characteristic = Chars}.

-spec epc_sgw() -> specification().
%% @doc EPC Serving Gateway (SGW) resource specification.
epc_sgw() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	PLMNIdList = #specification_char{name = "pLMNIdList",
			description = "List of PLMN-Id: Mobile Country Codes (MCC) or Mobile Network Codes(MNC) (3GPP 23.003)",
			value_type = "PLMNIdList",
			value_schema = ?PathCatalogSchema ++ "/epcNrm#/definitions/PLMNIdList"},
	TACList = #specification_char{name = "tACList",
			description = "List of TAC of MMEPoolArea used for traffic handling (3GPP 36.413)",
			value_type = "TACList",
			value_schema = ?PathCatalogSchema ++ "/epcNrm#/definitions/TACList"},
	Chars = [ID, UserLabel, VnfParametersList, PLMNIdList, TACList],
	{#specification_rel{} = EpRpEpsRel,
			#specification_ref{} = EpRpEpsCP} = eprpeps_refs(),
	#specification{name = "ServingGWFunction",
			description = "EPC Serving Gateway (SGW)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "EPC",
			target_schema = #target_schema_ref{class_type = "ServingGWFunction",
					schema = ?PathCatalogSchema ++ "ServingGWFunction"},
			characteristic = Chars,
			related = [EpRpEpsRel],
			connection_point = [EpRpEpsCP]}.

-spec epc_pgw() -> specification().
%% @doc EPC PDN Gateway (PGW) resource specification.
epc_pgw() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	Chars = [ID, UserLabel, VnfParametersList],
	{#specification_rel{} = EpRpEpsRel,
			#specification_ref{} = EpRpEpsCP} = eprpeps_refs(),
	#specification{name = "PGWFunction",
			description = "EPC PDN Gateway (PGW)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "EPC",
			target_schema = #target_schema_ref{class_type = "PGWFunction",
					schema = ?PathCatalogSchema ++ "PGWFunction"},
			characteristic = Chars,
			related = [EpRpEpsRel],
			connection_point = [EpRpEpsCP]}.

-spec epc_epdg() -> specification().
%% @doc EPC evolved Packet Data Gateway (ePDG) resource specification.
epc_epdg() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	LinkList = #specification_char{name = "linkList",
			description = "",
			value_type = "LinkListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/LinkListType"},
	Chars = [ID, UserLabel, VnfParametersList, LinkList],
	{#specification_rel{} = EpRpEpsRel,
			#specification_ref{} = EpRpEpsCP} = eprpeps_refs(),
	#specification{name = "EPDGFunction",
			description = "EPC evolved Packet Data Gateway (ePDG)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "EPC",
			target_schema = #target_schema_ref{class_type = "EPDGFunction",
					schema = ?PathCatalogSchema ++ "EPDGFunction"},
			characteristic = Chars,
			related = [EpRpEpsRel],
			connection_point = [EpRpEpsCP]}.

-spec epc_mme() -> specification().
%% @doc EPC Mobility Management Entity (MME) resource specification.
epc_mme() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	MMEC = #specification_char{name = "mMEC",
			description = "List of TAC of MMEPoolArea used for traffic handling (3GPP 36.413)",
			value_type = "integer",
			char_value = [#spec_char_value{from = -9223372036854775808, to = 9223372036854775808}]},
	MMEPool = #specification_char{name = "mMEPool",
			description = "DN of a MMEPool instance",
			value_type = "dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dn"},
	Chars = [ID, UserLabel, VnfParametersList, MMEC, MMEPool],
	{#specification_rel{} = EpRpEpsRel,
			#specification_ref{} = EpRpEpsCP} = eprpeps_refs(),
	#specification{name = "MMEFunction",
			description = "EPC Mobility Management Entity (MME)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "EPC",
			target_schema = #target_schema_ref{class_type = "MMEFunction",
					schema = ?PathCatalogSchema ++ "MMEFunction"},
			characteristic = Chars,
			related = [EpRpEpsRel],
			connection_point = [EpRpEpsCP]}.

-spec epc_pcrf() -> specification().
%% @doc EPC Policy Control Rules Function (PCRF) resource specification.
epc_pcrf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	LinkList = #specification_char{name = "linkList",
			description = "List of communication link or reference point to other network entities",
			value_type = "linkListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/linkListType"},
	Chars = [ID, UserLabel, VnfParametersList, LinkList],
	{#specification_rel{} = EpRpEpsRel,
			#specification_ref{} = EpRpEpsCP} = eprpeps_refs(),
	#specification{name = "PCRFFunction",
			description = "EPC Policy Control Rules Function (PCRF)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "EPC",
			target_schema = #target_schema_ref{class_type = "PCRFFunction",
					schema = ?PathCatalogSchema ++ "PCRFFunction"},
			characteristic = Chars,
			related = [EpRpEpsRel],
			connection_point = [EpRpEpsCP]}.

-spec epc_link_mme_mme() -> specification().
%% @doc EPC MMEFunction and MMEFunction Link resource specification.
epc_link_mme_mme() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	AEnd = #specification_char{name = "aEnd",
			description = [],
			value_type = "string"},
	ZEnd = #specification_char{name = "zEnd",
			description = [],
			value_type = "string"},
	LinkType = #specification_char{name = "linkType",
			description = [],
			value_type = "LinkType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/LinkType"},
	ProtocolName = #specification_char{name = "protocolName",
			description = [],
			value_type = "string"},
	ProtocolVersion = #specification_char{name = "protocolVersion",
			description = [],
			value_type = "string"},
	Chars = [ID, UserLabel, AEnd, ZEnd, LinkType, ProtocolName, ProtocolVersion],
	#specification{name = "Link_MME_MME",
			description = "EPC MMEFunction and MMEFunction Link",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "EPC",
			target_schema = #target_schema_ref{class_type = "Link_MME_MME",
					schema = ?PathCatalogSchema ++ "Link_MME_MME"},
			characteristic = Chars}.

-spec epc_link_hss_mme() -> specification().
%% @doc EPC HSSFunction and MMEFunction Link resource specification.
epc_link_hss_mme() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	AEnd = #specification_char{name = "aEnd",
			description = [],
			value_type = "string"},
	ZEnd = #specification_char{name = "zEnd",
			description = [],
			value_type = "string"},
	LinkType = #specification_char{name = "linkType",
			description = [],
			value_type = "LinkType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/LinkType"},
	ProtocolName = #specification_char{name = "protocolName",
			description = [],
			value_type = "string"},
	ProtocolVersion = #specification_char{name = "protocolVersion",
			description = [],
			value_type = "string"},
	Chars = [ID, UserLabel, AEnd, ZEnd, LinkType, ProtocolName, ProtocolVersion],
	#specification{name = "Link_HSS_MME",
			description = "EPC HSSFunction and MMEFunction Link",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "EPC",
			target_schema = #target_schema_ref{class_type = "Link_HSS_MME",
					schema = ?PathCatalogSchema ++ "Link_HSS_MME"},
			characteristic = Chars}.

-spec epc_link_mme_sgsn() -> specification().
%% @doc EPC MMEFunction and SgsnFunction Link resource specification.
epc_link_mme_sgsn() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	AEnd = #specification_char{name = "aEnd",
			description = [],
			value_type = "string"},
	ZEnd = #specification_char{name = "zEnd",
			description = [],
			value_type = "string"},
	LinkType = #specification_char{name = "linkType",
			description = [],
			value_type = "LinkType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/LinkType"},
	ProtocolName = #specification_char{name = "protocolName",
			description = [],
			value_type = "string"},
	ProtocolVersion = #specification_char{name = "protocolVersion",
			description = [],
			value_type = "string"},
	Chars = [ID, UserLabel, AEnd, ZEnd, LinkType, ProtocolName, ProtocolVersion],
	#specification{name = "Link_MME_SGSN",
			description = "EPC MMEFunction and SgsnFunction Link",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "EPC",
			target_schema = #target_schema_ref{class_type = "Link_MME_SGSN",
					schema = ?PathCatalogSchema ++ "Link_MME_SGSN"},
			characteristic = Chars}.

-spec epc_link_mme_servinggw() -> specification().
%% @doc EPC MMEFunction and ServingGWFunction Link resource specification.
epc_link_mme_servinggw() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	AEnd = #specification_char{name = "aEnd",
			description = [],
			value_type = "string"},
	ZEnd = #specification_char{name = "zEnd",
			description = [],
			value_type = "string"},
	LinkType = #specification_char{name = "linkType",
			description = [],
			value_type = "LinkType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/LinkType"},
	ProtocolName = #specification_char{name = "protocolName",
			description = [],
			value_type = "string"},
	ProtocolVersion = #specification_char{name = "protocolVersion",
			description = [],
			value_type = "string"},
	Chars = [ID, UserLabel, AEnd, ZEnd, LinkType, ProtocolName, ProtocolVersion],
	#specification{name = "Link_MME_ServingGW",
			description = "EPC MMEFunction and ServingGWFunction Link",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "EPC",
			target_schema = #target_schema_ref{class_type = "Link_MME_ServingGW",
					schema = ?PathCatalogSchema ++ "Link_MME_ServingGW"},
			characteristic = Chars}.

-spec epc_link_enb_mme() -> specification().
%% @doc EPC MMEFunction and MMEFunction Link resource specification.
epc_link_enb_mme() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	AEnd = #specification_char{name = "aEnd",
			description = [],
			value_type = "string"},
	ZEnd = #specification_char{name = "zEnd",
			description = [],
			value_type = "string"},
	LinkType = #specification_char{name = "linkType",
			description = [],
			value_type = "LinkType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/LinkType"},
	ProtocolName = #specification_char{name = "protocolName",
			description = [],
			value_type = "string"},
	ProtocolVersion = #specification_char{name = "protocolVersion",
			description = [],
			value_type = "string"},
	Chars = [ID, UserLabel, AEnd, ZEnd, LinkType, ProtocolName, ProtocolVersion],
	#specification{name = "Link_ENB_MME",
			description = "EPC MMEFunction and ENBFunction Link",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "EPC",
			target_schema = #target_schema_ref{class_type = "Link_ENB_MME",
					schema = ?PathCatalogSchema ++ "Link_ENB_MME"},
			characteristic = Chars}.

-spec epc_ep_rp_eps() -> specification().
%% @doc EPC End Point (EP) of Reference Point (RP) in Evolved Packet System (EPS) resource specification.
epc_ep_rp_eps() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	FarEndEntity = #specification_char{name = "farEndEntity",
			description = [],
			value_type = "string"},
	FarEndNeIpAddr = #specification_char{name = "farEndNeIpAddr",
			description = [],
			value_type = "string"},
	Chars = [ID, UserLabel, FarEndEntity, FarEndNeIpAddr],
	#specification{name = "EP_RP_EPS",
			description = "EPC End Point (EP) of Reference Point (RP) in Evolved Packet System (EPS)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "EPC",
			target_schema = #target_schema_ref{class_type = "EP_RP_EPS",
					schema = ?PathCatalogSchema ++ "EP_RP_EPS"},
			characteristic = Chars}.

-spec core_msc() -> specification().
%% @doc Core Mobile Switching Center (MSC) Server resource specification.
core_msc() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	MccList = #specification_char{name = "mccList",
			description = "List of Mobile Country Codes (MCC) (3GPP 23.003)",
			value_type = "longList",
			value_schema = ?PathCatalogSchema ++ "/coreNrm#/definitions/longList"},
	MncList = #specification_char{name = "mncList",
			description = "List of Mobile Network Codes (MNC) (3GPP 23.003)",
			value_type = "longList",
			value_schema = ?PathCatalogSchema ++ "/coreNrm#/definitions/longList"},
	LacList = #specification_char{name = "lacList",
			description = "List of Location Area Codes (LAC) (3GPP 23.003)",
			value_type = "longList",
			value_schema = ?PathCatalogSchema ++ "/coreNrm#/definitions/longList"},
	SacList = #specification_char{name = "sacList",
			description = "List of Service Area Codes (SAC) (3GPP 23.003)",
			value_type = "longList",
			value_schema = ?PathCatalogSchema ++ "/coreNrm#/definitions/longList"},
	GcaList = #specification_char{name = "gcaList",
			description = "List of Group Call Area (GCA) Codes (3GPP 23.003)",
			value_type = "longList",
			value_schema = ?PathCatalogSchema ++ "/coreNrm#/definitions/longList"},
	MscId  = #specification_char{name = "mscId",
			description = "Unique MSC ID (3GPP 23.002)",
			value_type = "integer"},
	MscServerFunctionGsmCell = #specification_char{name = "mscServerFunctionGsmCell",
			description = "This holds a set of DNs of GsmCell",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	MscServerFunctionExternalGsmCell = #specification_char{name = "mscServerFunctionExternalGsmCell",
			description = "This holds a set of DNs of ExternalGsmCell",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	MscServerFunctionCsMgwFunction = #specification_char{name = "mscServerFunctionCsMgwFunction",
			description = "This holds a set of DNs of CsMgwFunction",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	NriList = #specification_char{name = "nriList",
			description = "NRI shall be part of the TMSI (3GPP 23.003)",
			value_type = "longList",
			value_schema = ?PathCatalogSchema ++ "/coreNrm#/definitions/longList"},
	MscServerFunctionMscPool = #specification_char{name = "mscServerFunctionMscPool",
			description = "This holds a set of DNs of MscPool",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	DefaultMsc = #specification_char{name = "defaultMsc",
			description = "Whether this MSC-Server is default CN node in MscPool or not (3GPP 23.236)",
			value_type = "defaultMscType",
			value_schema = ?PathCatalogSchema ++ "/coreNrm#/definitions/defaultMscType"},
	Chars = [ID, UserLabel, VnfParametersList, MccList, MncList, LacList,
			SacList, GcaList, MscId, MscServerFunctionGsmCell,
			MscServerFunctionExternalGsmCell, MscServerFunctionCsMgwFunction,
			NriList, MscServerFunctionMscPool, DefaultMsc],
	#specification{name = "MscServerFunction",
			description = "Core Mobile Switching Center (MSC) Server",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "MscServerFunction",
					schema = ?PathCatalogSchema ++ "MscServerFunction"},
			characteristic = Chars,
			related = specification_rel(["IucsLink", "ALink"])}.

-spec core_mgw() -> specification().
%% @doc Core Circuit Switched (CS) Media Gateway (MGW) resource specification.
core_mgw() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	CsMgwFunctionMscServerFunction = #specification_char{name = "csMgwFunctionMscServerFunction",
			description = "This holds the DN of an MscServerFunction",
			value_type = "dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dn"},
	CsMgwFunctionIucsLink = #specification_char{name = "csMgwFunctionIucsLink",
			description = "This holds a set of DNs of IucsLink",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	CsMgwFunctionALink = #specification_char{name = "csMgwFunctionALink",
			description = "This holds a set of DNs of ALink",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	Chars = [ID, UserLabel, VnfParametersList, CsMgwFunctionMscServerFunction,
			CsMgwFunctionIucsLink, CsMgwFunctionALink],
	#specification{name = "CsMgwFunction",
			description = "Core Circuit Switched (CS) Media Gateway (MGW)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "CsMgwFunction",
					schema = ?PathCatalogSchema ++ "CsMgwFunction"},
			characteristic = Chars}.

-spec core_sgsn() -> specification().
%% @doc Core Serving GPRS Support Node (SGSN) resource specification.
core_sgsn() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	MccList = #specification_char{name = "mccList",
			description = "List of Mobile Country Codes (MCC) (3GPP 23.003)",
			value_type = "longList",
			value_schema = ?PathCatalogSchema ++ "/coreNrm#/definitions/longList"},
	MncList = #specification_char{name = "mncList",
			description = "List of Mobile Network Codes (MNC) (3GPP 23.003)",
			value_type = "longList",
			value_schema = ?PathCatalogSchema ++ "/coreNrm#/definitions/longList"},
	LacList = #specification_char{name = "lacList",
			description = "List of Location Area Codes (LAC) (3GPP 23.003)",
			value_type = "longList",
			value_schema = ?PathCatalogSchema ++ "/coreNrm#/definitions/longList"},
	SacList = #specification_char{name = "sacList",
			description = "List of Service Area Codes (SAC) (3GPP 23.003)",
			value_type = "longList",
			value_schema = ?PathCatalogSchema ++ "/coreNrm#/definitions/longList"},
	SgsnId = #specification_char{name = "sgsnId",
			description = "Unique SGSN ID (3GPP 23.002)",
			value_type = "integer"},
	SgsnFunctionGsmCell = #specification_char{name = "sgsnFunctionGsmCell",
			description = "This holds a set of DNs of GsmCell",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	SgsnFunctionExternalGsmCell = #specification_char{name = "sgsnFunctionExternalGsmCell",
			description = "This holds a set of DNs of ExternalGsmCell",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	SgsnFunctionMscPool = #specification_char{name = "sgsnFunctionMscPool",
			description = "This holds a set of DNs of SgsnPool",
			value_type = "dnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/dnList"},
	NriList = #specification_char{name = "nriList",
			description = "NRI shall be part of the TMSI (3GPP 23.003)",
			value_type = "longList",
			value_schema = ?PathCatalogSchema ++ "/coreNrm#/definitions/longList"},
	ProceduralStatus = #specification_char{name = "proceduralStatus",
			description = "Procedural status of the object instance (ITU-T X.731)",
			value_type = "proceduralStatus",
			value_schema = ?PathCatalogSchema ++ "/statemanagementIRP#/definitions/proceduralStatus"},
	Chars = [ID, UserLabel, VnfParametersList, MccList, MncList, LacList,
			SacList, SgsnId, SgsnFunctionGsmCell, SgsnFunctionExternalGsmCell,
			SgsnFunctionMscPool, NriList, ProceduralStatus],
	#specification{name = "SgsnFunction",
			description = "Core Serving GPRS Support Node (SGSN)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "SgsnFunction",
					schema = ?PathCatalogSchema ++ "SgsnFunction"},
			characteristic = Chars,
			related = specification_rel(["GbLink", "IupsLink"])}.

-spec core_ggsn() -> specification().
%% @doc Core Gateway GPRS Support Node (GGSN) resource specification.
core_ggsn() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	ProceduralStatus = #specification_char{name = "proceduralStatus",
			description = "Procedural status of the object instance (ITU-T X.731)",
			value_type = "proceduralStatus",
			value_schema = ?PathCatalogSchema ++ "/statemanagementIRP#/definitions/proceduralStatus"},
	Chars = [ID, UserLabel, VnfParametersList, ProceduralStatus],
	#specification{name = "GgsnFunction",
			description = "Core Gateway GPRS Support Node (GGSN)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "GgsnFunction",
					schema = ?PathCatalogSchema ++ "GgsnFunction"},
			characteristic = Chars}.

-spec core_auc() -> specification().
%% @doc Core Authentication Center (AUC) resource specification.
core_auc() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	Chars = [ID, UserLabel, VnfParametersList],
	#specification{name = "AucFunction",
			description = "Core Authentication Center (AUC)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "AucFunction",
					schema = ?PathCatalogSchema ++ "AucFunction"},
			characteristic = Chars}.

-spec core_hlr() -> specification().
%% @doc Core Home Location Register (HLR) resource specification.
core_hlr() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	Chars = [ID, UserLabel, VnfParametersList],
	#specification{name = "HlrFunction",
			description = "Core Home Location Register (HLR)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "HlrFunction",
					schema = ?PathCatalogSchema ++ "HlrFunction"},
			characteristic = Chars}.

-spec core_eir() -> specification().
%% @doc Core Equipment Identity Register (EIR) resource specification.
core_eir() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	Chars = [ID, UserLabel, VnfParametersList],
	#specification{name = "EirFunction",
			description = "Core Equipment Identity Register (EIR)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "EirFunction",
					schema = ?PathCatalogSchema ++ "EirFunction"},
			characteristic = Chars}.

-spec core_mnpsrf() -> specification().
%% @doc Core Mobile Number Portability-Signaling Relay Function (MNP-SRF) resource specification.
core_mnpsrf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	Chars = [ID, UserLabel, VnfParametersList],
	#specification{name = "MnpSrfFunction",
			description = "Core Mobile Number Portability-Signaling Relay Function (MNP-SRF)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "MnpSrfFunction",
					schema = ?PathCatalogSchema ++ "MnpSrfFunction"},
			characteristic = Chars}.

-spec core_cgf() -> specification().
%% @doc Core Charging Gateway Function (CGF) resource specification.
core_cgf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	Chars = [ID, UserLabel, VnfParametersList],
	#specification{name = "CgfFunction",
			description = "Core Charging Gateway Function (CGF)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "CgfFunction",
					schema = ?PathCatalogSchema ++ "CgfFunction"},
			characteristic = Chars}.

-spec core_sgw() -> specification().
%% @doc Core Signalling Gateway (Sgw) resource specification.
core_sgw() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	Chars = [ID, UserLabel, VnfParametersList],
	#specification{name = "SgwFunction",
			description = "Core Signalling Gateway (Sgw)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "SgwFunction",
					schema = ?PathCatalogSchema ++ "SgwFunction"},
			characteristic = Chars}.

-spec core_cbc() -> specification().
%% @doc Core Cell Broadcast Centre (CBC) resource specification.
core_cbc() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	Chars = [ID, UserLabel, VnfParametersList],
	#specification{name = "CbcFunction",
			description = "Core Cell Broadcast Centre (CBC)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "CbcFunction",
					schema = ?PathCatalogSchema ++ "CbcFunction"},
			characteristic = Chars,
			related = specification_rel(["IubcLink"])}.

-spec core_iucs() -> specification().
%% @doc Core Iu-cs Interface Link resource specification.
core_iucs() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	ConnectedRnc = #specification_char{name = "connectedRnc",
			description = "Holds the DN of an RncFunction or an ExternalRncFunction",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	ConnectedBss = #specification_char{name = "connectedBss",
			description = "Holds the DN of an BssFunction or an ExternalBssFunction",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	ConnectedHNBGW = #specification_char{name = "connectedHNBGW",
			description = "Holds the DN of an HNBGWFunction",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	Chars = [ID, UserLabel, VnfParametersList, ConnectedRnc,
			ConnectedBss, ConnectedHNBGW],
	#specification{name = "IucsLink",
			description = "Core Iu-cs interface link",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "IucsLink",
					schema = ?PathCatalogSchema ++ "IucsLink"},
			characteristic = Chars}.

-spec core_iups() -> specification().
%% @doc Core Iu-ps Interface Link resource specification.
core_iups() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	ConnectedRnc = #specification_char{name = "connectedRnc",
			description = "Holds the DN of an RncFunction or an ExternalRncFunction",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	ConnectedBss = #specification_char{name = "connectedBss",
			description = "Holds the DN of an BssFunction or an ExternalBssFunction",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	ConnectedHNBGW = #specification_char{name = "connectedHNBGW",
			description = "Holds the DN of an HNBGWFunction",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	Chars = [ID, UserLabel, VnfParametersList, ConnectedRnc,
			ConnectedBss, ConnectedHNBGW],
	#specification{name = "IupsLink",
			description = "Core Iu-ps interface link",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "IupsLink",
					schema = ?PathCatalogSchema ++ "IupsLink"},
			characteristic = Chars}.

-spec core_iubc() -> specification().
%% @doc Core Iu-bc Interface Link resource specification.
core_iubc() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	ConnectedRnc = #specification_char{name = "connectedRnc",
			description = "Holds the DN of an RncFunction or an ExternalRncFunction",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	ConnectedHNBGW = #specification_char{name = "connectedHNBGW",
			description = "Holds the DN of an HNBGWFunction",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	Chars = [ID, UserLabel, VnfParametersList, ConnectedRnc, ConnectedHNBGW],
	#specification{name = "IubcLink",
			description = "Core Iu-bc interface link",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "IubcLink",
					schema = ?PathCatalogSchema ++ "IubcLink"},
			characteristic = Chars}.

-spec core_alink() -> specification().
%% @doc Core A Interface Link resource specification.
core_alink() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	ConnectedBss = #specification_char{name = "connectedBss",
			description = "Holds the DN of an BssFunction or an ExternalBssFunction",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	Chars = [ID, UserLabel, VnfParametersList, ConnectedBss],
	#specification{name = "ALink",
			description = "Core A interface link",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "ALink",
					schema = ?PathCatalogSchema ++ "ALink"},
			characteristic = Chars}.

-spec core_gb_link() -> specification().
%% @doc Core Gb Interface Link resource specification.
core_gb_link() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	ConnectedBss = #specification_char{name = "connectedBss",
			description = "Holds the DN of an BssFunction or an ExternalBssFunction",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	Chars = [ID, UserLabel, VnfParametersList, ConnectedBss],
	#specification{name = "GbLink",
			description = "Core Gb interface link",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "GbLink",
					schema = ?PathCatalogSchema ++ "GbLink"},
			characteristic = Chars}.

-spec ims_as() -> specification().
%% @doc IMS Application Server (AS) resource specification.
ims_as() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	LinkList = #specification_char{name = "linkList",
			description = "List of related link object distiguished names (DN)",
			value_type = "linkList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/linkList"},
	ContainedNrmClass = #specification_char{name = "ASFunctionOptionallyContainedNrmClass",
			description = "List of optionally contained NRM Class objects",
			value_type = "ASFunctionOptionallyContainedNrmClassList",
			value_schema = ?PathCatalogSchema ++ "/imsNrm#/definitions/ASFunctionOptionallyContainedNrmClassList"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VsDataContainerList"},
	Chars = [ID, UserLabel, VnfParametersList, LinkList, ContainedNrmClass, VsDataContainer],
	#specification{name = "ASFunction",
			description = "IMS Application Server (AS)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "IMS",
			target_schema = #target_schema_ref{class_type = "ASFunction",
					schema = ?PathCatalogSchema ++ "ASFunction"},
			characteristic = Chars}.

-spec ims_hss() -> specification().
%% @doc IMS Home Subscriber Server (HSS) resource specification.
ims_hss() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	LinkList = #specification_char{name = "linkList",
			description = "List of related link object distiguished names (DN)",
			value_type = "linkList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/linkList"},
	ContainedNrmClass = #specification_char{name = "HSSFunctionOptionallyContainedNrmClass",
			description = "List of optionally contained NRM Class objects",
			value_type = "HSSFunctionOptionallyContainedNrmClassList",
			value_schema = ?PathCatalogSchema ++ "/imsNrm#/definitions/HSSFunctionOptionallyContainedNrmClassList"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VsDataContainerList"},
	Chars = [ID, UserLabel, VnfParametersList, LinkList, ContainedNrmClass, VsDataContainer],
	#specification{name = "HSSFunction",
			description = "IMS Home Subscriber Server (HSS)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "IMS",
			target_schema = #target_schema_ref{class_type = "HSSFunction",
					schema = ?PathCatalogSchema ++ "HSSFunction"},
			characteristic = Chars}.

-spec ims_pcscf() -> specification().
%% @doc IMS Proxy Call Session Control Function (P-CSCF) resource specification.
ims_pcscf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	LinkList = #specification_char{name = "linkList",
			description = "List of related link object distiguished names (DN)",
			value_type = "linkList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/linkList"},
	ContainedNrmClass = #specification_char{name = "PCSCFFunctionOptionallyContainedNrmClass",
			description = "List of optionally contained NRM Class objects",
			value_type = "PCSCFFunctionOptionallyContainedNrmClassList",
			value_schema = ?PathCatalogSchema ++ "/imsNrm#/definitions/PCSCFFunctionOptionallyContainedNrmClassList"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VsDataContainerList"},
	Chars = [ID, UserLabel, VnfParametersList, LinkList, ContainedNrmClass, VsDataContainer],
	#specification{name = "PCSCFFunction",
			description = "IMS Proxy Call Session Control Function (P-CSCF)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "IMS",
			target_schema = #target_schema_ref{class_type = "PCSCFFunction",
					schema = ?PathCatalogSchema ++ "PCSCFFunction"},
			characteristic = Chars}.

-spec ims_scscf() -> specification().
%% @doc  IMS Serving Call Session Control Function (S-CSCF) resource specification.
ims_scscf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	LinkList = #specification_char{name = "linkList",
			description = "List of related link object distiguished names (DN)",
			value_type = "linkList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/linkList"},
	ContainedNrmClass = #specification_char{name = "SCSCFFunctionOptionallyContainedNrmClass",
			description = "List of optionally contained NRM Class objects",
			value_type = "SCSCFFunctionOptionallyContainedNrmClassList",
			value_schema = ?PathCatalogSchema ++ "/imsNrm#/definitions/SCSCFFunctionOptionallyContainedNrmClassList"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VsDataContainerList"},
	Chars = [ID, UserLabel, VnfParametersList, LinkList, ContainedNrmClass, VsDataContainer],
	#specification{name = "SCSCFFunction",
			description = "IMS Serving Call Session Control Function (S-CSCF)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "IMS",
			target_schema = #target_schema_ref{class_type = "SCSCFFunction",
					schema = ?PathCatalogSchema ++ "SCSCFFunction"},
			characteristic = Chars}.

-spec ims_icscf() -> specification().
%% @doc IMS Interrogating Call Session Control Function (I-CSCF) resource specification.
ims_icscf() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	LinkList = #specification_char{name = "linkList",
			description = "List of related link object distiguished names (DN)",
			value_type = "linkList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/linkList"},
	ContainedNrmClass = #specification_char{name = "ICSCFFunctionOptionallyContainedNrmClass",
			description = "List of optionally contained NRM Class objects",
			value_type = "ICSCFFunctionOptionallyContainedNrmClassList",
			value_schema = ?PathCatalogSchema ++ "/imsNrm#/definitions/ICSCFFunctionOptionallyContainedNrmClassList"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VsDataContainerList"},
	Chars = [ID, UserLabel, VnfParametersList, LinkList, ContainedNrmClass, VsDataContainer],
	#specification{name = "ICSCFFunction",
			description = "IMS Interrogating Call Session Control Function (I-CSCF)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "IMS",
			target_schema = #target_schema_ref{class_type = "ICSCFFunction",
					schema = ?PathCatalogSchema ++ "ICSCFFunction"},
			characteristic = Chars}.

-spec pee_me() -> specification().
%% @doc Power, Energy and Environmental (PEE) Monitored Entity resource function specification.
pee_me() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	MeId = #specification_char{name = "meId",
			description = "PEE monitored entity ID",
			value_type = "string"},
	PeeMeDescription = #specification_char{name = "peeMeDescription",
			description = "PEE monitored entity description",
			value_type = "PEEMEDescription",
			value_schema = ?PathCatalogSchema ++ "/peeNrm#/definitions/PEEMEDescription"},
	PeeMeConfiguration = #specification_char{name = "peeMeConfiguration",
			description = "PEE monitored entity configuration",
			value_type = "PEEMEConfiguration",
			value_schema = ?PathCatalogSchema ++ "/peeNrm#/definitions/PEEMEConfiguration"},
	Chars = [ID, MeId, PeeMeDescription, PeeMeConfiguration],
	#specification{name = "PEEMonitoredEntity",
			description = "Power, Energy and Environmental (PEE) Monitored Entity",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "PEE",
			target_schema = #target_schema_ref{class_type = "PEEMonitoredEntity",
					schema = ?PathCatalogSchema ++ "PEEMonitoredEntity"},
			characteristic = Chars}.

-spec epcn3ai_proxy() -> specification().
%% @doc EPCN3AI 3GPP Authentication, Authorization and Accounting Proxy resource function specification.
epcn3ai_proxy() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VsDataContainerList"},
	Chars = [ID, UserLabel, VnfParametersList, VsDataContainer],
	#specification{name = "3GPPAAAProxyFunction",
			description = "EPCN3AI 3GPP Authentication, Authorization and Accounting Proxy",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "EPC",
			target_schema = #target_schema_ref{class_type = "3GPPAAAProxyFunction",
					schema = ?PathCatalogSchema ++ "3GPPAAAProxyFunction"},
			characteristic = Chars}.

-spec epcn3ai_server() -> specification().
%% @doc EPCN3AI 3GPP Authentication, Authorization and Accounting Server resource function specification.
epcn3ai_server() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	VnfParametersList = #specification_char{name = "vnfParametersList",
			description = "Parameter set of the VNF instance(s)",
			value_type = "VnfParametersListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VnfParametersListType"},
	VsDataContainer = #specification_char{name = "VsDataContainer",
			description = "Container for vendor specific data",
			value_type = "VsDataContainerList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/VsDataContainerList"},
	Chars = [ID, UserLabel, VnfParametersList, VsDataContainer],
	#specification{name = "3GPPAAAServerFunction",
			description = "EPCN3AI 3GPP Authentication, Authorization and Accounting Server",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "EPC",
			target_schema = #target_schema_ref{class_type = "3GPPAAAServerFunction",
					schema = ?PathCatalogSchema ++ "3GPPAAAServerFunction"},
			characteristic = Chars}.

-spec im_iu() -> specification().
%% @doc IM Inventory Unit resource specification.
im_iu() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	InventoryUnitType = #specification_char{name = "inventoryUnitType",
			description = "Type of inventory unit (3GPP 32.690)",
			value_type = "string"},
	VendorUnitFamilyType = #specification_char{name = "vendorUnitFamilyType",
			description = "Mnemonic of inventory unit family type (e.g. Fan, PSU) assigned by vendor",
			value_type = "string"},
	VendorUnitTypeNumber = #specification_char{name = "vendorUnitTypeNumber",
			description = "",
			value_type = "string"},
	VendorName = #specification_char{name = "vendorName",
			description = "Name of inventory unit vendor",
			value_type = "string"},
	SerialNumber = #specification_char{name = "serialNumber",
			description = "Serial number of inventory unit",
			value_type = "string"},
	DateOfManufacture = #specification_char{name = "dateOfManufacture",
			description = "Date of Manufacture of inventory unit",
			value_type = "string"},
	DateOfLastService = #specification_char{name = "dateOfLastService",
			description = "Date of last service or repair of inventory unit",
			value_type = "string"},
	UnitPosition = #specification_char{name = "unitPosition",
			description = "Position of inventory unit (e.g. Rack, shelf, slot, etc.)",
			value_type = "string"},
	ManufacturerData = #specification_char{name = "manufacturerData",
			description = "Manufacturer specific data of inventory unit",
			value_type = "string"},
	VersionNumber = #specification_char{name = "versionNumber",
			description = "The version information related to (vendorUnitTypeNumber)",
			value_type = "string"},
	RelatedFunction = #specification_char{name = "relatedFunction",
			description = "This attribute carries the DN of related (ManagedFunction)",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	Chars = [ID, InventoryUnitType, VendorUnitFamilyType, VendorUnitTypeNumber, VendorName, SerialNumber,
			DateOfManufacture, DateOfLastService, UnitPosition, ManufacturerData, VersionNumber, RelatedFunction],
	#specification{name = "InventoryUnit",
			description = "IM Inventory Unit",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "IM",
			target_schema = #target_schema_ref{class_type = "InventoryUnit",
					schema = ?PathCatalogSchema ++ "InventoryUnit"},
			characteristic = Chars,
			related = specification_rel(["TmaInventoryUnit", "AntennaInventoryUnit"])}.

-spec im_tmaiu() -> specification().
%% @doc IM Tower Mounted Amplifier (TMA) Inventory Unit resource specification.
im_tmaiu() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	InventoryUnitType = #specification_char{name = "inventoryUnitType",
			description = "Type of inventory unit (3GPP 32.690)",
			value_type = "string"},
	VendorUnitFamilyType = #specification_char{name = "vendorUnitFamilyType",
			description = "Mnemonic of inventory unit family type (e.g. Fan, PSU) assigned by vendor",
			value_type = "string"},
	VendorUnitTypeNumber = #specification_char{name = "vendorUnitTypeNumber",
			description = "",
			value_type = "string"},
	VendorName = #specification_char{name = "vendorName",
			description = "Type of inventory unit (3GPP 32.690)",
			value_type = "string"},
	SerialNumber = #specification_char{name = "serialNumber",
			description = "Serial number of inventory unit",
			value_type = "string"},
	DateOfManufacture = #specification_char{name = "dateOfManufacture",
			description = "Date of Manufacture of inventory unit",
			value_type = "string"},
	DateOfLastService = #specification_char{name = "dateOfLastService",
			description = "Date of last service or repair of inventory unit",
			value_type = "string"},
	UnitPosition = #specification_char{name = "unitPosition",
			description = "Position of inventory unit (e.g. Rack, shelf, slot, etc.)",
			value_type = "string"},
	ManufacturerData = #specification_char{name = "manufacturerData",
			description = "Manufacturer specific data of inventory unit",
			value_type = "string"},
	VersionNumber = #specification_char{name = "versionNumber",
			description = "The version information related to (vendorUnitTypeNumber)",
			value_type = "string"},
	RelatedFunction = #specification_char{name = "relatedFunction",
			description = "This attribute carries the DN of related (ManagedFunction)",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	TMANumberOfNonLinearGainValues = #specification_char{name = "tmaNumberOfNonLinearGainValues",
			description = "",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	TMANonLinearGainValue = #specification_char{name = "tmaNonLinearGainValue",
			description = "",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	TMAAdditionalDataFieldNumber = #specification_char{name = "tmaAdditionalDataFieldNumber",
			description = "Identifies a standard data field which has no operational impact",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	TMAAntennaModelNumber = #specification_char{name = "tmaAntennaModelNumber",
			description = "",
			value_type = "string"},
	TMAAntennaOperatingBands = #specification_char{name = "tmaAntennaOperatingBands",
			description = "",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	TMABeamwidthForEachOpBandInBandOrder = #specification_char{name = "tmaBeamwidthForEachOpBandInBandOrder",
			description = "",
			value_type = "EightOctetsType",
			value_schema = ?PathCatalogSchema ++ "/inventoryNrm#/definitions/EightOctetsType"},
	TMAGainForEachOpBandInBandOrder = #specification_char{name = "tmaGainForEachOpBandInBandOrder",
			description = "",
			value_type = "FourOctetsType",
			value_schema = ?PathCatalogSchema ++ "/inventoryNrm#/definitions/FourOctetsType"},
	TMAInstallationDate = #specification_char{name = "tmaInstallationDate",
			description = "",
			value_type = "string"},
	TMAInstallersId = #specification_char{name = "tmaInstallersId",
			description = "",
			value_type = "string"},
	TMAMaxSupportedGain = #specification_char{name = "tmaMaxSupportedGain",
			description = "",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	TMAMinSupportedGain = #specification_char{name = "tmaMinSupportedGain",
			description = "",
			value_type = "integer",
			char_value = [#spec_char_value{from = -32768, to = 32767}]},
	Chars = [ID, InventoryUnitType, VendorUnitFamilyType, VendorUnitTypeNumber, VendorName, SerialNumber,
			DateOfManufacture, DateOfLastService, UnitPosition, ManufacturerData, VersionNumber, RelatedFunction,
			TMANumberOfNonLinearGainValues, TMANonLinearGainValue, TMAAdditionalDataFieldNumber, TMAAntennaModelNumber,
			TMAAntennaOperatingBands, TMABeamwidthForEachOpBandInBandOrder, TMAGainForEachOpBandInBandOrder,
			TMAInstallationDate, TMAInstallersId, TMAMaxSupportedGain, TMAMinSupportedGain],
	#specification{name = "TmaInventoryUnit",
			description = "IM Tower Mounted Amplifier (TMA) Inventory Unit",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "IM",
			target_schema = #target_schema_ref{class_type = "TmaInventoryUnit",
					schema = ?PathCatalogSchema ++ "TmaInventoryUnit"},
			characteristic = Chars}.

-spec im_aiu() -> specification().
%% @doc IM Antenna Inventory Unit resource specification.
im_aiu() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	InventoryUnitType = #specification_char{name = "inventoryUnitType",
			description = "Type of inventory unit (3GPP 32.690)",
			value_type = "string"},
	VendorUnitFamilyType = #specification_char{name = "vendorUnitFamilyType",
			description = "Mnemonic of inventory unit family type (e.g. Fan, PSU) assigned by vendor",
			value_type = "string"},
	VendorUnitTypeNumber = #specification_char{name = "vendorUnitTypeNumber",
			description = "",
			value_type = "string"},
	VendorName = #specification_char{name = "vendorName",
			description = "Type of inventory unit (3GPP 32.690)",
			value_type = "string"},
	SerialNumber = #specification_char{name = "serialNumber",
			description = "Serial number of inventory unit",
			value_type = "string"},
	DateOfManufacture = #specification_char{name = "dateOfManufacture",
			description = "Date of Manufacture of inventory unit",
			value_type = "string"},
	DateOfLastService = #specification_char{name = "dateOfLastService",
			description = "Date of last service or repair of inventory unit",
			value_type = "string"},
	UnitPosition = #specification_char{name = "unitPosition",
			description = "Position of inventory unit (e.g. Rack, shelf, slot, etc.)",
			value_type = "string"},
	ManufacturerData = #specification_char{name = "manufacturerData",
			description = "Manufacturer specific data of inventory unit",
			value_type = "string"},
	VersionNumber = #specification_char{name = "versionNumber",
			description = "The version information related to (vendorUnitTypeNumber)",
			value_type = "string"},
	RelatedFunction = #specification_char{name = "relatedFunction",
			description = "This attribute carries the DN of related (ManagedFunction)",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	MaxTiltValue = #specification_char{name = "maxTiltValue",
			description = "The maximum amount of tilt the RET system can support",
			value_type = "AngleValueType",
			value_schema = ?PathCatalogSchema ++ "/inventoryNrm#/definitions/AngleValueType"},
	MinTiltValue = #specification_char{name = "minTiltValue",
			description = "The minimum amount of tilt the RET system can support",
			value_type = "AngleValueType",
			value_schema = ?PathCatalogSchema ++ "/inventoryNrm#/definitions/AngleValueType"},
	MechanicalOffset = #specification_char{name = "mechanicalOffset",
			description = "Represent a non-adjustable tilt value, which is imparted to physical installation of antenna",
			value_type = "AngleValueType",
			value_schema = ?PathCatalogSchema ++ "/inventoryNrm#/definitions/AngleValueType"},
	BaseElevation = #specification_char{name = "baseElevation",
			description = "The elevation in meters above sea level at the base of the antenna structure",
			value_type = "integer"},
	Latitude = #specification_char{name = "latitude",
			description = "The latitude of the antenna location",
			value_type = "decimal"},
	Longitude = #specification_char{name = "longitude",
			description = "The longitude of the antenna location",
			value_type = "decimal"},
	PatternLabel = #specification_char{name = "patternLabel",
			description = "Textual, alpha-numeric string, allow identification of antenna pattern along with the its vendor information such as model number, etc",
			value_type = "string"},
	Chars = [ID, InventoryUnitType, VendorUnitFamilyType, VendorUnitTypeNumber, VendorName, SerialNumber,
			DateOfManufacture, DateOfLastService, UnitPosition, ManufacturerData, VersionNumber, RelatedFunction,
			MaxTiltValue, MinTiltValue, MechanicalOffset, BaseElevation, Latitude, Longitude, PatternLabel],
	#specification{name = "AntennaInventoryUnit",
			description = "IM Antenna Inventory Unit",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "IM",
			target_schema = #target_schema_ref{class_type = "AntennaInventoryUnit",
					schema = ?PathCatalogSchema ++ "AntennaInventoryUnit"},
			characteristic = Chars}.

-spec im_iu_ne() -> specification().
%% @doc IM Inventory Unit Network Element resource specification.
im_iu_ne() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	NeId = #specification_char{name = "neId",
			description = "Vendor defined unique identifier of a logical or physical network element unit",
			value_type = "string"},
	CustomerIdentifier = #specification_char{name = "customerIdentifier",
			description = "Unique identification of a vendor's customer",
			value_type = "string"},
	ProductName = #specification_char{name = "productName",
			description = "NE name classifying a vendor's product family or function",
			value_type = "string"},
	VendorName = #specification_char{name = "vendorName",
			description = "Name of inventory unit vendor",
			value_type = "string"},
	ProductType = #specification_char{name = "productType",
			description = "Identifier of the product (e.g. platform), based on different HW/SW platforms",
			value_type = "string"},
	SalesUniqueId = #specification_char{name = "salesUniqueId",
			description = "Date of Manufacture of inventory unit",
			value_type = "string"},
	OperatorUniqueName = #specification_char{name = "operatorUniqueName",
			description = "Unique NE identifier used by operator",
			value_type = "string"},
	SiteId = #specification_char{name = "siteId",
			description = "NE site in customer network",
			value_type = "integer"},
	AdditionalInformation = #specification_char{name = "additionalInformation",
			description = "Supplementary information about inventory data",
			value_type = "string"},
	HWList = #specification_char{name = "hWList",
			description = "Carries the set of DN(s) of related InventoryUnitHw(s)",
			value_type = "DnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/DnList"},
	SWList = #specification_char{name = "sWList",
			description = "Carries the set of DN(s) of related InventoryUnitSw(s)",
			value_type = "DnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/DnList"},
	LICList = #specification_char{name = "lICList",
			description = "Carries the set of DN(s) of related InventoryUnitLic(s)",
			value_type = "DnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/DnList"},
	MFunction = #specification_char{name = "mFunction",
			description = "Carries the DN of related ManagedFunction",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	Chars = [ID, NeId, CustomerIdentifier, ProductName, VendorName, ProductType, SalesUniqueId, OperatorUniqueName,
			SiteId, AdditionalInformation, HWList, SWList, LICList, MFunction],
	#specification{name = "InventoryUnitNE",
			description = "IM Inventory Unit Network Element",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "IM",
			target_schema = #target_schema_ref{class_type = "InventoryUnitNE",
					schema = ?PathCatalogSchema ++ "InventoryUnitNE"},
			characteristic = Chars}.

-spec im_iu_hw() -> specification().
%% @doc IM Inventory Unit Hardware resource specification.
im_iu_hw() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	HwId = #specification_char{name = "hwId",
			description = "Hardware identifier allocated by the vendor",
			value_type = "string"},
	HwType = #specification_char{name = "hwType",
			description = "Type of the HW unit (e.g. equipment holder, carriage)",
			value_type = "string"},
	HwName = #specification_char{name = "hwName",
			description = "Mnemonic of hardware inventory unit family type (e.g. Fan, PSU) assigned by vendor",
			value_type = "string"},
	VendorName = #specification_char{name = "vendorName",
			description = "Name of inventory unit vendor",
			value_type = "string"},
	HwVersion = #specification_char{name = "hwVersion",
			description = "Version of current unit (e.g. firmware version)",
			value_type = "string"},
	SalesUniqueId = #specification_char{name = "salesUniqueId",
			description = "Date of Manufacture of inventory unit",
			value_type = "string"},
	HwUnitLocation = #specification_char{name = "hwUnitLocation",
			description = "Unique physical or logical location identifier within NE",
			value_type = "string"},
	Model = #specification_char{name = "model",
			description = "Equipment configuration",
			value_type = "string"},
	HwCapability = #specification_char{name = "hwCapability",
			description = "Capability of Hardware (e.g. capacity, size)",
			value_type = "string"},
	ModificationDate = #specification_char{name = "modificationDate",
			description = "Date/time stamp of last change",
			value_type = "string"},
	ManualDataEntry = #specification_char{name = "manualDataEntry",
			description = "Indicates whether unit is passive or active",
			value_type = "string"},
	AdditionalInformation = #specification_char{name = "additionalInformation",
			description = "Supplementary information about inventory data",
			value_type = "string"},
	NEList = #specification_char{name = "nEList",
			description = "Carries the set of DN(s) of related InventoryUnitNE(s)",
			value_type = "DnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/DnList"},
	SWList = #specification_char{name = "sWList",
			description = "Carries the set of DN(s) of related InventoryUnitSw(s)",
			value_type = "DnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/DnList"},
	LICList = #specification_char{name = "lICList",
			description = "Carries the set of DN(s) of related InventoryUnitLic(s)",
			value_type = "DnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/DnList"},
	MFunction = #specification_char{name = "mFunction",
			description = "Carries the DN of related ManagedFunction",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	Chars = [ID, HwId, HwType, HwName, VendorName, HwVersion, SalesUniqueId, HwUnitLocation, Model,
			HwCapability, ModificationDate, ManualDataEntry, AdditionalInformation, NEList, SWList,
			LICList, MFunction],
	#specification{name = "InventoryUnitHw",
			description = "IM Inventory Unit Hardware",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "IM",
			target_schema = #target_schema_ref{class_type = "InventoryUnitHw",
					schema = ?PathCatalogSchema ++ "InventoryUnitHw"},
			characteristic = Chars}.

-spec im_iu_sw() -> specification().
%% @doc IM Inventory Unit Software resource specification.
im_iu_sw() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	SwId = #specification_char{name = "swId",
			description = "Unique identifier of a Software unit",
			value_type = "string"},
	SwName = #specification_char{name = "swName",
			description = "Software release name used",
			value_type = "string"},
	VendorName = #specification_char{name = "vendorName",
			description = "Name of inventory unit vendor",
			value_type = "string"},
	SwVersion = #specification_char{name = "swVersion",
			description = "Version identifier of the software unit",
			value_type = "string"},
	SalesUniqueId = #specification_char{name = "salesUniqueId",
			description = "Date of Manufacture of inventory unit",
			value_type = "string"},
	Classification = #specification_char{name = "classification",
			description = "Name of installed Software",
			value_type = "string"},
	SwInstallationTime = #specification_char{name = "swInstallationTime",
			description = "Date and time when the software installation process ended and the sotware was installed",
			value_type = "string"},
	SwActivationTime = #specification_char{name = "swActivationTime",
			description = "Date and time when the software was activated",
			value_type = "string"},
	SwStatus = #specification_char{name = "swStatus",
			description = "Status of the SW unit (e.g. installed, archived)",
			value_type = "string"},
	AdditionalInformation = #specification_char{name = "additionalInformation",
			description = "Supplementary information about inventory data",
			value_type = "string"},
	NEList = #specification_char{name = "nEList",
			description = "Carries the set of DN(s) of related InventoryUnitNE(s)",
			value_type = "DnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/DnList"},
	HWList = #specification_char{name = "hWList",
			description = "Carries the set of DN(s) of related InventoryUnitSw(s)",
			value_type = "DnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/DnList"},
	LICList = #specification_char{name = "lICList",
			description = "Carries the set of DN(s) of related InventoryUnitLic(s)",
			value_type = "DnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/DnList"},
	MFunction = #specification_char{name = "mFunction",
			description = "Carries the DN of related ManagedFunction",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	Chars = [ID, SwId, SwName, VendorName, SwVersion, SalesUniqueId, Classification, SwInstallationTime,
			SwActivationTime, SwStatus, AdditionalInformation, NEList, HWList, LICList, MFunction],
	#specification{name = "InventoryUnitSw",
			description = "IM Inventory Unit Software",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "IM",
			target_schema = #target_schema_ref{class_type = "InventoryUnitSw",
					schema = ?PathCatalogSchema ++ "InventoryUnitSw"},
			characteristic = Chars}.

-spec im_iu_lic() -> specification().
%% @doc IM Inventory Unit License resource specification.
im_iu_lic() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	LicId = #specification_char{name = "licId",
			description = "Unique identifier of a license (e.g. name, code)",
			value_type = "string"},
	LicType = #specification_char{name = "licType",
			description = "Describing type of current license (e.g. capacity, particular feature, no. of subscribers)",
			value_type = "string"},
	VendorName = #specification_char{name = "vendorName",
			description = "Name of inventory unit vendor",
			value_type = "string"},
	Validity = #specification_char{name = "validity",
			description = "May include one of the elements duration, end (expiration date) or forever",
			value_type = "string"},
	Key = #specification_char{name = "key",
			description = "License activation key according to the used licensing system",
			value_type = "string"},
	LicActivationTime = #specification_char{name = "licActivationTime",
			description = "Date and time when the license was activated",
			value_type = "string"},
	LicStatus = #specification_char{name = "licStatus",
			description = "Applicable only for managed licenses",
			value_type = "string"},
	SalesUniqueId = #specification_char{name = "salesUniqueId",
			description = "Date of Manufacture of inventory unit",
			value_type = "string"},
	AdditionalInformation = #specification_char{name = "additionalInformation",
			description = "Supplementary information about inventory data",
			value_type = "string"},
	NEList = #specification_char{name = "nEList",
			description = "Carries the set of DN(s) of related InventoryUnitNE(s)",
			value_type = "DnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/DnList"},
	HWList = #specification_char{name = "hWList",
			description = "Carries the set of DN(s) of related InventoryUnitSw(s)",
			value_type = "DnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/DnList"},
	SWList = #specification_char{name = "sWList",
			description = "Carries the set of DN(s) of related InventoryUnitSw(s)",
			value_type = "DnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/DnList"},
	MFunction = #specification_char{name = "mFunction",
			description = "Carries the DN of related ManagedFunction",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	Chars = [ID, LicId, LicType, VendorName, Validity, Key, LicActivationTime, LicStatus,
			SalesUniqueId, AdditionalInformation, NEList, HWList, SWList, MFunction],
	#specification{name = "InventoryUnitLic",
			description = "IM Inventory Unit License",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "IM",
			target_schema = #target_schema_ref{class_type = "InventoryUnitLic",
					schema = ?PathCatalogSchema ++ "InventoryUnitLic"},
			characteristic = Chars}.

-spec generic_me() -> specification().
%% @doc Generic Managed Element resource specification.
generic_me() ->
	ID = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	DnPrefix = #specification_char{name = "dnPrefix",
			description = "Distinguished Name (DN) prefix (3GPP 32.300 Annex C)",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	ManagedElementTypeList = #specification_char{name = "managedElementTypeList",
			description = "",
			value_type = "ManagedElementTypeListType",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/ManagedElementTypeListType"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	LocationName = #specification_char{name = "locationName",
			description = "",
			value_type = "string"},
	ManagedBy = #specification_char{name = "managedBy",
			description = "",
			value_type = "DnList",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/DnList"},
	VendorName = #specification_char{name = "vendorName",
			description = "Name of inventory unit vendor",
			value_type = "string"},
	UserDefinedState = #specification_char{name = "userDefinedState",
			description = "",
			value_type = "string"},
	SwVersion = #specification_char{name = "swVersion",
			description = "Version identifier of the software unit",
			value_type = "string"},
	PriorityLabel = #specification_char{name = "priorityLabel",
			description = "",
			value_type = "string"},
	Chars = [ID, DnPrefix, ManagedElementTypeList, UserLabel, LocationName, ManagedBy, VendorName,
			UserDefinedState, SwVersion, PriorityLabel],
	#specification{name = "ManagedElement",
			description = "Generic Managed Element (ME)",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "",
			target_schema = #target_schema_ref{class_type = "ManagedElement",
					schema = ?PathCatalogSchema ++ "ManagedElement"},
			characteristic = Chars}.

-spec generic_subnetwork() -> specification().
%% @doc Generic Subnetwork resource function specification.
generic_subnetwork() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	DnPrefix = #specification_char{name = "dnPrefix",
			description = "Distinguished Name (DN) prefix (3GPP 32.300 Annex C)",
			value_type = "Dn",
			value_schema = ?PathCatalogSchema ++ "/genericNrm#/definitions/Dn"},
	UserLabel = #specification_char{name = "userLabel",
			description = "A user-friendly (and user assignable) name of this object",
			value_type = "string"},
	UserDefinedNetworkType = #specification_char{name = "userDefinedNetworkType",
			description = "User defined network type (3GPP 28.620)",
			value_type = "string"},
	SetOfMcc = #specification_char{name = "setOfMcc",
			description = "All Mobile Country Codes (MCC) included",
			value_type = "string"},
	Chars = [Id, DnPrefix, UserLabel, UserDefinedNetworkType, SetOfMcc],
	#specification{name = "SubNetwork",
			description = "",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "",
			target_schema = #target_schema_ref{class_type = "SubNetwork",
					schema = ?PathCatalogSchema ++ "SubNetwork"},
			characteristic = Chars,
			related = specification_rel(["ManagedElement"])}.

-spec huawei_usn() -> specification().
%% @doc Huawei Unified Serving Node (USN) resource specification.
huawei_usn() ->
	Fdn = #specification_char{name = "fdn",
			description = "",
			value_type = "string"},
	ClassName = #specification_char{name = "className",
			description = "",
			value_type = "string"},
	CLASSNAME = #specification_char{name = "cLASSNAME",
			description = "",
			value_type = "string"},
	MoIndex = #specification_char{name = "mOIndex",
			description = "Managed Object Index",
			value_type = "string"},
	Name = #specification_char{name = "name",
			description = "",
			value_type = "string"},
	NeID = #specification_char{name = "neID",
			description = "Network Element Identity",
			value_type = "string"},
	Chars = [Fdn, ClassName, CLASSNAME, MoIndex, Name, NeID],
	#specification{name = "USNFunction",
			description = "Unified Serving Node",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "USNFunction",
					schema = ""},
			characteristic = Chars}.

-spec huawei_ugw() -> specification().
%% @doc Huawei Universal Gateway (UGW) resource specification.
huawei_ugw() ->
	Fdn = #specification_char{name = "fdn",
			description = "",
			value_type = "string"},
	ClassName = #specification_char{name = "className",
			description = "",
			value_type = "string"},
	CLASSNAME = #specification_char{name = "CLASSNAME",
			description = "",
			value_type = "string"},
	MoIndex = #specification_char{name = "MOIndex",
			description = "Managed Object Index",
			value_type = "string"},
	Name = #specification_char{name = "name",
			description = "",
			value_type = "string"},
	NeID = #specification_char{name = "neID",
			description = "Network Element Identity",
			value_type = "string"},
	Chars = [Fdn, ClassName, CLASSNAME, MoIndex, Name, NeID],
	#specification{name = "UGWFunction",
			description = "Universal Gateway",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "UGWFunction",
					schema = ""},
			characteristic = Chars}.

-spec huawei_cgpomu() -> specification().
%% @doc Huawei Carrier Grade Platform (CGP) Operation and Management Unit (OMU) resource specification.
huawei_cgpomu() ->
	Fdn = #specification_char{name = "fdn",
			description = "",
			value_type = "string"},
	ClassName = #specification_char{name = "className",
			description = "",
			value_type = "string"},
	CLASSNAME = #specification_char{name = "CLASSNAME",
			description = "",
			value_type = "string"},
	MoIndex = #specification_char{name = "MOIndex",
			description = "Managed Object Index",
			value_type = "string"},
	Name = #specification_char{name = "name",
			description = "",
			value_type = "string"},
	NeID = #specification_char{name = "neID",
			description = "Network Element Identity",
			value_type = "string"},
	Chars = [Fdn, ClassName, CLASSNAME, MoIndex, Name, NeID],
	#specification{name = "CGPOMUFunction",
			description = "Carrier Grade Platform Operation and Management Unit",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "CGPOMUFunction",
					schema = ""},
			characteristic = Chars}.

-spec huawei_igwb() -> specification().
%% @doc Huawei Charging Gateway (iGWB) resource specification.
huawei_igwb() ->
	Fdn = #specification_char{name = "fdn",
			description = "",
			value_type = "string"},
	ClassName = #specification_char{name = "className",
			description = "",
			value_type = "string"},
	CLASSNAME = #specification_char{name = "CLASSNAME",
			description = "",
			value_type = "string"},
	MoIndex = #specification_char{name = "MOIndex",
			description = "Managed Object Index",
			value_type = "string"},
	Name = #specification_char{name = "name",
			description = "",
			value_type = "string"},
	NeID = #specification_char{name = "neID",
			description = "Network Element Identity",
			value_type = "string"},
	Chars = [Fdn, ClassName, CLASSNAME, MoIndex, Name, NeID],
	#specification{name = "iGWBFunction",
			description = "Charging Gateway",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "iGWBFunction",
					schema = ""},
			characteristic = Chars}.

-spec huawei_uscdb() -> specification().
%% @doc
huawei_uscdb() ->
	Fdn = #specification_char{name = "fdn",
			description = "",
			value_type = "string"},
	ClassName = #specification_char{name = "className",
			description = "",
			value_type = "string"},
	CLASSNAME = #specification_char{name = "CLASSNAME",
			description = "",
			value_type = "string"},
	MoIndex = #specification_char{name = "MOIndex",
			description = "Managed Object Index",
			value_type = "string"},
	Name = #specification_char{name = "name",
			description = "",
			value_type = "string"},
	NeID = #specification_char{name = "neID",
			description = "Network Element Identity",
			value_type = "string"},
	Chars = [Fdn, ClassName, CLASSNAME, MoIndex, Name, NeID],
	#specification{name = "USCDBFunction",
			description = "",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "USCDBFunction",
					schema = ?PathCatalogSchema ++ "USCDBFunction"},
			characteristic = Chars}.

-spec huawei_spsv3() -> specification().
%% @doc
huawei_spsv3() ->
	Fdn = #specification_char{name = "fdn",
			description = "",
			value_type = "string"},
	ClassName = #specification_char{name = "className",
			description = "",
			value_type = "string"},
	CLASSNAME = #specification_char{name = "CLASSNAME",
			description = "",
			value_type = "string"},
	MoIndex = #specification_char{name = "MOIndex",
			description = "Managed Object Index",
			value_type = "string"},
	Name = #specification_char{name = "name",
			description = "",
			value_type = "string"},
	NeID = #specification_char{name = "neID",
			description = "Network Element Identity",
			value_type = "string"},
	Chars = [Fdn, ClassName, CLASSNAME, MoIndex, Name, NeID],
	#specification{name = "SPSV3Function",
			description = "",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "SPSV3Function",
					schema = ?PathCatalogSchema ++ "SPSV3Function"},
			characteristic = Chars}.

-spec huawei_mscsiosp() -> specification().
%% @doc
huawei_mscsiosp() ->
	Fdn = #specification_char{name = "fdn",
			description = "",
			value_type = "string"},
	ClassName = #specification_char{name = "className",
			description = "",
			value_type = "string"},
	CNID = #specification_char{name = "cNID",
			description = "",
			value_type = "string"},
	IN1 = #specification_char{name = "iN1",
			description = "",
			value_type = "string"},
	IN2 = #specification_char{name = "iN2",
			description = "",
			value_type = "string"},
	INT2S = #specification_char{name = "iNT2S",
			description = "",
			value_type = "string"},
	INTS = #specification_char{name = "iNTS",
			description = "",
			value_type = "string"},
	IP2C = #specification_char{name = "iP2C",
			description = "",
			value_type = "integer"},
	IPC = #specification_char{name = "iPC",
			description = "",
			value_type = "integer"},
	MoIndex = #specification_char{name = "MOIndex",
			description = "Managed Object Index",
			value_type = "string"},
	NN = #specification_char{name = "nN",
			description = "",
			value_type = "string"},
	NN2 = #specification_char{name = "nN2",
			description = "",
			value_type = "string"},
	NP2C = #specification_char{name = "nP2C",
			description = "",
			value_type = "integer"},
	NPC = #specification_char{name = "nPC",
			description = "",
			value_type = "integer"},
	NT2S = #specification_char{name = "nT2S",
			description = "",
			value_type = "string"},
	NTS = #specification_char{name = "nTS",
			description = "",
			value_type = "string"},
	OfficeName = #specification_char{name = "officeName",
			description = "",
			value_type = "string"},
	REALIDX = #specification_char{name = "rEALIDX",
			description = "",
			value_type = "integer"},
	SPF = #specification_char{name = "sPF",
			description = "",
			value_type = "string"},
	STP = #specification_char{name = "sTP",
			description = "",
			value_type = "string"},
	Index = #specification_char{name = "index",
			description = "",
			value_type = "integer"},
	Name = #specification_char{name = "name",
			description = "",
			value_type = "string"},
	NeID = #specification_char{name = "neID",
			description = "Network Element Identity",
			value_type = "string"},
	Chars = [Fdn, ClassName, CNID, IN1, IN2, INT2S, INTS, IP2C, IPC, MoIndex,
			NN, NN2, NP2C, NPC, NT2S, NTS, OfficeName, REALIDX, SPF, STP,
			Index, Name, NeID],
	#specification{name = "MSCServerIntraOfiSigPoint",
			description = "",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "MSCServerIntraOfiSigPointSpec",
					schema = ?PathCatalogSchema ++ "MSCServerIntraOfiSigPointSpec"},
			characteristic = Chars}.

-spec huawei_mscso() -> specification().
%% @doc
huawei_mscso() ->
	Fdn = #specification_char{name = "fdn",
			description = "",
			value_type = "string"},
	ClassName = #specification_char{name = "className",
			description = "",
			value_type = "string"},
	DOA = #specification_char{name = "dOA",
			description = "",
			value_type = "integer"},
	DPC1 = #specification_char{name = "dPC1",
			description = "",
			value_type = "integer"},
	DPC2 = #specification_char{name = "dPC2",
			description = "",
			value_type = "integer"},
	DPC3 = #specification_char{name = "dPC3",
			description = "",
			value_type = "integer"},
	DPC4 = #specification_char{name = "dPC4",
			description = "",
			value_type = "integer"},
	DPC5 = #specification_char{name = "dPC5",
			description = "",
			value_type = "integer"},
	DPC6 = #specification_char{name = "dPC6",
			description = "",
			value_type = "integer"},
	DPC7 = #specification_char{name = "dPC7",
			description = "",
			value_type = "integer"},
	DPC8 = #specification_char{name = "dPC8",
			description = "",
			value_type = "integer"},
	DPC9 = #specification_char{name = "dPC9",
			description = "",
			value_type = "integer"},
	DPC10 = #specification_char{name = "dPC10",
			description = "",
			value_type = "integer"},
	DPC11 = #specification_char{name = "dPC11",
			description = "",
			value_type = "integer"},
	DPC12 = #specification_char{name = "dPC12",
			description = "",
			value_type = "integer"},
	DPC13 = #specification_char{name = "dPC13",
			description = "",
			value_type = "integer"},
	DPC14 = #specification_char{name = "dPC14",
			description = "",
			value_type = "integer"},
	DPC15 = #specification_char{name = "dPC15",
			description = "",
			value_type = "integer"},
	DPC16 = #specification_char{name = "dPC16",
			description = "",
			value_type = "integer"},
	MoIndex = #specification_char{name = "MOIndex",
			description = "Managed Object Index",
			value_type = "string"},
	NI = #specification_char{name = "nI",
			description = "",
			value_type = "integer"},
	OFCTYPE = #specification_char{name = "oFCTYPE",
			description = "",
			value_type = "integer"},
	OfficeName = #specification_char{name = "officeName",
			description = "",
			value_type = "string"},
	SIG = #specification_char{name = "sIG",
			description = "",
			value_type = "integer"},
	DestinationOfficeType = #specification_char{name = "destinationOfficeType",
			description = "",
			value_type = "integer"},
	Name = #specification_char{name = "name",
			description = "",
			value_type = "string"},
	NeID = #specification_char{name = "neID",
			description = "Network Element Identity",
			value_type = "string"},
	OfficeNo = #specification_char{name = "officeNo",
			description = "Network Element Identity",
			value_type = "string"},
	Chars = [Fdn, ClassName, DOA, DPC1, DPC2, DPC3, DPC4, DPC5, DPC6, DPC7, DPC8,
			DPC9, DPC10, DPC11, DPC12, DPC13, DPC14, DPC15, DPC16, MoIndex, NI,
			OFCTYPE, OfficeName, SIG, DestinationOfficeType, Name, NeID, OfficeNo],
	#specification{name = "MSCServerOffice",
			description = "",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "Core",
			target_schema = #target_schema_ref{class_type = "MSCServerOfficeSpec",
					schema = ?PathCatalogSchema ++ "MSCServerOfficeSpec"},
			characteristic = Chars}.

-spec mec_mehf() -> specification().
%% @doc MEC MobileEdgeHost resource function specification.
mec_mehf() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class",
			value_type = "string"},
	SupportedFeatures = #specification_char{name = "supportedFeatures",
			description = "The features supported by the MobileEdgeHostFunction",
			value_type = "string"},
	Version = #specification_char{name = "version",
			description = "Version of the MEC system",
			value_type = "string"},
	Chars = [Id, SupportedFeatures, Version],
	#specification{name = "MobileEdgeHostFunction",
			description = "MEC Mobile Edge Host Function",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "MEC",
			target_schema = #target_schema_ref{class_type = "MobileEdgeHostFunction",
					schema = ?PathCatalogSchema ++ "MobileEdgeHostFunction"},
			characteristic = Chars,
			related = specification_rel(["MobileEdgePlatform", "MobileEdgeApplication",
					"TrafficRule", "DNSRule"])}.

-spec mec_mep() -> specification().
%% @doc MEC Mobile Edge Platform resource specification.
mec_mep() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	Chars = [Id],
	#specification{name = "MobileEdgePlatform",
			description = "MEC Mobile Edge Platform",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "MEC",
			target_schema = #target_schema_ref{class_type = "MobileEdgePlatform",
					schema = ?PathCatalogSchema ++ "MobileEdgePlatform"},
			characteristic = Chars,
			related = specification_rel(["MobileEdgePlatformService"])}.

-spec mec_mea() -> specification().
%% @doc MEC Mobile Edge Application resource function specification.
mec_mea() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	AppDId = #specification_char{name = "appDId",
			description = "Identifier of the mobile edge application descriptor",
			value_type = "string"},
	AppName = #specification_char{name = "appName",
			description = "Human readable name of the mobile edge application",
			value_type = "string"},
	AppProvider = #specification_char{name = "appProvider",
			description = "Provider of the mobile edge application",
			value_type = "string"},
	AppSoftVersion = #specification_char{name = "appSoftVersion",
			description = "Version of the mobile edge application",
			value_type = "string"},
	AppDVersion = #specification_char{name = "appDVersion",
			description = "Identifies the version of the application descriptor",
			value_type = "string"},
	AppInfoName = #specification_char{name = "appInfoName",
			description = "Human readable name for the application product",
			value_type = "string"},
	AppDescription = #specification_char{name = "appDescription",
			description = "Human readable description of the mobile edge application",
			value_type = "string"},
	AppState = #specification_char{name = "appState",
			description = "The state of the application",
			value_type = "string"},
	InstantiationState = #specification_char{name = "instantiationState",
			description = "The instantiation state of the application",
			value_type = "string"},
	OperationalState = #specification_char{name = "operationalState",
			description = "It indicates the operational state of the object instance",
			value_type = "OperationalStateType", value_schema = ?PathCatalogSchema
			++ "/stateManagementIRPNrm#/definitions/OperationalStateType"},
	AppInstanceId = #specification_char{name = "appInstanceId",
			description = "Application instance identifier",
			value_type = "string"},
	Chars = [Id, AppDId, AppName, AppProvider, AppSoftVersion, AppDVersion,
			AppInfoName, AppDescription, AppState, InstantiationState,
			OperationalState, AppInstanceId],
	#specification{name = "MobileEdgeApplication",
			description = "MEC Mobile Edge Application",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "RAN",
			target_schema = #target_schema_ref{class_type = "MobileEdgeApplication",
					schema = ?PathCatalogSchema ++ "MobileEdgeApplication"},
			characteristic = Chars,
			related = specification_rel(["MobileEdgeApplicationService"])}.

-spec mec_meps() -> specification().
%% @doc MEC Mobile Edge Platform Service resource specification.
mec_meps() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	Address = #specification_char{name = "address",
			description = "The IP address where mobile edge services can be accessed",
			value_type = "string"},
	OperationalState = #specification_char{name = "operationalState",
			description = "It indicates the operational state of the object instance",
			value_type = "OperationalStateType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRPNrm#/definitions/OperationalStateType"},
	SerName = #specification_char{name = "serName",
			description = "The name of the service",
			value_type = "string"},
	SerCategory = #specification_char{name = "serCategory",
			description = "The category of the service",
			value_type = "string"},
	SerVersion = #specification_char{name = "serVersion",
			description = "The version of the service",
			value_type = "string"},
	SerDataFormat = #specification_char{name = "serDataFormat",
			description = [],
			value_type = []},
	Chars = [Id, Address, OperationalState, SerName, SerCategory, SerVersion,
			SerDataFormat],
	#specification{name = "MobileEdgePlatformService",
			description = "MEC Mobile Edge Platform Service",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "MEC",
			target_schema = #target_schema_ref{
					class_type = "MobileEdgePlatformService",
					schema = ?PathCatalogSchema ++ "MobileEdgePlatformService"},
			characteristic = Chars,
			related = specification_rel(["RNIService", "LocationService"])}.

-spec mec_meas() -> specification().
%% @doc MEC Mobile Edge Application Service resource specification.
mec_meas() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	Address = #specification_char{name = "address",
			description = "The IP address where mobile edge services can be accessed",
			value_type = "string"},
	OperationalState = #specification_char{name = "operationalState",
			description = "It indicates the operational state of the object instance",
			value_type = "OperationalStateType",
			value_schema = ?PathCatalogSchema ++ "/stateManagementIRPNrm#/definitions/OperationalStateType"},
	SerName = #specification_char{name = "serName",
			description = "The name of the service",
			value_type = "string"},
	SerCategory = #specification_char{name = "serCategory",
			description = "The category of the service",
			value_type = "string"},
	SerVersion = #specification_char{name = "serVersion",
			description = "The version of the service",
			value_type = "string"},
	TransportsSupported = #specification_char{name = "transportsSupported",
			description = "Indicates transports and serialization formats supported made available to the service-consuming application",
			value_type = []},
	Chars = [Id, Address, OperationalState, SerName, SerCategory, SerVersion,
			TransportsSupported],
	#specification{name = "MobileEdgeApplicationService",
			description = "MEC Mobile Edge Application Service",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "MEC",
			target_schema = #target_schema_ref{class_type = "MobileEdgeApplicationService",
					schema = ?PathCatalogSchema ++ "MobileEdgeApplicationService"},
			characteristic = Chars}.

-spec mec_rnis() -> specification().
%% @doc MEC Radio Network Information (RNI) Service resource specification.
mec_rnis() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	Chars = [Id],
	#specification{name = "RNIService",
			description = "MEC Radio Network Information (RNI) Service",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "MEC",
			target_schema = #target_schema_ref{class_type = "RNIService",
					schema = ?PathCatalogSchema ++ "RNIService"},
			characteristic = Chars}.

-spec mec_ls() -> specification().
%% @doc MEC Location Service resource specification.
mec_ls() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance of the object class.",
			value_type = "string"},
	Chars = [Id],
	#specification{name = "LocationService",
			description = "MEC Location Service",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "MEC",
			target_schema = #target_schema_ref{class_type = "LocationService",
					schema = ?PathCatalogSchema ++ "LocationService"},
			characteristic = Chars}.

-spec mec_tr() -> specification().
%% @doc MEC Traffic Rule resource specification.
mec_tr() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance"
					" of the object class.",
			value_type = "string"},
	TrafficRuleId = #specification_char{name = "trafficRuleId",
			description = "The identifier of the traffic rule",
			value_type = "string"},
	FilterType = #specification_char{name = "filterType",
			description = "Definition of filter type",
			value_type = []},
	Priority = #specification_char{name = "priority",
			description = "The priority of the associated traffic rule",
			value_type = "integer"},
	TrafficFilter = #specification_char{name = "trafficFilter",
			description = "The traffic filter used to identify specific flow/packets"
					"that need to be handled by the MEC host",
			value_type = []},
	Action = #specification_char{name = "action",
			description = "The action to be applied to a packet in case it matches"
					"the traffic rule or RAB filter rule",
			value_type = []},
	DstInterface = #specification_char{name = "dstInterface",
			description = [], value_type = []},
	Chars = [Id, TrafficRuleId, FilterType, Priority, TrafficFilter,
			Action, DstInterface],
	#specification{name = "TrafficRule",
			description = "MEC Traffic Rule",
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "MEC",
			target_schema = #target_schema_ref{class_type = "TrafficRule",
					schema = ?PathCatalogSchema ++ "TrafficRule"},
			characteristic = Chars}.

-spec mec_dnsr() -> specification().
%% @doc
mec_dnsr() ->
	Id = #specification_char{name = "id",
			description = "Used as an RDN when naming an instance"
					" of the object class.",
			value_type = "string"},
	DnsRuleId = #specification_char{name = "dnsRuleId",
			description = "The DNS rule id",
			value_type = []},
	DomainName = #specification_char{name = "domainName",
			description = "The FQDN of the DNS rule",
			value_type = "string"},
	IPAddressType = #specification_char{name = "iPAddressType",
			description = "The IP address type",
			value_type = []},
	IPAddress = #specification_char{name = "iPAddress",
			description = "The IP address to which the domain name is resolved",
			value_type = "string"},
	TimeToLive = #specification_char{name = "timeToLive",
			description = "The time to life of the resolved IP address",
			value_type = "integer"},
	Chars = [Id, DnsRuleId, DomainName, IPAddressType, IPAddress, TimeToLive],
	#specification{name = "DNSRule",
			description = [],
			class_type = "ResourceFunctionSpecification",
			status = active,
			version = "1.0",
			category = "MEC",
			target_schema = #target_schema_ref{class_type = "DNSRule",
					schema = ?PathCatalogSchema ++ "DNSRule"},
			characteristic = Chars}.

-spec im_catalog_api_spec() -> specification().
%% @doc
im_catalog_api_spec() ->
	PartyRef = #party_ref{id = "9f16f654223e", name = "SigScale", role = "Supplier",
			href = ?PathPartySpec ++ "9f16f654223e", ref_type = "Organization"},
	#specification{name = "TMF634",
			description = "Resource Catalog management API (TMF634) specification",
			class_type = "ApiSpecification",
			base_type = "SoftwareResourceSpecification",
			schema = "/resourceCatalogManagement/v4/schema/ApiSpecification",
			version = "0.1",
			status = in_test,
			category = "ODA",
			target_schema = #target_schema_ref{class_type = "API",
					schema = ?PathCatalogSchema ++ "/API"},
			party = [PartyRef],
			attributes = #{"apiProtocolType" => "REST",
					"internalUrl" => "/resourceCatalogManagement/v4/",
					"internalSchema" => "schema/resourceCatalogManagement.json",
					"authenticationType" => "BASIC",
					"allowedOperations" => ["POST", "PATCH", "GET", "DELETE"],
					"allowedApiEntities" => ["resourceCatalog", "resourceCategory",
							"resourceCandidate", "resourceSpecification"],
					"responseTypeFormat" => "application/json",
					"majorVersion" => "4",
					"minorVersion" => "1",
					"maintenanceVersion" => "0"}}.

-spec im_catalog_spec() -> specification().
%% @doc
im_catalog_spec() ->
	PartyRef = #party_ref{id = "9f16f654223e", name = "SigScale",
			role = "Supplier", href = ?PathPartySpec ++ "9f16f654223e",
			ref_type = "Organization"},
	#specification{name = "Resource Catalog",
			description = "Resource Catalog resource function specification",
			class_type = "ResourceFunctionSpecification",
			version = "0.1",
			status = in_test,
			category = "ODA",
			target_schema = #target_schema_ref{class_type = "ResourceFunction",
					schema = ?PathCatalogSchema ++ "/ResourceFunction"},
			party = [PartyRef],
			connection_point
					= specification_conn_point(["TMF634"])}.

-spec im_inventory_api_spec() -> specification().
%% @doc
im_inventory_api_spec() ->
	PartyRef = #party_ref{id = "9f16f654223e", name = "SigScale", role = "Supplier",
			href = ?PathPartySpec ++ "9f16f654223e", ref_type = "Organization"},
	#specification{name = "TMF639",
			description = "Resource Inventory management API (TMF639) specification",
			class_type = "ApiSpecification",
			base_type = "SoftwareResourceSpecification",
			schema = "/resourceCatalogManagement/v4/schema/ApiSpecification",
			version = "0.1",
			status = in_test,
			category = "ODA",
			target_schema = #target_schema_ref{class_type = "API",
					schema = ?PathCatalogSchema ++ "/API"},
			party = [PartyRef],
			attributes = #{"apiProtocolType" => "REST",
					"internalUrl" => "/resourceCatalogManagement/v4/",
					"internalSchema" => "schema/resourceInventoryManagement.json",
					"authenticationType" => "BASIC",
					"allowedOperations" => ["POST", "PATCH", "GET", "DELETE"],
					"allowedApiEntities" => ["resource", "logicalResource"],
					"responseTypeFormat" => "application/json",
					"majorVersion" => "4",
					"minorVersion" => "0",
					"maintenanceVersion" => "1"}}.

-spec im_inventory_spec() -> specification().
%% @doc
im_inventory_spec() ->
	PartyRef = #party_ref{id = "9f16f654223e", name = "SigScale",
			role = "Supplier", href = ?PathPartySpec ++ "9f16f654223e",
			ref_type = "Organization"},
	#specification{name = "Resource Inventory",
			description = "Resource Inventory resource function specification",
			class_type = "ResourceFunctionSpecification",
			version = "0.1",
			status = in_test,
			category = "ODA",
			target_schema = #target_schema_ref{class_type = "ResourceFunction",
					schema = ?PathCatalogSchema ++ "/ResourceFunction"},
			party = [PartyRef],
			connection_point
					= specification_conn_point(["TMF639"])}.

-spec im_application_spec() -> specification().
%% @doc
im_application_spec() ->
	RestPageSize = #specification_char{name = "restPageSize",
			description = "The number of records that are displayed when a page loads.",
			value_type = "integer"},
	RestPageTime = #specification_char{name = "restPageTimeout",
			description = "How long it takes to respond to a request.",
			value_type = "integer"},
	TlsKey = #specification_char{name = "tlsKey",
			description = "Symmetric encryption key.",
			value_type = "string"},
	TlsCert = #specification_char{name = "tlsCert",
			description = "Symmetric encryption certificate.",
			value_type = "string"},
	TlsCacert = #specification_char{name = "tlsCacert",
			description = "Symmetric encryption certification authority.",
			value_type = "string"},
	OauthAud = #specification_char{name = "oauthAudience",
			description = "Open authorization audience.",
			value_type = "string"},
	OauthIss = #specification_char{name = "oauthIssuer",
			description = "Open authorization issuer.",
			value_type = "string"},
	OauthKey = #specification_char{name = "oauthKey",
			description = "Open authorization key.",
			value_type = "string"},
	Chars = [RestPageSize, RestPageTime, TlsKey,
			TlsCert, TlsCacert, OauthAud, OauthIss, OauthKey],
	PartyRef = #party_ref{id = "9f16f654223e", name = "SigScale",
			role = "Supplier", href = ?PathPartySpec ++ "9f16f654223e",
			ref_type = "Organization"},
	#specification{name = "im",
			description = "Erlang application im software specification",
			class_type = "SoftwareSpecification",
			base_type = "SoftwareResourceSpecification",
			schema = "/resourceCatalogManagement/v4/schema/SoftwareSpecification",
			version = "0.1",
			status = in_test,
			category = "ODA",
			target_schema = #target_schema_ref{class_type = "InstalledSoftware",
					schema = ?PathCatalogSchema ++ "/InstalledSoftware"},
			characteristic = Chars,
			party = [PartyRef],
			attributes = #{"releaseStatus" => "beta",
					"majorVersion" => "2",
					"minorVersion" => "0",
					"maintenanceVersion" => "2",
					"isDistributable" => true,
					"installSize" => #{"amount" => 256.00,
							"units" => "MB"}}}.

-spec im_erlang_spec() -> specification().
%% @doc
im_erlang_spec() ->
	PartyRef = #party_ref{id = "9f16q754823e", name = "Ericsson",
			role = "Supplier", href = ?PathPartySpec ++ "9f16q754823e",
			ref_type = "Organization"},
	#specification{name = "Erlang",
			description = "Erlang runtime environment software specification",
			class_type = "SoftwareSpecification",
			base_type = "SoftwareResourceSpecification",
			schema = "/resourceCatalogManagement/v4/schema/SoftwareSpecification",
			version = "0.1",
			status = in_test,
			category = "ODA",
			target_schema = #target_schema_ref{class_type = "InstalledSoftware",
					schema = ?PathCatalogSchema ++ "/InstalledSoftware"},
			party = [PartyRef],
			attributes = #{"releaseStatus" => "generalDeployment",
					"majorVersion" => "24",
					"minorVersion" => "0",
					"maintenanceVersion" => "5",
					"isDistributable" => true,
					"installSize" => #{"amount" => 256.00,
							"units" => "MB"}}}.

-spec im_inets_spec() -> specification().
%% @doc
im_inets_spec() ->
	PartyRef = #party_ref{id = "9f16q754823e", name = "Ericsson",
			role = "Supplier", href = ?PathPartySpec ++ "9f16q754823e",
			ref_type = "Organization"},
	#specification{name = "inets",
			description = "Erlang application inets software specification",
			class_type = "SoftwareSpecification",
			base_type = "SoftwareResourceSpecification",
			schema = "/resourceCatalogManagement/v4/schema/SoftwareSpecification",
			version = "0.1",
			status = in_test,
			category = "ODA",
			target_schema = #target_schema_ref{class_type = "InstalledSoftware",
					schema = ?PathCatalogSchema ++ "/InstalledSoftware"},
			party = [PartyRef],
			attributes = #{"releaseStatus" => "generalDeployment",
					"majorVersion" => "7",
					"minorVersion" => "4",
					"maintenanceVersion" => "1",
					"isDistributable" => true,
					"installSize" => #{"amount" => 256.00,
							"units" => "MB"}}}.

-spec im_httpd_spec() -> specification().
%% @doc
im_httpd_spec() ->
	ServerName = #specification_char{name = "serverName",
			description = "Domain Name System (DNS) name.",
			value_type = "string"},
	BindAddress = #specification_char{name = "bindAddress",
			description = "IP address or name of the host to which the protocol handler is bound.",
			value_type = "string"},
	Port = #specification_char{name = "port",
			description = "Address of the service within the System.",
			value_type = "integer"},
	SocketType = #specification_char{name = "socketType",
			description = "The communication properties visible to a user.",
			value_type = "string"},
	ServerRoot = #specification_char{name = "serverRoot",
			description = "Root directory of the server.",
			value_type = "string"},
	DocumentRoot = #specification_char{name = "documentRoot",
			description = "Document directory of the application.",
			value_type = "string"},
	Chars = [ServerName, BindAddress, Port,
			SocketType, ServerRoot, DocumentRoot],
	PartyRef = #party_ref{id = "9f16q754823e", name = "Ericsson",
			role = "Supplier", href = ?PathPartySpec ++ "9f16q754823e",
			ref_type = "Organization"},
	#specification{name = "httpd",
			description = "httpd resource function specification",
			class_type = "ResourceFunctionSpecification",
			version = "0.1",
			status = in_test,
			category = "ODA",
			target_schema = #target_schema_ref{class_type = "ResourceFunction",
					schema = ?PathCatalogSchema ++ "/ResourceFunction"},
			characteristic = Chars,
			party = [PartyRef],
			connection_point
					= specification_conn_point(["TMF634", "TMF639"])}.

-spec im_erlang_node_spec() -> specification().
%% @doc
im_erlang_node_spec() ->
	PartyRef = #party_ref{id = "9f16q754823e", name = "Ericsson",
			role = "Supplier", href = ?PathPartySpec ++ "9f16q754823e",
			ref_type = "Organization"},
	#specification{name = "Erlang Runtime",
			description = "Erlang runtime environment resource function specification",
			class_type = "ResourceFunctionSpecification",
			version = "0.1",
			status = in_test,
			category = "ODA",
			target_schema = #target_schema_ref{class_type = "ResourceFunction",
					schema = ?PathCatalogSchema ++ "/ResourceFunction"},
			party = [PartyRef]}.

-spec im_kernel_spec() -> specification().
%% @doc
im_kernel_spec() ->
	PartyRef = #party_ref{id = "9f16q754823e", name = "Ericsson",
			role = "Supplier", href = ?PathPartySpec ++ "9f16q754823e",
			ref_type = "Organization"},
	#specification{name = "kernel",
			description = "Erlang kernel software specification",
			class_type = "SoftwareSpecification",
			base_type = "SoftwareResourceSpecification",
			schema = "/resourceCatalogManagement/v4/schema/SoftwareSpecification",
			version = "0.1",
			status = in_test,
			category = "ODA",
			target_schema = #target_schema_ref{class_type = "InstalledSoftware",
					schema = ?PathCatalogSchema ++ "/InstalledSoftware"},
			party = [PartyRef],
			attributes = #{"releaseStatus" => "generalDeployment",
					"majorVersion" => "8",
					"minorVersion" => "0",
					"maintenanceVersion" => "2",
					"isDistributable" => true,
					"installSize" => #{"amount" => 256.00,
							"units" => "MB"}}}.

-spec im_net_kernel_spec() -> specification().
%% @doc
im_net_kernel_spec() ->
	PartyRef = #party_ref{id = "9f16q754823e", name = "Ericsson",
			role = "Supplier", href = ?PathPartySpec ++ "9f16q754823e",
			ref_type = "Organization"},
	#specification{name = "net_kernel",
			description = "Erlang kernel resource function specification",
			class_type = "ResourceFunctionSpecification",
			version = "0.1",
			status = in_test,
			category = "ODA",
			target_schema = #target_schema_ref{class_type = "ResourceFunction",
					schema = ?PathCatalogSchema ++ "/ResourceFunction"},
			party = [PartyRef],
			connection_point = specification_conn_point(["RPC"])}.

-spec im_rpc_spec() -> specification().
%% @doc
im_rpc_spec() ->
	PartyRef = #party_ref{id = "9f16q754823e", name = "Ericsson",
			role = "Supplier", href = ?PathPartySpec ++ "9f16q754823e",
			ref_type = "Organization"},
	#specification{name = "RPC",
			description = "Remote procedure call service specification",
			class_type = "ApiSpecification",
			base_type = "SoftwareResourceSpecification",
			schema = "/resourceCatalogManagement/v4/schema/ApiSpecification",
			version = "0.1",
			status = in_test,
			category = "ODA",
			target_schema = #target_schema_ref{class_type = "API",
					schema = ?PathCatalogSchema ++ "/API"},
			party = [PartyRef]}.

-spec sigscale_rim_spec() -> specification().
%% @doc
sigscale_rim_spec() ->
	PartyRef = #party_ref{id = "9f16f654223e", name = "SigScale",
			role = "Supplier", href = ?PathPartySpec ++ "9f16f654223e",
			ref_type = "Organization"},
	#specification{name = "SigScale RIM",
			description = "SigScale RIM resource function specification",
			class_type = "ResourceFunctionSpecification",
			version = "0.1",
			status = in_test,
			category = "ODA",
			target_schema = #target_schema_ref{class_type = "ResourceFunction",
					schema = ?PathCatalogSchema ++ "/ResourceFunction"},
			party = [PartyRef],
			related = specification_rel(["Erlang Runtime"])}.

-spec ngc_category() -> category().
%% @doc
ngc_category() ->
	#category{name = "5GC",
			description = "5G Core Network (5GC)",
			class_type = "ResourceCategory",
			status = active,
			version = "1.0"}.

-spec nr_category() -> category().
%% @doc
nr_category() ->
	#category{name = "NR",
			description = "5G New Radio",
			class_type = "ResourceCategory",
			status = active,
			version = "1.0"}.

-spec epc_category() -> category().
%% @doc
epc_category() ->
	#category{name = "EPC",
			description = "Evolved Packet Core (EPC)",
			class_type = "ResourceCategory",
			status = active,
			version = "1.0"}.

-spec lte_category() -> category().
%% @doc
lte_category() ->
	#category{name = "LTE",
			description = "Evolved Universal Terrestrial Radio "
					"Access Network (E-UTRAN)",
			class_type = "ResourceCategory",
			status = active,
			version = "1.0"}.

-spec core_category() -> category().
%% @doc
core_category() ->
	#category{name = "Core",
			description = "Core Network (CN)",
			class_type = "ResourceCategory",
			status = active,
			version = "1.0"}.

-spec umts_category() -> category().
%% @doc
umts_category() ->
	#category{name = "UMTS",
			description = "Universal Terrestrial Radio Access Network (UTRAN)",
			class_type = "ResourceCategory",
			status = active,
			version = "1.0"}.

-spec gsm_category() -> category().
%% @doc
gsm_category() ->
	#category{name = "GSM",
			description = "GSM/EDGE Radio Access Network (GERAN) Network",
			class_type = "ResourceCategory",
			status = active,
			version = "1.0"}.

-spec ims_category() -> category().
%% @doc
ims_category() ->
	#category{name = "IMS",
			description = "IP Multimedia Subsystem (IMS)",
			class_type = "ResourceCategory",
			status = active,
			version = "1.0"}.

-spec oda_category() -> category().
%% @doc
oda_category() ->
	#category{name = "ODA",
			description = "TM Forum ODA Components",
			class_type = "ResourceCategory",
			status = active,
			version = "1.0"}.

-spec ng_catalog() -> catalog().
%% @doc
ng_catalog() ->
%	CategoryNames = ["5GC", "NR", "Slice", "MEC"],
	CategoryNames = ["5GC", "NR"],
	Fcategoryref = fun(Name, Acc) ->
			case im:get_category_name(Name) of
				{ok, #category{id = CategoryId, href = CategoryHref,
						name = CategoryName, version = CategoryVersion}} ->
					[#category_ref{id = CategoryId, href = CategoryHref,
							name = CategoryName, version = CategoryVersion} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource category",
							{category, Name}, {error, Reason}]),
					Acc
			end
	end,
	CategoryRefs = lists:foldr(Fcategoryref, [], CategoryNames),
	#catalog{name = "5G",
			description = "Catalog for 5G",
			class_type = "ResourceCatalog",
			status = active,
			version = "1.0",
			category = CategoryRefs}.

-spec lte_catalog() -> catalog().
%% @doc
lte_catalog() ->
	CategoryNames = ["EPC", "LTE", "IMS"],
	Fcategoryref = fun(Name, Acc) ->
			case im:get_category_name(Name) of
				{ok, #category{id = CategoryId, href = CategoryHref,
						name = CategoryName, version = CategoryVersion}} ->
					[#category_ref{id = CategoryId, href = CategoryHref,
							name = CategoryName, version = CategoryVersion} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource category",
							{category, Name}, {error, Reason}]),
					Acc
			end
	end,
	#catalog{name = "4G",
			description = "Catalog for 4G",
			class_type = "ResourceCatalog",
			status = active,
			version = "1.0",
			category = lists:foldr(Fcategoryref, [], CategoryNames)}.

-spec umts_catalog() -> catalog().
%% @doc
umts_catalog() ->
	CategoryNames = ["Core", "UMTS", "IMS"],
	Fcategoryref = fun(Name, Acc) ->
			case im:get_category_name(Name) of
				{ok, #category{id = CategoryId, href = CategoryHref,
						name = CategoryName, version = CategoryVersion}} ->
					[#category_ref{id = CategoryId, href = CategoryHref,
							name = CategoryName, version = CategoryVersion} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource category",
							{category, Name}, {error, Reason}]),
					Acc
			end
	end,
	#catalog{name = "3G",
			description = "Catalog for 3G",
			class_type = "ResourceCatalog",
			status = active,
			version = "1.0",
			category = lists:foldr(Fcategoryref, [], CategoryNames)}.

-spec gsm_catalog() -> catalog().
%% @doc
gsm_catalog() ->
	CategoryNames = ["Core", "GSM"],
	Fcategoryref = fun(Name, Acc) ->
			case im:get_category_name(Name) of
				{ok, #category{id = CategoryId, href = CategoryHref,
						name = CategoryName, version = CategoryVersion}} ->
					[#category_ref{id = CategoryId, href = CategoryHref,
							name = CategoryName, version = CategoryVersion} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource category",
							{category, Name}, {error, Reason}]),
					Acc
			end
	end,
	#catalog{name = "2G",
			description = "Catalog for 2G",
			class_type = "ResourceCatalog",
			status = active,
			version = "1.0",
			category = lists:foldr(Fcategoryref, [], CategoryNames)}.

-spec oda_catalog() -> catalog().
%% @doc
oda_catalog() ->
	CategoryNames = ["ODA"],
	Fcategoryref = fun(Name, Acc) ->
			case im:get_category_name(Name) of
				{ok, #category{id = CategoryId, href = CategoryHref,
						name = CategoryName, version = CategoryVersion}} ->
					[#category_ref{id = CategoryId, href = CategoryHref,
							name = CategoryName, version = CategoryVersion} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource category",
							{category, Name}, {error, Reason}]),
					Acc
			end
	end,
	#catalog{name = "ODA",
			description = "Catalog for ODA components",
			class_type = "ResourceCatalog",
			status = active,
			version = "1.0",
			category = lists:foldr(Fcategoryref, [], CategoryNames)}.

-spec im_catalog_api_res(Node) -> Resource
	when
		Node :: atom(),
		Resource :: resource().
%% @doc Resource Catalog API.
im_catalog_api_res(_Node) ->
	Name = "TMF634",
	case im:get_specification_name(Name) of
		{ok, #specification{id = Id, href = Href,
				name = Name, class_type = Type, version = Version}} ->
			#resource{name = Name,
					description = "Resource Catalog management API (TMF634)",
					category = "ODA",
					class_type = "API",
					version = "0.1",
					specification = #specification_ref{id = Id, href = Href,
							name = Name, ref_type = Type, version = Version}};
		{error, Reason} ->
			error_logger:warning_report(["Error reading resource specification",
					{specification, Name}, {error, Reason}]),
			{error, Reason}
	end.

-spec im_catalog_res(Node) -> Resource
	when
		Node :: atom(),
		Resource :: resource().
%% @doc Inventory Catalog resource function.
im_catalog_res(_Node) ->
	im_catalog_res(_Node, im:get_specification_name("Resource Catalog")).
%% @hidden
im_catalog_res(_Node, {ok, #specification{id = SId, href = SHref, name = SName,
		class_type = SType, version = SVersion}}) ->
	#resource{name = "Resource Catalog",
			description = "Resource Catalog resource function",
			category = "ODA",
			class_type = "ResourceFunction",
			version = "0.1",
			specification = #specification_ref{id = SId, href = SHref,
					name = SName, ref_type = SType, version = SVersion},
			connection_point = resource_conn_point(["TMF634"])};
im_catalog_res(_Node, {error, Reason}) ->
	error_logger:warning_report(["Error reading resource specification",
			{error, Reason}]),
	{error, Reason}.

-spec im_inventory_api_res(Node) -> Resource
	when
		Node :: atom(),
		Resource :: resource().
%% @doc Component Inventory API.
im_inventory_api_res(_Node) ->
	Name = "TMF639",
	case im:get_specification_name(Name) of
		{ok, #specification{id = Id, href = Href,
				name = Name, class_type = Type, version = Version}} ->
			#resource{name = Name,
					description = "Resource Inventory management API (TMF639)",
					category = "ODA",
					class_type = "API",
					version = "0.1",
					specification = #specification_ref{id = Id, href = Href,
							name = Name, ref_type = Type, version = Version}};
		{error, Reason} ->
			error_logger:warning_report(["Error reading resource specification",
					{specification, Name}, {error, Reason}]),
			{error, Reason}
	end.

-spec im_inventory_res(Node) -> Resource
	when
		Node :: atom(),
		Resource :: resource().
%% @doc Resource Inventory resource function.
im_inventory_res(_Node) ->
	im_inventory_res(_Node, im:get_specification_name("Resource Inventory")).
%% @hidden
im_inventory_res(_Node, {ok, #specification{id = SId,
		href = SHref, name = SName, class_type = SType, version = SVersion}}) ->
	#resource{name = "Resource Inventory",
			description = "Resource Inventory resource function",
			category = "ODA",
			class_type = "ResourceFunction",
			version = "0.1",
			specification = #specification_ref{id = SId, href = SHref,
					name = SName, ref_type = SType, version = SVersion},
			connection_point = resource_conn_point(["TMF639"])};
im_inventory_res(_Node, {error, Reason}) ->
	error_logger:warning_report(["Error reading specification resource",
			{error, Reason}]),
	{error, Reason}.

-spec im_application_res(Node) -> Resource
	when
		Node :: atom(),
		Resource :: resource().
%% @doc Erlang application for SigScale RIM (im).
im_application_res(_Node) ->
	im_application_res(_Node, im:get_specification_name("im")).
%% @hidden
im_application_res(_Node, {error, Reason}) ->
	error_logger:warning_report(["Error reading resource specification",
			{error, Reason}]),
	{error, Reason};
im_application_res(_Node, {ok, #specification{id = SId, href = SHref,
		name = SName, class_type = SType, version = SVersion}}) ->
	Chars = ["restPageSize", "restPageTimeout", "tlsKey", "tlsCert",
			"tlsCacert", "oauthAudience", "oauthIssuer", "oauthKey"],
	#resource{name = "im",
			description = "Erlang application im",
			category = "ODA",
			class_type = "InstalledSoftware",
			base_type = "LogicalResource",
			schema = ?PathInventorySchema ++ "/InstalledSoftware",
			version = "0.1",
			specification = #specification_ref{id = SId, href = SHref,
					name = SName, ref_type = SType, version = SVersion},
			characteristic = lists:filtermap(fun get_rim_chars/1, Chars)}.

-spec im_inets_res(Node) -> Resource
	when
		Node :: atom(),
		Resource :: resource().
%% @doc Erlang inets application.
im_inets_res(_Node) ->
	im_inets_res(_Node, im:get_specification_name("inets")).
%% @hidden
im_inets_res(_Node, {ok, #specification{id = SId, href = SHref, name = SName,
		class_type = SType, version = SVersion}}) ->
	#resource{name = "inets",
			description = "Erlang application inets",
			category = "ODA",
			class_type = "InstalledSoftware",
			version = "0.1",
			specification = #specification_ref{id = SId, href = SHref,
					name = SName, ref_type = SType, version = SVersion}};
im_inets_res(_Node, {error, Reason}) ->
	error_logger:warning_report(["Error reading resource specification",
			{error, Reason}]),
	{error, Reason}.

-spec im_erlang_res(Node) -> Resource
	when
		Node :: atom(),
		Resource :: resource().
%% @doc Component Catalog resource function.
im_erlang_res(_Node) ->
	im_erlang_res(_Node, im:get_specification_name("Erlang")).
%% @hidden
im_erlang_res(_Node, {ok, #specification{id = SId, href = SHref, name = SName,
		class_type = SType, version = SVersion}}) ->
	#resource{name = "Erlang",
			description = "Erlang runtime environment",
			category = "ODA",
			class_type = "InstalledSoftware",
			version = "0.1",
			specification = #specification_ref{id = SId, href = SHref,
					name = SName, ref_type = SType, version = SVersion}};
im_erlang_res(_Node, {error, Reason}) ->
	error_logger:warning_report(["Error reading resource specification",
			{error, Reason}]),
	{error, Reason}.

-spec im_httpd_res(Node) -> Resource
	when
		Node :: atom(),
		Resource :: resource().
%% @doc Erlang httpd resource function.
im_httpd_res(_Node) ->
	im_httpd_res(_Node, im:get_specification_name("httpd")).
%% @hidden
im_httpd_res(_Node, {ok, #specification{id = SId, href = SHref, name = SName,
		class_type = SType, version = SVersion}}) ->
	#resource{name = "httpd",
			description = "Erlang httpd resource function",
			category = "ODA",
			class_type = "ResourceFunction",
			version = "0.1",
			specification = #specification_ref{id = SId, href = SHref,
					name = SName, ref_type = SType, version = SVersion},
			characteristic = get_httpd_chars(
					application:get_env(inets, services)),
			connection_point = resource_conn_point(["TMF634", "TMF639"])};
im_httpd_res(_Node, {error, Reason}) ->
	error_logger:warning_report(["Error reading resource specification",
			{error, Reason}]),
	{error, Reason}.

-spec im_erlang_node_res(Node) -> Resource
	when
		Node :: atom(),
		Resource :: resource().
%% @doc Erlang node resource function.
im_erlang_node_res(Node) when is_atom(Node) ->
	im_erlang_node_res(Node, im:get_specification_name("Erlang Runtime")).
%% @hidden
im_erlang_node_res(Node, {ok, #specification{id = SId,
		href = SHref, name = SName, class_type = SType, version = SVersion}}) ->
	#resource{name = atom_to_list(Node),
			description = "Erlang node resource function",
			category = "ODA",
			class_type = "ResourceFunction",
			version = "0.1",
			specification = #specification_ref{id = SId, href = SHref,
					name = SName, ref_type = SType, version = SVersion}};
im_erlang_node_res(_Node, {error, Reason}) ->
	error_logger:warning_report(["Error reading resource specification",
			{error, Reason}]),
	{error, Reason}.

-spec im_kernel_res(Node) -> Resource
	when
		Node :: atom(),
		Resource :: resource().
%% @doc Erlang kernel resource function.
im_kernel_res(_Node) ->
	im_kernel_res(_Node, im:get_specification_name("kernel")).
%% @hidden
im_kernel_res(_Node, {ok, #specification{id = SId, href = SHref, name = SName,
		class_type = SType, version = SVersion}}) ->
	#resource{name = "kernel",
			description = "Erlang kernel resource function",
			category = "ODA",
			class_type = "InstalledSoftware",
			version = "0.1",
			specification = #specification_ref{id = SId, href = SHref,
					name = SName, ref_type = SType, version = SVersion}};
im_kernel_res(_Node, {error, Reason}) ->
	error_logger:warning_report(["Error reading resource specification",
			{error, Reason}]),
	{error, Reason}.

-spec im_net_kernel_res(Node) -> Resource
	when
		Node :: atom(),
		Resource :: resource().
%% @doc Erlang net kernel resource function.
im_net_kernel_res(_Node) ->
	im_net_kernel_res(_Node, im:get_specification_name("net_kernel")).
%% @hidden
im_net_kernel_res(_Node, {ok, #specification{id = SId,
		href = SHref, name = Name, class_type = SType, version = SVersion}}) ->
	#resource{name = Name,
			description = "Erlang kernel resource function",
			category = "ODA",
			class_type = "ResourceFunction",
			version = "0.1",
			specification = #specification_ref{id = SId, href = SHref,
					name = Name, ref_type = SType, version = SVersion},
			connection_point = resource_conn_point(["RPC"])};
im_net_kernel_res(_Node, {error, Reason}) ->
	error_logger:warning_report(["Error reading resource specification",
			{error, Reason}]),
	{error, Reason}.

-spec im_rpc_res(Node) -> Resource
	when
		Node :: atom(),
		Resource :: resource().
%% @doc Remote procedure call API resource.
im_rpc_res(_Node) ->
	Name = "RPC",
	case im:get_specification_name(Name) of
		{ok, #specification{id = Id, href = Href,
				name = Name, class_type = Type, version = Version}} ->
			#resource{name = Name,
					description = "Remote procedure call API",
					category = "ODA",
					class_type = "API",
					version = "0.1",
					specification = #specification_ref{id = Id, href = Href,
							name = Name, ref_type = Type, version = Version}};
		{error, Reason} ->
			error_logger:warning_report(["Error reading resource specification",
					{specification, Name}, {error, Reason}]),
			{error, Reason}
	end.

-spec sigscale_rim_res() -> resource().
%% @doc SigScale RIM resource function.
sigscale_rim_res() ->
	sigscale_rim_res(im:get_specification_name("SigScale RIM")).
%% @hidden
sigscale_rim_res({ok, #specification{id = SId, href = SHref, name = Name,
		class_type = SType, version = SVersion}}) ->
	#resource{name = Name,
			description = "SigScale RIM resource function",
			category = "ODA",
			class_type = "ResourceFunction",
			version = "0.1",
			specification = #specification_ref{id = SId, href = SHref,
					name = Name, ref_type = SType, version = SVersion}};
sigscale_rim_res({error, Reason}) ->
	error_logger:warning_report(["Error reading resource specification",
			{error, Reason}]),
	{error, Reason}.

%%----------------------------------------------------------------------
%% internal functions
%%----------------------------------------------------------------------

%% @hidden
eprpeps_refs() ->
	Name = "EP_RP_EPS",
	case im:get_specification_name(Name) of
		{ok, #specification{id = Sid, href = Shref, name = Name,
				class_type = Stype}} ->
			EpSbiXRel = #specification_rel{id = Sid, href = Shref, name = Name,
					ref_type = Stype, rel_type = "composedOf"},
			EpSbiXCP = #specification_ref{id = Sid, href = Shref, name = Name,
					class_type = "ConnectionPointRef", ref_type = Stype},
			{EpSbiXRel, EpSbiXCP};
		{error, Reason} ->
			error_logger:warning_report(["Error reading resource specification",
					{specification, Name}, {error, Reason}])
	end.

%% @hidden
specification_rel(SpecificationNames) ->
	Fspecrel = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{id = Sid, href = Shref,
						name = Sname, class_type = Stype}} ->
					[#specification_rel{id = Sid, href = Shref, name = Sname,
							ref_type = Stype, rel_type = "composedOf"} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	lists:reverse(lists:foldl(Fspecrel, [], SpecificationNames)).

%% @hidden
specification_conn_point(SpecificationNames) ->
	Fspeccp = fun(Name, Acc) ->
			case im:get_specification_name(Name) of
				{ok, #specification{id = Sid, href = Shref,
						name = Sname, class_type = Stype}} ->
					[#specification_ref{id = Sid, href = Shref, name = Sname,
							ref_type = Stype, class_type = "ConnectionPointRef"} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource specification",
							{specification, Name}, {error, Reason}]),
					Acc
			end
	end,
	lists:foldr(Fspeccp, [], SpecificationNames).

%% @hidden
resource_conn_point(ResourceNames) ->
	Fspeccp = fun(Name, Acc) ->
			case im:get_resource_name(Name) of
				{ok, #resource{id = Rid, href = Rhref,
						name = Rname, class_type = Rtype}} ->
					[#resource_ref{id = Rid, href = Rhref,
							name = Rname, ref_type = Rtype,
							class_type = "ConnectionPointRef"} | Acc];
				{error, Reason} ->
					error_logger:warning_report(["Error reading resource inventory",
							{resource, Name}, {error, Reason}]),
					Acc
			end
	end,
	lists:foldr(Fspeccp, [], ResourceNames).

-spec get_rim_chars(Char) -> Result
	when
		Char :: string(),
		Result :: {true, #resource_char{}} | false.
%% @doc used in lists:filtermap for ODA characteristics.
get_rim_chars("restPageSize" = Char) ->
	get_rim_chars(Char, application:get_env(im, rest_page_size));
get_rim_chars("restPageTimeout" = Char) ->
	get_rim_chars(Char, application:get_env(im, rest_page_timeout));
get_rim_chars("tlsKey" = Char) ->
	get_rim_chars(Char, application:get_env(im, tls_key));
get_rim_chars("tlsCert" = Char) ->
	get_rim_chars(Char, application:get_env(im, tls_cert));
get_rim_chars("tlsCacert" = Char) ->
	get_rim_chars(Char, application:get_env(im, tls_cacert));
get_rim_chars("oauthAudience" = Char) ->
	get_rim_chars(Char, application:get_env(im, oauth_audience));
get_rim_chars("oauthIssuer" = Char) ->
	get_rim_chars(Char, application:get_env(im, oauth_issuer));
get_rim_chars("oauthKey" = Char) ->
	get_rim_chars(Char, application:get_env(im, oauth_key)).
%% @hidden
get_rim_chars(_Char, undefined) ->
	false;
get_rim_chars(_Char, {ok, undefined}) ->
	false;
get_rim_chars(Char, {ok, Size}) when
		Char == "restPageSize"; Char == "restPageTimeout", is_integer(Size) ->
	{true, #resource_char{name = Char, value = Size}};
get_rim_chars(Char, {ok, Size}) when is_list(Size) ->
	{true, #resource_char{name = Char, value = Size}}.

%% @hidden
get_httpd_chars(undefined) ->
	[];
get_httpd_chars({ok, undefined}) ->
	[];
get_httpd_chars({ok, [{httpd, Config}]}) ->
	Keys = [server_name, port, server_root, document_root],
	F2 = fun(false) ->
				false;
			({_, undefined}) ->
				false;
			({server_name, Value}) when is_list(Value) ->
				{true, #resource_char{name = "serverName", value = Value}};
			({port, Value}) when is_integer(Value) ->
				{true, #resource_char{name = "port", value = Value}};
			({server_root, Value}) ->
				{true, #resource_char{name = "serverRoot", value = Value}};
			({document_root, Value}) ->
				{true, #resource_char{name = "documentRoot", value = Value}}
	end,
	F1 = fun(Key) ->
			F2(lists:keyfind(Key, 1, Config))
	end,
	lists:filtermap(F1, Keys).

