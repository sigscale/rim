%% im_xml

-record(state,
		{parse_module :: atom(),
		parse_function :: atom(),
		parse_state :: term(),
		rule :: string() | undefined,
		location = [] :: [map()],
		dn_prefix = [] :: [string()],
		stack = [] :: list(),
		spec_cache = [] :: [specification_ref()]}).
-type state() :: #state{}.

-record(generic_state,
		{subnet = [] :: [string()],
		me_context = [] :: [string()],
		managed_element = [] :: [string()],
		node = [] :: [string()],
		vs_data :: map() | undefined,
		vs_data_container = [] :: [map()]}).
-type generic_state() :: #generic_state{}.

-record(geran_state,
		{bss :: map() | undefined,
		bts :: map() | undefined,
		cell :: map() | undefined,
		gsm_rel :: map() | undefined,
		vs_data :: map() | undefined,
		btss = [] :: [string()],
		cells = [] :: [string()]}).
-type geran_state() :: #geran_state{}.

-record(utran_state,
		{rnc :: map() | undefined,
		fdd :: map() | undefined,
		utran_rel :: map() | undefined,
		tdd_lcr :: map() | undefined,
		tdd_hcr :: map() | undefined,
		nodeb :: map() | undefined,
		iub :: map() | undefined,
		iucs :: map() | undefined,
		iups :: map() | undefined,
		iur :: map() | undefined,
		fdds = [] :: [resource_rel()],
		tdd_lcrs = [] :: [resource_rel()],
		tdd_hcrs = [] :: [resource_rel()],
		iubs = [] :: [resource_rel()]}).
-type utran_state() :: #utran_state{}.

-record(eutran_state,
		{enb :: map() | undefined,
		fdd :: map() | undefined,
		tdd :: map() | undefined,
		eutran_rel :: map() | undefined,
		fdds = [] :: [string()],
		tdds = [] :: [string()]}).
-type eutran_state() :: #eutran_state{}.

-record(epc_state,
		{epdg :: map() | undefined,
		mme :: map() | undefined,
		pcrf :: map() | undefined,
		pgw :: map() | undefined,
		sgw :: map() | undefined,
		ep_rp_eps :: map() | undefined,
		ep_rp_epss = [] :: [string()]}).
-type epc_state() :: #epc_state{}.

-record(core_state,
		{msc :: map() | undefined,
		iucs :: map() | undefined,
		mgw :: map() | undefined,
		ggsn :: map() | undefined,
		sgsn :: map() | undefined,
		iups :: map() | undefined,
		auc :: map() | undefined,
		hlr :: map() | undefined,
		eir :: map() | undefined,
		mnp_srf :: map() | undefined,
		cgf :: map() | undefined}).
-type core_state() :: #core_state{}.

-record(pee_state,
		{me :: map() | undefined,
		me_description :: map() | undefined,
		me_config :: map() | undefined}).
-type pee_state() :: #pee_state{}.

-record(epcn3ai_state,
		{proxy :: map() | undefined,
		server :: map() | undefined}).
-type epcn3ai_state() :: #epcn3ai_state{}.

-record(ims_state,
		{as :: map() | undefined,
		hss :: map() | undefined,
		icscf :: map() | undefined,
		pcscf :: map() | undefined,
		scscf :: map() | undefined}).
-type ims_state() :: #ims_state{}.

-record(im1_state,
		{iu :: map() | undefined,
		tmaiu :: map() | undefined,
		aiu :: map() | undefined,
		ius = [] :: [string()],
		tmaius = [] :: [string()],
		aius = [] :: [string()]}).
-type im1_state() :: #im1_state{}.

-record(im2_state,
		{iu_ne :: map() | undefined,
		iu_hw :: map() | undefined,
		iu_sw :: map() | undefined,
		iu_lic :: map() | undefined,
		iu_nes = [] :: [string()],
		iu_hws= [] :: [string()],
		iu_sws= [] :: [string()],
		iu_lics = [] :: [string()]}).
-type im2_state() :: #im2_state{}.

-record(zte_state,
		{bts :: map() | undefined,
		vs_data :: map() | undefined,
		cell :: map() | undefined,
		cells = [] :: [string()],
		vs_data_container = [] :: [map()]}).
-type zte_state() :: #zte_state{}.

-record(huawei_state,
		{gsm_function :: map() | undefined,
		bts :: map() | undefined,
		gcell :: map() | undefined,
		umts_function :: map() | undefined,
		nodeb :: map() | undefined,
		ucell :: map() | undefined}).
-type huawei_state() :: #huawei_state{}.
