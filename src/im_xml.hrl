%% im_xml 

-record(state,
		{parse_module :: atom(),
		parse_function :: atom(),
		parse_state :: term(),
		dn_prefix = [] :: [string()],
		stack = [] :: list(),
		spec_cache = [] :: [specification_ref()]}).
-type state() :: #state{}.

-record(generic_state,
		{subnet = [] :: [string()],
		me_context = [] :: [string()],
		managed_element = [] :: [string()],
		vs_data :: map(),
		vs_data_container = [] :: [map()]}).
-type generic_state() :: #generic_state{}.

-record(geran_state,
		{bss :: map(),
		bts :: map(),
		cell :: map(),
		gsm_rel :: map(),
		btss = [] :: [string()],
		cells = [] :: [string()]}).
-type geran_state() :: #geran_state{}.

-record(utran_state,
		{rnc :: map(),
		fdd :: map(),
		utran_rel :: map(),
		tdd_lcr :: map(),
		tdd_hcr :: map(),
		nodeb :: map(),
		iub :: map(),
		iucs :: map(),
		iups :: map(),
		iur :: map(),
		fdds = [] :: [string()]}).
-type utran_state() :: #utran_state{}.

-record(epc_state,
		{epdg :: map(),
		mme :: map(),
		pcrf :: map(),
		pgw :: map(),
		sgw :: map(),
		ep_rp_eps :: map(),
		ep_rp_epss :: [string()]}).
-type epc_state() :: #epc_state{}.

-record(core_state,
		{msc :: map(),
		iucs :: map(),
		mgw :: map(),
		ggsn :: map(),
		sgsn :: map(),
		iups :: map()}).
-type core_state() :: #core_state{}.

-record(zte_state,
		{bts :: map(),
		vs_data :: map(),
		cell :: map(),
		cells = [] :: [string()],
		vs_data_container = [] :: [map()]}).
-type zte_state() :: #zte_state{}.

-record(huawei_state,
		{gsm_function :: map(),
		umts_function :: map(),
		bts :: map(),
		nodeb :: map()}).
-type huawei_state() :: #huawei_state{}.
