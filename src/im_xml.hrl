%% im_xml

-record(state,
		{parse_module :: atom(),
		parse_function :: atom(),
		parse_state :: term(),
		rule :: string() | undefined,
		dn_prefix = [] :: [string()],
		stack = [] :: list(),
		spec_cache = [] :: [specification_ref()]}).
-type state() :: #state{}.

-record(generic_state,
		{subnet = [] :: [string()],
		me_context = [] :: [string()],
		managed_element = [] :: [string()],
		vs_data :: map() | undefined,
		vs_data_container = [] :: [map()]}).
-type generic_state() :: #generic_state{}.

-record(geran_state,
		{bss :: map() | undefined,
		bts :: map() | undefined,
		cell :: map() | undefined,
		gsm_rel :: map() | undefined,
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
		fdds = [] :: [string()]}).
-type utran_state() :: #utran_state{}.

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
		iups :: map() | undefined}).
-type core_state() :: #core_state{}.

-record(pee_state,
		{me :: map() | undefined,
		me_description :: map() | undefined,
		me_config :: map() | undefined}).
-type pee_state() :: #pee_state{}.

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
