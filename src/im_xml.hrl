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
		vs_data = [] :: [map()]}).
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
