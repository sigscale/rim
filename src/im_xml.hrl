%% im_xml 

-record(state,
		{parse_module :: atom(),
		parse_function :: atom(),
		parse_state = [] :: [term()],
		dn_prefix = [] :: [string()],
		stack = [] :: list(),
		spec_cache = [] :: [specification_ref()]}).
-type state() :: #state{}.
