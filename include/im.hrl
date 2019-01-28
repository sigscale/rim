%% im.hrl

-type catalog_status() :: in_study | in_design | in_test
		| rejected | active | launched | retired | obsolete.

-record(related,
		{id :: string(),
		href :: string(),
		name :: string(),
		version :: string()}).
-type related() :: #related{}.

-record(related_party,
		{id :: string(),
		href :: string(),
		name :: string(),
		role :: string(),
		start_date :: pos_integer(),
		end_date :: pos_integer()}).
-type related_party() :: #related_party{}.

-record(related_spec,
		{id :: string(),
		href :: string(),
		name :: string(),
		type :: string(),
		role :: string(),
		start_date :: pos_integer(),
		end_date :: pos_integer()}).
-type spec_relationship() :: #related_spec{}.

-record(related_spec_char,
		{id :: string(),
		href :: string(),
		name :: string(),
		type :: string(),
		role :: string(),
		class_type :: string(),
		start_date :: pos_integer(),
		end_date :: pos_integer()}).
-type char_relationship() :: #related_spec_char{}.

-record(feature,
		{id :: string(),
		href :: string(),
		name :: string(),
		version :: string(),
		class_type :: string(),
		bundle = false :: boolean(),
		start_date :: pos_integer(),
		end_date :: pos_integer(),
		enabled = false :: boolean()}).
-type feature() :: #feature{}.

-record(attachment,
		{id :: string(),
		href :: string(),
		description :: string(),
		type :: string(),
		url :: string()}).
-type attachment() :: #attachment{}.

-record(catalog,
		{id :: string(),
		href :: string(),
		name :: string(),
		description :: string(),
		version :: string(),
		start_date :: pos_integer(),
		end_date :: pos_integer(),
		last_modified :: pos_integer(),
		status :: catalog_status(),
		related_party = [] :: list(),
		category = [] :: [string()]}).
-type catalog() :: #catalog{}.

-record(category,
		{id :: string(),
		href :: string(),
		name :: string(),
		description :: string(),
		version :: string(),
		start_date :: pos_integer(),
		end_date :: pos_integer(),
		last_modified :: pos_integer(),
		status :: catalog_status(),
		parent :: string(),
		root = false :: boolean(),
		related_party = [] :: [related_party()],
		category = [] :: [related()],
		candidate = [] :: [related()]}).
-type category() :: #category{}.

-record(candidate,
		{id :: string(),
		href :: string(),
		name :: string(),
		description :: string(),
		version :: string(),
		start_date :: pos_integer(),
		end_date :: pos_integer(),
		last_modified :: pos_integer(),
		status :: catalog_status(),
		category = [] :: [related()],
		specification = [] :: [related()]}).
-type candidate() :: #candidate{}.

-record(specification,
		{id :: string() | '_',
		href :: string(),
		name :: string(),
		description :: string(),
		category :: string(),
		type :: string(),
		base_type :: string(),
		schema :: string(),
		status :: string(),
		version :: string(),
		start_date :: pos_integer(),
		end_date :: pos_integer(),
		last_modified :: pos_integer(),
		bundle = false :: boolean(),
		feature = [] :: [feature()],
		attachment = [] :: [attachment()],
		related_party = [] :: [related_party()],
		characteristic = [] :: [characteristic()],
		spec_relation = [] :: [spec_relationship()],
		device_manufacture :: pos_integer(),
		device_power :: string(),
		device_serial :: string(),
		device_version :: string(),
		value :: string()}).
-type specification() :: #specification{}.

-record(char_value,
		{value_type :: string(),
		default = false :: boolean(),
		class_type :: string(),
		schema :: string(),
		unit :: string(),
		start_date :: pos_integer(),
		end_date :: pos_integer(),
		from :: term(),
		to :: term(),
		interval :: open | closed | closed_bottom | closed_top,
		regex :: {CompiledRegEx :: re:mp(), OriginalRegEx :: string()},
		value :: term()}).
-type char_value() :: #char_value{}.

-record(characteristic,
		{name :: string(),
		description :: string(),
		value_type :: string(),
		type :: string(),
		schema :: string(),
		value_schema :: string(),
		configurable :: boolean(),
		start_date :: pos_integer(),
		end_date :: pos_integer(),
		min :: non_neg_integer(),
		max :: non_neg_integer(),
		unique :: boolean(),
		regex :: {CompiledRegEx :: re:mp(), OriginalRegEx :: string()},
		extensible :: boolean(),
		char_relation = [] :: [char_relationship()],
		char_value = [] :: [char_value()]}).
-type characteristic() :: #characteristic{}.

-record(resource,
		{id :: string() | '_' | '$1',
		href :: string() | '_',
		name :: string() | '_' | '$2',
		description :: string() | '_',
		category :: string() | '_',
		type :: string() | '_' | '$3',
		base_type :: string() | '_',
		schema :: string() | '_',
		status :: string() | '_',
		version :: string() | '_',
		start_date :: pos_integer(),
		end_date :: pos_integer(),
		last_modified :: pos_integer(),
		specification :: string() | '_',
		characteristic = [] :: [{Name :: string(), Value :: term()}] | '_'}).
-type resource() :: #resource{}.

-record(sites,
		{name :: string(),
		network :: string(),
		longtitude :: string(),
		latitude :: string(),
		city :: string(),
		country :: string(),
		countryCity :: string()}).
-type sites() :: #sites{}.

-record(gsm_relation,
		{id :: string(),
		adjacent_cell :: string(),
		bcch_frequency :: integer(),
		ncc :: integer(),
		bcc :: integer(),
		lac :: integer(),
		is_remove_allowed :: boolean(),
		is_hoa_allowed :: boolean(),
		is_covered_by :: no | yes | partial,
		vs_data_container :: [term()]}).

-record(utran_relation,
		{id :: string(),
		adjacent_cell :: string(),
		vs_data_container :: [term()]}).

-record(eutran_relation,
		{id :: string(),
		tci :: integer(),
		is_remove_allowed :: boolean(),
		is_hoa_allowed :: boolean(),
		is_icic_information_send_allowed :: boolean(),
		is_lb_allowed :: boolean(),
		adjacent_cell :: string(),
		is_es_covered_by :: no | yes | partial,
		cell_individual_offset :: string(),
		q_offset :: string(),
		vs_data_container :: [term()]}).

-record(inter_rat_es_policies,
		{id :: string(),
		act_original_cell_params :: relative_cell_load_parameters(),
		act_candidate_cell_params :: relative_cell_load_parameters(),
		deact_candidate_cell_params :: relative_cell_load_parameters()}).

-record(rel_cell_load_params,
		{load_threshold :: 0..10000,
		time_duration :: 0..900}).
-type relative_cell_load_parameters() :: #inter_rat_es_policies{}.

