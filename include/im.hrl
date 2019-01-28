%% im.hrl

-type catalog_status() :: in_study | in_design | in_test
		| rejected | active | launched | retired | obsolete.

-record(category_ref,
		{id :: string(),
		href :: string(),
		name :: string(),
		version :: string()}).
-type category_ref() :: #category_ref{}.

-record(candidate_ref,
		{id :: string(),
		href :: string(),
		name :: string(),
		version :: string()}).
-type candidate_ref() :: #candidate_ref{}.

-record(related_party_ref,
		{id :: string(),
		href :: string(),
		name :: string(),
		role :: string(),
		start_date :: pos_integer(),
		end_date :: pos_integer()}).
-type related_party_ref() :: #related_party_ref{}.

-record(specification_ref,
		{id :: string(),
		href :: string(),
		name :: string(),
		version :: string()}).
-type specification_ref() :: #specification_ref{}.

-record(specification_rel,
		{id :: string(),
		href :: string(),
		name :: string(),
		type :: string(),
		role :: string(),
		start_date :: pos_integer(),
		end_date :: pos_integer()}).
-type specification_rel() :: #specification_rel{}.

-record(spec_char_rel,
		{id :: string(),
		href :: string(),
		name :: string(),
		type :: string(),
		class_type :: string(),
		start_date :: pos_integer(),
		end_date :: pos_integer()}).
-type spec_char_rel() :: #spec_char_rel{}.

-record(resource_rel,
		{id :: string(),
		href :: string(),
		type :: string(),
		start_date :: pos_integer(),
		end_date :: pos_integer()}).
-type resource_rel() :: #resource_rel{}.

-record(place_ref,
		{id :: string(),
		href :: string(),
		name :: string(),
		role :: string()}).
-type place_ref() :: #place_ref{}.

-record(target_schema_ref,
		{class_type :: string(),
		schema :: string()}).
-type target_schema_ref() :: #target_schema_ref{}.

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

-record(note,
		{author :: string(),
		date :: pos_integer(),
		text :: string()}).
-type note() :: #note{}.

-record(catalog,
		{id :: string(),
		href :: string(),
		name :: string(),
		description :: string(),
		class_type :: string(),
		base_type :: string(),
		schema :: string(),
		version :: string(),
		start_date :: pos_integer(),
		end_date :: pos_integer(),
		last_modified :: pos_integer(),
		status :: catalog_status(),
		related_party = [] :: [related_party_ref()],
		category = [] :: [string()]}).
-type catalog() :: #catalog{}.

-record(category,
		{id :: string(),
		href :: string(),
		name :: string(),
		description :: string(),
		class_type :: string(),
		base_type :: string(),
		schema :: string(),
		version :: string(),
		start_date :: pos_integer(),
		end_date :: pos_integer(),
		last_modified :: pos_integer(),
		status :: catalog_status(),
		parent :: string(),
		root = false :: boolean(),
		related_party = [] :: [related_party_ref()],
		category = [] :: [category_ref()],
		candidate = [] :: [candidate_ref()]}).
-type category() :: #category{}.

-record(candidate,
		{id :: string(),
		href :: string(),
		name :: string(),
		description :: string(),
		class_type :: string(),
		base_type :: string(),
		schema :: string(),
		version :: string(),
		start_date :: pos_integer(),
		end_date :: pos_integer(),
		last_modified :: pos_integer(),
		status :: catalog_status(),
		category = [] :: [category_ref()],
		specification = [] :: [specification_ref()]}).
-type candidate() :: #candidate{}.

-record(specification,
		{id :: string() | '_',
		href :: string(),
		name :: string(),
		description :: string(),
		class_type :: string(),
		base_type :: string(),
		schema :: string(),
		status :: string(),
		version :: string(),
		start_date :: pos_integer(),
		end_date :: pos_integer(),
		last_modified :: pos_integer(),
		bundle = false :: boolean(),
		category :: string(),
		target_schema :: target_schema_ref(),
		model :: string(),
		part :: string(),
		sku :: string(),
		vendor :: string(),
		device_serial :: string(),
		device_version :: string(),
		feature = [] :: [feature()],
		attachment = [] :: [attachment()],
		related_party = [] :: [related_party_ref()],
		characteristic = [] :: [specification_char()],
		related = [] :: [specification_rel()]}).
-type specification() :: #specification{}.

-record(spec_char_value,
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
-type spec_char_value() :: #spec_char_value{}.

-record(specification_char,
		{name :: string(),
		description :: string(),
		value_type :: string(),
		class_type :: string(),
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
		char_relation = [] :: [spec_char_rel()],
		char_value = [] :: [spec_char_value()]}).
-type specification_char() :: #specification_char{}.

-record(resource,
		{id :: string() | '_' | '$1',
		href :: string() | '_',
		public_id :: string() | '_',
		name :: string() | '_' | '$2',
		description :: string() | '_',
		category :: string() | '_',
		class_type :: string() | '_' | '$3',
		base_type :: string() | '_',
		schema :: string() | '_',
		status :: string() | '_',
		version :: string() | '_',
		start_date :: pos_integer() | '_',
		end_date :: pos_integer() | '_',
		last_modified :: pos_integer() | '_',
		related = [] :: [resource_rel()] | '_',
		place = [] :: [place_ref()] | '_',
		note = [] ::[note()] | '_',
		specification :: specification_ref() | '_',
		related_party = [] :: [related_party_ref()] | '_',
		characteristic = [] :: [resource_char()] | '_'}).
-type resource() :: #resource{}.

-record(resource_char,
		{name :: string(),
		value :: term(),
		class_type :: string(),
		schema :: string()}).
-type resource_char() :: #resource_char{}.

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

