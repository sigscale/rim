%% im.hrl

-type catalog_status() :: in_study | in_design | in_test
		| rejected | active | launched | retired | obsolete.

-record(category_ref,
		{id :: string() | undefined,
		href :: string() | undefined,
		name :: string() | undefined,
		version :: string() | undefined}).
-type category_ref() :: #category_ref{}.

-record(candidate_ref,
		{id :: string() | undefined,
		href :: string() | undefined,
		name :: string() | undefined,
		version :: string() | undefined}).
-type candidate_ref() :: #candidate_ref{}.

-record(related_party_ref,
		{id :: string() | undefined,
		href :: string() | undefined,
		name :: string() | undefined,
		role :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined}).
-type related_party_ref() :: #related_party_ref{}.

-record(specification_ref,
		{id :: string() | undefined,
		href :: string() | undefined,
		name :: string() | undefined,
		version :: string() | undefined}).
-type specification_ref() :: #specification_ref{}.

-record(specification_rel,
		{id :: string() | undefined,
		href :: string() | undefined,
		name :: string() | undefined,
		type :: string() | undefined,
		role :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined}).
-type specification_rel() :: #specification_rel{}.

-record(spec_char_rel,
		{id :: string() | undefined,
		href :: string() | undefined,
		name :: string() | undefined,
		type :: string() | undefined,
		class_type :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined}).
-type spec_char_rel() :: #spec_char_rel{}.

-record(resource_rel,
		{id :: string() | undefined,
		href :: string() | undefined,
		type :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined}).
-type resource_rel() :: #resource_rel{}.

-record(place_ref,
		{id :: string() | undefined,
		href :: string() | undefined,
		name :: string() | undefined,
		role :: string() | undefined}).
-type place_ref() :: #place_ref{}.

-record(target_schema_ref,
		{class_type :: string() | undefined,
		schema :: string() | undefined}).
-type target_schema_ref() :: #target_schema_ref{}.

-record(feature,
		{id :: string() | undefined,
		href :: string() | undefined,
		name :: string() | undefined,
		version :: string() | undefined,
		class_type :: string() | undefined,
		bundle = false :: boolean(),
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		enabled = false :: boolean()}).
-type feature() :: #feature{}.

-record(attachment,
		{id :: string() | undefined,
		href :: string() | undefined,
		description :: string() | undefined,
		type :: string() | undefined,
		url :: string() | undefined}).
-type attachment() :: #attachment{}.

-record(note,
		{author :: string() | undefined,
		date :: pos_integer() | undefined,
		text :: string() | undefined}).
-type note() :: #note{}.

-record(catalog,
		{id :: string() | undefined,
		href :: string() | undefined,
		name :: string() | undefined,
		description :: string() | undefined,
		class_type :: string() | undefined,
		base_type :: string() | undefined,
		schema :: string() | undefined,
		version :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		last_modified :: {TS :: pos_integer(), N :: pos_integer()} | undefined,
		status :: catalog_status() | undefined,
		related_party = [] :: [related_party_ref()],
		category = [] :: [category_ref()]}).
-type catalog() :: #catalog{}.

-record(category,
		{id :: string() | undefined,
		href :: string() | undefined,
		name :: string() | undefined,
		description :: string() | undefined,
		class_type :: string() | undefined,
		base_type :: string() | undefined,
		schema :: string() | undefined,
		version :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		last_modified :: {TS :: pos_integer(), N :: pos_integer()} | undefined,
		status :: catalog_status() | undefined,
		parent :: string() | undefined,
		root = false :: boolean(),
		related_party = [] :: [related_party_ref()],
		category = [] :: [category_ref()],
		candidate = [] :: [candidate_ref()]}).
-type category() :: #category{}.

-record(candidate,
		{id :: string() | undefined,
		href :: string() | undefined,
		name :: string() | undefined,
		description :: string() | undefined,
		class_type :: string() | undefined,
		base_type :: string() | undefined,
		schema :: string() | undefined,
		version :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		last_modified :: {TS :: pos_integer(), N :: pos_integer()} | undefined,
		status :: catalog_status() | undefined,
		category = [] :: [category_ref()],
		specification :: specification_ref() | undefined}).
-type candidate() :: #candidate{}.

-record(specification,
		{id :: string() | undefined,
		href :: string() | undefined,
		name :: string() | undefined,
		description :: string() | undefined,
		class_type :: string() | undefined,
		base_type :: string() | undefined,
		schema :: string() | undefined,
		status :: string() | undefined,
		version :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		last_modified :: {TS :: pos_integer(), N :: pos_integer()} | undefined,
		bundle = false :: boolean() | undefined,
		category :: string() | undefined,
		target_schema :: target_schema_ref() | undefined,
		model :: string() | undefined,
		part :: string() | undefined,
		sku :: string() | undefined,
		vendor :: string() | undefined,
		device_serial :: string() | undefined,
		device_version :: string() | undefined,
		feature = [] :: [feature()],
		attachment = [] :: [attachment()],
		related_party = [] :: [related_party_ref()],
		characteristic = [] :: [specification_char()],
		related = [] :: [specification_rel()]}).
-type specification() :: #specification{}.

-record(spec_char_value,
		{value_type :: string() | undefined,
		default = false :: boolean() | undefined,
		class_type :: string() | undefined,
		schema :: string() | undefined,
		unit :: string() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		from :: term() | undefined,
		to :: term() | undefined,
		interval :: open | closed | closed_bottom | closed_top | undefined,
		regex :: {CompiledRegEx :: re:mp(), OriginalRegEx :: string()} | undefined,
		value :: term() | undefined}).
-type spec_char_value() :: #spec_char_value{}.

-record(specification_char,
		{name :: string() | undefined,
		description :: string() | undefined,
		value_type :: string() | undefined,
		class_type :: string() | undefined,
		schema :: string() | undefined,
		value_schema :: string() | undefined,
		configurable :: boolean() | undefined,
		start_date :: pos_integer() | undefined,
		end_date :: pos_integer() | undefined,
		min :: non_neg_integer() | undefined,
		max :: non_neg_integer() | undefined,
		unique :: boolean() | undefined,
		regex :: {CompiledRegEx :: re:mp(), OriginalRegEx :: string()} | undefined,
		extensible :: boolean() | undefined,
		char_relation = [] :: [spec_char_rel()],
		char_value = [] :: [spec_char_value()]}).
-type specification_char() :: #specification_char{}.

-record(resource,
		{id :: string() | undefined | '_' | '$1',
		href :: string() | undefined | '_',
		public_id :: string() | undefined | '_',
		name :: string() | undefined | '_' | '$2',
		description :: string() | undefined | '_',
		category :: string() | undefined | '_',
		class_type :: string() | undefined | '_' | '$3',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		status :: string() | undefined | '_',
		version :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		last_modified :: {TS :: pos_integer(), N :: pos_integer()} | undefined | '_',
		place = [] :: [place_ref()] | '_',
		note = [] ::[note()] | '_',
		attachment = [] ::[attachment()] | '_',
		related = [] :: [resource_rel()] | '_',
		specification :: specification_ref() | undefined | '_',
		related_party = [] :: [related_party_ref()] | '_',
		characteristic = [] :: [resource_char()] | '_'}).
-type resource() :: #resource{}.

-record(resource_char,
		{name :: string() | undefined,
		class_type :: string() | undefined,
		schema :: string() | undefined,
		value :: term() | undefined}).
-type resource_char() :: #resource_char{}.

-record(sites,
		{name :: string() | undefined,
		network :: string() | undefined,
		longtitude :: string() | undefined,
		latitude :: string() | undefined,
		city :: string() | undefined,
		country :: string() | undefined,
		countryCity :: string() | undefined}).
-type sites() :: #sites{}.

-record(gsm_relation,
		{id :: string() | undefined,
		adjacent_cell :: string() | undefined,
		bcch_frequency :: integer() | undefined,
		ncc :: integer() | undefined,
		bcc :: integer() | undefined,
		lac :: integer() | undefined,
		is_remove_allowed :: boolean() | undefined,
		is_hoa_allowed :: boolean() | undefined,
		is_covered_by :: no | yes | partial | undefined,
		vs_data_container :: [term()] | undefined}).

-record(utran_relation,
		{id :: string() | undefined,
		adjacent_cell :: string() | undefined,
		vs_data_container :: [term()] | undefined}).

-record(eutran_relation,
		{id :: string() | undefined,
		tci :: integer() | undefined,
		is_remove_allowed :: boolean() | undefined,
		is_hoa_allowed :: boolean() | undefined,
		is_icic_information_send_allowed :: boolean() | undefined,
		is_lb_allowed :: boolean() | undefined,
		adjacent_cell :: string() | undefined,
		is_es_covered_by :: no | yes | partial | undefined,
		cell_individual_offset :: string() | undefined,
		q_offset :: string() | undefined,
		vs_data_container :: [term()] | undefined}).

-record(inter_rat_es_policies,
		{id :: string() | undefined,
		act_original_cell_params :: relative_cell_load_parameters() | undefined,
		act_candidate_cell_params :: relative_cell_load_parameters() | undefined,
		deact_candidate_cell_params :: relative_cell_load_parameters() | undefined}).

-record(rel_cell_load_params,
		{load_threshold :: 0..10000 | undefined,
		time_duration :: 0..900 | undefined}).
-type relative_cell_load_parameters() :: #inter_rat_es_policies{}.

