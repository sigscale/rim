%% im.hrl

-type catalog_status() :: in_study | in_design | in_test
		| rejected | active | launched | retired | obsolete.

-record(category_ref,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		version :: string() | undefined | '_'}).
-type category_ref() :: #category_ref{}.

-record(candidate_ref,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		version :: string() | undefined | '_'}).
-type candidate_ref() :: #candidate_ref{}.

-record(related_party_ref,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		role :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_'}).
-type related_party_ref() :: #related_party_ref{}.

-record(specification_ref,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		version :: string() | undefined | '_'}).
-type specification_ref() :: #specification_ref{}.

-record(specification_rel,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		type :: string() | undefined | '_',
		role :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_'}).
-type specification_rel() :: #specification_rel{}.

-record(spec_char_rel,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		type :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_'}).
-type spec_char_rel() :: #spec_char_rel{}.

-record(resource_rel,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		type :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_'}).
-type resource_rel() :: #resource_rel{}.

-record(place_ref,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		role :: string() | undefined | '_'}).
-type place_ref() :: #place_ref{}.

-record(target_schema_ref,
		{class_type :: string() | undefined | '_',
		schema :: string() | undefined | '_'}).
-type target_schema_ref() :: #target_schema_ref{}.

-record(feature,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		version :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		bundle = false :: boolean(),
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		enabled = false :: boolean()}).
-type feature() :: #feature{}.

-record(attachment,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		description :: string() | undefined | '_',
		type :: string() | undefined | '_',
		url :: string() | undefined | '_'}).
-type attachment() :: #attachment{}.

-record(note,
		{author :: string() | undefined | '_',
		date :: pos_integer() | undefined | '_',
		text :: string() | undefined | '_'}).
-type note() :: #note{}.

-record(catalog,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		description :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		version :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		last_modified :: {TS :: pos_integer(), N :: pos_integer()} | undefined | '_',
		status :: catalog_status() | undefined | '_',
		related_party = [] :: [related_party_ref()] | '_',
		category = [] :: [category_ref()] | '_'}).
-type catalog() :: #catalog{}.

-record(category,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		description :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		version :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		last_modified :: {TS :: pos_integer(), N :: pos_integer()} | undefined | '_',
		status :: catalog_status() | undefined | '_',
		parent :: string() | undefined | '_',
		root = false :: boolean() | '_',
		related_party = [] :: [related_party_ref()] | '_',
		category = [] :: [category_ref()] | '_',
		candidate = [] :: [candidate_ref()] | '_'}).
-type category() :: #category{}.

-record(candidate,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		description :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		version :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		last_modified :: {TS :: pos_integer(), N :: pos_integer()} | undefined | '_',
		status :: catalog_status() | undefined | '_',
		category = [] :: [category_ref()]  | '_',
		specification :: specification_ref() | undefined | '_'}).
-type candidate() :: #candidate{}.

-record(specification,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		description :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		status :: catalog_status() | undefined | '_',
		version :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		last_modified :: {TS :: pos_integer(), N :: pos_integer()} | undefined | '_',
		bundle = false :: boolean() | undefined | '_',
		category :: string() | undefined | '_',
		target_schema :: target_schema_ref() | undefined | '_',
		model :: string() | undefined | '_',
		part :: string() | undefined | '_',
		sku :: string() | undefined | '_',
		vendor :: string() | undefined | '_',
		device_serial :: string() | undefined | '_',
		device_version :: string() | undefined | '_',
		feature = [] :: [feature()] | '_',
		attachment = [] :: [attachment()] | '_',
		related_party = [] :: [related_party_ref()] | '_',
		characteristic = [] :: [specification_char()] | '_',
		related = [] :: [specification_rel()] | '_'}).
-type specification() :: #specification{}.

-record(spec_char_value,
		{value_type :: string() | undefined | '_',
		default = false :: boolean() | undefined | '_',
		class_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		unit :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		from :: term() | undefined | '_',
		to :: term() | undefined | '_',
		interval :: open | closed | closed_bottom | closed_top | undefined | '_',
		regex :: {CompiledRegEx :: re:mp(), OriginalRegEx :: string()} | undefined | '_',
		value :: term() | undefined | '_'}).
-type spec_char_value() :: #spec_char_value{}.

-record(specification_char,
		{name :: string() | undefined | '_',
		description :: string() | undefined | '_',
		value_type :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		value_schema :: string() | undefined | '_',
		configurable :: boolean() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		min :: non_neg_integer() | undefined | '_',
		max :: non_neg_integer() | undefined | '_',
		unique :: boolean() | undefined | '_',
		regex :: {CompiledRegEx :: re:mp(), OriginalRegEx :: string()} | undefined | '_',
		extensible :: boolean() | undefined | '_',
		char_relation = [] :: [spec_char_rel()] | '_',
		char_value = [] :: [spec_char_value()] | '_'}).
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
		{name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		value :: term() | undefined | '_'}).
-type resource_char() :: #resource_char{}.

-record(sites,
		{name :: string() | undefined | '_',
		network :: string() | undefined | '_',
		longtitude :: string() | undefined | '_',
		latitude :: string() | undefined | '_',
		city :: string() | undefined | '_',
		country :: string() | undefined | '_',
		countryCity :: string() | undefined | '_'}).
-type sites() :: #sites{}.

-record(inter_rat_es_policies,
		{id :: string() | undefined | '_',
		act_original_cell_params :: relative_cell_load_parameters() | undefined | '_',
		act_candidate_cell_params :: relative_cell_load_parameters() | undefined | '_',
		deact_candidate_cell_params :: relative_cell_load_parameters() | undefined | '_'}).

-record(rel_cell_load_params,
		{load_threshold :: 0..10000 | undefined | '_',
		time_duration :: 0..900 | undefined | '_'}).
-type relative_cell_load_parameters() :: #inter_rat_es_policies{}.

-record('BssFunction',
		{user_label:: string() | undefined | '_',
		vs_data_container :: [term()] | undefined | '_'}).

-record('BtsSiteMgr',
		{user_label:: string() | undefined | '_',
		vs_data_container :: [term()] | undefined | '_'}).
