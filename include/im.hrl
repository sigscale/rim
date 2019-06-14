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
		{id :: string() | undefined | '_' | '$1',
		href :: string() | undefined | '_' | '$2',
		name :: string() | undefined | '_' | '$3',
		description :: string() | undefined | '_' | '$4',
		class_type :: string() | undefined | '_' | '$5',
		base_type :: string() | undefined | '_' | '$6',
		schema :: string() | undefined | '_' | '$7',
		version :: string() | undefined | '_' | '$8',
		start_date :: pos_integer() | undefined | '_' | '$9',
		end_date :: pos_integer() | undefined | '_' | '$10',
		last_modified :: {TS :: pos_integer(), N :: pos_integer()} | undefined | '_' | '$11',
		status :: catalog_status() | undefined | '_' | '$12',
		related_party = [] :: [related_party_ref()] | '_' | '$13',
		category = [] :: [category_ref()] | '_' | '$14'}).
-type catalog() :: #catalog{}.

-record(category,
		{id :: string() | undefined | '_' | '$1',
		href :: string() | undefined | '_' | '$2',
		name :: string() | undefined | '_' | '$3',
		description :: string() | undefined | '_' | '$4',
		class_type :: string() | undefined | '_' | '$5',
		base_type :: string() | undefined | '_' | '$6',
		schema :: string() | undefined | '_' | '$7',
		version :: string() | undefined | '_' | '$8',
		start_date :: pos_integer() | undefined | '_' | '$9',
		end_date :: pos_integer() | undefined | '_' | '$10',
		last_modified :: {TS :: pos_integer(), N :: pos_integer()} | undefined | '_' | '$11',
		status :: catalog_status() | undefined | '_' | '$12',
		parent :: string() | undefined | '_' | '$13',
		root = false :: boolean() | '_' | '$14',
		related_party = [] :: [related_party_ref()] | '_' | '$15',
		category = [] :: [category_ref()] | '_' | '$16',
		candidate = [] :: [candidate_ref()] | '_' | '$17'}).
-type category() :: #category{}.

-record(candidate,
		{id :: string() | undefined | '_' | '$1',
		href :: string() | undefined | '_' | '$2',
		name :: string() | undefined | '_' | '$3',
		description :: string() | undefined | '_' | '$4',
		class_type :: string() | undefined | '_' | '$5',
		base_type :: string() | undefined | '_' | '$6',
		schema :: string() | undefined | '_' | '$7',
		version :: string() | undefined | '_' | '$8',
		start_date :: pos_integer() | undefined | '_' | '$9',
		end_date :: pos_integer() | undefined | '_' | '$10',
		last_modified :: {TS :: pos_integer(), N :: pos_integer()} | undefined | '_' | '$11',
		status :: catalog_status() | undefined | '_' | '$12',
		category = [] :: [category_ref()]  | '_' | '$13',
		specification :: specification_ref() | undefined | '_' | '$14'}).
-type candidate() :: #candidate{}.

-record(specification,
		{id :: string() | undefined | '_' | '$1',
		href :: string() | undefined | '_' | '$2',
		name :: string() | undefined | '_' | '$3',
		description :: string() | undefined | '_' | '$4',
		class_type :: string() | undefined | '_' | '$5',
		base_type :: string() | undefined | '_' | '$6',
		schema :: string() | undefined | '_' | '$7',
		status :: catalog_status() | undefined | '_' | '$8',
		version :: string() | undefined | '_' | '$9',
		start_date :: pos_integer() | undefined | '_' | '$10',
		end_date :: pos_integer() | undefined | '_' | '$11',
		last_modified :: {TS :: pos_integer(), N :: pos_integer()} | undefined | '_' | '$12',
		bundle = false :: boolean() | undefined | '_' | '$13',
		category :: string() | undefined | '_' | '$14',
		target_schema :: target_schema_ref() | undefined | '_' | '$15',
		model :: string() | undefined | '_' | '$16',
		part :: string() | undefined | '_' | '$17',
		sku :: string() | undefined | '_' | '$18',
		vendor :: string() | undefined | '_' | '$19',
		device_serial :: string() | undefined | '_' | '$20',
		device_version :: string() | undefined | '_' | '$21',
		feature = [] :: [feature()] | '_' | '$22',
		attachment = [] :: [attachment()] | '_' | '$23',
		related_party = [] :: [related_party_ref()] | '_' | '$24',
		characteristic = [] :: [specification_char()] | '_' | '$25',
		related = [] :: [specification_rel()] | '_' | '$26'}).
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
		href :: string() | undefined | '_' | '$2',
		public_id :: string() | undefined | '_' | '$3',
		name :: string() | undefined | '_' | '$4',
		description :: string() | undefined | '_' | '$5',
		category :: string() | undefined | '_' | '$6',
		class_type :: string() | undefined | '_' | '$7',
		base_type :: string() | undefined | '_' | '$8',
		schema :: string() | undefined | '_' | '$9',
		status :: string() | undefined | '_' | '$10',
		version :: string() | undefined | '_' | '$11',
		start_date :: pos_integer() | undefined | '_' | '$12',
		end_date :: pos_integer() | undefined | '_' | '$13',
		last_modified :: {TS :: pos_integer(), N :: pos_integer()}
				| undefined | '_' | '$14',
		place = [] :: [place_ref()] | '_' | '$15',
		note = [] :: [note()] | '_' | '$16',
		attachment = [] :: [attachment()] | '_' | '$17',
		related = [] :: [resource_rel()] | '_' | '$18',
		specification :: specification_ref() | undefined | '_' | '$19',
		related_party = [] :: [related_party_ref()] | '_' | '$20',
		characteristic = [] :: [resource_char()] | '_' | '$21'}).
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

-type rule() :: fun((DN :: string()) -> ets:match_spec()).
-record(pee_rule,
		{id :: string(),
		description :: string() | undefined,
		rule :: rule()}).
-type pee_rule() :: #pee_rule{}.

-record('BssFunction',
		{user_label:: string() | undefined | '_',
		vs_data_container :: [term()] | undefined | '_'}).

-record('BtsSiteMgr',
		{user_label:: string() | undefined | '_',
		vs_data_container :: [term()] | undefined | '_'}).
