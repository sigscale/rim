%% im.hrl

-type lifecycle_status() :: in_study | in_design | in_test
		| rejected | active | launched | retired | obsolete.

-record(category_ref,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		version :: string() | undefined | '_',
		ref_type :: string() | undefined | '_'}).
-type category_ref() :: #category_ref{}.

-record(candidate_ref,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		version :: string() | undefined | '_',
		ref_type :: string() | undefined | '_'}).
-type candidate_ref() :: #candidate_ref{}.

-record(party_ref,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		role :: string() | undefined | '_',
		ref_type :: string() | undefined | '_'}).
-type party_ref() :: #party_ref{}.

-record(specification_ref,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		version :: string() | undefined | '_',
		ref_type :: string() | undefined | '_'}).
-type specification_ref() :: #specification_ref{}.

-record(specification_rel,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		rel_type :: string() | undefined | '_',
		role :: string() | undefined | '_',
		min :: non_neg_integer() | undefined | '_',
		max :: non_neg_integer() | undefined | '_'}).
-type specification_rel() :: #specification_rel{}.

-record(spec_char_rel,
		{char_id :: string() | undefined | '_',
		name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		rel_type :: string() | undefined | '_',
		res_id :: string() | undefined | '_',
		res_href :: string() | undefined | '_'}).
-type spec_char_rel() :: #spec_char_rel{}.

-record(resource_rel,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		version :: string() | undefined | '_',
		ref_type :: string() | undefined | '_',
		rel_type :: string() | undefined | '_'}).
-type resource_rel() :: #resource_rel{}.

-record(res_char_rel,
		{id :: string() | undefined | '_',
		rel_type :: string() | undefined | '_'}).
-type res_char_rel() :: #res_char_rel{}.

-record(place_ref,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		version :: string() | undefined | '_',
		role :: string() | undefined | '_',
		ref_type :: string() | undefined | '_'}).
-type place_ref() :: #place_ref{}.

-record(target_schema_ref,
		{class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_'}).
-type target_schema_ref() :: #target_schema_ref{}.

-record(constraint_ref,
		{id :: string() | undefined | '_',
		href :: string() | undefined | '_',
		name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		version :: string() | undefined | '_',
		ref_type :: string() | undefined | '_'}).
-type constraint_ref() :: #constraint_ref{}.

-record(feature_spec,
		{id :: string() | undefined | '_',
		name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		bundle = false :: boolean() | '_',
		version :: string() | undefined | '_',
		enabled = true :: boolean() | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		constraint = [] :: [constraint_ref()] | '_',
		characteristic = [] :: [feature_spec_char()] | '_',
		related = [] :: [feature_spec_rel()] | '_'}).
-type feature_spec() :: #feature_spec{}.

-record(feat_char_rel,
		{char_id :: string() | undefined | '_',
		feat_id :: string() | undefined | '_',
		name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		rel_type :: string() | undefined | '_',
		res_id :: string() | undefined | '_',
		res_href :: string() | undefined | '_'}).
-type feat_char_rel() :: #feat_char_rel{}.

-record(feature_spec_char,
		{id :: string() | undefined | '_' ,
		name :: string() | undefined | '_',
		description :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		configurable :: boolean() | undefined | '_',
		extensible :: boolean() | undefined | '_',
		unique :: boolean() | undefined | '_',
		min :: non_neg_integer() | undefined | '_',
		max :: non_neg_integer() | undefined | '_',
		regex :: {CompiledRegEx :: re:mp(),
				OriginalRegEx :: string()} | undefined | '_',
		value_type :: string() | undefined | '_',
		value_schema :: string() | undefined | '_',
		related = [] :: [feat_char_rel()] | '_',
		char_value = [] :: [feat_char_value()] | '_'}).
-type feature_spec_char() :: #feature_spec_char{}.

-record(feature_spec_rel,
		{feat_id :: string() | undefined | '_',
		name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		res_id :: string() | undefined | '_',
		res_href :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		rel_type :: string() | undefined | '_'}).
-type feature_spec_rel() :: #feature_spec_rel{}.

-record(feature,
		{id :: string() | undefined | '_',
		name :: string() | undefined | '_',
		bundle = false :: boolean() | '_',
		enabled = true :: boolean() | '_',
		constraint = [] :: [constraint_ref()] | '_',
		characteristic = [] :: [feature_char()] | '_',
		related = [] :: [feature_rel()] | '_'}).
-type feature() :: #feature{}.

-record(feature_rel,
		{id :: string() | undefined | '_',
		name :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		rel_type :: string() | undefined | '_'}).
-type feature_rel() :: #feature_rel{}.

-record(feat_char_value,
		{default = false :: boolean() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		unit :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		value_type :: string() | undefined | '_',
		from :: term() | undefined | '_',
		to :: term() | undefined | '_',
		interval :: open | closed | closed_bottom | closed_top | undefined | '_',
		regex :: {CompiledRegEx :: re:mp(), OriginalRegEx :: string()} | undefined | '_',
		value :: term() | undefined | '_'}).
-type feat_char_value() :: #feat_char_value{}.

-record(feature_char,
		{id :: string() | undefined | '_',
		name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		value_type :: string() | undefined | '_',
		value_schema :: string() | undefined | '_',
		value :: term() | undefined | '_'}).
-type feature_char() :: #feature_char{}.

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
		status :: lifecycle_status() | undefined | '_' | '$12',
		party = [] :: [party_ref()] | '_' | '$13',
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
		status :: lifecycle_status() | undefined | '_' | '$12',
		parent :: string() | undefined | '_' | '$13',
		root = false :: boolean() | '_' | '$14',
		party = [] :: [party_ref()] | '_' | '$15',
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
		status :: lifecycle_status() | undefined | '_' | '$12',
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
		status :: lifecycle_status() | undefined | '_' | '$8',
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
		feature = [] :: [feature_spec()] | '_' | '$22',
		attachment = [] :: [attachment()] | '_' | '$23',
		party = [] :: [party_ref()] | '_' | '$24',
		characteristic = [] :: [specification_char()] | '_' | '$25',
		related = [] :: [specification_rel()] | '_' | '$26',
		connection_point = [] :: [specification_ref()] | '_' | '$27'}).
-type specification() :: #specification{}.

-record(spec_char_value,
		{value_type :: string() | undefined | '_',
		default = false :: boolean() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
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
		{id :: string() | undefined | '_' ,
		name :: string() | undefined | '_',
		description :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		start_date :: pos_integer() | undefined | '_',
		end_date :: pos_integer() | undefined | '_',
		configurable :: boolean() | undefined | '_',
		extensible :: boolean() | undefined | '_',
		unique :: boolean() | undefined | '_',
		min :: non_neg_integer() | undefined | '_',
		max :: non_neg_integer() | undefined | '_',
		regex :: {CompiledRegEx :: re:mp(),
				OriginalRegEx :: string()} | undefined | '_',
		value_type :: string() | undefined | '_',
		value_schema :: string() | undefined | '_',
		related = [] :: [spec_char_rel()] | '_',
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
		state :: string() | undefined | '_' | '$10',
		substate :: string() | undefined | '_' | '$11',
		version :: string() | undefined | '_' | '$12',
		start_date :: pos_integer() | undefined | '_' | '$13',
		end_date :: pos_integer() | undefined | '_' | '$14',
		last_modified :: {TS :: pos_integer(), N :: pos_integer()}
				| undefined | '_' | '$15',
		place = [] :: [place_ref()] | '_' | '$16',
		note = [] :: [note()] | '_' | '$17',
		attachment = [] :: [attachment()] | '_' | '$18',
		related = [] :: [resource_rel()] | '_' | '$19',
		specification :: specification_ref() | undefined | '_' | '$20',
		party = [] :: [party_ref()] | '_' | '$21',
		feature = [] :: [feature()] | '_' | '$22',
		characteristic = [] :: [resource_char()] | '_' | '$23',
		connectivity = [] :: [resource_graph()] | '_' | '$24',
		connection_point = [] :: [resource_rel()] | '_' | '$25'}).
-type resource() :: #resource{}.

-record(connection_spec,
		{name :: string() | undefined | '_',
		ass_type :: string() | undefined | '_',
		endpoint = [] :: [endpoint_spec()] | '_'}).
-type connection_spec() :: #connection_spec{}.

-record(connection,
		{id :: string() | undefined | '_',
		name :: string() | undefined | '_',
		ass_type :: string() | undefined | '_',
		endpoint = [] :: [endpoint_ref()] | '_'}).
-type connection() :: #connection{}.

-record(endpoint_spec,
		{href :: string() | undefined | '_',
		id :: string() | undefined | '_',
		name :: string() | undefined | '_',
		role :: string() | undefined | '_',
		is_root :: boolean() | undefined | '_',
		connection_point_specification = [] :: [connection_point_spec()] | '_'}).
-type endpoint_spec() :: #endpoint_spec{}.

-record(endpoint_ref,
		{href :: string() | undefined | '_',
		id :: string() | undefined | '_',
		name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		is_root :: boolean() | undefined | '_',
		ref_type :: string() | undefined | '_',
		connection_point = [] :: [resource_rel()] | '_'}).
-type endpoint_ref() :: #endpoint_ref{}.

-record(connection_point_spec,
		{href :: string() | undefined | '_',
		id :: string() | undefined | '_',
		name :: string() | undefined | '_',
		type :: string() | undefined | '_'}).
-type connection_point_spec() :: #connection_point_spec{}.

-record(resource_graph,
		{id :: string() | undefined | '_',
		name :: string() | undefined | '_',
		description :: string() | undefined,
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		connection = [] :: [connection()] | undefined | '_',
		related = [] :: [resource_graph_rel()] | undefined | '_'}).
-type resource_graph() :: #resource_graph{}.

-record(resource_graph_rel,
		{class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		rel_type :: string() | undefined | '_',
		graph :: resource_graph_ref() | undefined | '_'}).
-type resource_graph_rel() :: #resource_graph_rel{}.

-record(resource_graph_ref,
		{id :: string() | undefined | '_',
		name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		ref_type :: string() | undefined | '_'}).
-type resource_graph_ref() :: #resource_graph_ref{}.

-record(resource_char,
		{id :: string() | undefined | '_',
		name :: string() | undefined | '_',
		class_type :: string() | undefined | '_',
		base_type :: string() | undefined | '_',
		schema :: string() | undefined | '_',
		value_type :: string() | undefined | '_',
		value :: term() | undefined | '_',
		related = [] :: [res_char_rel()] | '_'}).
-type resource_char() :: #resource_char{}.

-type rule() :: fun((Input :: term()) -> ets:match_spec()).
-record(pee_rule,
		{id :: string() | undefined,
		description :: string() | undefined,
		rule :: rule() | string() | undefined}).
-type pee_rule() :: #pee_rule{}.

