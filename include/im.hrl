%% im.hrl

-record(resource_spec,
		{id :: string(),
		href :: string(),
		name :: string(),
		description :: string(),
		category :: string(),
		type :: string(),
		base_type :: string(),
		schema :: string(),
		status :: string(),
		version :: string(),
		bundle = false :: boolean(),
		characteristic = [] :: [resource_char()],
		device_manufacture :: pos_integer(),
		device_power :: string(),
		device_serial :: string(),
		device_version :: string(),
		value :: string()}).
-type resource_spec() :: #resource_spec{}.

-record(resource_value,
		{type :: string(),
		schema :: string(),
		unit :: string(),
		value_type :: string(),
		from :: term(),
		to :: term(),
		interval :: open | closed | closed_bottom | closed_top,
		regex :: {CompiledRegEx :: re:mp(), OriginalRegEx :: string()},
		value :: term()}).
-type resource_value() :: #resource_value{}.

-record(resource_char,
		{href :: string(),
		name :: string(),
		description :: string(),
		type :: string(),
		base_type :: string(),
		schema :: string(),
		value_type :: string(),
		value_schema :: string(),
		configurable :: boolean(),
		min :: non_neg_integer(),
		max :: non_neg_integer(),
		unique :: boolean(),
		regex :: {CompiledRegEx :: re:mp(), OriginalRegEx :: string()},
		extensible :: boolean(),
		value = [] :: [resource_value()]}).
-type resource_char() :: #resource_char{}.

-record(resource,
		{id :: string(),
		href :: string(),
		name :: string(),
		description :: string(),
		category :: string(),
		type :: string(),
		base_type :: string(),
		schema :: string(),
		status :: string(),
		version :: string(),
		specification :: resource_spec(),
		characteristic = [] :: [{Name :: string(), Value :: term()}]}).
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

