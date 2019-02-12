%%% im_rest_res_specification.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2019 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This library module implements resource handling functions
%%% 	for a REST server in the {@link //im. im} application.
%%%
%%% 	Handle `ResourceSpecification' collection.
%%%
-module(im_rest_res_specification).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([get_specifications/2, get_specification/2, post_specification/1,
		delete_specification/1]).
-export([specification/1]).
 
-include("im.hrl").

-define(PathCatalog, "/resourceCatalogManagement/v3/").

%%----------------------------------------------------------------------
%%  The im public API
%%----------------------------------------------------------------------

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/json-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec get_specifications(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Handle `GET' request on `ResourceSpecification' collection.
get_specifications(Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_specifications(NewQuery, Filters, Headers);
		false ->
			get_specifications(Query, [], Headers)
	end.
%% @hidden
get_specifications(Query, Filters, Headers) ->
	case {lists:keyfind("if-match", 1, Headers),
			lists:keyfind("if-range", 1, Headers),
			lists:keyfind("range", 1, Headers)} of
		{{"if-match", Etag}, false, {"range", Range}} ->
			case global:whereis_name(Etag) of
				undefined ->
					{error, 412};
				PageServer ->
					case im_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_page(PageServer, Etag, Query, Filters, Start, End)
					end
			end;
		{{"if-match", Etag}, false, false} ->
			case global:whereis_name(Etag) of
				undefined ->
					{error, 412};
				PageServer ->
					query_page(PageServer, Etag, Query, Filters, undefined, undefined)
			end;
		{false, {"if-range", Etag}, {"range", Range}} ->
			case global:whereis_name(Etag) of
				undefined ->
					case im_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_start(Query, Filters, Start, End)
					end;
				PageServer ->
					case im_rest:range(Range) of
						{error, _} ->
							{error, 400};
						{ok, {Start, End}} ->
							query_page(PageServer, Etag, Query, Filters, Start, End)
					end
			end;
		{{"if-match", _}, {"if-range", _}, _} ->
			{error, 400};
		{_, {"if-range", _}, false} ->
			{error, 400};
		{false, false, {"range", Range}} ->
			case im_rest:range(Range) of
				{error, _} ->
					{error, 400};
				{ok, {Start, End}} ->
					query_start(Query, Filters, Start, End)
			end;
		{false, false, false} ->
			query_start(Query, Filters, undefined, undefined)
	end.

-spec get_specification(Id, Query) -> Result
	when
		Id :: string(),
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Handle `GET' request on a `ResourceSpecification' resource.
get_specification(Id, Query) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, L}, NewQuery} ->
			get_specification(Id, NewQuery, string:tokens(L, ","));
		false ->
			get_specification(Id, Query, [])
	end.
%% @hidden
get_specification(Id, [] = _Query, _Filters) ->
	case im:get_specification(Id) of
		{ok, #specification{last_modified = LastModified} = Specification} ->
			Headers = [{content_type, "application/json"},
					{etag, im_rest:etag(LastModified)}],
			Body = zj:encode(specification(Specification)),
			{ok, Headers, Body};
		{error, _Reason} ->
			{error, 404}
	end;
get_specification(_, _, _) ->
	{error, 400}.

-spec post_specification(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Handle `POST' request on `ResourceSpecification' collection.
post_specification(RequestBody) ->
	try
		{ok, SpecificationMap} = zj:decode(RequestBody),
		case im:add_specification(specification(SpecificationMap)) of
			{ok, #specification{href = Href, last_modified = LM} = NewSpecification} ->
				Body = zj:encode(specification(NewSpecification)),
				Headers = [{location, Href}, {etag, im_rest:etag(LM)}],
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec delete_specification(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Handle `DELETE' request on a `ResourceSpecification' resource.
delete_specification(Id) ->
	case im:del_specification(Id) of
		ok ->
			{ok, [], []};
		{error, _Reason} ->
			{error, 400}
	end.

-spec specification(ResourceSpecification) -> ResourceSpecification
	when
		ResourceSpecification :: specification() | map().
%% @doc CODEC for `ResourceSpecification'.
specification(#specification{} = ResourceSpecification) ->
	specification(record_info(fields, specification), ResourceSpecification, #{});
specification(#{} = ResourceSpecification) ->
	specification(record_info(fields, specification), ResourceSpecification, #specification{}).
%% @hidden
specification([id | T], #specification{id = Id} = R, Acc)
		when is_list(Id) ->
	specification(T, R, Acc#{"id" => Id});
specification([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	specification(T, M, Acc#specification{id = Id});
specification([href | T], #specification{href = Href} = R, Acc)
		when is_list(Href) ->
	specification(T, R, Acc#{"href" => Href});
specification([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	specification(T, M, Acc#specification{href = Href});
specification([name | T], #specification{name = Name} = R, Acc)
		when is_list(Name) ->
	specification(T, R, Acc#{"name" => Name});
specification([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	specification(T, M, Acc#specification{name = Name});
specification([description| T],
		#specification{description = Description} = R, Acc)
		when is_list(Description) ->
	specification(T, R, Acc#{"description" => Description});
specification([description| T], #{"description" := Description} = M, Acc)
		when is_list(Description) ->
	specification(T, M, Acc#specification{description = Description});
specification([class_type | T], #specification{class_type = Type} = R, Acc)
		when is_list(Type) ->
	specification(T, R, Acc#{"@type" => Type});
specification([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	specification(T, M, Acc#specification{class_type = Type});
specification([base_type | T], #specification{base_type = Type} = R, Acc)
		when is_list(Type) ->
	specification(T, R, Acc#{"@baseType" => Type});
specification([base_type | T], #{"@baseType" := Type} = M, Acc)
		when is_list(Type) ->
	specification(T, M, Acc#specification{base_type = Type});
specification([schema | T], #specification{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	specification(T, R, Acc#{"@schemaLocation" => Schema});
specification([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	specification(T, M, Acc#specification{schema = Schema});
specification([version | T], #specification{version = Version} = R, Acc)
		when is_list(Version) ->
	specification(T, R, Acc#{"version" => Version});
specification([version | T], #{"version" := Version} = M, Acc)
		when is_list(Version) ->
	specification(T, M, Acc#specification{version = Version});
specification([start_date | T], #specification{start_date = StartDate} = R, Acc)
		when is_integer(StartDate) ->
	ValidFor = #{"startDateTime" => im_rest:iso8601(StartDate)},
	specification(T, R, Acc#{"validFor" => ValidFor});
specification([start_date | T],
		#{"validFor" := #{"startDateTime" := Start}} = M, Acc)
		when is_list(Start) ->
	specification(T, M, Acc#specification{start_date = im_rest:iso8601(Start)});
specification([end_date | T], #specification{end_date = End} = R,
		#{"validFor" := ValidFor} = Acc) when is_integer(End) ->
	NewValidFor = ValidFor#{"endDateTime" => im_rest:iso8601(End)},
	specification(T, R, Acc#{"validFor" := NewValidFor});
specification([end_date | T], #specification{end_date = End} = R, Acc)
		when is_integer(End) ->
	ValidFor = #{"endDateTime" => im_rest:iso8601(End)},
	specification(T, R, Acc#{"validFor" := ValidFor});
specification([end_date | T],
		#{"validFor" := #{"endDateTime" := End}} = M, Acc)
		when is_list(End) ->
	specification(T, M, Acc#specification{end_date = im_rest:iso8601(End)});
specification([last_modified | T], #specification{last_modified = {TS, _}} = R, Acc)
		when is_integer(TS) ->
	specification(T, R, Acc#{"lastUpdate" => im_rest:iso8601(TS)});
specification([last_modified | T], #{"lastUpdate" := DateTime} = M, Acc)
		when is_list(DateTime) ->
	LM = {im_rest:iso8601(DateTime), erlang:unique_integer([positive])},
	specification(T, M, Acc#specification{last_modified = LM});
specification([status | T], #specification{status = Status} = R, Acc)
		when Status /= undefined ->
	specification(T, R, Acc#{"lifecycleStatus" => im_rest:lifecycle_status(Status)});
specification([status | T], #{"lifecycleStatus" := Status} = M, Acc)
		when is_list(Status) ->
	specification(T, M, Acc#specification{status = im_rest:lifecycle_status(Status)});
specification([bundle | T], #specification{bundle = Bundle} = R, Acc)
		when is_boolean(Bundle) ->
	specification(T, R, Acc#{"isBundle" => Bundle});
specification([bundle | T], #{"isBundle" := Bundle} = M, Acc)
		when is_boolean(Bundle) ->
	specification(T, M, Acc#specification{bundle = Bundle});
specification([related_party | T], #specification{related_party = PartyRefs} = R, Acc)
		when is_list(PartyRefs), length(PartyRefs) > 0 ->
	specification(T, R, Acc#{"relatedParty" => im_rest:related_party_ref(PartyRefs)});
specification([related_party | T], #{"relatedParty" := PartyRefs} = M, Acc)
		when is_list(PartyRefs) ->
	specification(T, M, Acc#specification{related_party = im_rest:related_party_ref(PartyRefs)});
specification([category | T], #specification{category = Category} = R, Acc)
		when is_list(Category) ->
	specification(T, R, Acc#{"category" => Category});
specification([category | T], #{"category" := Category} = M, Acc)
		when is_list(Category) ->
	specification(T, M, Acc#specification{category = Category});
specification([characteristic | T], #specification{characteristic = SpecChars} = R, Acc)
		when is_list(SpecChars), length(SpecChars) > 0->
	specification(T, R, Acc#{"resourceSpecCharacteristic" => specification_char(SpecChars)});
specification([characteristic | T], #{"resourceSpecCharacteristic" := SpecChars} = M, Acc)
		when is_list(SpecChars) ->
	specification(T, M, Acc#specification{characteristic = specification_char(SpecChars)});
specification([related | T], #specification{related = SpecRels} = R, Acc)
		when is_list(SpecRels), length(SpecRels) > 0->
	specification(T, R, Acc#{"resourceSpecRelationship" => specification_rel(SpecRels)});
specification([related | T], #{"resourceSpecRelationship" := SpecRels} = M, Acc)
		when is_list(SpecRels) ->
	specification(T, M, Acc#specification{related = specification_rel(SpecRels)});
specification([_ | T], R, Acc) ->
	specification(T, R, Acc);
specification([], _, Acc) ->
	Acc.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec specification_rel(ResourceSpecRelationship) -> ResourceSpecRelationship
	when
		ResourceSpecRelationship :: [specification_rel()] | [map()].
%% @doc CODEC for `ResourceSpecRelationship'.
%% @private
specification_rel([#specification_rel{} | _] = List) ->
	Fields = record_info(fields, specification_rel),
	[specification_rel(Fields, R, #{}) || R <- List];
specification_rel([#{} | _] = List) ->
	Fields = record_info(fields, specification_rel),
	[specification_rel(Fields, M, #specification_rel{}) || M <- List];
specification_rel([]) ->
	[].
%% @hidden
specification_rel([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	specification_rel(T, M, Acc#specification_rel{id = Id});
specification_rel([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	specification_rel(T, M, Acc#specification_rel{id = Id});
specification_rel([href | T], #specification_rel{href = Href} = R, Acc)
		when is_list(Href) ->
	specification_rel(T, R, Acc#{"href" => Href});
specification_rel([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	specification_rel(T, M, Acc#specification_rel{href = Href});
specification_rel([name | T], #specification_rel{name = Name} = R, Acc)
		when is_list(Name) ->
	specification_rel(T, R, Acc#{"name" => Name});
specification_rel([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	specification_rel(T, M, Acc#specification_rel{name = Name});
specification_rel([type | T], #specification_rel{type = Type} = R, Acc)
		when is_list(Type) ->
	specification_rel(T, R, Acc#{"type" => Type});
specification_rel([type | T], #{"type" := Type} = M, Acc)
		when is_list(Type) ->
	specification_rel(T, M, Acc#specification_rel{type = Type});
specification_rel([role | T], #specification_rel{role = Role} = R, Acc)
		when is_list(Role) ->
	specification_rel(T, R, Acc#{"role" => Role});
specification_rel([role | T], #{"role" := Role} = M, Acc)
		when is_list(Role) ->
	specification_rel(T, M, Acc#specification_rel{role = Role});
specification_rel([start_date | T], #specification_rel{start_date = StartDate} = R, Acc)
		when is_integer(StartDate) ->
	ValidFor = #{"startDateTime" => im_rest:iso8601(StartDate)},
	specification_rel(T, R, Acc#{"validFor" => ValidFor});
specification_rel([start_date | T],
		#{"validFor" := #{"startDateTime" := Start}} = M, Acc)
		when is_list(Start) ->
	specification_rel(T, M, Acc#specification_rel{start_date = im_rest:iso8601(Start)});
specification_rel([end_date | T], #specification_rel{end_date = End} = R,
		#{"validFor" := ValidFor} = Acc) when is_integer(End) ->
	NewValidFor = ValidFor#{"endDateTime" => im_rest:iso8601(End)},
	specification_rel(T, R, Acc#{"validFor" := NewValidFor});
specification_rel([end_date | T], #specification_rel{end_date = End} = R, Acc)
		when is_integer(End) ->
	ValidFor = #{"endDateTime" => im_rest:iso8601(End)},
	specification_rel(T, R, Acc#{"validFor" := ValidFor});
specification_rel([end_date | T], #{"validFor" := #{"endDateTime" := End}} = M, Acc)
		when is_list(End) ->
	specification_rel(T, M, Acc#specification_rel{end_date = im_rest:iso8601(End)});
specification_rel([_ | T], R, Acc) ->
	specification_rel(T, R, Acc);
specification_rel([], _, Acc) ->
	Acc.

-spec spec_char_value(ResourceSpecCharacteristicValue) -> ResourceSpecCharacteristicValue
	when
		ResourceSpecCharacteristicValue :: [spec_char_value()] | [map()].
%% @doc CODEC for `ResourceSpecCharacteristicValue'.
%% @private
spec_char_value([#spec_char_value{} | _] = List) ->
	Fields = record_info(fields, spec_char_value),
	[spec_char_value(Fields, R, #{}) || R <- List];
spec_char_value([#{} | _] = List) ->
	Fields = record_info(fields, spec_char_value),
	[spec_char_value(Fields, M, #spec_char_value{}) || M <- List];
spec_char_value([]) ->
	[].
%% @hidden
spec_char_value([value_type | T], #spec_char_value{value_type = Type} = R, Acc)
		when is_list(Type) ->
	spec_char_value(T, R, Acc#{"valueType" => Type});
spec_char_value([value_type | T], #{"valueType" := Type} = M, Acc)
		when is_list(Type) ->
	spec_char_value(T, M, Acc#spec_char_value{value_type = Type});
spec_char_value([class_type | T], #spec_char_value{class_type = Type} = R, Acc)
		when is_list(Type) ->
	spec_char_value(T, R, Acc#{"@type" => Type});
spec_char_value([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	spec_char_value(T, M, Acc#spec_char_value{class_type = Type});
spec_char_value([schema | T], #spec_char_value{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	spec_char_value(T, R, Acc#{"@schemaLocation" => Schema});
spec_char_value([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	spec_char_value(T, M, Acc#spec_char_value{schema = Schema});
spec_char_value([default | T], #spec_char_value{default = Default} = R, Acc)
		when is_boolean(Default) ->
	spec_char_value(T, R, Acc#{"isDefault" => Default});
spec_char_value([default | T], #{"isDefault" := Default} = M, Acc)
		when is_boolean(Default) ->
	spec_char_value(T, M, Acc#spec_char_value{default = Default});
spec_char_value([start_date | T], #spec_char_value{start_date = StartDate} = R, Acc)
		when is_integer(StartDate) ->
	ValidFor = #{"startDateTime" => im_rest:iso8601(StartDate)},
	spec_char_value(T, R, Acc#{"validFor" => ValidFor});
spec_char_value([start_date | T],
		#{"validFor" := #{"startDateTime" := Start}} = M, Acc)
		when is_list(Start) ->
	spec_char_value(T, M, Acc#spec_char_value{start_date = im_rest:iso8601(Start)});
spec_char_value([end_date | T], #spec_char_value{end_date = End} = R,
		#{"validFor" := ValidFor} = Acc) when is_integer(End) ->
	NewValidFor = ValidFor#{"endDateTime" => im_rest:iso8601(End)},
	spec_char_value(T, R, Acc#{"validFor" := NewValidFor});
spec_char_value([end_date | T], #spec_char_value{end_date = End} = R, Acc)
		when is_integer(End) ->
	ValidFor = #{"endDateTime" => im_rest:iso8601(End)},
	spec_char_value(T, R, Acc#{"validFor" := ValidFor});
spec_char_value([end_date | T],
		#{"validFor" := #{"endDateTime" := End}} = M, Acc)
		when is_list(End) ->
	spec_char_value(T, M, Acc#spec_char_value{end_date = im_rest:iso8601(End)});
spec_char_value([unit | T], #spec_char_value{unit = Unit} = R, Acc)
		when is_list(Unit) ->
	spec_char_value(T, R, Acc#{"unitOfMeasure" => Unit});
spec_char_value([unit | T], #{"unitOfMeasure" := Unit} = M, Acc)
		when is_list(Unit) ->
	spec_char_value(T, M, Acc#spec_char_value{unit = Unit});
spec_char_value([from | T], #spec_char_value{from = From} = R, Acc)
		when is_integer(From) ->
	spec_char_value(T, R, Acc#{"valueFrom" => From});
spec_char_value([from | T], #{"valueFrom" := From} = M, Acc)
		when is_integer(From) ->
	spec_char_value(T, M, Acc#spec_char_value{from = From});
spec_char_value([to | T], #spec_char_value{to = To} = R, Acc)
		when is_integer(To) ->
	spec_char_value(T, R, Acc#{"valueTo" => To});
spec_char_value([to | T], #{"valueTo" := To} = M, Acc)
		when is_integer(To) ->
	spec_char_value(T, M, Acc#spec_char_value{to = To});
spec_char_value([interval | T], #spec_char_value{interval = Interval} = R, Acc)
		when Interval /= undefined ->
	spec_char_value(T, R, Acc#{"interval" => atom_to_list(Interval)});
spec_char_value([interval | T], #{"interval" := "closed"} = M, Acc) ->
	spec_char_value(T, M, Acc#spec_char_value{interval = closed});
spec_char_value([interval | T], #{"interval" := "closed_bottom"} = M, Acc) ->
	spec_char_value(T, M, Acc#spec_char_value{interval = closed_bottom});
spec_char_value([interval | T], #{"interval" := "closed_top"} = M, Acc) ->
	spec_char_value(T, M, Acc#spec_char_value{interval = closed_top});
spec_char_value([interval | T], #{"interval" := "open"} = M, Acc) ->
	spec_char_value(T, M, Acc#spec_char_value{interval = open});
spec_char_value([regex | T], #spec_char_value{regex = {_, RegEx}} = R, Acc)
		when is_list(RegEx) ->
	spec_char_value(T, R, Acc#{"regex" => RegEx});
spec_char_value([regex | T], #{"regex" := RegEx} = M, Acc)
		when is_list(RegEx) ->
	{ok, MP} = re:compile(RegEx),
	spec_char_value(T, M, Acc#spec_char_value{regex = {MP, RegEx}});
spec_char_value([value | T], #spec_char_value{value = Value} = R, Acc)
		when Value /= undefined ->
	spec_char_value(T, R, Acc#{"value" => Value});
spec_char_value([value | T], #{"value" := Value} = M, Acc) ->
	spec_char_value(T, M, Acc#spec_char_value{value = Value});
spec_char_value([_ | T], R, Acc) ->
	spec_char_value(T, R, Acc);
spec_char_value([], _, Acc) ->
	Acc.

-spec specification_char(ResourceSpecCharacteristic) -> ResourceSpecCharacteristic
	when
		ResourceSpecCharacteristic :: [specification_char()] | [map()].
%% @doc CODEC for `ResourceSpecCharacteristic'.
%% @private
specification_char([#specification_char{} | _] = List) ->
	Fields = record_info(fields, specification_char),
	[specification_char(Fields, R, #{}) || R <- List];
specification_char([#{} | _] = List) ->
	Fields = record_info(fields, specification_char),
	[specification_char(Fields, M, #specification_char{}) || M <- List];
specification_char([]) ->
	[].
%% @hidden
specification_char([name | T], #specification_char{name = Name} = R, Acc)
		when is_list(Name) ->
	specification_char(T, R, Acc#{"name" => Name});
specification_char([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	specification_char(T, M, Acc#specification_char{name = Name});
specification_char([description | T], #specification_char{description = Description} = R, Acc)
		when is_list(Description) ->
	specification_char(T, R, Acc#{"description" => Description});
specification_char([description | T], #{"description" := Description} = M, Acc)
		when is_list(Description) ->
	specification_char(T, M, Acc#specification_char{description = Description});
specification_char([value_type | T], #specification_char{value_type = Type} = R, Acc)
		when is_list(Type) ->
	specification_char(T, R, Acc#{"valueType" => Type});
specification_char([value_type | T], #{"valueType" := Type} = M, Acc)
		when is_list(Type) ->
	specification_char(T, M, Acc#specification_char{value_type = Type});
specification_char([class_type | T], #specification_char{class_type = Type} = R, Acc)
		when is_list(Type) ->
	specification_char(T, R, Acc#{"@type" => Type});
specification_char([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	specification_char(T, M, Acc#specification_char{class_type = Type});
specification_char([schema | T], #specification_char{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	specification_char(T, R, Acc#{"@schemaLocation" => Schema});
specification_char([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	specification_char(T, M, Acc#specification_char{schema = Schema});
specification_char([value_schema | T], #specification_char{value_schema = Schema} = R, Acc)
		when is_list(Schema) ->
	specification_char(T, R, Acc#{"@valueSchemaLocation" => Schema});
specification_char([value_schema | T], #{"@valueSchemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	specification_char(T, M, Acc#specification_char{value_schema = Schema});
specification_char([configurable | T], #specification_char{configurable = Configurable} = R, Acc)
		when is_boolean(Configurable) ->
	specification_char(T, R, Acc#{"configurable" => Configurable});
specification_char([configurable | T], #{"configurable" := Configurable} = M, Acc)
		when is_boolean(Configurable) ->
	specification_char(T, M, Acc#specification_char{configurable = Configurable});
specification_char([min | T], #specification_char{min = Min} = R, Acc)
		when is_integer(Min) ->
	specification_char(T, R, Acc#{"minCardinality" => Min});
specification_char([min | T], #{"minCardinality" := Min} = M, Acc)
		when is_integer(Min) ->
	specification_char(T, M, Acc#specification_char{min = Min});
specification_char([max | T], #specification_char{max = Max} = R, Acc)
		when is_integer(Max) ->
	specification_char(T, R, Acc#{"maxCardinality" => Max});
specification_char([max | T], #{"maxCardinality" := Max} = M, Acc)
		when is_integer(Max) ->
	specification_char(T, M, Acc#specification_char{max = Max});
specification_char([unique | T], #specification_char{unique = Unique} = R, Acc)
		when is_boolean(Unique) ->
	specification_char(T, R, Acc#{"unique" => Unique});
specification_char([unique | T], #{"unique" := Unique} = M, Acc)
		when is_boolean(Unique) ->
	specification_char(T, M, Acc#specification_char{unique = Unique});
specification_char([regex | T], #specification_char{regex = {_, RegEx}} = R, Acc)
		when is_list(RegEx) ->
	specification_char(T, R, Acc#{"regex" => RegEx});
specification_char([regex | T], #{"regex" := RegEx} = M, Acc)
		when is_list(RegEx) ->
	{ok, MP} = re:compile(RegEx),
	specification_char(T, M, Acc#specification_char{regex = {MP, RegEx}});
specification_char([extensible | T], #specification_char{extensible = Ext} = R, Acc)
		when is_boolean(Ext) ->
	specification_char(T, R, Acc#{"extensible" => Ext});
specification_char([extensible | T], #{"extensible" := Ext} = M, Acc)
		when is_boolean(Ext) ->
	specification_char(T, M, Acc#specification_char{extensible = Ext});
specification_char([start_date | T], #specification_char{start_date = StartDate} = R, Acc)
		when is_integer(StartDate) ->
	ValidFor = #{"startDateTime" => im_rest:iso8601(StartDate)},
	specification_char(T, R, Acc#{"validFor" => ValidFor});
specification_char([start_date | T],
		#{"validFor" := #{"startDateTime" := Start}} = M, Acc)
		when is_list(Start) ->
	specification_char(T, M, Acc#specification_char{start_date = im_rest:iso8601(Start)});
specification_char([end_date | T], #specification_char{end_date = End} = R,
		#{"validFor" := ValidFor} = Acc) when is_integer(End) ->
	NewValidFor = ValidFor#{"endDateTime" => im_rest:iso8601(End)},
	specification_char(T, R, Acc#{"validFor" := NewValidFor});
specification_char([end_date | T], #specification_char{end_date = End} = R, Acc)
		when is_integer(End) ->
	ValidFor = #{"endDateTime" => im_rest:iso8601(End)},
	specification_char(T, R, Acc#{"validFor" := ValidFor});
specification_char([end_date | T],
		#{"validFor" := #{"endDateTime" := End}} = M, Acc)
		when is_list(End) ->
	specification_char(T, M, Acc#specification_char{end_date = im_rest:iso8601(End)});
specification_char([char_relation | T],
		#specification_char{char_relation = CharRels} = R, Acc)
		when is_list(CharRels) ->
	specification_char(T, R,
			Acc#{"resourceSpecCharRelationship" => spec_char_rel(CharRels)});
specification_char([char_relation | T],
		#{"resourceSpecCharRelationship" := CharRels} = M, Acc)
		when is_list(CharRels) ->
	specification_char(T, M,
			Acc#specification_char{char_relation = spec_char_rel(CharRels)});
specification_char([char_value | T],
		#specification_char{char_value = CharVals} = R, Acc)
		when is_list(CharVals) ->
	specification_char(T, R,
			Acc#{"resourceSpecCharacteristicValue" => spec_char_value(CharVals)});
specification_char([char_value | T],
		#{"resourceSpecCharacteristicValue" := CharVals} = M, Acc)
		when is_list(CharVals) ->
	specification_char(T, M,
			Acc#specification_char{char_value = spec_char_value(CharVals)});
specification_char([_ | T], R, Acc) ->
	specification_char(T, R, Acc);
specification_char([], _, Acc) ->
	Acc.

-spec spec_char_rel(ResourceSpecCharRelationship) -> ResourceSpecCharRelationship
	when
		ResourceSpecCharRelationship :: [spec_char_rel()] | [map()].
%% @doc CODEC for `ResourceSpecCharRelationship'.
%% @private
spec_char_rel([#spec_char_rel{} | _] = List) ->
	Fields = record_info(fields, spec_char_rel),
	[spec_char_rel(Fields, R, #{}) || R <- List];
spec_char_rel([#{} | _] = List) ->
	Fields = record_info(fields, spec_char_rel),
	[spec_char_rel(Fields, M, #spec_char_rel{}) || M <- List];
spec_char_rel([]) ->
	[].
%% @hidden
spec_char_rel([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	spec_char_rel(T, M, Acc#spec_char_rel{id = Id});
spec_char_rel([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	spec_char_rel(T, M, Acc#spec_char_rel{id = Id});
spec_char_rel([href | T], #spec_char_rel{href = Href} = R, Acc)
		when is_list(Href) ->
	spec_char_rel(T, R, Acc#{"href" => Href});
spec_char_rel([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	spec_char_rel(T, M, Acc#spec_char_rel{href = Href});
spec_char_rel([name | T], #spec_char_rel{name = Name} = R, Acc)
		when is_list(Name) ->
	spec_char_rel(T, R, Acc#{"name" => Name});
spec_char_rel([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	spec_char_rel(T, M, Acc#spec_char_rel{name = Name});
spec_char_rel([type | T], #spec_char_rel{type = Type} = R, Acc)
		when is_list(Type) ->
	spec_char_rel(T, R, Acc#{"type" => Type});
spec_char_rel([type | T], #{"type" := Type} = M, Acc)
		when is_list(Type) ->
	spec_char_rel(T, M, Acc#spec_char_rel{type = Type});
spec_char_rel([class_type | T], #spec_char_rel{class_type = Type} = R, Acc)
		when is_list(Type) ->
	spec_char_rel(T, R, Acc#{"@type" => Type});
spec_char_rel([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	spec_char_rel(T, M, Acc#spec_char_rel{class_type = Type});
spec_char_rel([start_date | T], #spec_char_rel{start_date = StartDate} = R, Acc)
		when is_integer(StartDate) ->
	ValidFor = #{"startDateTime" => im_rest:iso8601(StartDate)},
	spec_char_rel(T, R, Acc#{"validFor" => ValidFor});
spec_char_rel([start_date | T],
		#{"validFor" := #{"startDateTime" := Start}} = M, Acc)
		when is_list(Start) ->
	spec_char_rel(T, M, Acc#spec_char_rel{start_date = im_rest:iso8601(Start)});
spec_char_rel([end_date | T], #spec_char_rel{end_date = End} = R,
		#{"validFor" := ValidFor} = Acc) when is_integer(End) ->
	NewValidFor = ValidFor#{"endDateTime" => im_rest:iso8601(End)},
	spec_char_rel(T, R, Acc#{"validFor" := NewValidFor});
spec_char_rel([end_date | T], #spec_char_rel{end_date = End} = R, Acc)
		when is_integer(End) ->
	ValidFor = #{"endDateTime" => im_rest:iso8601(End)},
	spec_char_rel(T, R, Acc#{"validFor" := ValidFor});
spec_char_rel([end_date | T],
		#{"validFor" := #{"endDateTime" := End}} = M, Acc)
		when is_list(End) ->
	spec_char_rel(T, M, Acc#spec_char_rel{end_date = im_rest:iso8601(End)});
spec_char_rel([_ | T], R, Acc) ->
	spec_char_rel(T, R, Acc);
spec_char_rel([], _, Acc) ->
	Acc.

%% @hidden
query_start(Query, Filters, RangeStart, RangeEnd) ->
	try
		case lists:keyfind("filter", 1, Query) of
			{_, String} ->
				{ok, Tokens, _} = im_rest_query_scanner:string(String),
				case im_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex, [{"id", like, [Id]}]}]}]} ->
						{#catalog{id = Id ++ '_', _ = '_'}, []};
					{ok, [{array, [{complex, [{"id", exact, [Id]}]}]}]} ->
						{#catalog{id = Id, _ = '_'}, []}
				end;
			false ->
				{'_', []}
		end
	of
		{MatchHead, MatchConditions} ->
			MFA = [im, query_specification, [MatchHead, MatchConditions]],
			case supervisor:start_child(im_rest_pagination_sup, [MFA]) of
				{ok, PageServer, Etag} ->
					query_page(PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
				{error, _Reason} ->
					{error, 500}
			end
	catch
		_ ->
			{error, 400}
	end.

%% @hidden
query_page(PageServer, Etag, _Query, _Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}, infinity) of
		{error, Status} ->
			{error, Status};
		{Events, ContentRange} ->
			Specifications = lists:map(fun specification/1, Events),
			Body = zj:encode(Specifications),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange}],
			{ok, Headers, Body}
	end.

