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
-export([get_specifications/3, get_specification/2, post_specification/1,
		delete_specification/1, patch_specification/4]).
-export([specification/1]).

-include("im.hrl").
-define(MILLISECOND, milli_seconds).

-define(PathSpecification, "/resourceCatalogManagement/v4/resourceSpecification/").

%%----------------------------------------------------------------------
%%  The im public API
%%----------------------------------------------------------------------

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations accepted.
content_types_accepted() ->
	["application/json", "application/merge-patch+json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec get_specifications(Method, Query, Headers) -> Result
	when
		Method :: string(), % "GET" | "HEAD"
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET|HEAD /resourceCatalogManagement/v4/resourceSpecification'
%% 	requests.
get_specifications(Method, Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_specifications(Method, NewQuery, Filters, Headers);
		false ->
			get_specifications(Method, Query, [], Headers)
	end.
%% @hidden
get_specifications(Method, Query, Filters, Headers) ->
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
							query_start(Method, Query, Filters, Start, End)
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
		{false, false, {"range", "items=1-" ++ _ = Range}} ->
			case im_rest:range(Range) of
				{error, _} ->
					{error, 400};
				{ok, {Start, End}} ->
					query_start(Method, Query, Filters, Start, End)
			end;
		{false, false, {"range", _Range}} ->
			{error, 416};
		{false, false, false} ->
			query_start(Method, Query, Filters, undefined, undefined)
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

-spec patch_specification(Id, Etag, ContentType, ReqBody) -> Result
	when
		Id :: string(),
		Etag :: undefined | string(),
		ContentType :: string(),
		ReqBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()} .
%% @doc Update a existing `specification'.
%%
%% 	Respond to `PATCH /resourceCatalogManagement/v4/resourceSpecification/{Id}' request.
%%
patch_specification(Id, Etag, "application/merge-patch+json", ReqBody) ->
	try
		case Etag of
			undefined ->
				{undefined, zj:decode(ReqBody)};
			Etag ->
				{fm_rest:etag(Etag) , zj:decode(ReqBody)}
		end
	of
		{EtagT, {ok, Patch}} ->
			F = fun() ->
					case mnesia:read(specification, Id, write) of
						[#specification{last_modified = LM}]
								when EtagT /= undefined, LM /= EtagT ->
							mnesia:abort(412);
						[#specification{} = Specification] ->
							TS = erlang:system_time(?MILLISECOND),
							N = erlang:unique_integer([positive]),
							LM = {TS, N},
							Specification1 = Specification#specification{last_modified = LM},
							case catch im:merge(Specification1, specification(Patch)) of
								#specification{} = Specification2 ->
									mnesia:write(specification, Specification2, write),
									Specification2;
								_ ->
									mnesia:abort(400)
							end;
						[] ->
							mnesia:abort(404)
					end
			end,
			case mnesia:transaction(F) of
				{atomic, #specification{last_modified = LM1} = NewSpecification} ->
					Body = zj:encode(specification(NewSpecification)),
					Headers = [{content_type, "application/json"},
							{location, ?PathSpecification ++ Id},
							{etag, im_rest:etag(LM1)}],
					{ok, Headers, Body};
				{aborted, Status} when is_integer(Status) ->
					{error, Status};
				{aborted, _Reason} ->
					{error, 500}
			end;
		_ ->
			{error, 400}
	catch
		_:_ ->
			{error, 400}
	end;
patch_specification(_, _, "application/json", _) ->
	{error, 415}.

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
specification([description | T],
		#specification{description = Description} = R, Acc)
		when is_list(Description) ->
	specification(T, R, Acc#{"description" => Description});
specification([description | T], #{"description" := Description} = M, Acc)
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
specification([party | T], #specification{party = PartyRefs} = R, Acc)
		when is_list(PartyRefs), length(PartyRefs) > 0 ->
	specification(T, R, Acc#{"relatedParty" => im_rest:party_ref(PartyRefs)});
specification([party | T], #{"relatedParty" := PartyRefs} = M, Acc)
		when is_list(PartyRefs) ->
	specification(T, M, Acc#specification{party = im_rest:party_ref(PartyRefs)});
specification([category | T], #specification{category = Category} = R, Acc)
		when is_list(Category) ->
	specification(T, R, Acc#{"category" => Category});
specification([category | T], #{"category" := Category} = M, Acc)
		when is_list(Category) ->
	specification(T, M, Acc#specification{category = Category});
specification([target_schema | T], #specification{target_schema = TS} = M, Acc)
		when is_record(TS, target_schema_ref) ->
	specification(T, M, Acc#{"targetResourceSchema" => im_rest:target_schema_ref(TS)});
specification([target_schema | T], #{"targetResourceSchema" := TS} = M, Acc)
		when is_map(TS) ->
	specification(T, M, Acc#specification{target_schema = im_rest:target_schema_ref(TS)});
specification([characteristic | T], #specification{characteristic = SpecChars} = R, Acc)
		when is_list(SpecChars), length(SpecChars) > 0->
	specification(T, R, Acc#{"resourceSpecCharacteristic" => specification_char(SpecChars)});
specification([characteristic | T], #{"resourceSpecCharacteristic" := SpecChars} = M, Acc)
		when is_list(SpecChars) ->
	specification(T, M, Acc#specification{characteristic = specification_char(SpecChars)});
specification([feature | T], #specification{feature = SpecFeature} = R, Acc)
		when is_list(SpecFeature) ->
	specification(T, R, Acc#{"resourceSpecFeature" => feature(SpecFeature)});
specification([feature | T], #{"resourceSpecFeature" := SpecFeature} = M, Acc)
		when is_list(SpecFeature) ->
	specification(T, M, Acc#specification{feature = feature(SpecFeature)});
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
specification_rel([id | T], #specification_rel{id = Id} = M, Acc)
		when is_list(Id) ->
	specification_rel(T, M, Acc#{"id" => Id});
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
specification_rel([class_type | T], #specification_rel{class_type = Type} = R, Acc)
		when is_list(Type) ->
	specification_rel(T, R, Acc#{"@type" => Type});
specification_rel([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	specification_rel(T, M, Acc#specification_rel{class_type = Type});
specification_rel([base_type | T], #specification_rel{base_type = Type} = R, Acc)
		when is_list(Type) ->
	specification_rel(T, R, Acc#{"@baseType" => Type});
specification_rel([base_type | T], #{"@baseType" := Type} = M, Acc)
		when is_list(Type) ->
	specification_rel(T, M, Acc#specification_rel{base_type = Type});
specification_rel([schema | T], #specification_rel{schema = Type} = R, Acc)
		when is_list(Type) ->
	specification_rel(T, R, Acc#{"@schemaLocation" => Type});
specification_rel([schema | T], #{"@schemaLocation" := Type} = M, Acc)
		when is_list(Type) ->
	specification_rel(T, M, Acc#specification_rel{schema = Type});
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
specification_rel([rel_type | T], #specification_rel{rel_type = Type} = R, Acc)
		when is_list(Type) ->
	specification_rel(T, R, Acc#{"relationshipType" => Type});
specification_rel([rel_type | T], #{"relationshipType" := Type} = M, Acc)
		when is_list(Type) ->
	specification_rel(T, M, Acc#specification_rel{rel_type = Type});
specification_rel([role | T], #specification_rel{role = Role} = R, Acc)
		when is_list(Role) ->
	specification_rel(T, R, Acc#{"role" => Role});
specification_rel([role | T], #{"role" := Role} = M, Acc)
		when is_list(Role) ->
	specification_rel(T, M, Acc#specification_rel{role = Role});
specification_rel([min | T], #specification_rel{min = Min} = R, Acc)
		when is_integer(Min), Min >= 0 ->
	specification_rel(T, R, Acc#{"minimumQuantity" => Min});
specification_rel([min | T], #{"minimumQuantity" := Min} = M, Acc)
		when is_integer(Min), Min >= 0 ->
	specification_rel(T, M, Acc#specification_rel{min = Min});
specification_rel([max | T], #specification_rel{max = Max} = R, Acc)
		when is_integer(Max), Max >= 0 ->
	specification_rel(T, R, Acc#{"maximumQuantity" => Max});
specification_rel([max | T], #{"maximumQuantity" := Max} = M, Acc)
		when is_integer(Max), Max >= 0 ->
	specification_rel(T, M, Acc#specification_rel{max = Max});
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

-spec feature(ResourceSpecFeature) -> ResourceSpecFeature
	when
		ResourceSpecFeature :: [feature()] | [map()].
%% @doc CODEC for `ResourceSpecFeature'.
%% @private
feature([#feature{} | _] = List) ->
	Fields = record_info(fields, feature),
	[feature(Fields, R, #{}) || R <- List];
feature([#{} | _] = List) ->
	Fields = record_info(fields, feature),
	[feature(Fields, M, #feature{}) || M <- List];
feature([]) ->
	[].
%% @hidden
feature([name | T], #feature{name = Name} = R, Acc)
		when is_list(Name) ->
	feature(T, R, Acc#{"name" => Name});
feature([name | T], #{"name" := Name} = R, Acc)
		when is_list(Name) ->
	feature(T, R, Acc#feature{name = Name});
feature([id | T], #feature{id = Id} = R, Acc)
		when is_list(Id) ->
	feature(T, R, Acc#{"id" => Id});
feature([id | T], #{"id" := Id} = R, Acc)
		when is_list(Id) ->
	feature(T, R, Acc#feature{id = Id});
feature([href | T], #feature{href = Href} = R, Acc)
		when is_list(Href) ->
	feature(T, R, Acc#{"href" => Href});
feature([href | T], #{"href" := Href} = R, Acc)
		when is_list(Href) ->
	feature(T, R, Acc#feature{href = Href});
feature([version | T], #feature{version = Version} = R, Acc)
		when is_list(Version) ->
	feature(T, R, Acc#{"version" => Version});
feature([version | T], #{"version" := Version} = R, Acc)
		when is_list(Version) ->
	feature(T, R, Acc#feature{version = Version});
feature([class_type | T], #feature{class_type = Type} = R, Acc)
		when is_list(Type) ->
	feature(T, R, Acc#{"class_type" => Type});
feature([class_type | T], #{"class_type" := Type} = R, Acc)
		when is_list(Type) ->
	feature(T, R, Acc#feature{class_type = Type});
feature([bundle | T], #feature{bundle = Bundle} = R, Acc)
		when is_boolean(Bundle) ->
	feature(T, R, Acc#{"bundle" => Bundle});
feature([bundle | T], #{"bundle" := Bundle} = R, Acc)
		when is_boolean(Bundle) ->
	feature(T, R, Acc#feature{bundle = Bundle});
feature([start_date | T], #feature{start_date = StartDate} = R, Acc)
		when is_integer(StartDate) ->
	feature(T, R, Acc#{"start_date" => StartDate});
feature([start_date | T], #{"start_date" := StartDate} = R, Acc)
		when is_integer(StartDate) ->
	feature(T, R, Acc#feature{start_date = StartDate});
feature([end_date | T], #feature{end_date = EndDate} = R, Acc)
		when is_integer(EndDate) ->
	feature(T, R, Acc#{"end_date" => EndDate});
feature([end_date | T], #{"end_date" := EndDate} = R, Acc)
		when is_integer(EndDate) ->
	feature(T, R, Acc#feature{end_date = EndDate});
feature([enabled | T], #feature{enabled = Enabled} = R, Acc)
		when is_boolean(Enabled) ->
	feature(T, R, Acc#{"enabled" => Enabled});
feature([enabled | T], #{"enabled" := Enabled} = R, Acc)
		when is_boolean(Enabled) ->
	feature(T, R, Acc#feature{enabled = Enabled});
feature([_ | T], R, Acc) ->
	feature(T, R, Acc);
feature([], _, Acc) ->
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
specification_char([id | T], #specification_char{id = Id} = R, Acc)
		when is_list(Id) ->
	specification_char(T, R, Acc#{"id" => Id});
specification_char([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	specification_char(T, M, Acc#specification_char{id = Id});
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
specification_char([class_type | T], #specification_char{class_type = Type} = R, Acc)
		when is_list(Type) ->
	specification_char(T, R, Acc#{"@type" => Type});
specification_char([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	specification_char(T, M, Acc#specification_char{class_type = Type});
specification_char([base_type | T], #specification_char{base_type = Type} = R, Acc)
		when is_list(Type) ->
	specification_char(T, R, Acc#{"@baseType" => Type});
specification_char([base_type | T], #{"@baseType" := Type} = M, Acc)
		when is_list(Type) ->
	specification_char(T, M, Acc#specification_char{base_type = Type});
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
specification_char([related | T],
		#specification_char{related = CharRels} = R, Acc)
		when is_list(CharRels) ->
	specification_char(T, R,
			Acc#{"resourceSpecCharRelationship" => spec_char_rel(CharRels)});
specification_char([related | T],
		#{"resourceSpecCharRelationship" := CharRels} = M, Acc)
		when is_list(CharRels) ->
	specification_char(T, M,
			Acc#specification_char{related = spec_char_rel(CharRels)});
specification_char([value_type | T], #specification_char{value_type = Type} = R, Acc)
		when is_list(Type) ->
	specification_char(T, R, Acc#{"valueType" => Type});
specification_char([value_type | T], #{"valueType" := Type} = M, Acc)
		when is_list(Type) ->
	specification_char(T, M, Acc#specification_char{value_type = Type});
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
spec_char_rel([char_id | T], #spec_char_rel{char_id = Id} = M, Acc)
		when is_list(Id) ->
	spec_char_rel(T, M, Acc#{"characteristicSpecificationId" => Id});
spec_char_rel([char_id | T], #{"characteristicSpecificationId" := Id} = M, Acc)
		when is_list(Id) ->
	spec_char_rel(T, M, Acc#spec_char_rel{char_id = Id});
spec_char_rel([name | T], #spec_char_rel{name = Name} = R, Acc)
		when is_list(Name) ->
	spec_char_rel(T, R, Acc#{"name" => Name});
spec_char_rel([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	spec_char_rel(T, M, Acc#spec_char_rel{name = Name});
spec_char_rel([class_type | T], #spec_char_rel{class_type = Type} = R, Acc)
		when is_list(Type) ->
	spec_char_rel(T, R, Acc#{"@type" => Type});
spec_char_rel([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	spec_char_rel(T, M, Acc#spec_char_rel{class_type = Type});
spec_char_rel([base_type | T], #spec_char_rel{base_type = Type} = R, Acc)
		when is_list(Type) ->
	spec_char_rel(T, R, Acc#{"@baseType" => Type});
spec_char_rel([base_type | T], #{"@baseType" := Type} = M, Acc)
		when is_list(Type) ->
	spec_char_rel(T, M, Acc#spec_char_rel{class_type = Type});
spec_char_rel([schema | T], #spec_char_rel{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	spec_char_rel(T, R, Acc#{"@schemaLocation" => Schema});
spec_char_rel([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	spec_char_rel(T, M, Acc#spec_char_rel{schema = Schema});
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
spec_char_rel([rel_type | T], #spec_char_rel{rel_type = Type} = R, Acc)
		when is_list(Type) ->
	spec_char_rel(T, R, Acc#{"relationshipType" => Type});
spec_char_rel([rel_type | T], #{"relationshipType" := Type} = M, Acc)
		when is_list(Type) ->
	spec_char_rel(T, M, Acc#spec_char_rel{rel_type = Type});
spec_char_rel([res_id | T], #spec_char_rel{res_id = Id} = R, Acc)
		when is_list(Id) ->
	spec_char_rel(T, R, Acc#{"resourceSpecificationId" => Id});
spec_char_rel([res_id | T], #{"resourceSpecificationId" := Id} = M, Acc)
		when is_list(Id) ->
	spec_char_rel(T, M, Acc#spec_char_rel{res_id = Id});
spec_char_rel([res_href | T], #spec_char_rel{res_href = Href} = R, Acc)
		when is_list(Href) ->
	spec_char_rel(T, R, Acc#{"resourceSpecificationHref" => Href});
spec_char_rel([res_href | T], #{"resourceSpecificationHref" := Href} = M, Acc)
		when is_list(Href) ->
	spec_char_rel(T, M, Acc#spec_char_rel{res_href = Href});
spec_char_rel([_ | T], R, Acc) ->
	spec_char_rel(T, R, Acc);
spec_char_rel([], _, Acc) ->
	Acc.

%% @hidden
match([{Key, Value} | T], Acc) ->
	match(T, [{exact, Key, Value} | Acc]);
match([], Acc) ->
	Acc.

%% @hidden
query_start(Method, Query, Filters, RangeStart, RangeEnd) ->
	try
		CountOnly = case Method of
			"GET" ->
				false;
			"HEAD" ->
				true
		end,
		FilterArgs = case lists:keyfind("filter", 1, Query) of
			{_, StringF} ->
				{ok, Tokens, _} = im_rest_query_scanner:string(StringF),
				{ok, Filter} = im_rest_query_parser:parse(Tokens),
				parse_filter(Filter);
			false when length(Query) > 0 ->
				Rest = match(Query, []),
				Rest1 = [{array,[{complex, Rest}]}],
				parse_filter(Rest1);
			false ->
				'_'
		end,
		Sort = case lists:keyfind("sort", 1, Query) of
			{_, StringS} ->
				sorts(StringS);
			false ->
				[]
		end,
		MFA = [im, query, [specification, Sort, FilterArgs, CountOnly]],
		case supervisor:start_child(im_rest_pagination_sup, [MFA]) of
			{ok, PageServer, Etag} ->
				query_page(PageServer, Etag, Query, Filters, RangeStart, RangeEnd);
			{error, _Reason} ->
				{error, 500}
		end
	catch
		_:_ ->
			{error, 400}
	end.

%% @hidden
query_page(PageServer, Etag, _Query, _Filters, Start, End) ->
	case gen_server:call(PageServer, {Start, End}, infinity) of
		{error, Status} ->
			{error, Status};
		{undefined, ContentRange} ->
			Headers = [{content_type, "application/json"},
				{etag, Etag}, {accept_ranges, "items"},
				{content_range, ContentRange}],
			{ok, Headers, []};
		{Events, ContentRange} ->
			JsonObj = lists:map(fun specification/1, Events),
			Body = zj:encode(JsonObj),
			Headers = [{content_type, "application/json"},
				{etag, Etag}, {accept_ranges, "items"},
				{content_range, ContentRange}],
			{ok, Headers, Body}
	end.

%% @hidden
sorts(Query) ->
	sorts(string:tokens(Query, [$,]), []).
%% @hidden
sorts(["id" | T], Acc) ->
	sorts(T, [#specification.id | Acc]);
sorts(["-id" | T], Acc) ->
	sorts(T, [-#specification.id | Acc]);
sorts(["href" | T], Acc) ->
	sorts(T, [#specification.href | Acc]);
sorts(["-href" | T], Acc) ->
	sorts(T, [-#specification.href | Acc]);
sorts(["name" | T], Acc) ->
	sorts(T, [#specification.name | Acc]);
sorts(["-name" | T], Acc) ->
	sorts(T, [-#specification.name | Acc]);
sorts(["description" | T], Acc) ->
	sorts(T, [#specification.description | Acc]);
sorts(["-description" | T], Acc) ->
	sorts(T, [-#specification.description | Acc]);
sorts(["@type" | T], Acc) ->
	sorts(T, [#specification.class_type | Acc]);
sorts(["-@type" | T], Acc) ->
	sorts(T, [-#specification.class_type | Acc]);
sorts(["@baseType" | T], Acc) ->
	sorts(T, [#specification.base_type | Acc]);
sorts(["-@baseType" | T], Acc) ->
	sorts(T, [-#specification.base_type | Acc]);
sorts(["@schemaLocation" | T], Acc) ->
	sorts(T, [#specification.schema | Acc]);
sorts(["-@schemaLocation" | T], Acc) ->
	sorts(T, [-#specification.schema | Acc]);
sorts(["lifecycleStatus" | T], Acc) ->
	sorts(T, [#specification.status | Acc]);
sorts(["-lifecycleStatus" | T], Acc) ->
	sorts(T, [-#specification.status | Acc]);
sorts(["version" | T], Acc) ->
	sorts(T, [#specification.version | Acc]);
sorts(["-version" | T], Acc) ->
	sorts(T, [-#specification.version | Acc]);
sorts(["lastUpdate" | T], Acc) ->
	sorts(T, [#specification.last_modified | Acc]);
sorts(["-lastUpdate" | T], Acc) ->
	sorts(T, [-#specification.last_modified | Acc]);
sorts(["isBundle" | T], Acc) ->
	sorts(T, [#specification.bundle | Acc]);
sorts(["-isBundle" | T], Acc) ->
	sorts(T, [-#specification.bundle | Acc]);
sorts(["category" | T], Acc) ->
	sorts(T, [#specification.category | Acc]);
sorts(["-category" | T], Acc) ->
	sorts(T, [-#specification.category | Acc]);
sorts(["targetResourceSchema" | T], Acc) ->
	sorts(T, [#specification.target_schema | Acc]);
sorts(["-targetResourceSchema" | T], Acc) ->
	sorts(T, [-#specification.target_schema | Acc]);
sorts([], Acc) ->
	lists:reverse(Acc).

-spec parse_filter(Query) -> Result
	when
		Query :: term(),
		Result :: ets:match_spec().
%% @doc Create `[MatchHead, MatchConditions]' from `Query'.
%% 	MatchHead = ets:match_pattern()
%%		MatchConditions = [tuple()]
%% @private
parse_filter(Query) ->
	parse_filter(Query, #specification{_ = '_'}, []).
%% @hidden
parse_filter([{array, [{complex, {all, Filters}}]}], MatchHead, MatchConditions) ->
	parse_filter(Filters, all, MatchHead, MatchConditions);
parse_filter([{array, [{complex, {any, Filters}}]}], MatchHead, MatchConditions) ->
	parse_filter(Filters, any, MatchHead, MatchConditions);
parse_filter([{array, [{complex, [{in, Filter}]}]}], MatchHead, MatchConditions) ->
	parse_filter(Filter, all, MatchHead, MatchConditions);
parse_filter([{array, [{complex, Filter}]}], MatchHead, MatchConditions) ->
	parse_filter(Filter, all, MatchHead, MatchConditions);
parse_filter([], MatchHead, MatchConditions) ->
	[{MatchHead, MatchConditions, ['$_']}].

%% @hidden
parse_filter([{like, "id", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#specification{id = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#specification{id = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$1', Like} | MatchConditions],
			{MatchHead#specification{id = '$1'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "id", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$1', Cond, []),
	NewMatchHead = MatchHead#specification{id = '$1'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "id", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#specification{id = Name}, MatchConditions);
parse_filter([{exact, "id", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$1', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#specification{id = '$1'}, NewMatchConditions);
parse_filter([{notexact, "id", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#specification{id = '$1'},
	NewMatchConditions = [{'/=', '$1', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "id", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$1', []),
	NewMatchHead = MatchHead#specification{id = '$1'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "href", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#specification{href = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#specification{href = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$2', Like} | MatchConditions],
			{MatchHead#specification{href = '$2'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "href", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$2', Cond, []),
	NewMatchHead = MatchHead#specification{href = '$2'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "href", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#specification{href = Name}, MatchConditions);
parse_filter([{exact, "href", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$2', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#specification{href = '$2'}, NewMatchConditions);
parse_filter([{notexact, "href", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#specification{href = '$2'},
	NewMatchConditions = [{'/=', '$2', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "href", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$2', []),
	NewMatchHead = MatchHead#specification{href = '$2'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "name", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#specification{name = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#specification{name = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$3', Like} | MatchConditions],
			{MatchHead#specification{name = '$3'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "name", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$3', Cond, []),
	NewMatchHead = MatchHead#specification{name = '$3'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "name", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#specification{name = Name}, MatchConditions);
parse_filter([{exact, "name", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$3', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#specification{name = '$3'}, NewMatchConditions);
parse_filter([{notexact, "name", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#specification{name = '$3'},
	NewMatchConditions = [{'/=', '$3', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "name", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$3', []),
	NewMatchHead = MatchHead#specification{name = '$3'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "description", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#specification{description = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#specification{description = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$4', Like} | MatchConditions],
			{MatchHead#specification{description = '$4'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "description", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$4', Cond, []),
	NewMatchHead = MatchHead#specification{description = '$4'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "description", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#specification{description = Name}, MatchConditions);
parse_filter([{exact, "description", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$4', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#specification{description = '$4'}, NewMatchConditions);
parse_filter([{notexact, "description", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#specification{description = '$4'},
	NewMatchConditions = [{'/=', '$4', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "description", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$4', []),
	NewMatchHead = MatchHead#specification{description = '$4'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "@type", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#specification{class_type = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#specification{class_type = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$5', Like} | MatchConditions],
			{MatchHead#specification{class_type = '$5'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "@type", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$5', Cond, []),
	NewMatchHead = MatchHead#specification{class_type = '$5'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "@type", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#specification{class_type = Name}, MatchConditions);
parse_filter([{exact, "@type", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$5', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#specification{class_type = '$5'}, NewMatchConditions);
parse_filter([{notexact, "@type", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#specification{class_type = '$5'},
	NewMatchConditions = [{'/=', '$5', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "@type", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$5', []),
	NewMatchHead = MatchHead#specification{class_type = '$5'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Study"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#specification{status = in_study}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Design"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#specification{status = in_design}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Test"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#specification{status = in_test}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Rejected"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#specification{status = rejected}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Active"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#specification{status = active}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Launched"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#specification{status = launched}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Retired"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#specification{status = retired}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "Obsolete"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#specification{status = obsolete}, MatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Study"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$8', in_study} | MatchConditions],
	parse_filter(T, any, MatchHead#specification{status = '$8'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Design"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$8', in_design} | MatchConditions],
	parse_filter(T, any, MatchHead#specification{status = '$8'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "In Test"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$8', in_test} | MatchConditions],
	parse_filter(T, any, MatchHead#specification{status = '$8'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Rejected"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$8', rejected} | MatchConditions],
	parse_filter(T, any, MatchHead#specification{status = '$8'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Active"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$8', active} | MatchConditions],
	parse_filter(T, any, MatchHead#specification{status = '$8'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Launched"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$8', launched} | MatchConditions],
	parse_filter(T, any, MatchHead#specification{status = '$8'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Retired"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$8', retired} | MatchConditions],
	parse_filter(T, any, MatchHead#specification{status = '$8'}, NewMatchConditions);
parse_filter([{exact, "lifecycleStatus", "Obsolete"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$8', obsolete} | MatchConditions],
	parse_filter(T, any, MatchHead#specification{status = '$8'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "In Study"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$8', in_study} | MatchConditions],
	parse_filter(T, Cond, MatchHead#specification{status = '$8'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "In Design"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$8', in_design} | MatchConditions],
	parse_filter(T, Cond, MatchHead#specification{status = '$8'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "In Test"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$8', in_test} | MatchConditions],
	parse_filter(T, Cond, MatchHead#specification{status = '$8'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Rejected"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$8', rejected} | MatchConditions],
	parse_filter(T, Cond, MatchHead#specification{status = '$8'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Active"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$8', active} | MatchConditions],
	parse_filter(T, Cond, MatchHead#specification{status = '$8'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Launched"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$8', launched} | MatchConditions],
	parse_filter(T, Cond, MatchHead#specification{status = '$8'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Retired"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$8', retired} | MatchConditions],
	parse_filter(T, Cond, MatchHead#specification{status = '$8'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleStatus", "Obsolete"} | T], Cond, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'/=', '$8', obsolete} | MatchConditions],
	parse_filter(T, Cond, MatchHead#specification{status = '$8'}, NewMatchConditions);
parse_filter([{in, "lifecycleStatus", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$8', []),
	NewMatchHead = MatchHead#specification{status = '$8'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "category", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#specification{category = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#specification{category = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$14', Like} | MatchConditions],
			{MatchHead#specification{category = '$14'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "category", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$14', Cond, []),
	NewMatchHead = MatchHead#specification{category = '$14'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "category", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#specification{category = Name}, MatchConditions);
parse_filter([{exact, "category", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$14', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#specification{category = '$14'}, NewMatchConditions);
parse_filter([{notexact, "category", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#specification{category = '$14'},
	NewMatchConditions = [{'/=', '$14', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "category", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$14', []),
	NewMatchHead = MatchHead#specification{category = '$14'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "isBundle", "true"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#specification{bundle = true}, MatchConditions);
parse_filter([{exact, "isBundle", "false"} | T], all, MatchHead, MatchConditions) ->
	parse_filter(T, all, MatchHead#specification{bundle = false}, MatchConditions);
parse_filter([{exact, "isBundle", "true"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$13', true} | MatchConditions],
	parse_filter(T, any, MatchHead#specification{bundle = '$13'}, NewMatchConditions);
parse_filter([{exact, "isBundle", "false"} | T], any, MatchHead, MatchConditions) ->
	NewMatchConditions = [{'==', '$13', false} | MatchConditions],
	parse_filter(T, any, MatchHead#specification{bundle = '$13'}, NewMatchConditions);
parse_filter([], all, MatchHead, MatchConditions) ->
	[{MatchHead, MatchConditions, ['$_']}];
parse_filter([], any, MatchHead, MatchConditions) ->
	NewMatchConditions =  list_to_tuple(['or' | MatchConditions]),
	[{MatchHead, [NewMatchConditions], ['$_']}].

%% @hidden
in(["In Study" | T], '$8' = Var, Acc) ->
	in(T, Var, [{'==', Var, in_study} | Acc]);
in(["In Design" | T], '$8' = Var, Acc) ->
	in(T, Var, [{'==', Var, in_design} | Acc]);
in(["In Test" | T], '$8' = Var, Acc) ->
	in(T, Var, [{'==', Var, in_test} | Acc]);
in(["Rejected" | T], '$8' = Var, Acc) ->
	in(T, Var, [{'==', Var, rejected} | Acc]);
in(["Active" | T], '$8' = Var, Acc) ->
	in(T, Var, [{'==', Var, active} | Acc]);
in(["Launched" | T], '$8' = Var, Acc) ->
	in(T, Var, [{'==', Var, launched} | Acc]);
in(["Retired" | T], '$8' = Var, Acc) ->
	in(T, Var, [{'==', Var, retired} | Acc]);
in([H |T], Var, Acc) ->
	in(T, Var, [{'==', Var, H} | Acc]);
in([], _, Acc) when length(Acc) > 1 ->
	[list_to_tuple(['or' | Acc])];
in([], _, Acc) ->
	Acc.

%% @hidden
like([H |T], Var, Cond, Acc) ->
	case lists:last(H) of
		$% ->
			Prefix = lists:droplast(H),
			NewMatchConditions = match_prefix(Prefix, Var),
			like(T, Var, Cond, [NewMatchConditions | Acc]);
		_ ->
			like(T, Var, Cond, [{'==', Var, H} | Acc])
	end;
like([], _, _, Acc) when length(Acc) > 1 ->
	[list_to_tuple(['or' | Acc])];
like([], _, _, Acc) ->
	Acc.

%% @hidden
match_prefix([A] = Start, Var) ->
	{'and', {'>=', Var, Start}, {'<', Var, [A + 1]}};
match_prefix(Start, Var) ->
	{A, [B]} = lists:split(length(Start) - 1, Start),
	{'and', {'>=', Var, Start}, {'<', Var, A ++ [B + 1]}}.

