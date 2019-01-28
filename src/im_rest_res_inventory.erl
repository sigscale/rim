%%% im_rest_res_inventory.erl
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
%%% 	Handle `Resource' collection.
%%%
-module(im_rest_res_inventory).
-copyright('Copyright (c) 2019 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([get_inventory/2, get_resource/2, post_resource/1, delete_resource/1]).
-export([resource/1]).
 
-include("im.hrl").

-define(PathInventory, "/resourceInventoryManagement/v3/").
-define(PathFunction, "/resourceFunctionActivationConfiguration/v2/").

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

-spec get_inventory(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Handle `GET' request on `Resource' collection.
get_inventory(Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_inventory(NewQuery, Filters, Headers);
		false ->
			get_inventory(Query, [], Headers)
	end.
%% @hidden
get_inventory(Query, Filters, Headers) ->
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

-spec get_resource(Id, Query) -> Result
	when
		Id :: string(),
		Query :: [{Key :: string(), Value :: string()}],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Handle `GET' request on a `Resource' resource.
get_resource(Id, Query) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, L}, NewQuery} ->
			get_resource(Id, NewQuery, string:tokens(L, ","));
		false ->
			get_resource(Id, Query, [])
	end.
%% @hidden
get_resource(Id, [] = _Query, _Filters) ->
	case im:get_resource(Id) of
		{ok, #resource{last_modified = LM} = Resource} ->
			Headers = [{content_type, "application/json"},
					{etag, im_rest:etag(LM)}],
			Body = zj:encode(resource(Resource)),
			{ok, Headers, Body};
		{error, _Reason} ->
			{error, 404}
	end;
get_resource(_, _, _) ->
	{error, 400}.

-spec post_resource(RequestBody) -> Result
	when
		RequestBody :: list(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
			| {error, ErrorCode :: integer()}.
%% @doc Handle `POST' request on `Resource' collection.
post_resource(RequestBody) ->
	try
		Resource = resource(zj:decode(RequestBody)),
		case im:add_resource(Resource) of
			{ok, #resource{id = Id, last_modified = LM} = Resource} ->
				Location = "/resourceInventoryManagement/v3/resource/" ++ Id,
				Headers = [{location, Location}, {etag, im_rest:etag(LM)}],
				Body = zj:encode(resource(Resource)),
				{ok, Headers, Body};
			{error, _Reason} ->
				{error, 400}
		end
	catch
		_:_Reason1 ->
			{error, 400}
	end.

-spec delete_resource(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()} .
%% @doc Handle `DELETE' request on a `Resource' resource.
delete_resource(Id) ->
	case im:delete_resource(Id) of
		ok ->
			{ok, [], []};
		{error, _Reason} ->
			{error, 400}
	end.

-spec resource(Resource) -> Resource
	when
		Resource :: #resource{} | map().
%% @doc CODEC for `Resource'.
resource(#resource{} = Resource) ->
	resource(record_info(fields, resource), Resource, #{});
resource(#{} = Resource) ->
	resource(record_info(fields, resource), Resource, #resource{});
resource([#resource{} | _] = List) ->
	Fields = record_info(fields, resource),
	[resource(Fields, R, #{}) || R <- List];
resource([#{} | _] = List) ->
	Fields = record_info(fields, resource),
	[resource(Fields, M, #resource{}) || M <- List].
%% @hidden
resource([id | T], #resource{id = Id} = R, Acc) ->
	resource(T, R, Acc#{"id" => Id});
resource([id | T], #{"id" := Id} = M, Acc) ->
	resource(T, M, Acc#resource{id = Id});
resource([href | T], #resource{href = Href} = R, Acc) ->
	resource(T, R, Acc#{"href" => Href});
resource([href | T], #{"href" := Href} = M, Acc) ->
	resource(T, M, Acc#resource{href = Href});
resource([name | T], #resource{name = Name} = R, Acc) ->
	resource(T, R, Acc#{"name" => Name});
resource([name | T], #{"name" := Name} = M, Acc) ->
	resource(T, M, Acc#resource{name = Name});
resource([public_id | T], #resource{public_id = PublicId} = R, Acc) ->
	resource(T, R, Acc#{"public_id" => PublicId});
resource([public_id | T], #{"public_id" := PublicId} = M, Acc) ->
	resource(T, M, Acc#resource{public_id = PublicId});
resource([description| T],
		#resource{description = Description} = R, Acc) ->
	resource(T, R, Acc#{"description" => Description});
resource([description| T], #{"description" := Description} = M, Acc) ->
	resource(T, M, Acc#resource{description = Description});
resource([category | T], #resource{category = Category} = R, Acc) ->
	resource(T, R, Acc#{"category" => Category});
resource([category | T], #{"category" := Category} = M, Acc) ->
	resource(T, M, Acc#resource{category = Category});
resource([class_type | T], #resource{class_type = Type} = R, Acc) ->
	resource(T, R, Acc#{"@baseType" => Type});
resource([class_type | T], #{"@baseType" := Type} = M, Acc) ->
	resource(T, M, Acc#resource{class_type = Type});
resource([base_type | T], #resource{base_type = Type} = R, Acc) ->
	resource(T, R, Acc#{"@type" => Type});
resource([base_type | T], #{"@type" := Type} = M, Acc) ->
	resource(T, M, Acc#resource{base_type = Type});
resource([schema | T], #resource{schema = Schema} = R, Acc) ->
	resource(T, R, Acc#{"@schemaLocation" => Schema});
resource([schema | T], #{"@schemaLocation" := Schema} = M, Acc) ->
	resource(T, M, Acc#resource{schema = Schema});
resource([version | T], #{"version" := Version} = M, Acc) ->
	resource(T, M, Acc#resource{version = Version});
resource([start_date | T], #resource{start_date = StartDate} = R, Acc)
		when is_integer(StartDate) ->
	ValidFor = #{"startDateTime" => im_rest:iso8601(StartDate)},
	resource(T, R, Acc#{"validFor" => ValidFor});
resource([start_date | T],
		#{"validFor" := #{"startDateTime" := Start}} = M, Acc) ->
	resource(T, M, Acc#resource{start_date = im_rest:iso8601(Start)});
resource([end_date | T], #resource{end_date = End} = R,
		#{validFor := ValidFor} = Acc) when is_integer(End) ->
	NewValidFor = ValidFor#{"endDateTime" => im_rest:iso8601(End)},
	resource(T, R, Acc#{"validFor" := NewValidFor});
resource([end_date | T], #resource{end_date = End} = R, Acc)
		when is_integer(End) ->
	ValidFor = #{"endDateTime" => im_rest:iso8601(End)},
	resource(T, R, Acc#{"validFor" := ValidFor});
resource([end_date | T],
		#{"validFor" := #{"endDateTime" := End}} = M, Acc) ->
	resource(T, M, Acc#resource{end_date = im_rest:iso8601(End)});
resource([last_modified | T], #resource{last_modified = LM} = R, Acc) ->
	resource(T, R, Acc#{"lastUpdate" => im_rest:iso8601(LM)});
resource([last_modified | T], #{"lastUpdate" := LM} = M, Acc) ->
	resource(T, M, Acc#resource{last_modified = im_rest:iso8601(LM)});
resource([status | T], #resource{status = Status} = R, Acc)
		when Status /= undefined ->
	resource(T, R, Acc#{"lifecycleStatus" => im_rest:lifecycle_status(Status)});
resource([status | T], #{"lifecycleStatus" := Status} = M, Acc) ->
	resource(T, M, Acc#resource{status = im_rest:lifecycle_status(Status)});
resource([place | T], #resource{place = PlaceRef} = R, Acc) ->
	resource(T, R, Acc#{"place" => place_ref(PlaceRef)});
resource([place | T], #{"place" := PlaceRef} = M, Acc) ->
	resource(T, M, Acc#resource{place = place_ref(PlaceRef)});
resource([note | T], #resource{note = Note} = R, Acc) ->
	resource(T, R, Acc#{"note" => note(Note)});
resource([note | T], #{"note" := Note} = M, Acc) ->
	resource(T, M, Acc#resource{note = note(Note)});
resource([attachment | T], #resource{attachment = Attachment} = R, Acc) ->
	resource(T, R, Acc#{"resourceAttachment" => attachment(Attachment)});
resource([attachment | T], #{"resourceAttachment" := Attachment} = M, Acc) ->
	resource(T, M, Acc#resource{attachment = attachment(Attachment)});
resource([related | T], #resource{related = ResRel} = R, Acc) ->
	resource(T, R, Acc#{"resourceRelationship" => resource_rel(ResRel)});
resource([related | T], #{"resourceRelationship" := ResRel} = M, Acc) ->
	resource(T, M, Acc#resource{related = resource_rel(ResRel)});
resource([specification | T], #resource{specification = SpecRef} = R, Acc)
		when is_record(SpecRef, specification_ref) ->
	resource(T, R, Acc#{"resourceSpecification" => im_rest:specification_ref(SpecRef)});
resource([specification | T], #{"resourceSpecification" := SpecRef} = M, Acc) ->
	resource(T, M, Acc#resource{specification = im_rest:specification_ref(SpecRef)});
resource([characteristic | T], #resource{characteristic = ResChar} = R, Acc)
		when is_record(ResChar, resource_char) ->
	resource(T, R, Acc#{"resorceCharacteristic" => resource_char(ResChar)});
resource([characteristic | T], #{"resorceCharacteristic" := ResChar} = M, Acc) ->
	resource(T, M, Acc#resource{characteristic = resource_char(ResChar)});
resource([_ | T], R, Acc) ->
	resource(T, R, Acc);
resource([], _, Acc) ->
	Acc.

-spec place_ref(PlaceRef) -> PlaceRef
	when
		PlaceRef :: #place_ref{} | map().
%% @doc CODEC for `PlaceRef'.
place_ref(#place_ref{} = PlaceRef) ->
	place_ref(record_info(fields, place_ref), PlaceRef, #{});
place_ref(#{} = PlaceRef) ->
	place_ref(record_info(fields, place_ref), PlaceRef, #place_ref{});
place_ref([#place_ref{} | _] = List) ->
	Fields = record_info(fields, place_ref),
	[place_ref(Fields, R, #{}) || R <- List];
place_ref([#{} | _] = List) ->
	Fields = record_info(fields, place_ref),
	[place_ref(Fields, M, #place_ref{}) || M <- List].
%% @hidden
place_ref([id | T], #place_ref{id = Id} = R, Acc) ->
	place_ref(T, R, Acc#{"id" => Id});
place_ref([id | T], #{"id" := Id} = M, Acc) ->
	place_ref(T, M, Acc#place_ref{id = Id});
place_ref([href | T], #place_ref{href = Href} = R, Acc) ->
	place_ref(T, R, Acc#{"href" => Href});
place_ref([href | T], #{"href" := Href} = M, Acc) ->
	place_ref(T, M, Acc#place_ref{href = Href});
place_ref([name | T], #place_ref{name = Name} = R, Acc) ->
	place_ref(T, R, Acc#{"name" => Name});
place_ref([name | T], #{"name" := Name} = M, Acc) ->
	place_ref(T, M, Acc#place_ref{name = Name});
place_ref([role | T], #place_ref{role = Role} = R, Acc) ->
	place_ref(T, R, Acc#{"role" => Role});
place_ref([role | T], #{"role" := Role} = M, Acc) ->
	place_ref(T, M, Acc#place_ref{role = Role});
place_ref([_ | T], R, Acc) ->
	place_ref(T, R, Acc);
place_ref([], _, Acc) ->
	Acc.

-spec note(Note) -> Note
	when
		Note :: #note{} | map().
%% @doc CODEC for `Note'.
note(#note{} = Note) ->
	note(record_info(fields, note), Note, #{});
note(#{} = Note) ->
	note(record_info(fields, note), Note, #note{});
note([#note{} | _] = List) ->
	Fields = record_info(fields, note),
	[note(Fields, R, #{}) || R <- List];
note([#{} | _] = List) ->
	Fields = record_info(fields, note),
	[note(Fields, M, #note{}) || M <- List].
%% @hidden
note([author | T], #note{author = Author} = R, Acc) ->
	note(T, R, Acc#{"author" => Author});
note([author | T], #{"author" := Author} = M, Acc) ->
	note(T, M, Acc#note{author = Author});
note([date | T], #note{date = Date} = R, Acc) ->
	note(T, R, Acc#{"date" => Date});
note([date | T], #{"date" := Date} = M, Acc) ->
	note(T, M, Acc#note{date = Date});
note([text | T], #note{text = Text} = R, Acc) ->
	note(T, R, Acc#{"text" => Text});
note([text | T], #{"text" := Text} = M, Acc) ->
	note(T, M, Acc#note{text = Text});
note([_ | T], R, Acc) ->
	note(T, R, Acc);
note([], _, Acc) ->
	Acc.

-spec attachment(ResourceAttachment) -> ResourceAttachment
	when
		ResourceAttachment :: #attachment{} | map().
%% @doc CODEC for `ResourceAttachment'.
attachment(#attachment{} = ResourceAttachment) ->
	attachment(record_info(fields, attachment), ResourceAttachment, #{});
attachment(#{} = ResourceAttachment) ->
	attachment(record_info(fields, attachment), ResourceAttachment, #attachment{});
attachment([#attachment{} | _] = List) ->
	Fields = record_info(fields, attachment),
	[attachment(Fields, R, #{}) || R <- List];
attachment([#{} | _] = List) ->
	Fields = record_info(fields, attachment),
	[attachment(Fields, M, #attachment{}) || M <- List].
%% @hidden
attachment([id | T], #attachment{id = Id} = R, Acc) ->
	attachment(T, R, Acc#{"id" => Id});
attachment([id | T], #{"id" := Id} = M, Acc) ->
	attachment(T, M, Acc#attachment{id = Id});
attachment([href | T], #attachment{href = Href} = R, Acc) ->
	attachment(T, R, Acc#{"href" => Href});
attachment([href | T], #{"href" := Href} = M, Acc) ->
	attachment(T, M, Acc#attachment{href = Href});
attachment([description| T],
		#attachment{description = Description} = R, Acc) ->
	attachment(T, R, Acc#{"description" => Description});
attachment([description| T], #{"description" := Description} = M, Acc) ->
	attachment(T, M, Acc#attachment{description = Description});
attachment([type | T], #attachment{type = Type} = R, Acc) ->
	attachment(T, R, Acc#{"type" => Type});
attachment([type | T], #{"type" := Type} = M, Acc) ->
	attachment(T, M, Acc#attachment{type = Type});
attachment([url | T], #attachment{url = Date} = R, Acc) ->
	attachment(T, R, Acc#{"url" => Date});
attachment([url | T], #{"url" := Date} = M, Acc) ->
	attachment(T, M, Acc#attachment{url = Date});
attachment([_ | T], R, Acc) ->
	attachment(T, R, Acc);
attachment([], _, Acc) ->
	Acc.

-spec resource_rel(ResourceRelationship) -> ResourceRelationship
	when
		ResourceRelationship :: #resource_rel{} | map().
%% @doc CODEC for `ResourceRelationship'.
resource_rel(#resource_rel{} = ResourceRelationship) ->
	resource_rel(record_info(fields, resource_rel), ResourceRelationship, #{});
resource_rel(#{} = ResourceRelationship) ->
	resource_rel(record_info(fields, resource_rel), ResourceRelationship, #resource_rel{});
resource_rel([#resource_rel{} | _] = List) ->
	Fields = record_info(fields, resource_rel),
	[resource_rel(Fields, R, #{}) || R <- List];
resource_rel([#{} | _] = List) ->
	Fields = record_info(fields, resource_rel),
	[resource_rel(Fields, M, #resource_rel{}) || M <- List].
%% @hidden
resource_rel([id | T], #resource_rel{id = Id} = R, Acc) ->
	resource_rel(T, R, Acc#{"id" => Id});
resource_rel([id | T], #{"id" := Id} = M, Acc) ->
	resource_rel(T, M, Acc#resource_rel{id = Id});
resource_rel([href | T], #resource_rel{href = Href} = R, Acc) ->
	resource_rel(T, R, Acc#{"href" => Href});
resource_rel([href | T], #{"href" := Href} = M, Acc) ->
	resource_rel(T, M, Acc#resource_rel{href = Href});
resource_rel([type | T], #resource_rel{type = Type} = R, Acc) ->
	resource_rel(T, R, Acc#{"type" => Type});
resource_rel([type | T], #{"type" := Type} = M, Acc) ->
	resource_rel(T, M, Acc#resource_rel{type = Type});
resource_rel([start_date | T], #resource_rel{start_date = StartDate} = R, Acc)
		when is_integer(StartDate) ->
	ValidFor = #{"startDateTime" => im_rest:iso8601(StartDate)},
	resource_rel(T, R, Acc#{"validFor" => ValidFor});
resource_rel([start_date | T],
		#{"validFor" := #{"startDateTime" := Start}} = M, Acc) ->
	resource_rel(T, M, Acc#resource_rel{start_date = im_rest:iso8601(Start)});
resource_rel([end_date | T], #resource_rel{end_date = End} = R,
		#{validFor := ValidFor} = Acc) when is_integer(End) ->
	NewValidFor = ValidFor#{"endDateTime" => im_rest:iso8601(End)},
	resource_rel(T, R, Acc#{"validFor" := NewValidFor});
resource_rel([end_date | T], #resource_rel{end_date = End} = R, Acc)
		when is_integer(End) ->
	ValidFor = #{"endDateTime" => im_rest:iso8601(End)},
	resource_rel(T, R, Acc#{"validFor" := ValidFor});
resource_rel([end_date | T],
		#{"validFor" := #{"endDateTime" := End}} = M, Acc) ->
	resource_rel(T, M, Acc#resource_rel{end_date = im_rest:iso8601(End)});
resource_rel([_ | T], R, Acc) ->
	resource_rel(T, R, Acc);
resource_rel([], _, Acc) ->
	Acc.

-spec resource_char(ResourceCharacteristic) -> ResourceCharacteristic
	when
		ResourceCharacteristic :: #resource_char{} | map().
%% @doc CODEC for `ResourceCharacteristic'.
resource_char(#resource_char{} = ResourceCharacteristic) ->
	resource_char(record_info(fields, resource_char), ResourceCharacteristic, #{});
resource_char(#{} = ResourceCharacteristic) ->
	resource_char(record_info(fields, resource_char), ResourceCharacteristic, #resource_char{});
resource_char([#resource_char{} | _] = List) ->
	Fields = record_info(fields, resource_char),
	[resource_char(Fields, R, #{}) || R <- List];
resource_char([#{} | _] = List) ->
	Fields = record_info(fields, resource_char),
	[resource_char(Fields, M, #resource_char{}) || M <- List].
%% @hidden
resource_char([name | T], #resource_char{name = Name} = R, Acc) ->
	resource_char(T, R, Acc#{"name" => Name});
resource_char([name | T], #{"name" := Name} = M, Acc) ->
	resource_char(T, M, Acc#resource_char{name = Name});
resource_char([class_type | T], #resource_char{class_type = Type} = R, Acc) ->
	resource_char(T, R, Acc#{"@type" => Type});
resource_char([class_type | T], #{"@type" := Type} = M, Acc) ->
	resource_char(T, M, Acc#resource_char{class_type = Type});
resource_char([schema | T], #resource_char{schema = Schema} = R, Acc) ->
	resource_char(T, R, Acc#{"@schemaLocation" => Schema});
resource_char([schema | T], #{"@schemaLocation" := Schema} = M, Acc) ->
	resource_char(T, M, Acc#resource_char{schema = Schema});
resource_char([value | T], #resource_char{value = Value} = R, Acc) ->
	resource_char(T, R, Acc#{"value" => Value});
resource_char([value | T], #{"value" := Value} = M, Acc) ->
	resource_char(T, M, Acc#resource_char{value = Value});
resource_char([_ | T], R, Acc) ->
	resource_char(T, R, Acc);
resource_char([], _, Acc) ->
	Acc.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
query_start(Query, Filters, RangeStart, RangeEnd) ->
	try
		case lists:keyfind("filter", 1, Query) of
			{_, String} ->
				{ok, Tokens, _} = im_rest_query_scanner:string(String),
				case im_rest_query_parser:parse(Tokens) of
					{ok, [{array, [{complex, [{"id", like, [Id]}]}]}]} ->
						{{like, Id}, '_'}
				end;
			false ->
				{'_', '_'}
		end
	of
		{MatchId, MatchLocale} ->
			MFA = [im, query_inventory, [MatchId, MatchLocale]],
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
			Resources = lists:map(fun resource/1, Events),
			Body = zj:encode(Resources),
			Headers = [{content_type, "application/json"},
					{etag, Etag}, {accept_ranges, "items"},
					{content_range, ContentRange}],
			{ok, Headers, Body}
	end.

