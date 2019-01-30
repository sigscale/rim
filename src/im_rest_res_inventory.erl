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
	case im:del_resource(Id) of
		ok ->
			{ok, [], []};
		{error, _Reason} ->
			{error, 400}
	end.

-spec resource(Resource) -> Resource
	when
		Resource :: resource() | map().
%% @doc CODEC for `Resource'.
resource(#resource{} = Resource) ->
	resource(record_info(fields, resource), Resource, #{});
resource(#{} = Resource) ->
	resource(record_info(fields, resource), Resource, #resource{}).
%% @hidden
resource([id | T], #resource{id = Id} = R, Acc)
		when is_list(Id) ->
	resource(T, R, Acc#{"id" => Id});
resource([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	resource(T, M, Acc#resource{id = Id});
resource([href | T], #resource{href = Href} = R, Acc)
		when is_list(Href) ->
	resource(T, R, Acc#{"href" => Href});
resource([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	resource(T, M, Acc#resource{href = Href});
resource([name | T], #resource{name = Name} = R, Acc)
		when is_list(Name) ->
	resource(T, R, Acc#{"name" => Name});
resource([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	resource(T, M, Acc#resource{name = Name});
resource([public_id | T], #resource{public_id = PublicId} = R, Acc)
		when is_list(PublicId) ->
	resource(T, R, Acc#{"public_id" => PublicId});
resource([public_id | T], #{"public_id" := PublicId} = M, Acc)
		when is_list(PublicId) ->
	resource(T, M, Acc#resource{public_id = PublicId});
resource([description| T],
		#resource{description = Description} = R, Acc)
		when is_list(Description) ->
	resource(T, R, Acc#{"description" => Description});
resource([description| T], #{"description" := Description} = M, Acc)
		when is_list(Description) ->
	resource(T, M, Acc#resource{description = Description});
resource([category | T], #resource{category = Category} = R, Acc)
		when is_list(Category) ->
	resource(T, R, Acc#{"category" => Category});
resource([category | T], #{"category" := Category} = M, Acc)
		when is_list(Category) ->
	resource(T, M, Acc#resource{category = Category});
resource([class_type | T], #resource{class_type = Type} = R, Acc)
		when is_list(Type) ->
	resource(T, R, Acc#{"@baseType" => Type});
resource([class_type | T], #{"@baseType" := Type} = M, Acc)
		when is_list(Type) ->
	resource(T, M, Acc#resource{class_type = Type});
resource([base_type | T], #resource{base_type = Type} = R, Acc)
		when is_list(Type) ->
	resource(T, R, Acc#{"@type" => Type});
resource([base_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	resource(T, M, Acc#resource{base_type = Type});
resource([schema | T], #resource{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	resource(T, R, Acc#{"@schemaLocation" => Schema});
resource([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	resource(T, M, Acc#resource{schema = Schema});
resource([version | T], #{"version" := Version} = M, Acc)
		when is_list(Version) ->
	resource(T, M, Acc#resource{version = Version});
resource([start_date | T], #resource{start_date = StartDate} = R, Acc)
		when is_integer(StartDate) ->
	ValidFor = #{"startDateTime" => im_rest:iso8601(StartDate)},
	resource(T, R, Acc#{"validFor" => ValidFor});
resource([start_date | T],
		#{"validFor" := #{"startDateTime" := Start}} = M, Acc)
		when is_list(Start) ->
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
		#{"validFor" := #{"endDateTime" := End}} = M, Acc)
		when is_list(End) ->
	resource(T, M, Acc#resource{end_date = im_rest:iso8601(End)});
resource([last_modified | T], #resource{last_modified = {TS, _}} = R, Acc)
		when is_integer(TS) ->
	resource(T, R, Acc#{"lastUpdate" => im_rest:iso8601(TS)});
resource([last_modified | T], #{"lastUpdate" := DateTime} = M, Acc)
		when is_list(DateTime) ->
	LM = {im_rest:iso8601(DateTime), erlang:unique_integer([positive])},
	resource(T, M, Acc#resource{last_modified = LM});
resource([status | T], #resource{status = Status} = R, Acc)
		when Status /= undefined ->
	resource(T, R, Acc#{"lifecycleStatus" => im_rest:lifecycle_status(Status)});
resource([status | T], #{"lifecycleStatus" := Status} = M, Acc)
		when is_list(Status) ->
	resource(T, M, Acc#resource{status = im_rest:lifecycle_status(Status)});
resource([place | T], #resource{place = PlaceRef} = R, Acc)
		when is_list(PlaceRef) ->
	resource(T, R, Acc#{"place" => place_ref(PlaceRef)});
resource([place | T], #{"place" := PlaceRef} = M, Acc)
		when is_list(PlaceRef) ->
	resource(T, M, Acc#resource{place = place_ref(PlaceRef)});
resource([note | T], #resource{note = Note} = R, Acc)
		when is_list(Note) ->
	resource(T, R, Acc#{"note" => note(Note)});
resource([note | T], #{"note" := Note} = M, Acc)
		when is_list(Note) ->
	resource(T, M, Acc#resource{note = note(Note)});
resource([attachment | T], #resource{attachment = Attachment} = R, Acc)
		when is_list(Attachment) ->
	resource(T, R, Acc#{"resourceAttachment" => attachment(Attachment)});
resource([attachment | T], #{"resourceAttachment" := Attachment} = M, Acc)
		when is_list(Attachment) ->
	resource(T, M, Acc#resource{attachment = attachment(Attachment)});
resource([related | T], #resource{related = ResRel} = R, Acc)
		when is_list(ResRel) ->
	resource(T, R, Acc#{"resourceRelationship" => resource_rel(ResRel)});
resource([related | T], #{"resourceRelationship" := ResRel} = M, Acc)
		when is_list(ResRel) ->
	resource(T, M, Acc#resource{related = resource_rel(ResRel)});
resource([specification | T], #resource{specification = SpecRef} = R, Acc)
		when is_record(SpecRef, specification_ref) ->
	resource(T, R, Acc#{"resourceSpecification" => im_rest:specification_ref(SpecRef)});
resource([specification | T], #{"resourceSpecification" := SpecRef} = M, Acc)
		when is_tuple(SpecRef) ->
	resource(T, M, Acc#resource{specification = im_rest:specification_ref(SpecRef)});
resource([characteristic | T], #resource{characteristic = ResChar} = R, Acc)
		when is_list(ResChar) ->
	resource(T, R, Acc#{"resorceCharacteristic" => resource_char(ResChar)});
resource([characteristic | T], #{"resorceCharacteristic" := ResChar} = M, Acc)
		when is_list(ResChar) ->
	resource(T, M, Acc#resource{characteristic = resource_char(ResChar)});
resource([_ | T], R, Acc) ->
	resource(T, R, Acc);
resource([], _, Acc) ->
	Acc.

-spec place_ref(PlaceRefs) -> PlaceRefs
	when
		PlaceRefs :: [place_ref()] | [map()].
%% @doc CODEC for `PlaceRef'.
place_ref([#place_ref{} | _] = List) ->
	Fields = record_info(fields, place_ref),
	[place_ref(Fields, R, #{}) || R <- List];
place_ref([#{} | _] = List) ->
	Fields = record_info(fields, place_ref),
	[place_ref(Fields, M, #place_ref{}) || M <- List];
place_ref([]) ->
	[].
%% @hidden
place_ref([id | T], #place_ref{id = Id} = R, Acc)
		when is_list(Id) ->
	place_ref(T, R, Acc#{"id" => Id});
place_ref([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	place_ref(T, M, Acc#place_ref{id = Id});
place_ref([href | T], #place_ref{href = Href} = R, Acc)
		when is_list(Href) ->
	place_ref(T, R, Acc#{"href" => Href});
place_ref([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	place_ref(T, M, Acc#place_ref{href = Href});
place_ref([name | T], #place_ref{name = Name} = R, Acc)
		when is_list(Name) ->
	place_ref(T, R, Acc#{"name" => Name});
place_ref([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	place_ref(T, M, Acc#place_ref{name = Name});
place_ref([role | T], #place_ref{role = Role} = R, Acc)
		when is_list(Role) ->
	place_ref(T, R, Acc#{"role" => Role});
place_ref([role | T], #{"role" := Role} = M, Acc)
		when is_list(Role) ->
	place_ref(T, M, Acc#place_ref{role = Role});
place_ref([_ | T], R, Acc) ->
	place_ref(T, R, Acc);
place_ref([], _, Acc) ->
	Acc.

-spec note(Note) -> Note
	when
		Note :: [note()] | [map()].
%% @doc CODEC for `Note'.
note([#note{} | _] = List) ->
	Fields = record_info(fields, note),
	[note(Fields, R, #{}) || R <- List];
note([#{} | _] = List) ->
	Fields = record_info(fields, note),
	[note(Fields, M, #note{}) || M <- List];
note([]) ->
	[].
%% @hidden
note([author | T], #note{author = Author} = R, Acc)
		when is_list(Author) ->
	note(T, R, Acc#{"author" => Author});
note([author | T], #{"author" := Author} = M, Acc)
		when is_list(Author) ->
	note(T, M, Acc#note{author = Author});
note([date | T], #note{date = Date} = R, Acc)
		when is_integer(Date) ->
	note(T, R, Acc#{"date" => im_rest:iso8601(Date)});
note([date | T], #{"date" := Date} = M, Acc)
		when is_list(Date) ->
	note(T, M, Acc#note{date = im_rest:iso8601(Date)});
note([text | T], #note{text = Text} = R, Acc)
		when is_list(Text) ->
	note(T, R, Acc#{"text" => Text});
note([text | T], #{"text" := Text} = M, Acc)
		when is_list(Text) ->
	note(T, M, Acc#note{text = Text});
note([_ | T], R, Acc) ->
	note(T, R, Acc);
note([], _, Acc) ->
	Acc.

-spec attachment(ResourceAttachment) -> ResourceAttachment
	when
		ResourceAttachment :: [attachment()] | [map()].
%% @doc CODEC for `ResourceAttachment'.
attachment([#attachment{} | _] = List) ->
	Fields = record_info(fields, attachment),
	[attachment(Fields, R, #{}) || R <- List];
attachment([#{} | _] = List) ->
	Fields = record_info(fields, attachment),
	[attachment(Fields, M, #attachment{}) || M <- List];
attachment([]) ->
	[].
%% @hidden
attachment([id | T], #attachment{id = Id} = R, Acc)
		when is_list(Id) ->
	attachment(T, R, Acc#{"id" => Id});
attachment([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	attachment(T, M, Acc#attachment{id = Id});
attachment([href | T], #attachment{href = Href} = R, Acc)
		when is_list(Href) ->
	attachment(T, R, Acc#{"href" => Href});
attachment([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	attachment(T, M, Acc#attachment{href = Href});
attachment([description| T],
		#attachment{description = Description} = R, Acc)
		when is_list(Description) ->
	attachment(T, R, Acc#{"description" => Description});
attachment([description| T], #{"description" := Description} = M, Acc)
		when is_list(Description) ->
	attachment(T, M, Acc#attachment{description = Description});
attachment([type | T], #attachment{type = Type} = R, Acc)
		when is_list(Type) ->
	attachment(T, R, Acc#{"type" => Type});
attachment([type | T], #{"type" := Type} = M, Acc)
		when is_list(Type) ->
	attachment(T, M, Acc#attachment{type = Type});
attachment([url | T], #attachment{url = Url} = R, Acc)
		when is_list(Url) ->
	attachment(T, R, Acc#{"url" => Url});
attachment([url | T], #{"url" := Url} = M, Acc)
		when is_list(Url) ->
	attachment(T, M, Acc#attachment{url = Url});
attachment([_ | T], R, Acc) ->
	attachment(T, R, Acc);
attachment([], _, Acc) ->
	Acc.

-spec resource_rel(ResourceRelationship) -> ResourceRelationship
	when
		ResourceRelationship :: [resource_rel()] | [map()].
%% @doc CODEC for `ResourceRelationship'.
resource_rel([#resource_rel{} | _] = List) ->
	Fields = record_info(fields, resource_rel),
	[resource_rel(Fields, R, #{}) || R <- List];
resource_rel([#{} | _] = List) ->
	Fields = record_info(fields, resource_rel),
	[resource_rel(Fields, M, #resource_rel{}) || M <- List];
resource_rel([]) ->
	[].
%% @hidden
resource_rel([id | T], #resource_rel{id = Id} = R, Acc)
		when is_list(Id) ->
	resource_rel(T, R, Acc#{"id" => Id});
resource_rel([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	resource_rel(T, M, Acc#resource_rel{id = Id});
resource_rel([href | T], #resource_rel{href = Href} = R, Acc)
		when is_list(Href) ->
	resource_rel(T, R, Acc#{"href" => Href});
resource_rel([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	resource_rel(T, M, Acc#resource_rel{href = Href});
resource_rel([type | T], #resource_rel{type = Type} = R, Acc)
		when is_list(Type) ->
	resource_rel(T, R, Acc#{"type" => Type});
resource_rel([type | T], #{"type" := Type} = M, Acc)
		when is_list(Type) ->
	resource_rel(T, M, Acc#resource_rel{type = Type});
resource_rel([start_date | T], #resource_rel{start_date = StartDate} = R, Acc)
		when is_integer(StartDate) ->
	ValidFor = #{"startDateTime" => im_rest:iso8601(StartDate)},
	resource_rel(T, R, Acc#{"validFor" => ValidFor});
resource_rel([start_date | T],
		#{"validFor" := #{"startDateTime" := Start}} = M, Acc)
		when is_list(Start) ->
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
		#{"validFor" := #{"endDateTime" := End}} = M, Acc)
		when is_list(End) ->
	resource_rel(T, M, Acc#resource_rel{end_date = im_rest:iso8601(End)});
resource_rel([_ | T], R, Acc) ->
	resource_rel(T, R, Acc);
resource_rel([], _, Acc) ->
	Acc.

-spec resource_char(ResourceCharacteristic) -> ResourceCharacteristic
	when
		ResourceCharacteristic :: [resource_char()] | [map()].
%% @doc CODEC for `ResourceCharacteristic'.
resource_char([#resource_char{} | _] = List) ->
	Fields = record_info(fields, resource_char),
	[resource_char(Fields, R, #{}) || R <- List];
resource_char([#{} | _] = List) ->
	Fields = record_info(fields, resource_char),
	[resource_char(Fields, M, #resource_char{}) || M <- List];
resource_char([]) ->
	[].
%% @hidden
resource_char([name | T], #resource_char{name = Name} = R, Acc)
		when is_list(Name) ->
	resource_char(T, R, Acc#{"name" => Name});
resource_char([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	resource_char(T, M, Acc#resource_char{name = Name});
resource_char([class_type | T], #resource_char{class_type = Type} = R, Acc)
		when is_list(Type) ->
	resource_char(T, R, Acc#{"@type" => Type});
resource_char([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	resource_char(T, M, Acc#resource_char{class_type = Type});
resource_char([schema | T], #resource_char{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	resource_char(T, R, Acc#{"@schemaLocation" => Schema});
resource_char([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	resource_char(T, M, Acc#resource_char{schema = Schema});
resource_char([value | T], #resource_char{value = Value} = R, Acc)
		when is_list(Value) ->
	resource_char(T, R, Acc#{"value" => Value});
resource_char([value | T], #{"value" := Value} = M, Acc)
		when is_list(Value) ->
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

