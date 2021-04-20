%%% im_rest_res_resource.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018-2020 SigScale Global Inc.
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
-module(im_rest_res_resource).
-copyright('Copyright (c) 2018-2020 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0]).
-export([get_resources/3, get_resource/2, post_resource/1, delete_resource/1]).
-export([resource/1]).

-include("im.hrl").

%%----------------------------------------------------------------------
%%  The im public API
%%----------------------------------------------------------------------

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations accepted.
content_types_accepted() ->
	["application/json"].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: list().
%% @doc Returns list of resource representations available.
content_types_provided() ->
	["application/json"].

-spec get_resources(Method, Query, Headers) -> Result
	when
		Method :: string(), % "GET" | "HEAD"
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for
%% 	`GET|HEAD /resourceCatalogManagement/v4/resource'
%% 	requests.
get_resources(Method, Query, Headers) ->
	case lists:keytake("fields", 1, Query) of
		{value, {_, Filters}, NewQuery} ->
			get_resources(Method, NewQuery, Filters, Headers);
		false ->
			get_resources(Method, Query, [], Headers)
	end.
%% @hidden
get_resources(Method, Query, Filters, Headers) ->
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
		{error, not_found} ->
			{error, 404};
		{error, _Reason} ->
			{error, 500}
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
		{ok, ResourceMap} = zj:decode(RequestBody),
		case im:add_resource(resource(ResourceMap)) of
			{ok, #resource{href = Href, last_modified = LM} = Resource} ->
				Headers = [{location, Href}, {etag, im_rest:etag(LM)}],
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
	resource(T, R, Acc#{"publicIdentifier" => PublicId});
resource([public_id | T], #{"publicIdentifier" := PublicId} = M, Acc)
		when is_list(PublicId) ->
	resource(T, M, Acc#resource{public_id = PublicId});
resource([description | T],
		#resource{description = Description} = R, Acc)
		when is_list(Description) ->
	resource(T, R, Acc#{"description" => Description});
resource([description | T], #{"description" := Description} = M, Acc)
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
	resource(T, R, Acc#{"@type" => Type});
resource([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	resource(T, M, Acc#resource{class_type = Type});
resource([base_type | T], #resource{base_type = Type} = R, Acc)
		when is_list(Type) ->
	resource(T, R, Acc#{"@baseType" => Type});
resource([base_type | T], #{"@baseType" := Type} = M, Acc)
		when is_list(Type) ->
	resource(T, M, Acc#resource{base_type = Type});
resource([schema | T], #resource{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	resource(T, R, Acc#{"@schemaLocation" => Schema});
resource([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	resource(T, M, Acc#resource{schema = Schema});
resource([version | T], #resource{version = Version} = R, Acc)
		when is_list(Version) ->
	resource(T, R, Acc#{"version" => Version});
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
		#{"validFor" := ValidFor} = Acc) when is_integer(End) ->
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
resource([state | T], #resource{state = State} = R, Acc)
		when State /= undefined ->
	resource(T, R, Acc#{"lifecycleState" => State});
resource([state | T], #{"lifecycleState" := State} = M, Acc)
		when is_list(State) ->
	resource(T, M, Acc#resource{state = State});
resource([substate | T], #resource{substate = SubState} = R, Acc)
		when SubState /= undefined ->
	resource(T, R, Acc#{"lifecycleSubState" => SubState});
resource([substate | T], #{"lifecycleSubState" := SubState} = M, Acc)
		when is_list(SubState) ->
	resource(T, M, Acc#resource{substate = SubState});
resource([place | T], #resource{place = PlaceRef} = R, Acc)
		when is_list(PlaceRef), length(PlaceRef) > 0 ->
	resource(T, R, Acc#{"place" => place_ref(PlaceRef)});
resource([place | T], #{"place" := PlaceRef} = M, Acc)
		when is_list(PlaceRef) ->
	resource(T, M, Acc#resource{place = place_ref(PlaceRef)});
resource([note | T], #resource{note = Note} = R, Acc)
		when is_list(Note), length(Note) > 0 ->
	resource(T, R, Acc#{"note" => note(Note)});
resource([note | T], #{"note" := Note} = M, Acc)
		when is_list(Note) ->
	resource(T, M, Acc#resource{note = note(Note)});
resource([attachment | T], #resource{attachment = Attachment} = R, Acc)
		when is_list(Attachment), length(Attachment) > 0 ->
	resource(T, R, Acc#{"resourceAttachment" => attachment(Attachment)});
resource([attachment | T], #{"resourceAttachment" := Attachment} = M, Acc)
		when is_list(Attachment) ->
	resource(T, M, Acc#resource{attachment = attachment(Attachment)});
resource([related | T], #resource{related = ResRel} = R, Acc)
		when is_list(ResRel), length(ResRel) > 0 ->
	resource(T, R, Acc#{"resourceRelationship" => resource_rel(ResRel)});
resource([related | T], #{"resourceRelationship" := ResRel} = M, Acc)
		when is_list(ResRel) ->
	resource(T, M, Acc#resource{related = resource_rel(ResRel)});
resource([specification | T], #resource{specification = SpecRef} = R, Acc)
		when is_record(SpecRef, specification_ref) ->
	resource(T, R, Acc#{"resourceSpecification" => im_rest:specification_ref(SpecRef)});
resource([specification | T], #{"resourceSpecification" := SpecRef} = M, Acc)
		when is_map(SpecRef) ->
	resource(T, M, Acc#resource{specification = im_rest:specification_ref(SpecRef)});
resource([party | T], #resource{party = PartyRefs} = R, Acc)
		when is_list(PartyRefs), length(PartyRefs) > 0 ->
	resource(T, R, Acc#{"relatedParty" => im_rest:party_ref(PartyRefs)});
resource([party | T], #{"relatedParty" := PartyRefs} = M, Acc)
		when is_list(PartyRefs), length(PartyRefs) > 0 ->
	resource(T, M, Acc#resource{party = im_rest:party_ref(PartyRefs)});
resource([feature | T], #resource{feature = Feature} = R, Acc)
		when is_list(Feature), length(Feature) > 0 ->
	resource(T, R, Acc#{"activationFeature" => feature(Feature)});
resource([feature | T], #{"activationFeature" := Feature} = M, Acc)
		when is_list(Feature) ->
	resource(T, M, Acc#resource{feature = feature(Feature)});
resource([characteristic | T], #resource{characteristic = ResChar} = R, Acc)
		when is_list(ResChar), length(ResChar) > 0 ->
	resource(T, R, Acc#{"resourceCharacteristic" => characteristic(ResChar)});
resource([characteristic | T], #{"resourceCharacteristic" := ResChar} = M, Acc)
		when is_list(ResChar) ->
	resource(T, M, Acc#resource{characteristic = characteristic(ResChar)});
resource([connectivity | T], #resource{connectivity = Graphs} = R, Acc)
		when is_list(Graphs) ->
	resource(T, R, Acc#{"connectivity" => resource_graph(Graphs)});
resource([connectivity | T], #{"connectivity" := Graphs} = M, Acc)
		when is_list(Graphs) ->
	resource(T, M, Acc#resource{connectivity = resource_graph(Graphs)});
resource([connection_point | T], #resource{connection_point = Conns} = R, Acc)
		when is_list(Conns), length(Conns) > 0 ->
	resource(T, R, Acc#{"connectionPoint" => resource_ref(Conns)});
resource([connection_point | T], #{"connectionPoint" := Conns} = M, Acc)
		when is_list(Conns) ->
	resource(T, M, Acc#resource{connection_point = resource_ref(Conns)});
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
place_ref([class_type | T], #place_ref{class_type = Type} = R, Acc)
		when is_list(Type) ->
	place_ref(T, R, Acc#{"@type" => Type});
place_ref([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	place_ref(T, M, Acc#place_ref{class_type = Type});
place_ref([base_type | T], #place_ref{base_type = Type} = R, Acc)
		when is_list(Type) ->
	place_ref(T, R, Acc#{"@baseType" => Type});
place_ref([base_type | T], #{"@baseType" := Type} = M, Acc)
		when is_list(Type) ->
	place_ref(T, M, Acc#place_ref{base_type = Type});
place_ref([schema | T], #place_ref{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	place_ref(T, R, Acc#{"@schemaLocation" => Schema});
place_ref([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	place_ref(T, M, Acc#place_ref{schema = Schema});
place_ref([role | T], #place_ref{role = Role} = R, Acc)
		when is_list(Role) ->
	place_ref(T, R, Acc#{"role" => Role});
place_ref([role | T], #{"role" := Role} = M, Acc)
		when is_list(Role) ->
	place_ref(T, M, Acc#place_ref{role = Role});
place_ref([ref_type | T], #place_ref{ref_type = RefType} = R, Acc)
		when is_list(RefType) ->
	place_ref(T, R, Acc#{"@referredType" => RefType});
place_ref([ref_type | T], #{"@referredType" := RefType} = M, Acc)
		when is_list(RefType) ->
	place_ref(T, M, Acc#place_ref{ref_type = RefType});
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
attachment([description | T],
		#attachment{description = Description} = R, Acc)
		when is_list(Description) ->
	attachment(T, R, Acc#{"description" => Description});
attachment([description | T], #{"description" := Description} = M, Acc)
		when is_list(Description) ->
	attachment(T, M, Acc#attachment{description = Description});
attachment([type | T], #attachment{type = Type} = R, Acc)
		when is_list(Type) ->
	attachment(T, R, Acc#{"attachmentType" => Type});
attachment([type | T], #{"attachmentType" := Type} = M, Acc)
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
%%
%% Internally we condense `ResourceRefOrValue' with one record.
%%
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
	resource_rel(T, R, Acc#{"resource" => #{"id" => Id}});
resource_rel([id | T], #{"resource" := #{"id" := Id}} = M, Acc)
		when is_list(Id) ->
	resource_rel(T, M, Acc#resource_rel{id = Id});
resource_rel([href | T], #resource_rel{href = Href} = R,
		#{"resource" := Res} = Acc) when is_list(Href) ->
	resource_rel(T, R, Acc#{"resource" => Res#{"href" => Href}});
resource_rel([href | T], #{"resource" := #{"href" := Href}} = M, Acc)
		when is_list(Href) ->
	resource_rel(T, M, Acc#resource_rel{href = Href});
resource_rel([name | T], #resource_rel{name = Name} = R,
		#{"resource" := Res} = Acc) when is_list(Name) ->
	resource_rel(T, R, Acc#{"resource" => Res#{"name" => Name}});
resource_rel([name | T], #{"resource" := #{"name" := Name}} = M, Acc)
		when is_list(Name) ->
	resource_rel(T, M, Acc#resource_rel{name = Name});
resource_rel([class_type | T], #resource_rel{class_type = Type} = R,
		#{"resource" := Res} = Acc) when is_list(Type) ->
	resource_rel(T, R, Acc#{"resource" => Res#{"@type" => Type}});
resource_rel([class_type | T], #{"resource" := #{"@type" := Type}} = M, Acc)
		when is_list(Type) ->
	resource_rel(T, M, Acc#resource_rel{class_type = Type});
resource_rel([base_type | T], #resource_rel{base_type = Type} = R,
		#{"resource" := Res} = Acc) when is_list(Type) ->
	resource_rel(T, R, Acc#{"resource" => Res#{"@baseType" => Type}});
resource_rel([base_type | T], #{"resource" := #{"@baseType" := Type}} = M, Acc)
		when is_list(Type) ->
	resource_rel(T, M, Acc#resource_rel{base_type = Type});
resource_rel([schema | T], #resource_rel{schema = Schema} = R,
		#{"resource" := Res} = Acc) when is_list(Schema) ->
	resource_rel(T, R, Acc#{"resource" => Res#{"@schemaLocation" => Schema}});
resource_rel([schema | T], #{"resource" := #{"@schemaLocation" := Schema}} = M,
		Acc) when is_list(Schema) ->
	resource_rel(T, M, Acc#resource_rel{schema = Schema});
resource_rel([version | T], #resource_rel{version = Version} = R,
		#{"resource" := Res} = Acc) when is_list(Version) ->
	resource_rel(T, R, Acc#{"resource" => Res#{"version" => Version}});
resource_rel([version | T], #{"resource" := #{"version" := Version}} = M, Acc)
		when is_list(Version) ->
	resource_rel(T, M, Acc#resource_rel{version = Version});
resource_rel([ref_type | T], #resource_rel{ref_type = RefType} = R,
		#{"resource" := Res} = Acc) when is_list(RefType) ->
	resource_rel(T, R, Acc#{"resource" => Res#{"@referredType" => RefType}});
resource_rel([ref_type | T], #{"resource" := #{"@referredType" := RefType}} = M,
		Acc) when is_list(RefType) ->
	resource_rel(T, M, Acc#resource_rel{ref_type = RefType});
resource_rel([rel_type | T], #resource_rel{rel_type = RelType} = R, Acc)
		when is_list(RelType) ->
	resource_rel(T, R, Acc#{"relationshipType" => RelType});
resource_rel([rel_type | T], #{"relationshipType" := RelType} = M, Acc)
		when is_list(RelType) ->
	resource_rel(T, M, Acc#resource_rel{rel_type = RelType});
resource_rel([_ | T], R, Acc) ->
	resource_rel(T, R, Acc);
resource_rel([], _, Acc) ->
	Acc.

-spec resource_ref(ResourceRef) -> ResourceRef
	when
		ResourceRef :: [resource_ref()] | [map()].
%% @doc CODEC for `ResourceRef'.
resource_ref([#resource_ref{} | _] = List) ->
	Fields = record_info(fields, resource_ref),
	[resource_ref(Fields, R, #{}) || R <- List];
resource_ref([#{} | _] = List) ->
	Fields = record_info(fields, resource_ref),
	[resource_ref(Fields, M, #resource_ref{}) || M <- List];
resource_ref([]) ->
	[].
%% @hidden
resource_ref([id | T], #resource_ref{id = Id} = R, Acc)
		when is_list(Id) ->
	resource_ref(T, R, Acc#{"id" => Id});
resource_ref([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	resource_ref(T, M, Acc#resource_ref{id = Id});
resource_ref([href | T], #resource_ref{href = Href} = R, Acc)
		when is_list(Href) ->
	resource_ref(T, R, Acc#{"href" => Href});
resource_ref([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	resource_ref(T, M, Acc#resource_ref{href = Href});
resource_ref([name | T], #resource_ref{name = Name} = R, Acc)
		when is_list(Name) ->
	resource_ref(T, R, Acc#{"name" => Name});
resource_ref([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	resource_ref(T, M, Acc#resource_ref{name = Name});
resource_ref([class_type | T], #resource_ref{class_type = Type} = R, Acc)
		when is_list(Type) ->
	resource_ref(T, R, Acc#{"@type" => Type});
resource_ref([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	resource_ref(T, M, Acc#resource_ref{class_type = Type});
resource_ref([base_type | T], #resource_ref{base_type = Type} = R, Acc)
		when is_list(Type) ->
	resource_ref(T, R, Acc#{"@baseType" => Type});
resource_ref([base_type | T], #{"@baseType" := Type} = M, Acc)
		when is_list(Type) ->
	resource_ref(T, M, Acc#resource_ref{base_type = Type});
resource_ref([schema | T], #resource_ref{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	resource_ref(T, R, Acc#{"@schemaLocation" => Schema});
resource_ref([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	resource_ref(T, M, Acc#resource_ref{schema = Schema});
resource_ref([ref_type | T],
		#resource_ref{ref_type = RefType} = R, Acc) when is_list(RefType) ->
	resource_ref(T, R, Acc#{"@referredType" => RefType});
resource_ref([ref_type | T], #{"@referredType" := RefType} = M, Acc)
		when is_list(RefType) ->
	resource_ref(T, M, Acc#resource_ref{ref_type = RefType});
resource_ref([_ | T], R, Acc) ->
	resource_ref(T, R, Acc);
resource_ref([], _, Acc) ->
	Acc.

-spec resource_graph(ResourceGraph) -> ResourceGraph
	when
		ResourceGraph :: [resource_graph()] | [map()].
%% @doc CODEC for `ResourceGraph'.
resource_graph([#resource_graph{} | _] = List) ->
	Fields = record_info(fields, resource_graph),
	[resource_graph(Fields, R, #{}) || R <- List];
resource_graph([#{} | _] = List) ->
	Fields = record_info(fields, resource_graph),
	[resource_graph(Fields, M, #resource_graph{}) || M <- List];
resource_graph([]) ->
	[].
%% @hidden
resource_graph([id | T], #resource_graph{id = Id} = R, Acc)
		when is_list(Id) ->
	resource_graph(T, R, Acc#{"id" => Id});
resource_graph([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	resource_graph(T, M, Acc#resource_graph{id = Id});
resource_graph([name | T], #resource_graph{name = Name} = R, Acc)
		when is_list(Name) ->
	resource_graph(T, R, Acc#{"name" => Name});
resource_graph([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	resource_graph(T, M, Acc#resource_graph{name = Name});
resource_graph([description | T], #resource_graph{description = Desc} = R, Acc)
		when is_list(Desc) ->
	resource_graph(T, R, Acc#{"description" => Desc});
resource_graph([description | T], #{"description" := Desc} = M, Acc)
		when is_list(Desc) ->
	resource_graph(T, M, Acc#resource_graph{description = Desc});
resource_graph([class_type | T], #resource_graph{class_type = Type} = R, Acc)
		when is_list(Type) ->
	resource_graph(T, R, Acc#{"@type" => Type});
resource_graph([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	resource_graph(T, M, Acc#resource_graph{class_type = Type});
resource_graph([base_type | T], #resource_graph{base_type = Type} = R, Acc)
		when is_list(Type) ->
	resource_graph(T, R, Acc#{"@baseType" => Type});
resource_graph([base_type | T], #{"@baseType" := Type} = M, Acc)
		when is_list(Type) ->
	resource_graph(T, M, Acc#resource_graph{base_type = Type});
resource_graph([schema | T], #resource_graph{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	resource_graph(T, R, Acc#{"@schemaLocation" => Schema});
resource_graph([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	resource_graph(T, M, Acc#resource_graph{schema = Schema});
resource_graph([connection | T], #resource_graph{connection = Connection} = R, Acc)
		when is_list(Connection) ->
	resource_graph(T, R, Acc#{"connection" => connection(Connection)});
resource_graph([connection | T], #{"connection" := Connection} = M, Acc)
		when is_list(Connection) ->
	resource_graph(T, M, Acc#resource_graph{connection = connection(Connection)});
resource_graph([related | T], #resource_graph{related = Graphs} = R, Acc)
		when is_list(Graphs) ->
	resource_graph(T, R, Acc#{"related" => resource_graph_rel(Graphs)});
resource_graph([related | T], #{"related" := Graphs} = M, Acc)
		when is_list(Graphs) ->
	resource_graph(T, M, Acc#resource_graph{related = resource_graph_rel(Graphs)});
resource_graph([_ | T], R, Acc) ->
	resource_graph(T, R, Acc);
resource_graph([], _, Acc) ->
	Acc.

-spec resource_graph_rel(ResourceGraphRelationship) -> ResourceGraphRelationship
	when
		ResourceGraphRelationship :: [resource_graph_rel()] | [map()].
%% @doc CODEC for `ResourceGraphRelationship'.
resource_graph_rel([#resource_graph_rel{} | _] = List) ->
	Fields = record_info(fields, resource_graph_rel),
	[resource_graph_rel(Fields, R, #{}) || R <- List];
resource_graph_rel([#{} | _] = List) ->
	Fields = record_info(fields, resource_graph_rel),
	[resource_graph_rel(Fields, M, #resource_graph_rel{}) || M <- List];
resource_graph_rel([]) ->
	[].
%% @hidden
resource_graph_rel([class_type | T], #resource_graph_rel{class_type = Type} = R, Acc)
		when is_list(Type) ->
	resource_graph_rel(T, R, Acc#{"@type" => Type});
resource_graph_rel([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	resource_graph_rel(T, M, Acc#resource_graph_rel{class_type = Type});
resource_graph_rel([base_type | T], #resource_graph_rel{base_type = Type} = R, Acc)
		when is_list(Type) ->
	resource_graph_rel(T, R, Acc#{"@type" => Type});
resource_graph_rel([base_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	resource_graph_rel(T, M, Acc#resource_graph_rel{base_type = Type});
resource_graph_rel([schema | T], #resource_graph_rel{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	resource_graph_rel(T, R, Acc#{"@schemaLocation" => Schema});
resource_graph_rel([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	resource_graph_rel(T, M, Acc#resource_graph_rel{schema = Schema});
resource_graph_rel([rel_type | T], #resource_graph_rel{rel_type = RelType} = R, Acc)
		when is_list(RelType) ->
	resource_graph_rel(T, R, Acc#{"relationshipType" => RelType});
resource_graph_rel([rel_type | T], #{"relationshipType" := RelType} = M, Acc)
		when is_list(RelType) ->
	resource_graph_rel(T, M, Acc#resource_graph_rel{rel_type = RelType});
resource_graph_rel([graph | T], #resource_graph_rel{graph = Graph} = R, Acc)
		when is_record(Graph, resource_graph_ref) ->
	resource_graph_rel(T, R, Acc#{"resourceGraph" => resource_graph_ref(Graph)});
resource_graph_rel([graph | T], #{"resourceGraph" := Graph} = M, Acc)
		when is_map(Graph) ->
	resource_graph_rel(T, M, Acc#resource_graph_rel{graph = resource_graph_ref(Graph)});
resource_graph_rel([_ | T], R, Acc) ->
	resource_graph_rel(T, R, Acc);
resource_graph_rel([], _, Acc) ->
	Acc.

-spec resource_graph_ref(ResourceGraphRef) -> ResourceGraphRef
	when
		ResourceGraphRef :: resource_graph_ref() | map().
%% @doc CODEC for `ResourceGraphRef'.
resource_graph_ref(#resource_graph_ref{} = R) ->
	Fields = record_info(fields, resource_graph_ref),
	resource_graph_ref(Fields, R, #{});
resource_graph_ref(#{} = M) ->
	Fields = record_info(fields, resource_graph_ref),
	resource_graph_ref(Fields, M, #resource_graph_ref{}).
%% @hidden
resource_graph_ref([id | T], #resource_graph_ref{id = Id} = R, Acc)
		when is_list(Id) ->
	resource_graph_ref(T, R, Acc#{"id" => Id});
resource_graph_ref([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	resource_graph_ref(T, M, Acc#resource_graph_ref{id = Id});
resource_graph_ref([name | T], #resource_graph_ref{name = Name} = R, Acc)
		when is_list(Name) ->
	resource_graph_ref(T, R, Acc#{"name" => Name});
resource_graph_ref([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	resource_graph_ref(T, M, Acc#resource_graph_ref{name = Name});
resource_graph_ref([class_type | T], #resource_graph_ref{class_type = Type} = R, Acc)
		when is_list(Type) ->
	resource_graph_ref(T, R, Acc#{"@type" => Type});
resource_graph_ref([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	resource_graph_ref(T, M, Acc#resource_graph_ref{class_type = Type});
resource_graph_ref([base_type | T], #resource_graph_ref{base_type = Type} = R, Acc)
		when is_list(Type) ->
	resource_graph_ref(T, R, Acc#{"@type" => Type});
resource_graph_ref([base_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	resource_graph_ref(T, M, Acc#resource_graph_ref{base_type = Type});
resource_graph_ref([schema | T], #resource_graph_ref{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	resource_graph_ref(T, R, Acc#{"@schemaLocation" => Schema});
resource_graph_ref([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	resource_graph_ref(T, M, Acc#resource_graph_ref{schema = Schema});
resource_graph_ref([ref_type | T], #resource_graph_ref{ref_type = RefType} = R, Acc)
		when is_list(RefType) ->
	resource_graph_ref(T, R, Acc#{"referredType" => RefType});
resource_graph_ref([ref_type | T], #{"referredType" := RefType} = M, Acc)
		when is_list(RefType) ->
	resource_graph_ref(T, M, Acc#resource_graph_ref{ref_type = RefType});
resource_graph_ref([_ | T], R, Acc) ->
	resource_graph_ref(T, R, Acc);
resource_graph_ref([], _, Acc) ->
	Acc.

-spec connection(Connection) -> Connection
	when
		Connection :: [connection()] | [map()].
%% @doc CODEC for `Connection'.
connection([#connection{} | _] = List) ->
	Fields = record_info(fields, connection),
	[connection(Fields, R, #{}) || R <- List];
connection([#{} | _] = List) ->
	Fields = record_info(fields, connection),
	[connection(Fields, M, #connection{}) || M <- List];
connection([]) ->
	[].
%% @hidden
connection([id | T], #connection{id = Id} = R, Acc)
		when is_list(Id) ->
	connection(T, R, Acc#{"id" => Id});
connection([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	connection(T, M, Acc#connection{id = Id});
connection([name | T], #connection{name = Name} = R, Acc)
		when is_list(Name) ->
	connection(T, R, Acc#{"name" => Name});
connection([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	connection(T, M, Acc#connection{name = Name});
connection([class_type | T], #connection{class_type = Type} = R, Acc)
		when is_list(Type) ->
	connection(T, R, Acc#{"@type" => Type});
connection([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	connection(T, M, Acc#connection{class_type = Type});
connection([base_type | T], #connection{base_type = Type} = R, Acc)
		when is_list(Type) ->
	connection(T, R, Acc#{"@type" => Type});
connection([base_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	connection(T, M, Acc#connection{base_type = Type});
connection([schema | T], #connection{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	connection(T, R, Acc#{"@schemaLocation" => Schema});
connection([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	connection(T, M, Acc#connection{schema = Schema});
connection([ass_type | T], #connection{ass_type = Type} = R, Acc)
		when is_list(Type) ->
	connection(T, R, Acc#{"associationType" => Type});
connection([ass_type | T], #{"associationType" := Type} = M, Acc)
		when is_list(Type) ->
	connection(T, M, Acc#connection{ass_type = Type});
connection([endpoint | T], #connection{endpoint = Endpoints} = R, Acc)
		when is_list(Endpoints) ->
	connection(T, R, Acc#{"endpoint" => endpoint_ref(Endpoints)});
connection([endpoint | T], #{"endpoint" := Endpoints} = M, Acc)
		when is_list(Endpoints) ->
	connection(T, M, Acc#connection{endpoint = endpoint_ref(Endpoints)});
connection([_ | T], R, Acc) ->
	connection(T, R, Acc);
connection([], _, Acc) ->
	Acc.

-spec endpoint_ref(EndpointRef) -> EndpointRef
	when
		EndpointRef :: [endpoint_ref()] | [map()].
%% @doc CODEC for `EndpointRef'.
endpoint_ref([#endpoint_ref{} | _] = List) ->
	Fields = record_info(fields, endpoint_ref),
	[endpoint_ref(Fields, R, #{}) || R <- List];
endpoint_ref([#{} | _] = List) ->
	Fields = record_info(fields, endpoint_ref),
	[endpoint_ref(Fields, M, #endpoint_ref{}) || M <- List];
endpoint_ref([]) ->
	[].
%% @hidden
endpoint_ref([id | T], #endpoint_ref{id = Id} = R, Acc)
		when is_list(Id) ->
	endpoint_ref(T, R, Acc#{"id" => Id});
endpoint_ref([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	endpoint_ref(T, M, Acc#endpoint_ref{id = Id});
endpoint_ref([href| T], #endpoint_ref{href = Href} = R, Acc)
		when is_list(Href) ->
	endpoint_ref(T, R, Acc#{"href" => Href});
endpoint_ref([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	endpoint_ref(T, M, Acc#endpoint_ref{href = Href});
endpoint_ref([name | T], #endpoint_ref{name = Name} = R, Acc)
		when is_list(Name) ->
	endpoint_ref(T, R, Acc#{"name" => Name});
endpoint_ref([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	endpoint_ref(T, M, Acc#endpoint_ref{name = Name});
endpoint_ref([class_type | T], #endpoint_ref{class_type = Type} = R, Acc)
		when is_list(Type) ->
	endpoint_ref(T, R, Acc#{"@type" => Type});
endpoint_ref([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	endpoint_ref(T, M, Acc#endpoint_ref{class_type = Type});
endpoint_ref([base_type | T], #endpoint_ref{base_type = Type} = R, Acc)
		when is_list(Type) ->
	endpoint_ref(T, R, Acc#{"@baseType" => Type});
endpoint_ref([base_type | T], #{"@baseType" := Type} = M, Acc)
		when is_list(Type) ->
	endpoint_ref(T, M, Acc#endpoint_ref{base_type = Type});
endpoint_ref([schema | T], #endpoint_ref{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	endpoint_ref(T, R, Acc#{"@schemaLocation" => Schema});
endpoint_ref([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	endpoint_ref(T, M, Acc#endpoint_ref{schema = Schema});
endpoint_ref([ref_type | T], #endpoint_ref{ref_type = RefType} = R, Acc)
		when is_list(RefType) ->
	endpoint_ref(T, R, Acc#{"@referredType" => RefType});
endpoint_ref([ref_type | T], #{"@referredType" := RefType} = M, Acc)
		when is_list(RefType) ->
	endpoint_ref(T, M, Acc#endpoint_ref{ref_type = RefType});
endpoint_ref([is_root | T], #endpoint_ref{is_root = IsRoot} = R, Acc)
		when is_boolean(IsRoot) ->
	endpoint_ref(T, R, Acc#{"isRoot" => IsRoot});
endpoint_ref([is_root | T], #{"isRoot" := IsRoot} = M, Acc)
		when is_boolean(IsRoot) ->
	endpoint_ref(T, M, Acc#endpoint_ref{is_root = IsRoot});
endpoint_ref([connection_point | T], #endpoint_ref{connection_point = CP} = R, Acc)
		when is_list(CP), length(CP) > 0 ->
	endpoint_ref(T, R, Acc#{"connectionPoint" => resource_rel(CP)});
endpoint_ref([connection_point | T], #{"connectionPoint" := CP} = M, Acc)
		when is_list(CP), length(CP) > 0 ->
	endpoint_ref(T, M, Acc#endpoint_ref{connection_point = resource_rel(CP)});
endpoint_ref([_ | T], R, Acc) ->
	endpoint_ref(T, R, Acc);
endpoint_ref([], _, Acc) ->
	Acc.

-spec characteristic(Characteristic) -> Characteristic
	when
		Characteristic :: [resource_char()] | [map()].
%% @doc CODEC for `Characteristic'.
characteristic([#resource_char{} | _] = List) ->
	Fields = record_info(fields, resource_char),
	[characteristic(Fields, R, #{}) || R <- List];
characteristic([#{} | _] = List) ->
	Fields = record_info(fields, resource_char),
	[characteristic(Fields, M, #resource_char{}) || M <- List];
characteristic([]) ->
	[].
%% @hidden
characteristic([id | T], #resource_char{id = Id} = R, Acc)
		when is_list(Id) ->
	characteristic(T, R, Acc#{"id" => Id});
characteristic([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	characteristic(T, M, Acc#resource_char{id = Id});
characteristic([name | T], #resource_char{name = Name} = R, Acc)
		when is_list(Name) ->
	characteristic(T, R, Acc#{"name" => Name});
characteristic([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	characteristic(T, M, Acc#resource_char{name = Name});
characteristic([class_type | T], #resource_char{class_type = Type} = R, Acc)
		when is_list(Type) ->
	characteristic(T, R, Acc#{"@type" => Type});
characteristic([class_type | T], #{"@type" := Type} = M, Acc)
		when is_list(Type) ->
	characteristic(T, M, Acc#resource_char{class_type = Type});
characteristic([base_type | T], #resource_char{base_type = Type} = R, Acc)
		when is_list(Type) ->
	characteristic(T, R, Acc#{"@baseType" => Type});
characteristic([base_type | T], #{"@baseType" := Type} = M, Acc)
		when is_list(Type) ->
	characteristic(T, M, Acc#resource_char{base_type = Type});
characteristic([schema | T], #resource_char{schema = Schema} = R, Acc)
		when is_list(Schema) ->
	characteristic(T, R, Acc#{"@schemaLocation" => Schema});
characteristic([schema | T], #{"@schemaLocation" := Schema} = M, Acc)
		when is_list(Schema) ->
	characteristic(T, M, Acc#resource_char{schema = Schema});
characteristic([value_type | T], #resource_char{value_type = Type} = R, Acc)
		when is_list(Type) ->
	characteristic(T, R, Acc#{"valueType" => Type});
characteristic([value_type | T], #{"valueType" := Type} = M, Acc)
		when is_list(Type) ->
	characteristic(T, M, Acc#resource_char{value_type = Type});
characteristic([value | T], #resource_char{value = Value} = R, Acc) ->
	characteristic(T, R, Acc#{"value" => Value});
characteristic([value | T], #{"value" := Value} = M, Acc) ->
	characteristic(T, M, Acc#resource_char{value = Value});
characteristic([related | T], #resource_char{related = CharRels} = R, Acc)
		when is_list(CharRels) ->
	characteristic(T, R, Acc#{"characteristicRelationship" => res_char_rel(CharRels)});
characteristic([related | T], #{"characteristicRelationship" := CharRels} = M, Acc)
		when is_list(CharRels) ->
	characteristic(T, M, Acc#resource_char{related = res_char_rel(CharRels)});
characteristic([_ | T], R, Acc) ->
	characteristic(T, R, Acc);
characteristic([], _, Acc) ->
	Acc.

-spec constraint_ref(ConstraintRef) -> ConstraintRef
	when
		ConstraintRef :: [constraint_ref()] | [map()].
%% @doc CODEC for `ConstraintRef'.
constraint_ref([#constraint_ref{} | _] = List) ->
	Fields = record_info(fields, constraint_ref),
	[constraint_ref(Fields, R, #{}) || R <- List];
constraint_ref([#{} | _] = List) ->
	Fields = record_info(fields, constraint_ref),
	[constraint_ref(Fields, M, #constraint_ref{}) || M <- List];
constraint_ref([]) ->
	[].
%% @hidden
constraint_ref([id | T], #constraint_ref{id = Id} = R, Acc)
		when is_list(Id) ->
	constraint_ref(T, R, Acc#{"id" => Id});
constraint_ref([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	constraint_ref(T, M, Acc#constraint_ref{id = Id});
constraint_ref([href | T], #constraint_ref{href = Href} = R, Acc)
		when is_list(Href) ->
	constraint_ref(T, R, Acc#{"href" => Href});
constraint_ref([href | T], #{"href" := Href} = M, Acc)
		when is_list(Href) ->
	constraint_ref(T, M, Acc#constraint_ref{href = Href});
constraint_ref([name | T], #constraint_ref{name = Name} = R, Acc)
		when is_list(Name) ->
	constraint_ref(T, R, Acc#{"name" => Name});
constraint_ref([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	constraint_ref(T, M, Acc#constraint_ref{name = Name});
constraint_ref([version | T], #constraint_ref{version = Version} = R, Acc)
		when is_list(Version) ->
	constraint_ref(T, R, Acc#{"version" => Version});
constraint_ref([version | T], #{"version" := Version} = M, Acc)
		when is_list(Version) ->
	constraint_ref(T, M, Acc#constraint_ref{version = Version});
constraint_ref([ref_type | T], #constraint_ref{ref_type = RefType} = R, Acc)
		when is_list(RefType) ->
	constraint_ref(T, R, Acc#{"@referredType" => RefType});
constraint_ref([ref_type | T], #{"@referredType" := RefType} = M, Acc)
		when is_list(RefType) ->
	constraint_ref(T, M, Acc#constraint_ref{ref_type = RefType});
constraint_ref([_ | T], R, Acc) ->
	constraint_ref(T, R, Acc);
constraint_ref([], _, Acc) ->
	Acc.

-spec res_char_rel(CharacteristicRelationship) -> CharacteristicRelationship
	when
		CharacteristicRelationship :: [res_char_rel()] | [map()].
%% @doc CODEC for `CharacteristicRelationship'.
%%
res_char_rel([#res_char_rel{} | _] = List) ->
	Fields = record_info(fields, res_char_rel),
	[res_char_rel(Fields, R, #{}) || R <- List];
res_char_rel([#{} | _] = List) ->
	Fields = record_info(fields, res_char_rel),
	[res_char_rel(Fields, M, #res_char_rel{}) || M <- List];
res_char_rel([]) ->
	[].
%% @hidden
res_char_rel([id | T], #res_char_rel{id = Id} = R, Acc)
		when is_list(Id) ->
	res_char_rel(T, R, Acc#{"id" => Id});
res_char_rel([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	res_char_rel(T, M, Acc#res_char_rel{id = Id});
res_char_rel([rel_type | T], #res_char_rel{rel_type = RelType} = R, Acc)
		when is_list(RelType) ->
	res_char_rel(T, R, Acc#{"relationshipType" => RelType});
res_char_rel([rel_type | T], #{"relationshipType" := RelType} = M, Acc)
		when is_list(RelType) ->
	res_char_rel(T, M, Acc#res_char_rel{rel_type = RelType});
res_char_rel([_ | T], R, Acc) ->
	res_char_rel(T, R, Acc);
res_char_rel([], _, Acc) ->
	Acc.

-spec feature(Feature) -> Feature
	when
		Feature :: [feature()] | [map()].
%% @doc CODEC for `Feature'.
feature([#feature{} | _] = List) ->
	Fields = record_info(fields, feature),
	[feature(Fields, R, #{}) || R <- List];
feature([#{} | _] = List) ->
	Fields = record_info(fields, feature),
	[feature(Fields, M, #feature{}) || M <- List];
feature([]) ->
	[].
%% @hidden
feature([id | T], #feature{id = Id} = R, Acc)
		when is_list(Id) ->
	feature(T, R, Acc#{"id" => Id});
feature([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	feature(T, M, Acc#feature{id = Id});
feature([name | T], #feature{name = Name} = R, Acc)
		when is_list(Name) ->
	feature(T, R, Acc#{"name" => Name});
feature([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	feature(T, M, Acc#feature{name = Name});
feature([bundle | T], #feature{bundle = Bundle} = R, Acc)
		when is_boolean(Bundle) ->
	feature(T, R, Acc#{"bundle" => Bundle});
feature([bundle | T], #{"bundle" := Bundle} = M, Acc)
		when is_boolean(Bundle) ->
	feature(T, M, Acc#feature{bundle = Bundle});
feature([enabled | T], #feature{enabled = Enabled} = R, Acc)
		when is_boolean(Enabled) ->
	feature(T, R, Acc#{"enabled" => Enabled});
feature([enabled | T], #{"enabled" := Enabled} = M, Acc)
		when is_boolean(Enabled) ->
	feature(T, M, Acc#feature{enabled = Enabled});
feature([constraint | T], #feature{constraint = Consts} = R, Acc)
		when is_list(Consts) ->
	feature(T, R, Acc#{"constraint" => constraint_ref(Consts)});
feature([constraint | T], #{"constraint" := Consts} = M, Acc)
		when is_list(Consts) ->
	feature(T, M, Acc#feature{constraint = constraint_ref(Consts)});
feature([characteristic | T], #feature{characteristic = Chars} = R, Acc)
		when is_list(Chars) ->
	feature(T, R, Acc#{"featureCharacteristic" => characteristic(Chars)});
feature([characteristic | T], #{"featureCharacteristic" := Chars} = M, Acc)
		when is_list(Chars) ->
	feature(T, M, Acc#feature{characteristic = characteristic(Chars)});
feature([related | T], #feature{related = CharRels} = R, Acc)
		when is_list(CharRels) ->
	feature(T, R, Acc#{"featureRelationship" => feature_rel(CharRels)});
feature([related | T], #{"featureRelationship" := CharRels} = M, Acc)
		when is_list(CharRels) ->
	feature(T, M, Acc#feature{related = feature_rel(CharRels)});
feature([_ | T], R, Acc) ->
	feature(T, R, Acc);
feature([], _, Acc) ->
	Acc.

-spec feature_rel(FeatureRelationship) -> FeatureRelationship
	when
		FeatureRelationship :: [feature_rel()] | [map()].
%% @doc CODEC for `FeatureRelationship'.
feature_rel([#feature_rel{} | _] = List) ->
	Fields = record_info(fields, feature_rel),
	[feature_rel(Fields, R, #{}) || R <- List];
feature_rel([#{} | _] = List) ->
	Fields = record_info(fields, feature_rel),
	[feature_rel(Fields, M, #feature_rel{}) || M <- List];
feature_rel([]) ->
	[].
%% @hidden
feature_rel([id | T], #feature_rel{id = Id} = R, Acc)
		when is_list(Id) ->
	feature_rel(T, R, Acc#{"id" => Id});
feature_rel([id | T], #{"id" := Id} = M, Acc)
		when is_list(Id) ->
	feature_rel(T, M, Acc#feature_rel{id = Id});
feature_rel([name | T], #feature_rel{name = Name} = R, Acc)
		when is_list(Name) ->
	feature_rel(T, R, Acc#{"name" => Name});
feature_rel([name | T], #{"name" := Name} = M, Acc)
		when is_list(Name) ->
	feature_rel(T, M, Acc#feature_rel{name = Name});
feature_rel([start_date | T], #feature_rel{start_date = StartDate} = R, Acc)
		when is_integer(StartDate) ->
	ValidFor = #{"startDateTime" => im_rest:iso8601(StartDate)},
	feature_rel(T, R, Acc#{"validFor" => ValidFor});
feature_rel([start_date | T],
		#{"validFor" := #{"startDateTime" := Start}} = M, Acc)
		when is_list(Start) ->
	feature_rel(T, M, Acc#feature_rel{start_date = im_rest:iso8601(Start)});
feature_rel([end_date | T], #feature_rel{end_date = End} = R,
		#{"validFor" := ValidFor} = Acc) when is_integer(End) ->
	NewValidFor = ValidFor#{"endDateTime" => im_rest:iso8601(End)},
	feature_rel(T, R, Acc#{"validFor" := NewValidFor});
feature_rel([end_date | T], #feature_rel{end_date = End} = R, Acc)
		when is_integer(End) ->
	ValidFor = #{"endDateTime" => im_rest:iso8601(End)},
	feature_rel(T, R, Acc#{"validFor" := ValidFor});
feature_rel([end_date | T],
		#{"validFor" := #{"endDateTime" := End}} = M, Acc)
		when is_list(End) ->
	feature_rel(T, M, Acc#feature_rel{end_date = im_rest:iso8601(End)});
feature_rel([rel_type | T], #feature_rel{rel_type = RelType} = R, Acc)
		when is_list(RelType) ->
	feature_rel(T, R, Acc#{"relationshipType" => RelType});
feature_rel([rel_type | T], #{"relationshipType" := RelType} = M, Acc)
		when is_list(RelType) ->
	feature_rel(T, M, Acc#feature_rel{rel_type = RelType});
feature_rel([_ | T], R, Acc) ->
	feature_rel(T, R, Acc);
feature_rel([], _, Acc) ->
	Acc.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

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
			false when length(Query) > 0  ->
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
		MFA = [im, query, [resource, Sort, FilterArgs, CountOnly]],
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
			JsonObj = lists:map(fun resource/1, Events),
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
	sorts(T, [#resource.id | Acc]);
sorts(["-id" | T], Acc) ->
	sorts(T, [-#resource.id | Acc]);
sorts(["href" | T], Acc) ->
	sorts(T, [#resource.href | Acc]);
sorts(["-href" | T], Acc) ->
	sorts(T, [-#resource.href | Acc]);
sorts(["publicIdentifier" | T], Acc) ->
	sorts(T, [#resource.public_id | Acc]);
sorts(["-publicIdentifier" | T], Acc) ->
	sorts(T, [-#resource.public_id | Acc]);
sorts(["name" | T], Acc) ->
	sorts(T, [#resource.name | Acc]);
sorts(["-name" | T], Acc) ->
	sorts(T, [-#resource.name | Acc]);
sorts(["description" | T], Acc) ->
	sorts(T, [#resource.description | Acc]);
sorts(["-description" | T], Acc) ->
	sorts(T, [-#resource.description | Acc]);
sorts(["category" | T], Acc) ->
	sorts(T, [#resource.category | Acc]);
sorts(["-category" | T], Acc) ->
	sorts(T, [-#resource.category | Acc]);
sorts(["@type" | T], Acc) ->
	sorts(T, [#resource.class_type | Acc]);
sorts(["-@type" | T], Acc) ->
	sorts(T, [-#resource.class_type | Acc]);
sorts(["@baseType" | T], Acc) ->
	sorts(T, [#resource.base_type | Acc]);
sorts(["-@baseType" | T], Acc) ->
	sorts(T, [-#resource.base_type | Acc]);
sorts(["@schemaLocation" | T], Acc) ->
	sorts(T, [#resource.schema | Acc]);
sorts(["-@schemaLocation" | T], Acc) ->
	sorts(T, [-#resource.schema | Acc]);
sorts(["lifecycleState" | T], Acc) ->
	sorts(T, [#resource.state | Acc]);
sorts(["-lifecycleState" | T], Acc) ->
	sorts(T, [-#resource.state | Acc]);
sorts(["lifecycleSubState" | T], Acc) ->
	sorts(T, [#resource.substate | Acc]);
sorts(["-lifecycleSubState" | T], Acc) ->
	sorts(T, [-#resource.substate | Acc]);
sorts(["version" | T], Acc) ->
	sorts(T, [#resource.version | Acc]);
sorts(["-version" | T], Acc) ->
	sorts(T, [-#resource.version | Acc]);
sorts(["lastUpdate" | T], Acc) ->
	sorts(T, [#resource.last_modified | Acc]);
sorts(["-lastUpdate" | T], Acc) ->
	sorts(T, [-#resource.last_modified | Acc]);
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
	parse_filter(Query, #resource{_ = '_'}, []).
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
			{MatchHead#resource{id = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#resource{id = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$1', Like} | MatchConditions],
			{MatchHead#resource{id = '$1'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "id", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$1', Cond, []),
	NewMatchHead = MatchHead#resource{id = '$1'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "id", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#resource{id = Name}, MatchConditions);
parse_filter([{exact, "id", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$1', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#resource{id = '$1'}, NewMatchConditions);
parse_filter([{notexact, "id", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#resource{id = '$1'},
	NewMatchConditions = [{'/=', '$1', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "id", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$1', []),
	NewMatchHead = MatchHead#resource{id = '$1'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "href", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#resource{href = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#resource{href = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$2', Like} | MatchConditions],
			{MatchHead#resource{href = '$2'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "href", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$2', Cond, []),
	NewMatchHead = MatchHead#resource{href = '$2'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "href", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#resource{href = Name}, MatchConditions);
parse_filter([{exact, "href", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$2', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#resource{href = '$2'}, NewMatchConditions);
parse_filter([{notexact, "href", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#resource{href = '$2'},
	NewMatchConditions = [{'/=', '$2', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "href", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$2', []),
	NewMatchHead = MatchHead#resource{href = '$2'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "publicIdentifier", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#resource{public_id = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#resource{public_id = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$3', Like} | MatchConditions],
			{MatchHead#resource{public_id = '$3'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "publicIdentifier", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$3', Cond, []),
	NewMatchHead = MatchHead#resource{public_id = '$3'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "publicIdentifier", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#resource{public_id = Name}, MatchConditions);
parse_filter([{exact, "publicIdentifier", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$3', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#resource{public_id = '$3'}, NewMatchConditions);
parse_filter([{notexact, "publicIdentifier", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#resource{public_id = '$3'},
	NewMatchConditions = [{'/=', '$3', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "publicIdentifier", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$3', []),
	NewMatchHead = MatchHead#resource{public_id = '$3'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "name", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#resource{name = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#resource{name = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$4', Like} | MatchConditions],
			{MatchHead#resource{name = '$4'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "name", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$4', Cond, []),
	NewMatchHead = MatchHead#resource{name = '$4'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "name", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#resource{name = Name}, MatchConditions);
parse_filter([{exact, "name", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$4', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#resource{name = '$4'}, NewMatchConditions);
parse_filter([{notexact, "name", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#resource{name = '$4'},
	NewMatchConditions = [{'/=', '$4', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "name", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$4', []),
	NewMatchHead = MatchHead#resource{name = '$4'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "description", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#resource{description = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#resource{description = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$5', Like} | MatchConditions],
			{MatchHead#resource{description = '$5'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "description", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$5', Cond, []),
	NewMatchHead = MatchHead#resource{description = '$5'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "description", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#resource{description = Name}, MatchConditions);
parse_filter([{exact, "description", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$5', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#resource{description = '$5'}, NewMatchConditions);
parse_filter([{notexact, "description", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#resource{description = '$5'},
	NewMatchConditions = [{'/=', '$5', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "description", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$5', []),
	NewMatchHead = MatchHead#resource{description = '$5'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "category", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#resource{category = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#resource{category = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$6', Like} | MatchConditions],
			{MatchHead#resource{category = '$6'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "category", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$6', Cond, []),
	NewMatchHead = MatchHead#resource{category = '$6'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "category", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#resource{category = Name}, MatchConditions);
parse_filter([{exact, "category", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$6', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#resource{category = '$6'}, NewMatchConditions);
parse_filter([{notexact, "category", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#resource{category = '$6'},
	NewMatchConditions = [{'/=', '$6', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "category", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$6', []),
	NewMatchHead = MatchHead#resource{category = '$6'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "@type", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#resource{class_type = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#resource{class_type = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$7', Like} | MatchConditions],
			{MatchHead#resource{class_type = '$7'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "@type", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$7', Cond, []),
	NewMatchHead = MatchHead#resource{class_type = '$7'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "@type", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#resource{class_type = Name}, MatchConditions);
parse_filter([{exact, "@type", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$7', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#resource{class_type = '$7'}, NewMatchConditions);
parse_filter([{notexact, "@type", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#resource{class_type = '$7'},
	NewMatchConditions = [{'/=', '$7', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "@type", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$7', []),
	NewMatchHead = MatchHead#resource{class_type = '$7'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "@baseType", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#resource{base_type = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#resource{base_type = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$8', Like} | MatchConditions],
			{MatchHead#resource{base_type = '$8'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "@baseType", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$8', Cond, []),
	NewMatchHead = MatchHead#resource{base_type = '$8'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "@baseType", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#resource{base_type = Name}, MatchConditions);
parse_filter([{exact, "@baseType", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$8', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#resource{base_type = '$8'}, NewMatchConditions);
parse_filter([{notexact, "@baseType", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#resource{base_type = '$8'},
	NewMatchConditions = [{'/=', '$8', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "@baseType", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$8', []),
	NewMatchHead = MatchHead#resource{base_type = '$8'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "@schemaLocation", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#resource{schema = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#resource{schema = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$9', Like} | MatchConditions],
			{MatchHead#resource{schema = '$9'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "@schemaLocation", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$9', Cond, []),
	NewMatchHead = MatchHead#resource{schema = '$9'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "@schemaLocation", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#resource{schema = Name}, MatchConditions);
parse_filter([{exact, "@schemaLocation", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$9', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#resource{schema = '$9'}, NewMatchConditions);
parse_filter([{notexact, "@schemaLocation", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#resource{schema = '$9'},
	NewMatchConditions = [{'/=', '$9', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "@schemaLocation", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$9', []),
	NewMatchHead = MatchHead#resource{schema = '$9'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "lifecycleState", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#resource{state = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#resource{state = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$10', Like} | MatchConditions],
			{MatchHead#resource{state = '$10'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "lifecycleState", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$10', Cond, []),
	NewMatchHead = MatchHead#resource{state = '$10'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "lifecycleState", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#resource{state = Name}, MatchConditions);
parse_filter([{exact, "lifecycleState", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$10', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#resource{state = '$10'}, NewMatchConditions);
parse_filter([{notexact, "lifecycleState", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#resource{state = '$10'},
	NewMatchConditions = [{'/=', '$10', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "lifecycleState", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$10', []),
	NewMatchHead = MatchHead#resource{state = '$10'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "version", [Like]} | T], Cond, MatchHead, MatchConditions)
		when is_list(Like) ->
	{NewMatchHead, NewMatchConditions} = case lists:last(Like) of
		$% when Cond == all ->
			Prefix = lists:droplast(Like),
			{MatchHead#resource{version = Prefix ++ '_'}, MatchConditions};
		_Name when Cond == all ->
			{MatchHead#resource{version = Like}, MatchConditions};
		$% when Cond == any ->
			NewMatchCondition1 = [{'==', '$12', Like} | MatchConditions],
			{MatchHead#resource{version = '$12'}, NewMatchCondition1}
	end,
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{like, "version", {all, Like}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(Like) ->
	NewMatchConditions = like(Like, '$12', Cond, []),
	NewMatchHead = MatchHead#resource{version = '$12'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{exact, "version", Name} | T], all, MatchHead, MatchConditions)
		when is_list(Name) ->
	parse_filter(T, all, MatchHead#resource{version = Name}, MatchConditions);
parse_filter([{exact, "version", Name} | T], any, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchConditions = [{'==', '$12', Name} | MatchConditions],
	parse_filter(T, any, MatchHead#resource{version = '$12'}, NewMatchConditions);
parse_filter([{notexact, "version", Name} | T], Cond, MatchHead, MatchConditions)
		when is_list(Name) ->
	NewMatchHead = MatchHead#resource{version = '$12'},
	NewMatchConditions = [{'/=', '$12', Name} | MatchConditions],
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([{in, "version", {all, In}} | T], Cond, MatchHead, _MatchConditions)
		when is_list(In) ->
	NewMatchConditions = in(In, '$12', []),
	NewMatchHead = MatchHead#resource{version = '$12'},
	parse_filter(T, Cond, NewMatchHead, NewMatchConditions);
parse_filter([], all, MatchHead, MatchConditions) ->
	[{MatchHead, MatchConditions, ['$_']}];
parse_filter([], any, MatchHead, MatchConditions) ->
	NewMatchConditions =  list_to_tuple(['or' | MatchConditions]),
	[{MatchHead, [NewMatchConditions], ['$_']}].

%% @hidden
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

