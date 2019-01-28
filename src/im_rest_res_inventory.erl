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
	resource(record_info(fields, resource), Resource, #resource{}).
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
resource([description| T],
		#resource{description = Description} = R, Acc) ->
	resource(T, R, Acc#{"description" => Description});
resource([description| T], #{"description" := Description} = M, Acc) ->
	resource(T, M, Acc#resource{description = Description});
resource([version | T], #resource{version = Version} = R, Acc) ->
	resource(T, R, Acc#{"version" => Version});
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
resource([category | T], #resource{category = CatRef} = R, Acc)
		when is_record(CatRef, category_ref) ->
	resource(T, R, Acc#{"category" => im_rest:category_ref(CatRef)});
resource([category | T], #{"category" := CatRef} = M, Acc) ->
	resource(T, M, Acc#resource{category = im_rest:category_ref(CatRef)});
resource([specification | T], #resource{specification = Spec} = R, Acc)
		when is_record(Spec, specification_rel) ->
	resource(T, R, Acc#{"resourceSpecification" => im_rest:specification_rel(Spec)});
resource([specification | T], #{"resourceSpecification" := Spec} = M, Acc) ->
	resource(T, M, Acc#resource{specification = im_rest:specification_rel(Spec)});
resource([_ | T], R, Acc) ->
	resource(T, R, Acc);
resource([], _, Acc) ->
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

