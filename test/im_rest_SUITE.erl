%%% im_rest_SUITE.erl
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
%%% Test suite for the REST API of the
%%% {@link //sigscale_im. sigscale_im} application.
-module(im_rest_SUITE).
-copyright('Copyright (c) 2018-2020 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

-include("im.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("inets/include/mod_auth.hrl").

-define(PathCatalog, "/resourceCatalogManagement/v4/").
-define(PathInventory, "/resourceInventoryManagement/v4/").
-define(PathParty, "/partyManagement/v2/").
-define(PathRole, "/partyRoleManagement/v4/").

%% support deprecated_time_unit()
-define(SECOND, seconds).

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{userdata, [{doc, "Test suite for REST API"}]},
	{timetrap, {minutes, 1}},
	{require, rest_user}, {default_config, rest_user, "ct"},
	{require, rest_pass}, {default_config, rest_pass, "tag0bpp53wsf"},
	{require, rest_group}, {default_config, rest_group, "all"}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	PrivDir = ?config(priv_dir, Config),
	ok = application:set_env(mnesia, dir, PrivDir),
	ok = im_test_lib:initialize_db(),
	ok = im_test_lib:start(),
	{ok, Services} = application:get_env(inets, services),
	Fport = fun FPort([{httpd, L} | T]) ->
				case lists:keyfind(server_name, 1, L) of
					{server_name, "ct_rest"} ->
						H1 = lists:keyfind(bind_address, 1, L),
						P1 = lists:keyfind(port, 1, L),
						{H1, P1};
					_ ->
						FPort(T)
				end;
			FPort([_ | T]) ->
				FPort(T)
	end,
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	_RestGroup = ct:get_config(rest_group),
	{Host, Port} = case Fport(Services) of
		{{_, H2}, {_, P2}} when H2 == "localhost"; H2 == {127,0,0,1} ->
			{ok, _} = im:add_user(RestUser, RestPass, "en"),
			{"localhost", P2};
		{{_, H2}, {_, P2}} ->
			{ok, _} = im:add_user(RestUser, RestPass, "en"),
			case H2 of
				H2 when is_tuple(H2) ->
					{inet:ntoa(H2), P2};
				H2 when is_list(H2) ->
					{H2, P2}
			end;
		{false, {_, P2}} ->
			{ok, _} = im:add_user(RestUser, RestPass, "en"),
			{"localhost", P2}
	end,
	HostUrl = "https://" ++ Host ++ ":" ++ integer_to_list(Port),
	[{host_url, HostUrl}, {port, Port} | Config].

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok = application:stop(sigscale_im),
	ok = application:stop(mnesia).

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
%%
init_per_testcase(oauth_authentication, Config) ->
	ok = set_inet_mod(),
	application:stop(inets),
	application:start(inets),
	Config;
init_per_testcase(_TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(oauth_authentication, Config) ->
	ok = set_inet_mod(),
	application:stop(inets),
	application:start(inets),
	Config;
end_per_testcase(_TestCase, _Config) ->
	ok.

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() ->
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[map_to_catalog, catalog_to_map, post_catalog, get_catalogs, get_catalog,
			map_to_category, category_to_map, post_category, get_categories, get_category,
			map_to_candidate, candidate_to_map, post_candidate, get_candidates, get_candidate,
			map_to_specification, specification_to_map, post_specification, get_specifications,
			get_specification, map_to_resource, resource_to_map, post_resource, get_resources,
			get_resource, geoaxis, query_category, advanced_query_category, query_candidate,
			advanced_query_candidate, query_catalog, advanced_query_catalog, get_users,
			post_role, delete_role, get_role, get_roles, oauth_authentication,
			post_hub_role, delete_hub_role].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

get_users() ->
	[{userdata, [{doc, "Get the user collection."}]}].

get_users(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ "/partyManagement/v2/individual",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers).

post_role() ->
	[{userdata, [{doc, "POST to Role collection"}]}].

post_role(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathRole ++ "partyRole",
	RoleName = "Global_Pirates",
	RoleType = "PartyRole",
	StartDate = "2021-08-17T00:00Z",
	EndDate = "2022-12-31T00:00Z",
	RoleMap = #{"@type" => RoleType,
		"name" => RoleName,
		"validFor" => #{
				"startDateTime" => StartDate,
				"endDateTime" => EndDate
		}
	},
	RequestBody = zj:encode(RoleMap),
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{ok, #{"id" := RoleName, "name" := RoleName, "@type" := RoleType,
			"href" := "/partyRoleManagement/v4/partyRole/" ++ RoleName,
			"validFor" := #{"startDateTime" := StartDate,
					"endDateTime" := EndDate}}} = zj:decode(ResponseBody).

delete_role() ->
   [{userdata, [{doc,"Delete role in rest interface"}]}].

delete_role(Config) ->
   Name = "Queen",
	PartyRole = party_role(Name),
	RequestBody = zj:encode(PartyRole),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathRole ++ "partyRole",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()],
			ContentType, RequestBody},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers1, _ResponseBody1} = Result1,
	{_, Href} = lists:keyfind("location", 1, Headers1),
	Request2 = {HostUrl ++ Href, [Accept, auth_header()]},
   {ok, Result2} = httpc:request(delete, Request2, [], []),
   {{"HTTP/1.1", 204, _NoContent}, _Headers2, []} = Result2,
	{ok, {{"HTTP/1.1", 404, "Object Not Found"}, _Headers3, _ResponseBody3}}
			= httpc:request(get, Request2, [], []).

get_role() ->
	[{userdata, [{doc, "Get a user."}]}].

get_role(Config) ->
	PartyRole1 = party_role("SL_Pirates"),
	RequestBody = zj:encode(PartyRole1),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathRole ++ "partyRole",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()],
			ContentType, RequestBody},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers1, _ResponseBody1} = Result1,
	{_, Href} = lists:keyfind("location", 1, Headers1),
	Request2 = {HostUrl ++ Href, [Accept, auth_header()]},
	{ok, Result2} = httpc:request(get, Request2, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers2, ResponseBody2} = Result2,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers2),
	ContentLength = integer_to_list(length(ResponseBody2)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers2),
	{ok, PartyRole2} = zj:decode(ResponseBody2),
	true = lists:all(fun is_role/1, [PartyRole2]).

get_roles() ->
	[{userdata, [{doc, "Get the role collection."}]}].

get_roles(Config) ->
	PartyRole1 = party_role("USA_Pirates"),
	PartyRole2 = party_role("CA_Pirates"),
	RequestBody1 = zj:encode(PartyRole1),
	RequestBody2 = zj:encode(PartyRole2),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathRole ++ "partyRole",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request1 = {CollectionUrl, [Accept, auth_header()],
			ContentType, RequestBody1},
	{ok, Result1} = httpc:request(post, Request1, [], []),
	{{"HTTP/1.1", 201, Created}, _Headers1, _ResponseBody1} = Result1,
	Request2 = {CollectionUrl, [Accept, auth_header()],
			ContentType, RequestBody2},
	{ok, Result2} = httpc:request(post, Request2, [], []),
	{{"HTTP/1.1", 201, Created}, _Headers2, _ResponseBody2} = Result2,
	Request3 = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result3} = httpc:request(get, Request3, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers3, ResponseBody3} = Result3,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers3),
	ContentLength = integer_to_list(length(ResponseBody3)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers3),
	{ok, PartyRoles} = zj:decode(ResponseBody3),
	false = is_empty(PartyRoles),
	true = lists:all(fun is_role/1, PartyRoles).

map_to_catalog() ->
	[{userdata, [{doc, "Decode Catalog map()"}]}].

map_to_catalog(_Config) ->
	CatalogId = random_string(12),
	CatalogHref = ?PathCatalog ++ "resourceCatalog/" ++ CatalogId,
	CatalogName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceCatalog",
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceCatalog",
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	CategoryId = random_string(10),
	CategoryHref = ?PathCatalog ++ "resourceCcategory/" ++ CategoryId,
	CategoryName = random_string(10),
	Map = #{"id" => CatalogId,
			"href" => CatalogHref,
			"name" => CatalogName,
			"description" => Description,
			"@type" => ClassType,
			"@schemaLocation" => Schema,
			"@baseType" => "Catalog",
			"version" => Version,
			"validFor" => #{"startDateTime" => "2019-01-29T00:00",
					"endDateTime" => "2019-12-31T23:59"},
			"lifecycleStatus" => "Active",
			"relatedParty" => [#{"id" => PartyId,
					"href" => PartyHref,
					"role" => "Supplier",
					"name" => "ACME Inc.",
					"validFor" => #{"startDateTime" => "2019-01-29T00:00",
							"endDateTime" => "2019-12-31T23:59"}}],
			"category" => [#{"id" => CategoryId,
					"href" => CategoryHref,
					"name" => CategoryName,
					"version" => Version}]},
	#catalog{id = CatalogId, href = CatalogHref,
			description = Description, class_type = ClassType,
			schema = Schema, base_type = "Catalog", version = Version,
			start_date = StartDate, end_date = EndDate,
			status = active, party = [RP],
			category = [C]} = im_rest_res_catalog:catalog(Map),
	true = is_integer(StartDate),
	true = is_integer(EndDate),
	#party_ref{id = PartyId, href = PartyHref} = RP,
	#category_ref{id = CategoryId, href = CategoryHref,
			name = CategoryName, version = Version} = C.

catalog_to_map() ->
	[{userdata, [{doc, "Encode Catalog map()"}]}].

catalog_to_map(_Config) ->
	CatalogId = random_string(12),
	CatalogHref = ?PathCatalog ++ "resourceCatalog/" ++ CatalogId,
	CatalogName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceCatalog",
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceCatalog",
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	CategoryId = random_string(10),
	CategoryHref = ?PathCatalog ++ "resourceCategory/" ++ CategoryId,
	CategoryName = random_string(10),
	CatalogRecord = #catalog{id = CatalogId,
			href = CatalogHref,
			name = CatalogName,
			description = Description,
			class_type = ClassType,
			schema = Schema,
			base_type = "Catalog",
			version = Version,
			start_date = 1548720000000,
			end_date = 1577836740000,
			status = active,
			party = [#party_ref{id = PartyId,
					href = PartyHref,
					role = "Supplier",
					name = "ACME Inc."}],
			category = [#category_ref{id = CategoryId,
					href = CategoryHref,
					name = CategoryName,
					version = Version}]},
	#{"id" := CatalogId, "href" := CatalogHref,
			"description" := Description, "@type" := ClassType,
			"@schemaLocation" := Schema, "@baseType" := "Catalog", "version" := Version,
			"validFor" := #{"startDateTime" := Start, "endDateTime" := End},
			"lifecycleStatus" := "Active", "relatedParty" := [RP],
			"category" := [C]} = im_rest_res_catalog:catalog(CatalogRecord),
	true = is_list(Start),
	true = is_list(End),
	#{"id" := PartyId, "href" := PartyHref} = RP,
	#{"id" := CategoryId, "href" := CategoryHref,
			"name" := CategoryName, "version" := Version} = C.

post_catalog() ->
	[{userdata, [{doc, "POST to Catalog collection"}]}].

post_catalog(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathCatalog ++ "resourceCatalog",
	CatalogName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceCatalog",
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceCatalog",
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	CategoryId = random_string(10),
	CategoryHref = ?PathCatalog ++ "resourceCategory/" ++ CategoryId,
	CategoryName = random_string(10),
	RequestBody = "{\n"
			++ "\t\"name\": \"" ++ CatalogName ++ "\",\n"
			++ "\t\"description\": \"" ++ Description ++ "\",\n"
			++ "\t\"@type\": \"" ++ ClassType ++ "\",\n"
			++ "\t\"@schemaLocation\": \"" ++ Schema ++ "\",\n"
			++ "\t\"@baseType\": \"Catalog\",\n"
			++ "\t\"version\": \"" ++ Version ++ "\",\n"
			++ "\t\"validFor\": {\n"
			++ "\t\t\"startDateTime\": \"2019-01-29T00:00\",\n"
			++ "\t\t\"endDateTime\": \"2019-12-31T23:59\",\n"
			++ "\t},\n"
			++ "\t\"lifecycleStatus\": \"In Test\",\n"
			++ "\t\"relatedParty\": [\n"
			++ "\t\t{\n"
			++ "\t\t\t\"id\": \"" ++ PartyId ++ "\",\n"
			++ "\t\t\t\"href\": \"" ++ PartyHref ++ "\",\n"
			++ "\t\t\t\"role\": \"Supplier\",\n"
			++ "\t\t\t\"name\": \"ACME Inc.\",\n"
			++ "\t\t\t\"validFor\": {\n"
			++ "\t\t\t\t\"startDateTime\": \"2019-01-29T00:00\",\n"
			++ "\t\t\t\t\"endDateTime\": \"2019-12-31T23:59\"\n"
			++ "\t\t\t}\n"
			++ "\t\t}\n"
			++ "\t],\n"
			++ "\t\"category\": [\n"
			++ "\t\t{\n"
			++ "\t\t\t\"id\": \"" ++ CategoryId ++ "\",\n"
			++ "\t\t\t\"href\": \"" ++ CategoryHref ++ "\",\n"
			++ "\t\t\t\"name\": \"" ++ CategoryName ++ "\",\n"
			++ "\t\t\t\"version\": \"" ++ Version ++ "\"\n"
			++ "\t\t}\n"
			++ "\t]\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{?PathCatalog ++ "resourceCatalog/" ++ ID, _} = httpd_util:split_path(URI),
	{ok, #catalog{id = ID, name = CatalogName,
			description = Description, version = Version,
			class_type = ClassType, base_type = "Catalog",
			schema = Schema, party = [RP],
			category = [C]}} = im:get_catalog(ID),
	#party_ref{id = PartyId, href = PartyHref} = RP,
	#category_ref{id = CategoryId, href = CategoryHref,
			name = CategoryName, version = Version} = C.

get_catalogs() ->
	[{userdata, [{doc, "GET Catalog collection"}]}].

get_catalogs(Config) ->
	ok = fill_catalog(5),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathCatalog ++ "resourceCatalog",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{ok, Catalogs} = zj:decode(ResponseBody),
	false = is_empty(Catalogs),
	true = lists:all(fun is_catalog/1, Catalogs).

get_catalog() ->
	[{userdata, [{doc, "GET Catalog resource"}]}].

get_catalog(Config) ->
	HostUrl = ?config(host_url, Config),
	CatalogName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceCatalog",
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceCatalog",
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	CategoryId = random_string(10),
	CategoryHref = ?PathCatalog ++ "resourceCategory/" ++ CategoryId,
	CategoryName = random_string(10),
	CatalogRecord = #catalog{name = CatalogName,
			description = Description,
			class_type = ClassType,
			schema = Schema,
			base_type = "Catalog",
			version = Version,
			start_date = 1548720000000,
			end_date = 1577836740000,
			status = active,
			party = [#party_ref{id = PartyId,
					href = PartyHref,
					role = "Supplier",
					name = "ACME Inc."}],
			category = [#category_ref{id = CategoryId,
					href = CategoryHref,
					name = CategoryName,
					version = Version}]},
	{ok, #catalog{id = Id, href = Href}} = im:add_catalog(CatalogRecord),
	Accept = {"accept", "application/json"},
	Request = {HostUrl ++ Href, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{ok, CatalogMap} = zj:decode(ResponseBody),
	#{"id" := Id, "href" := Href, "name" := CatalogName,
			"description" := Description, "version" := Version,
			"@type" := ClassType, "@baseType" := "Catalog",
			"@schemaLocation" := Schema, "relatedParty" := [RP],
			"category" := [C]} = CatalogMap,
	true = is_party_ref(RP),
	true = is_category_ref(C).

map_to_category() ->
	[{userdata, [{doc, "Decode Category map()"}]}].

map_to_category(_Config) ->
	CategoryId = random_string(12),
	CategoryHref = ?PathCatalog ++ "resourceCategory/" ++ CategoryId,
	CategoryName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceCategory",
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceCategory",
	Parent = random_string(10),
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	CandidateId = random_string(10),
	CandidateHref = ?PathCatalog ++ "resourceCandidate/" ++ CandidateId,
	CandidateName = random_string(10),
	Map = #{"id" => CategoryId,
			"href" => CategoryHref,
			"name" => CategoryName,
			"description" => Description,
			"@type" => ClassType,
			"@schemaLocation" => Schema,
			"@baseType" => "Category",
			"version" => Version,
			"validFor" => #{"startDateTime" => "2019-01-29T00:00",
					"endDateTime" => "2019-12-31T23:59"},
			"lifecycleStatus" => "Active",
			"parentId" => Parent,
			"isRoot" => "false",
			"resourceCandidate" => [#{"id" => CandidateId,
					"href" => CandidateHref,
					"name" => CandidateName,
					"version" => Version}],
			"relatedParty" => [#{"id" => PartyId,
					"href" => PartyHref,
					"role" => "Supplier",
					"name" => "ACME Inc.",
					"validFor" => #{"startDateTime" => "2019-01-29T00:00",
							"endDateTime" => "2019-12-31T23:59"}}]},
	#category{id = CategoryId, href = CategoryHref,
			description = Description, class_type = ClassType,
			schema = Schema, base_type = "Category", version = Version,
			start_date = StartDate, end_date = EndDate,
			status = active, parent = Parent, root = false, party = [RP],
			candidate = [C]} = im_rest_res_category:category(Map),
	true = is_integer(StartDate),
	true = is_integer(EndDate),
	#party_ref{id = PartyId, href = PartyHref} = RP,
	#candidate_ref{id = CandidateId, href = CandidateHref,
			name = CandidateName, version = Version} = C.

category_to_map() ->
	[{userdata, [{doc, "Encode Category map()"}]}].

category_to_map(_Config) ->
	CategoryId = random_string(12),
	CategoryHref = ?PathCatalog ++ "resourceCategory/" ++ CategoryId,
	CategoryName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceCategory",
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceCategory",
	Parent = random_string(10),
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	CandidateId = random_string(10),
	CandidateHref = ?PathCatalog ++ "resourceCandidate/" ++ CandidateId,
	CandidateName = random_string(10),
	CategoryRecord = #category{id = CategoryId,
			href = CategoryHref,
			name = CategoryName,
			description = Description,
			class_type = ClassType,
			schema = Schema,
			base_type = "Category",
			version = Version,
			start_date = 1548720000000,
			end_date = 1577836740000,
			status = active,
			parent = Parent,
			root = false,
			party = [#party_ref{id = PartyId,
					href = PartyHref,
					role = "Supplier",
					name = "ACME Inc."}],
			candidate = [#candidate_ref{id = CandidateId,
					href = CandidateHref,
					name = CandidateName,
					version = Version}]},
	#{"id" := CategoryId, "href" := CategoryHref,
			"description" := Description, "@type" := ClassType,
			"@schemaLocation" := Schema, "@baseType" := "Category", "version" := Version,
			"validFor" := #{"startDateTime" := Start, "endDateTime" := End},
			"lifecycleStatus" := "Active", "parentId" := Parent, "isRoot" := false,
			"relatedParty" := [RP], "resourceCandidate" := [C]}
			= im_rest_res_category:category(CategoryRecord),
	true = is_list(Start),
	true = is_list(End),
	#{"id" := PartyId, "href" := PartyHref} = RP,
	#{"id" := CandidateId, "href" := CandidateHref,
			"name" := CandidateName, "version" := Version} = C.

post_category() ->
	[{userdata, [{doc, "POST to Category collection"}]}].

post_category(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathCatalog ++ "resourceCategory",
	CategoryName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceCategory",
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceCategory",
	Parent = random_string(10),
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	CandidateId = random_string(10),
	CandidateHref = ?PathCatalog ++ "resourceCandidate/" ++ CandidateId,
	CandidateName = random_string(10),
	RequestBody = "{\n"
			++ "\t\"name\": \"" ++ CategoryName ++ "\",\n"
			++ "\t\"description\": \"" ++ Description ++ "\",\n"
			++ "\t\"@type\": \"" ++ ClassType ++ "\",\n"
			++ "\t\"@schemaLocation\": \"" ++ Schema ++ "\",\n"
			++ "\t\"@baseType\": \"Category\",\n"
			++ "\t\"version\": \"" ++ Version ++ "\",\n"
			++ "\t\"validFor\": {\n"
			++ "\t\t\"startDateTime\": \"2019-01-29T00:00\",\n"
			++ "\t\t\"endDateTime\": \"2019-12-31T23:59\",\n"
			++ "\t},\n"
			++ "\t\"lifecycleStatus\": \"In Test\",\n"
			++ "\t\"parentId\": \"" ++ Parent ++ "\",\n"
			++ "\t\"isRoot\": true,\n"
			++ "\t\"resourceCandidate\": [\n"
			++ "\t\t{\n"
			++ "\t\t\t\"id\": \"" ++ CandidateId ++ "\",\n"
			++ "\t\t\t\"href\": \"" ++ CandidateHref ++ "\",\n"
			++ "\t\t\t\"name\": \"" ++ CandidateName ++ "\",\n"
			++ "\t\t\t\"version\": \"" ++ Version ++ "\"\n"
			++ "\t\t}\n"
			++ "\t],\n"
			++ "\t\"relatedParty\": [\n"
			++ "\t\t{\n"
			++ "\t\t\t\"id\": \"" ++ PartyId ++ "\",\n"
			++ "\t\t\t\"href\": \"" ++ PartyHref ++ "\",\n"
			++ "\t\t\t\"role\": \"Supplier\",\n"
			++ "\t\t\t\"name\": \"ACME Inc.\",\n"
			++ "\t\t\t\"validFor\": {\n"
			++ "\t\t\t\t\"startDateTime\": \"2019-01-29T00:00\",\n"
			++ "\t\t\t\t\"endDateTime\": \"2019-12-31T23:59\"\n"
			++ "\t\t\t}\n"
			++ "\t\t}\n"
			++ "\t]\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{?PathCatalog ++ "resourceCategory/" ++ ID, _} = httpd_util:split_path(URI),
	{ok, #category{id = ID, name = CategoryName,
			description = Description, version = Version,
			class_type = ClassType, base_type = "Category",
			parent = Parent, root = true,
			schema = Schema, party = [RP],
			candidate = [C]}} = im:get_category(ID),
	#party_ref{id = PartyId, href = PartyHref} = RP,
	#candidate_ref{id = CandidateId, href = CandidateHref,
			name = CandidateName, version = Version} = C.

get_categories() ->
	[{userdata, [{doc, "GET Category collection"}]}].

get_categories(Config) ->
	ok = fill_category(5),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathCatalog ++ "resourceCategory",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{ok, Categories} = zj:decode(ResponseBody),
	false = is_empty(Categories),
	true = lists:all(fun is_category/1, Categories).

get_category() ->
	[{userdata, [{doc, "GET Category resource"}]}].

get_category(Config) ->
	HostUrl = ?config(host_url, Config),
	CategoryName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceCategory",
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceCategory",
	Parent = random_string(10),
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	CandidateId = random_string(10),
	CandidateHref = ?PathCatalog ++ "resourceCandidate/" ++ CandidateId,
	CandidateName = random_string(10),
	CategoryRecord = #category{name = CategoryName,
			description = Description,
			class_type = ClassType,
			schema = Schema,
			base_type = "Category",
			version = Version,
			start_date = 1548720000000,
			end_date = 1577836740000,
			status = active,
			parent = Parent,
			root = true,
			party = [#party_ref{id = PartyId,
					href = PartyHref,
					role = "Supplier",
					name = "ACME Inc."}],
			candidate = [#candidate_ref{id = CandidateId,
					href = CandidateHref,
					name = CandidateName,
					version = Version}]},
	{ok, #category{id = Id, href = Href}} = im:add_category(CategoryRecord),
	Accept = {"accept", "application/json"},
	Request = {HostUrl ++ Href, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{ok, CategoryMap} = zj:decode(ResponseBody),
	#{"id" := Id, "href" := Href, "name" := CategoryName,
			"description" := Description, "version" := Version,
			"@type" := ClassType, "@baseType" := "Category", "parentId" := Parent,
			"isRoot" := true, "@schemaLocation" := Schema, "relatedParty" := [RP],
			"resourceCandidate" := [C]} = CategoryMap,
	true = is_party_ref(RP),
	true = is_candidate_ref(C).

map_to_candidate() ->
	[{userdata, [{doc, "Decode Candidate map()"}]}].

map_to_candidate(_Config) ->
	CandidateId = random_string(12),
	CandidateHref = ?PathCatalog ++ "resourceCandidate/" ++ CandidateId,
	CandidateName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceCandidate",
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceCandidate",
	CategoryId = random_string(10),
	CategoryHref = ?PathCatalog ++ "resourceCategory/" ++ CategoryId,
	CategoryName = random_string(10),
	SpecificationId = random_string(12),
	SpecificationHref = ?PathCatalog ++ "resourceSpecification/" ++ SpecificationId,
	SpecificationName = random_string(10),
	Map = #{"id" => CandidateId,
			"href" => CandidateHref,
			"name" => CandidateName,
			"description" => Description,
			"@type" => ClassType,
			"@schemaLocation" => Schema,
			"@baseType" => "Candidate",
			"version" => Version,
			"validFor" => #{"startDateTime" => "2019-01-29T00:00",
					"endDateTime" => "2019-12-31T23:59"},
			"lifecycleStatus" => "Active",
			"category" => [#{"id" => CategoryId,
					"href" => CategoryHref,
					"name" => CategoryName,
					"version" => Version}],
			"resourceSpecification" => #{"id" => SpecificationId,
					"href" => SpecificationHref,
					"name" => SpecificationName,
					"version" => Version}},
	#candidate{id = CandidateId, href = CandidateHref,
			description = Description, class_type = ClassType,
			schema = Schema, base_type = "Candidate", version = Version,
			start_date = StartDate, end_date = EndDate, status = active, category = [C],
			specification = S} = im_rest_res_candidate:candidate(Map),
	true = is_integer(StartDate),
	true = is_integer(EndDate),
	#category_ref{id = CategoryId, href = CategoryHref,
			name = CategoryName, version = Version} = C,
	#specification_ref{id = SpecificationId, href = SpecificationHref,
			name = SpecificationName, version = Version} = S.

candidate_to_map() ->
	[{userdata, [{doc, "Encode Candidate map()"}]}].

candidate_to_map(_Config) ->
	CandidateId = random_string(10),
	CandidateHref = ?PathCatalog ++ "resourceCandidate/" ++ CandidateId,
	CandidateName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceCandidate",
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceCandidate",
	CategoryId = random_string(12),
	CategoryHref = ?PathCatalog ++ "resourceCategory/" ++ CategoryId,
	CategoryName = random_string(10),
	SpecificationId = random_string(12),
	SpecificationHref = ?PathCatalog ++ "resourceSpecification/" ++ SpecificationId,
	SpecificationName = random_string(10),
	CandidateRecord = #candidate{id = CandidateId,
			href = CandidateHref,
			name = CandidateName,
			description = Description,
			class_type = ClassType,
			schema = Schema,
			base_type = "Candidate",
			version = Version,
			start_date = 1548720000000,
			end_date = 1577836740000,
			status = active,
			category = [#category_ref{id = CategoryId,
					href = CategoryHref,
					name = CategoryName,
					version = Version}],
			specification = #specification_ref{id = SpecificationId,
					href = SpecificationHref,
					name = SpecificationName,
					version = Version}},
	#{"id" := CandidateId, "href" := CandidateHref,
			"description" := Description, "@type" := ClassType,
			"@schemaLocation" := Schema, "@baseType" := "Candidate", "version" := Version,
			"validFor" := #{"startDateTime" := Start, "endDateTime" := End},
			"lifecycleStatus" := "Active", "category" := [C],
			"resourceSpecification" := S} = im_rest_res_candidate:candidate(CandidateRecord),
	true = is_list(Start),
	true = is_list(End),
	#{"id" := CategoryId, "href" := CategoryHref,
			"name" := CategoryName, "version" := Version} = C,
	#{"id" := SpecificationId, "href" := SpecificationHref,
			"name" := SpecificationName, "version" := Version} = S.

post_candidate() ->
	[{userdata, [{doc, "POST to Candidate collection"}]}].

post_candidate(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathCatalog ++ "resourceCandidate",
	CandidateName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceCandidate",
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceCandidate",
	CategoryId = random_string(10),
	CategoryHref = ?PathCatalog ++ "resourceCategory/" ++ CategoryId,
	CategoryName = random_string(10),
	SpecificationId = random_string(12),
	SpecificationHref = ?PathCatalog ++ "resourceSpecification/" ++ SpecificationId,
	SpecificationName = random_string(10),
	RequestBody = "{\n"
			++ "\t\"name\": \"" ++ CandidateName ++ "\",\n"
			++ "\t\"description\": \"" ++ Description ++ "\",\n"
			++ "\t\"@type\": \"" ++ ClassType ++ "\",\n"
			++ "\t\"@schemaLocation\": \"" ++ Schema ++ "\",\n"
			++ "\t\"@baseType\": \"Candidate\",\n"
			++ "\t\"version\": \"" ++ Version ++ "\",\n"
			++ "\t\"validFor\": {\n"
			++ "\t\t\"startDateTime\": \"2019-01-29T00:00\",\n"
			++ "\t\t\"endDateTime\": \"2019-12-31T23:59\",\n"
			++ "\t},\n"
			++ "\t\"lifecycleStatus\": \"In Test\",\n"
			++ "\t\"category\": [\n"
			++ "\t\t{\n"
			++ "\t\t\t\"id\": \"" ++ CategoryId ++ "\",\n"
			++ "\t\t\t\"href\": \"" ++ CategoryHref ++ "\",\n"
			++ "\t\t\t\"name\": \"" ++ CategoryName ++ "\",\n"
			++ "\t\t\t\"version\": \"" ++ Version ++ "\"\n"
			++ "\t\t}\n"
			++ "\t],\n"
			++ "\t\"resourceSpecification\": \n"
			++ "\t\t{\n"
			++ "\t\t\t\"id\": \"" ++ SpecificationId ++ "\",\n"
			++ "\t\t\t\"href\": \"" ++ SpecificationHref ++ "\",\n"
			++ "\t\t\t\"name\": \"" ++ SpecificationName ++ "\",\n"
			++ "\t\t\t\"version\": \"" ++ Version ++ "\"\n"
			++ "\t\t}\n"
			++ "\t\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{?PathCatalog ++ "resourceCandidate/" ++ ID, _} = httpd_util:split_path(URI),
	{ok, #candidate{id = ID, name = CandidateName,
			description = Description, version = Version,
			class_type = ClassType, base_type = "Candidate",
			schema = Schema, category = [C],
			specification = S}} = im:get_candidate(ID),
	#category_ref{id = CategoryId, href = CategoryHref,
			name = CategoryName, version = Version} = C,
	#specification_ref{id = SpecificationId, href = SpecificationHref,
			name = SpecificationName, version = Version} = S.

get_candidates() ->
	[{userdata, [{doc, "GET Candidate collection"}]}].

get_candidates(Config) ->
	ok = fill_candidate(5),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathCatalog ++ "resourceCandidate",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{ok, Candidates} = zj:decode(ResponseBody),
	false = is_empty(Candidates),
	true = lists:all(fun is_candidate/1, Candidates).

get_candidate() ->
	[{userdata, [{doc, "GET Candidate resource"}]}].

get_candidate(Config) ->
	HostUrl = ?config(host_url, Config),
	CandidateName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceCandidate",
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceCandidate",
	CategoryId = random_string(10),
	CategoryHref = ?PathCatalog ++ "resourceCategory/" ++ CategoryId,
	CategoryName = random_string(10),
	SpecificationId = random_string(10),
	SpecificationHref = ?PathCatalog ++ "resourceSpecification/" ++ SpecificationId,
	SpecificationName = random_string(10),
	CandidateRecord = #candidate{name = CandidateName,
			description = Description,
			class_type = ClassType,
			schema = Schema,
			base_type = "Candidate",
			version = Version,
			start_date = 1548720000000,
			end_date = 1577836740000,
			status = active,
			category = [#category_ref{id = CategoryId,
					href = CategoryHref,
					name = CategoryName,
					version = Version}],
			specification = #specification_ref{id = SpecificationId,
					href = SpecificationHref,
					name = SpecificationName,
					version = Version}},
	{ok, #candidate{id = Id, href = Href}} = im:add_candidate(CandidateRecord),
	Accept = {"accept", "application/json"},
	Request = {HostUrl ++ Href, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{ok, CandidateMap} = zj:decode(ResponseBody),
	#{"id" := Id, "href" := Href, "name" := CandidateName,
			"description" := Description, "version" := Version,
			"@type" := ClassType, "@baseType" := "Candidate",
			"@schemaLocation" := Schema, "category" := [C],
			"resourceSpecification" := S} = CandidateMap,
	true = is_category_ref(C),
	true = is_specification_ref(S).

map_to_specification() ->
	[{userdata, [{doc, "Decode Specification map()"}]}].

map_to_specification(_Config) ->
	SpecificationId = random_string(12),
	SpecificationHref = ?PathCatalog ++ "resourceSpecification/" ++ SpecificationId,
	SpecificationName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceFunctionSpecification",
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceSpecification",
	Category = random_string(5),
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	ResourceId = random_string(10),
	ResourceName = random_string(7),
	ResouceHref = ?PathCatalog ++ "resourceSpecification/" ++ ResourceId,
	Map = #{"id" => SpecificationId,
			"href" => SpecificationHref,
			"name" => SpecificationName,
			"description" => Description,
			"@type" => ClassType,
			"version" => Version,
			"validFor" => #{"startDateTime" => "2019-01-29T00:00",
					"endDateTime" => "2019-12-31T23:59"},
			"lifecycleStatus" => "Active",
			"category" => Category,
			"targetResourceSchema" => #{"@type" => ClassType,
					"@schemaLocation" => Schema},
			"relatedParty" => [#{"id" => PartyId,
					"href" => PartyHref,
					"role" => "Supplier",
					"name" => "ACME Inc.",
					"validFor" => #{"startDateTime" => "2019-01-29T00:00",
							"endDateTime" => "2019-12-31T23:59"}}],
			"resourceSpecRelationship" => [#{"id" => ResourceId,
					"href" => ResouceHref,
					"role" => "Supplier",
					"name" => ResourceName,
					"validFor" => #{"startDateTime" => "2019-01-29T00:00",
							"endDateTime" => "2019-12-31T23:59"}}]},
	#specification{id = SpecificationId, href = SpecificationHref,
			description = Description, class_type = ClassType, version = Version,
			start_date = StartDate, end_date = EndDate, status = active,
			category = Category, target_schema = TS, party = [RP],
			related = [R]} = im_rest_res_specification:specification(Map),
	true = is_integer(StartDate),
	true = is_integer(EndDate),
	#target_schema_ref{class_type = ClassType, schema = Schema} = TS,
	#party_ref{id = PartyId, href = PartyHref} = RP,
	#specification_rel{id = ResourceId, href = ResouceHref, role = "Supplier"} = R.

specification_to_map() ->
	[{userdata, [{doc, "Encode Specification map()"}]}].

specification_to_map(_Config) ->
	SpecificationId = random_string(12),
	SpecificationHref = ?PathCatalog ++ "resourceCatalog/" ++ SpecificationId,
	SpecificationName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceCatalog",
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceCatalog",
	Category = random_string(5),
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	ResourceId = random_string(10),
	ResourceName = random_string(7),
	ResouceHref = ?PathCatalog ++ "resourceSpecification/" ++ ResourceId,
	SpecificationRecord = #specification{id = SpecificationId,
			href = SpecificationHref,
			name = SpecificationName,
			description = Description,
			class_type = ClassType,
			version = Version,
			start_date = 1548720000000,
			end_date = 1577836740000,
			status = active,
			category = Category,
			target_schema = #target_schema_ref{class_type = ClassType,
					schema = Schema},
			party = [#party_ref{id = PartyId,
					href = PartyHref,
					role = "Supplier",
					name = "ACME Inc."}],
			related = [#specification_rel{id = ResourceId,
					href = ResouceHref,
					role = "Supplier",
					name = ResourceName,
					start_date = 1548720000000,
					end_date = 1577836740000}]},
	#{"id" := SpecificationId, "href" := SpecificationHref,
			"description" := Description, "@type" := ClassType,
			"version" := Version, "validFor" := #{"startDateTime" := Start,
			"endDateTime" := End}, "lifecycleStatus" := "Active",
			"category" := Category, "targetResourceSchema" := TS,
			"relatedParty" := [RP], "resourceSpecRelationship" := [R]}
			= im_rest_res_specification:specification(SpecificationRecord),
	true = is_list(Start),
	true = is_list(End),
	#{"@type" := ClassType, "@schemaLocation" := Schema} = TS,
	#{"id" := PartyId, "href" := PartyHref} = RP,
	#{"id" := ResourceId, "href" := ResouceHref, "name" := ResourceName} = R.

post_specification() ->
	[{userdata, [{doc, "POST to Specification collection"}]}].

post_specification(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathCatalog ++ "resourceSpecification",
	SpecificationName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceFunctionSpecification",
	TargetSchema = ?PathCatalog ++ "schema/BssFunction",
	Category = random_string(6),
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	CharDes1 = "Used as an RDN when naming an instance of the object class",
	CharDes2 = "A user-friendly (and user assignable) name of this object",
	RequestBody = "{\n"
			++ "\t\"name\": \"" ++ SpecificationName ++ "\",\n"
			++ "\t\"description\": \"" ++ Description ++ "\",\n"
			++ "\t\"@type\": \"" ++ ClassType ++ "\",\n"
			++ "\t\"version\": \"" ++ Version ++ "\",\n"
			++ "\t\"validFor\": {\n"
			++ "\t\t\"startDateTime\": \"2019-01-29T00:00\",\n"
			++ "\t\t\"endDateTime\": \"2019-12-31T23:59\"\n"
			++ "\t},\n"
			++ "\t\"lifecycleStatus\": \"In Test\",\n"
			++ "\t\"isBundle\": false,\n"
			++ "\t\"category\": \"" ++ Category ++ "\",\n"
			++ "\t\"targetResourceSchema\": {\n"
			++ "\t\t\"@type\": \"" ++ ClassType ++ "\",\n"
			++ "\t\t\"@schemaLocation\": \"" ++ TargetSchema ++ "\"\n"
			++ "\t\t},\n"
			++ "\t\"feature\": [],\n"
			++ "\t\"attachment\": [],\n"
			++ "\t\"relatedParty\": [\n"
			++ "\t\t{\n"
			++ "\t\t\t\"id\": \"" ++ PartyId ++ "\",\n"
			++ "\t\t\t\"href\": \"" ++ PartyHref ++ "\",\n"
			++ "\t\t\t\"role\": \"Supplier\",\n"
			++ "\t\t\t\"name\": \"ACME Inc.\",\n"
			++ "\t\t\t\"validFor\": {\n"
			++ "\t\t\t\t\"startDateTime\": \"2019-01-29T00:00\",\n"
			++ "\t\t\t\t\"endDateTime\": \"2019-12-31T23:59\"\n"
			++ "\t\t\t}\n"
			++ "\t\t}\n"
			++ "\t],\n"
			++ "\t\"resourceSpecCharacteristic\": [\n"
			++ "\t\t{\n"
			++ "\t\t\t\"name\": \"id\",\n"
			++ "\t\t\t\"description\": \"" ++ CharDes1 ++ "\",\n"
			++ "\t\t\t\"valueType\": \"string\"\n"
			++ "\t\t},\n"
			++ "\t\t{\n"
			++ "\t\t\t\"name\": \"userLabel\",\n"
			++ "\t\t\t\"description\": \"" ++ CharDes2 ++ "\",\n"
			++ "\t\t\t\"valueType\": \"string\"\n"
			++ "\t\t}\n"
			++ "\t],\n"
			++ "\t\"resourceSpecRelationship\": []\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{?PathCatalog ++ "resourceSpecification/" ++ ID, _} = httpd_util:split_path(URI),
	{ok, #specification{id = ID, name = SpecificationName,
			description = Description, version = Version, class_type = ClassType,
			party = [RP], characteristic = Chars}} = im:get_specification(ID),
	#party_ref{id = PartyId, href = PartyHref} = RP,
	#specification_char{name = "id", description = CharDes1,
			value_type = "string"}
			= lists:keyfind("id", #specification_char.name, Chars),
	#specification_char{name = "userLabel", description = CharDes2,
			value_type = "string"}
			= lists:keyfind("userLabel", #specification_char.name, Chars).

get_specifications() ->
	[{userdata, [{doc, "GET Specification collection"}]}].

get_specifications(Config) ->
	ok = fill_specification(5),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathCatalog ++ "resourceSpecification",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{ok, Specifications} = zj:decode(ResponseBody),
	false = is_empty(Specifications),
	true = lists:all(fun is_specification/1, Specifications).

get_specification() ->
	[{userdata, [{doc, "GET Specification resource"}]}].

get_specification(Config) ->
	HostUrl = ?config(host_url, Config),
	SpecificationName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceFunctionSpecification",
	TargetSchema = ?PathCatalog ++ "schema/BssFunction",
	Model = random_string(5),
	Part = random_string(5),
	Vendor = random_string(20),
	DeviceSerial = random_string(15),
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	Type = random_string(7),
	SpecificationRecord = #specification{name = SpecificationName,
			description = Description,
			class_type = ClassType,
			version = Version,
			start_date = 1548720000000,
			end_date = 1577836740000,
			status = active,
			model = Model,
			part = Part,
			vendor = Vendor,
			device_serial = DeviceSerial,
			target_schema = #target_schema_ref{class_type = ClassType,
					schema = TargetSchema},
			party = [#party_ref{id = PartyId,
					href = PartyHref,
					role = "Supplier",
					name = "ACME Inc."}],
			related = [#specification_rel{id = PartyId,
					href = PartyHref,
					role = "Supplier",
					name = "ACME Inc.",
					class_type = Type,
					start_date = 1548720000000,
					end_date = 1577836740000}]},
	{ok, #specification{id = Id, href = Href}} = im:add_specification(SpecificationRecord),
	Accept = {"accept", "application/json"},
	Request = {HostUrl ++ Href, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{ok, SpecificationMap} = zj:decode(ResponseBody),
	#{"id" := Id, "href" := Href, "name" := SpecificationName,
			"description" := Description, "version" := Version,
			"@type" := ClassType, "targetResourceSchema" := T,
			"relatedParty" := [RP], "resourceSpecRelationship" := [R]}
			= SpecificationMap,
	true = is_target_ref(T),
	true = is_party_ref(RP),
	true = is_related_ref(R).

map_to_resource() ->
	[{userdata, [{doc, "Decode Resource map()"}]}].

map_to_resource(_Config) ->
	Id = random_string(12),
	Href = ?PathInventory ++ "resource/" ++ Id,
	Name = random_string(10),
	Description = random_string(25),
	PublicIdentifier = random_string(15),
	Version = random_string(3),
	ClassType = "LogicalResource",
	Schema = ?PathInventory ++ "schema/resourceInventoryManagement#/definitions/v3/schema/BssFunction",
	Category = random_string(5),
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	ResourceId = random_string(10),
	ResourceName = random_string(7),
	ResourceType = random_string(5),
	ResouceHref = ?PathInventory ++ "resource/" ++ ResourceId,
	CharValue = "gsm-bssfunction",
	Map = #{"id" => Id,
			"href" => Href,
			"name" => Name,
			"publicIdentifier" => PublicIdentifier,
			"description" => Description,
			"category" => Category,
			"validFor" => #{"startDateTime" => "2019-01-29T00:00",
					"endDateTime" => "2019-12-31T23:59"},
			"lifecycleState" => "Active",
			"lifecycleSubState" => "Reject",
			"@type" => ClassType,
			"@baseType" => "Resource",
			"@schemaLocation" => Schema,
			"version" => Version,
			"connectivity" => [#{"name" => "Resource graph",
					"connection" => [#{"name" => "Connectivity test",
							"associationType" => "Connectivity type",
							"endpoint" => [#{"id" => "123",
									"href" => "http://35.229.193.25:8088/eventManagement/v1/event",
									"name" => "Point name",
									"isRoot" => true,
									"connectionPoint" => [#{"id" => "321",
											"name" => "Connection point",
											"href" => "http://35.229.193.25:8088/eventManagement/v1/event"}]}]}]}],
			"resourceSpecification" => #{"id" => ResourceId,
					"href" => ResouceHref,
					"name" => ResourceName,
					"@type" => ResourceType,
					"version" => "1.1",
					"connectivitySpecification" => [#{"name" => "Connectivity spec test",
						"associationType" => "Connectivity spec type",
						"endpoint" => [#{"id" => "258",
							"href" => "http://35.229.193.25:8088/eventManagement/v1/event",
							"name" => "Point spec name",
							"role" => "Reversal",
							"isRoot" => true,
							"connectionPointSpecification" => [#{"id" => "963",
								"href" => "http://35.229.193.25:8088/eventManagement/v1/event",
								"name" => "Connection point spec"}]}]}],
					"connectionPointSpecification" => [#{"id" => "963",
						"href" => "http://35.229.193.25:8088/eventManagement/v1/event",
						"name" => "Connection point spec"}]},
			"relatedParty" => [#{"id" => PartyId,
					"href" => PartyHref,
					"role" => "Supplier",
					"name" => "ACME Inc.",
					"validFor" => #{"startDateTime" => "2019-01-29T00:00",
							"endDateTime" => "2019-12-31T23:59"}}],
			"resourceCharacteristic" => [#{"name" => "userLabel",
					"value" => CharValue}]},
	#resource{id = Id, href = Href, name = Name, public_id = PublicIdentifier,
			description = Description, category = Category, class_type = ClassType,
			schema = Schema, base_type = "Resource", state = "Active",
			substate = "Reject", version = Version, start_date = StartDate,
			end_date = EndDate, specification = S, party = [RP],
			characteristic = [C], connectivity = [End]} = im_rest_res_resource:resource(Map),
	true = is_integer(StartDate),
	true = is_integer(EndDate),
	#specification_ref{id = ResourceId, href = ResouceHref, name = ResourceName,
			version = "1.1"} = S,
	#party_ref{id = PartyId, href = PartyHref} = RP,
	#resource_char{name = "userLabel", value = CharValue} = C,
	#resource_graph{name = "Resource graph",
			connection = [#connection{name = "Connectivity test",
					endpoint = [#endpoint_ref{id = "123", name = "Point name",
					href = "http://35.229.193.25:8088/eventManagement/v1/event",
					is_root = true,
					connection_point = [#resource_ref{id = "321",
							name = "Connection point",
							href = "http://35.229.193.25:8088/eventManagement/v1/event"}]}]}]} = End.
	
resource_to_map() ->
	[{userdata, [{doc, "Encode Resource map()"}]}].

resource_to_map(_Config) ->
	Id = random_string(12),
	Href = ?PathInventory ++ "resource/" ++ Id,
	Name = random_string(10),
	PublicId = random_string(15),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "LogicalResource",
	Schema = ?PathInventory ++ "schema/resourceInventoryManagement#/definitions/resource",
	Category = random_string(5),
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	ResourceId = random_string(10),
	ResourceName = random_string(7),
	ResouceHref = ?PathInventory ++ "resource/" ++ ResourceId,
	CharValue = "gsm-bssfunction",
	ResourceRecord = #resource{id = Id,
			href = Href,
			name = Name,
			public_id = PublicId,
			description = Description,
			category = Category,
			class_type = ClassType,
			schema = Schema,
			base_type = "Resource",
			version = Version,
			start_date = 1548720000000,
			end_date = 1577836740000,
			state = "Active",
			substate = "Reject",
			connectivity = [#resource_graph{name = "Resource graph",
					connection = [#connection{name = "Connectivity test",
							endpoint = [#endpoint_ref{id = "123", name = "Point name",
							href = "http://35.229.193.25:8088/eventManagement/v1/event",
							is_root = true,
							connection_point = [#resource_ref{id = "321",
									name = "Connection point",
									href = "http://35.229.193.25:8088/eventManagement/v1/event"}]}]}]}],
			specification = #specification_ref{id = ResourceId,
					href = ResouceHref, name = ResourceName, version = "1.1"},
			party = [#party_ref{id = PartyId,
					href = PartyHref,
					role = "Supplier",
					name = "ACME Inc."}],
			characteristic = [#resource_char{name = "userLabel",
					value = CharValue}]},
	#{"id" := Id, "href" := Href, "name" := Name, "publicIdentifier" := PublicId,
			"description" := Description, "@type" := ClassType,
			"@schemaLocation" := Schema, "@baseType" := "Resource", "version" := Version,
			"validFor" := #{"startDateTime" := Start, "endDateTime" := End},
			"lifecycleState" := "Active", "lifecycleSubState" := "Reject",
			"category" := Category, "resourceSpecification" := S, "relatedParty" := [RP],
			"resourceCharacteristic" := [C],
			"connectivity" := [#{"connection" := [Con]}]}
			= im_rest_res_resource:resource(ResourceRecord),
	true = is_list(Start),
	true = is_list(End),
	#{"id" := ResourceId, "href" := ResouceHref, "name" := ResourceName,
			"version" := "1.1"} = S,
	#{"id" := PartyId, "href" := PartyHref} = RP,
	#{"name" := "userLabel", "value" := CharValue} = C,
	#{"name" := "Connectivity test", "endpoint" := [#{"id" := "123",
				"href" := "http://35.229.193.25:8088/eventManagement/v1/event",
				"name" := "Point name",
				"isRoot" := true,
				"connectionPoint" := [#{"id" := "321",
						"name" := "Connection point",
						"href" := "http://35.229.193.25:8088/eventManagement/v1/event"}]}]} = Con.

post_resource() ->
	[{userdata, [{doc, "POST to Resource collection"}]}].

post_resource(Config) ->
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathInventory ++ "resource",
	Name = random_string(10),
	PublicId = random_string(15),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "LogicalResource",
	ClassSchema = ?PathInventory ++ "schema/resourceInventoryManagement",
	Category = random_string(6),
	ResourceId = random_string(10),
	ResourceName = random_string(7),
	ResouceHref = ?PathInventory ++ "resource/" ++ ResourceId,
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	ResourceRelId = random_string(10),
	ResourceRelHref = ?PathInventory ++ "resource/" ++ ResourceRelId,
	ResourceRelName = random_string(20),
	CharValue = random_string(10),
	CharSchema = ?PathInventory ++ "schema/resourceInventoryManagement#/definitions/v3/schema/geranNrm#/definitions/BtsSiteMgrList",
	RequestBody = "{\n"
			++ "\t\"name\": \"" ++ Name ++ "\",\n"
			++ "\t\"publicIdentifier\": \"" ++ PublicId ++ "\",\n"
			++ "\t\"description\": \"" ++ Description ++ "\",\n"
			++ "\t\"category\": \"" ++ Category ++ "\",\n"
			++ "\t\"@type\": \"" ++ ClassType ++ "\",\n"
			++ "\t\"@schemaLocation\": \"" ++ ClassSchema ++ "\",\n"
			++ "\t\"@baseType\": \"Resource\",\n"
			++ "\t\"version\": \"" ++ Version ++ "\",\n"
			++ "\t\"validFor\": {\n"
			++ "\t\t\"startDateTime\": \"2019-01-29T00:00\",\n"
			++ "\t\t\"endDateTime\": \"2019-12-31T23:59\"\n"
			++ "\t},\n"
			++ "\t\"lifecycleState\": \"In Test\",\n"
			++ "\t\"resourceSpecification\": {\n"
			++ "\t\t\"id\": \"" ++ ResourceId ++ "\",\n"
			++ "\t\t\"href\": \"" ++ ResouceHref ++ "\",\n"
			++ "\t\t\"name\": \"" ++ ResourceName ++ "\",\n"
			++ "\t\t\"@type\": \"" ++ ClassType ++ "\",\n"
			++ "\t\t\"version\": \"" ++ Version ++ "\"\n"
			++ "\t\t},\n"
			++ "\t\"relatedParty\": [\n"
			++ "\t\t{\n"
			++ "\t\t\t\"id\": \"" ++ PartyId ++ "\",\n"
			++ "\t\t\t\"href\": \"" ++ PartyHref ++ "\",\n"
			++ "\t\t\t\"role\": \"Supplier\",\n"
			++ "\t\t\t\"name\": \"ACME Inc.\",\n"
			++ "\t\t\t\"validFor\": {\n"
			++ "\t\t\t\t\"startDateTime\": \"2019-01-29T00:00\",\n"
			++ "\t\t\t\t\"endDateTime\": \"2019-12-31T23:59\"\n"
			++ "\t\t\t}\n"
			++ "\t\t}\n"
			++ "\t],\n"
			++ "\t\"resourceRelationship\": [\n"
			++ "\t\t{\n"
			++ "\t\t\t\"resource\": {\n"
			++ "\t\t\t\t\"id\": \"" ++ ResourceRelId ++ "\",\n"
			++ "\t\t\t\t\"href\": \"" ++ ResourceRelHref ++ "\",\n"
			++ "\t\t\t\t\"@referredType\": \"ResourceFunction\",\n"
			++ "\t\t\t\t\"name\": \"" ++ ResourceRelName ++ "\"\n"
			++ "\t\t\t},\n"
			++ "\t\t\t\"relationshipType\": \"contains\"\n"
			++ "\t\t}\n"
			++ "\t],\n"
			++ "\t\"resourceCharacteristic\": [\n"
			++ "\t\t{\n"
			++ "\t\t\t\"name\": \"BtsSiteMgr\",\n"
			++ "\t\t\t\"value\": \"" ++ CharValue ++ "\",\n"
			++ "\t\t\t\"@type\": \"BtsSiteMgrList\",\n"
			++ "\t\t\t\"@schemaLocation\": \"" ++ CharSchema ++ "\"\n"
			++ "\t\t}\n"
			++ "\t]\n"
			++ "}\n",
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, URI} = lists:keyfind("location", 1, Headers),
	{?PathInventory ++ "resource/" ++ ID, _} = httpd_util:split_path(URI),
	{ok, #resource{id = ID, name = Name, public_id = PublicId,
			description = Description, version = Version, category = Category,
			class_type = ClassType, base_type = "Resource",
			schema = ClassSchema, specification = S, party = [RP],
			related = [Rel], characteristic = [C]}} = im:get_resource(ID),
	#specification_ref{id = ResourceId, href = ResouceHref, name = ResourceName,
			version = Version} = S,
	#party_ref{id = PartyId, href = PartyHref} = RP,
	#resource_char{name = "BtsSiteMgr", class_type = "BtsSiteMgrList",
			schema = CharSchema, value = CharValue} = C,
	#resource_rel{id = ResourceRelId, href = ?PathInventory ++ "resource/"
			++ ResourceRelId, name = ResourceRelName,
			ref_type = "ResourceFunction", rel_type = "contains"} = Rel.

get_resources() ->
	[{userdata, [{doc, "GET Resource collection"}]}].

get_resources(Config) ->
	ok = fill_resource(5),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathInventory ++ "resource",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{ok, Resources} = zj:decode(ResponseBody),
	false = is_empty(Resources),
	true = lists:all(fun is_resource/1, Resources).

get_resource() ->
	[{userdata, [{doc, "GET inventory Resource"}]}].

get_resource(Config) ->
	HostUrl = ?config(host_url, Config),
	Name = random_string(10),
	PublicId = random_string(15),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "LogicalResource",
	Schema = ?PathInventory ++ "schema/resourceInventoryManagement#/definitions/LogicalResource",
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	ResourceId = random_string(10),
	ResourceName = random_string(7),
	ResouceHref = ?PathInventory ++ "resource/" ++ ResourceId,
	ResourceRelId = random_string(10),
	ResourceRelHref = ?PathInventory ++ "resource/" ++ ResourceRelId,
	ResourceRelName = random_string(15),
	CharValue = random_string(10),
	CharSchema = ?PathInventory ++ "schema/resourceInventoryManagement#/definitions/v3/schema/geranNrm#/definitions/BtsSiteMgrList",
	ResourceRecord = #resource{name = Name,
			public_id = PublicId,
			description = Description,
			class_type = ClassType,
			schema = Schema,
			base_type = "Resource",
			version = Version,
			start_date = 1548720000000,
			end_date = 1577836740000,
			state = "Active",
			specification = #specification_ref{id = ResourceId,
					href = ResouceHref, name = ResourceName, version = "1.1"},
			party = [#party_ref{id = PartyId,
					href = PartyHref,
					role = "Supplier",
					name = "ACME Inc."}],
			related = [#resource_rel{id = ResourceRelId, href = ResourceRelHref,
					name = ResourceRelName, ref_type = "ResourceFunction",
					rel_type = "contains"}],
			characteristic = [#resource_char{name = "BtsSiteMgr",
					class_type = "BtsSiteMgrList",
					value = CharValue,
					schema = CharSchema}]},
	{ok, #resource{id = Id, href = Href}} = im:add_resource(ResourceRecord),
	Accept = {"accept", "application/json"},
	Request = {HostUrl ++ Href, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{ok, ResourceMap} = zj:decode(ResponseBody),
	#{"id" := Id, "href" := Href, "name" := Name, "publicIdentifier" := PublicId,
			"description" := Description, "version" := Version,
			"@type" := ClassType, "@baseType" := "Resource",
			"@schemaLocation" := Schema, "resourceSpecification" := S,
			"relatedParty" := [RP], "resourceRelationship" :=
					[#{"resource" := #{"id" := ResourceRelId,
							"href" := ResourceRelHref, "name" := ResourceRelName,
							"@referredType" := "ResourceFunction"},
					"relationshipType" := "contains"}],
			"resourceCharacteristic" := [C]} = ResourceMap,
	true = is_resource_spec(S),
	true = is_party_ref(RP),
	true = is_resource_char(C).

geoaxis() ->
	[{userdata, [{doc, "CODEC for latitude/longitude"}]}].

geoaxis(_Config) ->
	"-180" = im_rest:geoaxis(-1800000),
	"90" = im_rest:geoaxis(900000),
	"0" = im_rest:geoaxis(0),
	"0.0001" = im_rest:geoaxis(1),
	"0.001" = im_rest:geoaxis(10),
	"0.01" = im_rest:geoaxis(100),
	"0.1" = im_rest:geoaxis(1000),
	"1" = im_rest:geoaxis(10000),
	"-0.0001" = im_rest:geoaxis(-1),
	"-0.001" = im_rest:geoaxis(-10),
	"-0.01" = im_rest:geoaxis(-100),
	"-0.1" = im_rest:geoaxis(-1000),
	"-1" = im_rest:geoaxis(-10000),
	123456 = im_rest:geoaxis("12.3456"),
	123450 = im_rest:geoaxis("12.345"),
	123400 = im_rest:geoaxis("12.34"),
	123000 = im_rest:geoaxis("12.3"),
	120000 = im_rest:geoaxis("12"),
	-123456 = im_rest:geoaxis("-12.3456"),
	-123450 = im_rest:geoaxis("-12.345"),
	-123400 = im_rest:geoaxis("-12.34"),
	-123000 = im_rest:geoaxis("-12.3"),
	-120000 = im_rest:geoaxis("-12"),
	Pos = rand:uniform(1800000),
	Pos = im_rest:geoaxis(im_rest:geoaxis(Pos)),
	Neg = -(rand:uniform(1800000)),
	Neg = im_rest:geoaxis(im_rest:geoaxis(Neg)).

query_category() ->
	[{userdata, [{doc, "Query category entry in category table"}]}].

query_category(Config) ->
	F = fun F(0, Acc) ->
					Acc;
			F(N, Acc) ->
				Category = #category{name = get_name(rand:uniform(3)),
						class_type = "ResourceCategory", base_type = "Category",
						start_date = erlang:system_time(?SECOND),
						end_date = erlang:system_time(?SECOND)},
				{ok, C} = im:add_category(Category),
				F(N - 1, [C | Acc])
	end,
	Categories = F(rand:uniform(100), []),
	#category{id = Id, name = Name} = lists:nth(rand:uniform(length(Categories)), Categories),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Query = "id=" ++ Id ++ "&name=" ++ Name,
	Request = {HostUrl ++ ?PathCatalog ++ "/resourceCategory?" ++ Query,
			[Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{ok, ResourceMap} = zj:decode(ResponseBody),
	[#{"id" := Id, "name" := Name, "@type" := "ResourceCategory",
			"@baseType" := "Category"}] = ResourceMap.

advanced_query_category() ->
	[{userdata, [{doc, "Query category entry in category table using advanced query"}]}].

advanced_query_category(Config) ->
	F = fun F(0, Acc) ->
					Acc;
			F(N, Acc) ->
				Category = #category{name = get_name(rand:uniform(3)),
						class_type = "ResourceCategory", base_type = "Category",
						start_date = erlang:system_time(?SECOND),
						end_date = erlang:system_time(?SECOND)},
				{ok, C} = im:add_category(Category),
				F(N - 1, [C | Acc])
	end,
	Categories = F(rand:uniform(100), []),
	F2 = fun(#category{name = "RAN"}) ->
				true;
			(_) ->
				false
	end,
	FilteredCategories = lists:filter(F2, Categories),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Filter = "?filter=%22%5B%7Bname=RAN%7D%5D%22",
	Request = {HostUrl ++ ?PathCatalog ++ "resourceCategory" ++ Filter,
			[Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{ok, ResourceMap} = zj:decode(ResponseBody),
	[#{"name" := "RAN", "@type" := "ResourceCategory",
			"@baseType" := "Category"} | _] = ResourceMap,
	length(FilteredCategories) == length(ResourceMap).

query_candidate() ->
	[{userdata, [{doc, "Query candidate entry in candidate table"}]}].

query_candidate(Config) ->
	F = fun F(0, Acc) ->
					Acc;
			F(N, Acc) ->
				Candidate = #candidate{name = get_name(rand:uniform(3)),
						class_type = "ResourceCandidate", base_type = "Candidate",
						start_date = erlang:system_time(?SECOND),
						end_date = erlang:system_time(?SECOND)},
				{ok, C} = im:add_candidate(Candidate),
				F(N - 1, [C | Acc])
	end,
	Candidates = F(rand:uniform(100), []),
	#candidate{id = Id, name = Name} = lists:nth(rand:uniform(length(Candidates)), Candidates),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Query = "id=" ++ Id ++ "&name=" ++ Name,
	Request = {HostUrl ++ ?PathCatalog ++ "resourceCandidate?" ++ Query,
			[Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{ok, ResourceMap} = zj:decode(ResponseBody),
	[#{"id" := Id, "name" := Name, "@type" := "ResourceCandidate",
			"@baseType" := "Candidate"}] = ResourceMap.

advanced_query_candidate() ->
	[{userdata, [{doc, "Query candidate entry in candidate table using advanced query"}]}].

advanced_query_candidate(Config) ->
	F = fun F(0, Acc) ->
					Acc;
			F(N, Acc) ->
				Candidate = #candidate{name = get_name(rand:uniform(3)),
						class_type = "ResourceCandidate", base_type = "Candidate",
						start_date = erlang:system_time(?SECOND),
						end_date = erlang:system_time(?SECOND)},
				{ok, C} = im:add_candidate(Candidate),
				F(N - 1, [C | Acc])
	end,
	Candidates = F(rand:uniform(100), []),
	F2 = fun(#candidate{name = "EPC"}) ->
				true;
			(_) ->
				false
	end,
	FilteredCandidates = lists:filter(F2, Candidates),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Filter = "?filter=%22%5B%7Bname=EPC%7D%5D%22",
	Request = {HostUrl ++ ?PathCatalog ++ "resourceCandidate" ++ Filter,
			[Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{ok, ResourceMap} = zj:decode(ResponseBody),
	[#{"name" := "EPC", "@type" := "ResourceCandidate",
			"@baseType" := "Candidate"} | _] = ResourceMap,
	length(FilteredCandidates) == length(ResourceMap).

query_catalog() ->
	[{userdata, [{doc, "Query catalog entry in catalog table"}]}].

query_catalog(Config) ->
	F = fun F(0, Acc) ->
					Acc;
			F(N, Acc) ->
				Catalog = #catalog{name = get_name(rand:uniform(3)),
						class_type = "ResourceCatalog", base_type = "Catalog",
						start_date = erlang:system_time(?SECOND),
						end_date = erlang:system_time(?SECOND)},
				{ok, C} = im:add_catalog(Catalog),
				F(N - 1, [C | Acc])
	end,
	Catalogs = F(rand:uniform(100), []),
	#catalog{id = Id, name = Name}
			= lists:nth(rand:uniform(length(Catalogs)), Catalogs),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Query = "id=" ++ Id ++ "&name=" ++ Name,
	Request = {HostUrl ++ ?PathCatalog ++ "resourceCatalog?" ++ Query,
			[Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{ok, ResourceMap} = zj:decode(ResponseBody),
	[#{"id" := Id, "name" := Name, "@type" := "ResourceCatalog",
			"@baseType" := "Catalog"}] = ResourceMap.

advanced_query_catalog() ->
	[{userdata, [{doc, "Query catalog entry in catalog table using advanced query"}]}].

advanced_query_catalog(Config) ->
	F = fun F(0, Acc) ->
					Acc;
			F(N, Acc) ->
				Catalog = #catalog{name = get_name(rand:uniform(3)),
						class_type = "ResourceCatalog", base_type = "Catalog",
						start_date = erlang:system_time(?SECOND),
						end_date = erlang:system_time(?SECOND)},
				{ok, C} = im:add_catalog(Catalog),
				F(N - 1, [C | Acc])
	end,
	Catalogs = F(rand:uniform(100), []),
	F2 = fun(#catalog{name = "Core"}) ->
				true;
			(_) ->
				false
	end,
	FilteredCatalogs = lists:filter(F2, Catalogs),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Filter = "?filter=%22%5B%7Bname=Core%7D%5D%22",
	Request = {HostUrl ++ ?PathCatalog ++ "resourceCatalog" ++ Filter,
			[Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	{ok, ResourceMap} = zj:decode(ResponseBody),
	[#{"name" := "Core", "@type" := "ResourceCatalog",
			"@baseType" := "Catalog"} | _] = ResourceMap,
	length(FilteredCatalogs) == length(ResourceMap).

oauth_authentication()->
	[{userdata, [{doc, "Authenticate a JWT using oauth"}]}].

oauth_authentication(Config)->
	ID = "cornflakes",
	Locale = "es",
	{ok, _} = im:add_user(ID, "", Locale),
	ok = application:set_env(im, oauth_issuer, "joe"),
	ok = application:set_env(im, oauth_audience, "network-subscriber.sigscale-im"),
	HostUrl = ?config(host_url, Config),
	Accept = {"accept", "application/json"},
	Header = "{\n"
			++ "\t\"alg\": \"RS256\",\n"
			++ "\t\"typ\": \"JWT\"\n"
			++ "}",
	Expiry = os:system_time(seconds) + 86400,
	Payload = zj:encode(#{"iss" => "joe",
			"exp" => Expiry,
			"email" => "cornflakes",
			"aud" => [
					#{"network-subscriber.sigscale-im" => "account"}
			],
			"preferred_username" => "flakes"}),
	EncodedHeader = encode_base64url(Header),
	EncodedPayload = encode_base64url(Payload),
	Path = ?config(data_dir, Config),
	KeyPath = Path ++ "key.pem",
	{ok, PrivBin} = file:read_file(KeyPath),
	[RSAPrivEntry] = public_key:pem_decode(PrivBin),
	Key = public_key:pem_entry_decode(RSAPrivEntry),
	M = Key#'RSAPrivateKey'.modulus,
	E = Key#'RSAPrivateKey'.publicExponent,
	RSAPublicKey = #'RSAPublicKey'{modulus = M, publicExponent = E},
	PemEntry = public_key:pem_entry_encode('RSAPublicKey', RSAPublicKey),
	PemBin = public_key:pem_encode([PemEntry]),
	file:write_file(Path ++ "pub.pem", PemBin),
	Msg = list_to_binary(EncodedHeader ++ "." ++ EncodedPayload),
	Signature = public_key:sign(Msg, sha256, Key),
	EncodedSignature = encode_base64url(binary_to_list(Signature)),
	AuthKey = "Bearer " ++ EncodedHeader ++ "." ++ EncodedPayload ++ "." ++ EncodedSignature,
	Authentication = {"authorization", AuthKey},
	Request = {HostUrl, [Accept, Authentication]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _}, _, _} = Result.

post_hub_role() ->
	[{userdata, [{doc, "Register hub listener for role"}]}].

post_hub_role(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathRole ++ "hub/",
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = zj:encode(#{"callback" => Callback}),
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, Result} = httpc:request(post, Request, [], []),
	{{"HTTP/1.1", 201, _Created}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{_, Location} = lists:keyfind("location", 1, Headers),
	Id = string:substr(Location, string:rstr(Location, PathHub) + length(PathHub)),
	{ok, #{"id" := Id, "callback" := Callback}} = zj:decode(ResponseBody).

delete_hub_role() ->
	[{userdata, [{doc, "Unregister hub listener for role"}]}].

delete_hub_role(Config) ->
	HostUrl = ?config(host_url, Config),
	PathHub = ?PathRole ++ "hub/",
	CollectionUrl = HostUrl ++ PathHub,
	Callback = "http://in.listener.com",
	RequestBody = zj:encode(#{"callback" => Callback}),
	ContentType = "application/json",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()], ContentType, RequestBody},
	{ok, {{_, 201, _}, _, ResponseBody}} = httpc:request(post, Request, [], []),
	{ok, #{"id" := Id, "callback" := Callback}} = zj:decode(ResponseBody),
	Request1 = {HostUrl ++ PathHub ++ Id, [Accept, auth_header()]},
	{ok, {{_, 204, _}, _, []}} = httpc:request(delete, Request1, [], []).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

is_empty([]) ->
	true;
is_empty(_) ->
	false.

random_string(Length) ->
	Charset = lists:seq($a, $z),
	NumChars = length(Charset),
	Random = crypto:strong_rand_bytes(Length),
	random_string(Random, Charset, NumChars,[]).
random_string(<<N, Rest/binary>>, Charset, NumChars, Acc) ->
	CharNum = (N rem NumChars) + 1,
	NewAcc = [lists:nth(CharNum, Charset) | Acc],
	random_string(Rest, Charset, NumChars, NewAcc);
random_string(<<>>, _Charset, _NumChars, Acc) ->
	Acc.

basic_auth() ->
	RestUser = ct:get_config(rest_user),
	RestPass = ct:get_config(rest_pass),
	EncodeKey = base64:encode_to_string(RestUser ++ ":" ++ RestPass),
	"Basic " ++ EncodeKey.

auth_header() ->
	{"authorization", basic_auth()}.

is_party_ref(#{"id" := Id, "href" := Href,
		"name" := Name, "role" := Role}) when is_list(Id),
		is_list(Href), is_list(Name), is_list(Role) ->
	true;
is_party_ref(_RP) ->
	false.

is_category_ref(#{"id" := Id, "href" := Href,
		"name" := Name, "version" := Version})
		when is_list(Id), is_list(Href),
		is_list(Name), is_list(Version) ->
	true;
is_category_ref(_) ->
	false.

is_candidate_ref(#{"id" := Id, "href" := Href,
		"name" := Name, "version" := Version})
		when is_list(Id), is_list(Href),
		is_list(Name), is_list(Version) ->
	true;
is_candidate_ref(_) ->
	false.

is_specification_ref(#{"id" := Id, "href" := Href,
		"name" := Name, "version" := Version})
		when is_list(Id), is_list(Href),
		is_list(Name), is_list(Version) ->
	true;
is_specification_ref(_) ->
	false.

is_target_ref(#{"@type" := Type, "@schemaLocation" := Schema})
		when is_list(Type), is_list(Schema) ->
	true;
is_target_ref(_) ->
	false.

is_spec_char(#{"name" := Name, "description" := Des, "valueType" := ValType})
		when is_list(Name), is_list(Des), is_list(ValType) ->
	true;
is_spec_char(_C) ->
	false.

is_related_ref(#{"id" := Id, "href" := Href,
		"name" := Name, "role" := Role,
		"validFor" := #{"startDateTime" := Start,
		"endDateTime" := End}}) when is_list(Id),
		is_list(Href), is_list(Name), is_list(Role),
		is_list(Start), is_list(End) ->
	im_rest:iso8601(End) > im_rest:iso8601(Start);
is_related_ref(_R) ->
	false.

is_resource_spec(#{"id" := ResourceId, "href" := ResouceHref,
		"name" := ResourceName, "version" := Version})
		when is_list(ResourceId), is_list(ResouceHref),
		is_list(ResourceName), is_list(Version) ->
	true;
is_resource_spec(_) ->
	false.

is_resource_char(#{"name" := Name, "value" := Value,
		"@type" := Type, "@schemaLocation" := CharSchema})
		when is_list(Name), is_list(Value),
		is_list(Type), is_list(CharSchema) ->
	true;
is_resource_char(_) ->
	false.

is_catalog(#{"id" := Id, "href" := Href, "name" := Name,
		"description" := Description, "version" := Version,
		"@type" := ClassType, "@baseType" := "Catalog",
		"@schemaLocation" := Schema, "relatedParty" := RelatedParty,
		"category" := Category}) when is_list(Id),
		is_list(Href), is_list(Name), is_list(Description),
		is_list(Version), is_list(ClassType), is_list(Schema),
		is_list(RelatedParty) ->
	lists:all(fun is_party_ref/1, RelatedParty),
	lists:all(fun is_category_ref/1, Category);
is_catalog(#{"id" := Id, "href" := Href, "name" := Name,
		"description" := Description, "version" := Version,
		"@type" := ClassType, "category" := Category}) when is_list(Id),
		is_list(Href), is_list(Name), is_list(Description),
		is_list(Version), is_list(ClassType), is_list(Category) ->
	lists:all(fun is_category_ref/1, Category);
is_catalog(_) ->
	false.

is_category(#{"id" := Id, "href" := Href, "name" := Name,
		"description" := Description, "version" := Version,
		"@type" := ClassType, "@baseType" := "Category",
		"@schemaLocation" := Schema, "parentId" := Parent, "isRoot" := Bool,
		"relatedParty" := RelatedParty, "resourceCandidate" := Candidate})
		when is_list(Id), is_list(Href), is_list(Name), is_list(Description),
		is_list(Version), is_list(ClassType), is_list(Schema),
		is_list(Parent), is_list(RelatedParty), is_list(Candidate),
		is_boolean(Bool) ->
	lists:all(fun is_party_ref/1, RelatedParty),
	lists:all(fun is_candidate_ref/1, Candidate);
is_category(#{"id" := Id, "href" := Href, "name" := Name,
		"description" := Description, "version" := Version,
		"@type" := ClassType, "resourceCandidate" := Candidate})
		when is_list(Id), is_list(Href), is_list(Name), is_list(Description),
		is_list(Version), is_list(ClassType), is_list(Candidate) ->
	lists:all(fun is_candidate_ref/1, Candidate);
is_category(_C) ->
	false.

is_candidate(#{"id" := Id, "href" := Href, "name" := Name,
		"description" := Description, "version" := Version,
		"@type" := ClassType, "@baseType" := "Candidate",
		"@schemaLocation" := Schema, "category" := Category,
		"resourceSpecification" := Specification}) when is_list(Id), is_list(Href),
		is_list(Name), is_list(Description), is_list(Version),
		is_list(ClassType), is_list(Schema), is_list(Category),
		is_map(Specification) ->
	lists:all(fun is_category_ref/1, Category),
	is_specification_ref(Specification);
is_candidate(#{"id" := Id, "href" := Href, "name" := Name,
		"description" := Description, "version" := Version,
		"@type" := ClassType, "category" := Category,
		"resourceSpecification" := Specification}) when is_list(Id), is_list(Href),
		is_list(Name), is_list(Description), is_list(Version),
		is_list(ClassType), is_list(Category),
		is_map(Specification) ->
	lists:all(fun is_category_ref/1, Category),
	is_specification_ref(Specification);
is_candidate(_) ->
	false.

is_specification(#{"id" := Id, "href" := Href, "name" := Name,
		"description" := Description, "version" := Version, "@type" := ClassType,
		"targetResourceSchema" := T, "resourceSpecCharacteristic" := Chars})
		when is_list(Id), is_list(Href), is_list(Name), is_list(Description),
		is_list(Version), is_list(ClassType), is_list(Chars) ->
	true = is_target_ref(T),
	lists:all(fun is_spec_char/1, Chars);
is_specification(#{"id" := Id, "href" := Href, "name" := Name,
		"description" := Description, "version" := Version, "@type" := ClassType,
		"targetResourceSchema" := T}) when is_list(Id), is_list(Href),
		is_list(Name), is_list(Description),
		is_list(Version), is_list(ClassType) ->
	is_target_ref(T);
is_specification(_S) ->
	false.

is_resource(#{"id" := Id, "publicIdentifier" := PublicId, "name" := Name,
		"description" := Description, "version" := Version,
		"@type" := ClassType, "@baseType" := "Resource",
		"@schemaLocation" := Schema, "resourceSpecification" := S,
		"relatedParty" := RelatedParty, "resourceCharacteristic" := Char})
		when is_list(Id), is_list(PublicId), is_list(Name), is_list(Description),
		is_list(Version), is_list(ClassType), is_list(Schema),
		is_list(RelatedParty), is_list(Char) ->
	true = is_resource_spec(S);
is_resource(#{"id" := Id, "href" := Href, "name" := Name,
		"description" := Description, "category" := Category,
		"version" := Version, "@type" := ClassType, "resourceSpecification" := S})
		when is_list(Id), is_list(Href), is_list(Name), is_list(Description),
		is_list(Version), is_list(ClassType), is_list(Category) ->
	true = is_resource_spec(S);
is_resource(_) ->
	false.

fill_catalog(0) ->
	ok;
fill_catalog(N) ->
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceCatalog",
	Catalog = #catalog{name = random_string(10),
			description = random_string(25),
			class_type = "ResourceCatalog",
			schema = Schema,
			base_type = "Catalog",
			version = random_string(3),
			start_date = 1548720000000,
			end_date = 1577836740000,
			status = active,
			party = fill_party(3),
			category = fill_category_ref(5)},
	{ok, _} = im:add_catalog(Catalog),
	fill_catalog(N - 1).

fill_category(0) ->
	ok;
fill_category(N) ->
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceCategory",
	Category = #category{name = random_string(10),
			description = random_string(25),
			class_type = "ResourceCategory",
			schema = Schema,
			base_type = "Category",
			version = random_string(3),
			start_date = 1548720000000,
			end_date = 1577836740000,
			status = active,
			parent = random_string(10),
			root = true,
			party = fill_party(3),
			candidate = fill_candidate_ref(5)},
	{ok, _} = im:add_category(Category),
	fill_category(N - 1).

fill_candidate(0) ->
	ok;
fill_candidate(N) ->
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceCandidate",
	Id = random_string(10),
	Href = ?PathParty ++ "organization/" ++ Id,
	Candidate = #candidate{name = random_string(10),
			description = random_string(25),
			class_type = "ResourceCandidate",
			schema = Schema,
			base_type = "Candidate",
			version = random_string(3),
			start_date = 1548720000000,
			end_date = 1577836740000,
			status = active,
			category = fill_category_ref(5),
			specification = #specification_ref{id = Id, href = Href,
					name = random_string(10), version = random_string(3)}},
	{ok, _} = im:add_candidate(Candidate),
	fill_candidate(N - 1).

fill_specification(0) ->
	ok;
fill_specification(N) ->
	Schema = ?PathCatalog ++ "schema/resourceCatalogManagement#/definitions/ResourceSpecification",
	Specification = #specification{name = random_string(10),
			description = random_string(25),
			class_type = "ResourceFunctionSpecification",
			version = random_string(3),
			start_date = 1548720000000,
			end_date = 1577836740000,
			status = active,
			model = random_string(5),
			part = random_string(5),
			vendor = random_string(20),
			device_serial = random_string(15),
			target_schema = #target_schema_ref{class_type = "ResourceSpecification",
					schema = Schema},
			party = fill_party(3),
			related = fill_related_ref(3),
			characteristic = fill_spec_char(4)},
	{ok, _} = im:add_specification(Specification),
	fill_specification(N - 1).

fill_resource(0) ->
	ok;
fill_resource(N) ->
	Schema = ?PathInventory ++ "schema/resourceInventoryManagement#/definitions/resource",
	Version = random_string(3),
	Resource = #resource{name = random_string(10),
			public_id = random_string(15),
			description = random_string(25),
			class_type = "LogicalResource",
			base_type = "Resource",
			schema = Schema,
			version = random_string(3),
			start_date = 1548720000000,
			end_date = 1577836740000,
			state = "Active",
			specification = #specification_ref{id = random_string(10),
					href = random_string(25), name = random_string(10),
					version = Version},
			party = fill_party(3),
			characteristic  = fill_resource_char(3)},
	{ok, _} = im:add_resource(Resource),
	fill_resource(N - 1).

fill_party(N) ->
	fill_party(N, []).
fill_party(0, Acc) ->
	Acc;
fill_party(N, Acc) ->
	Id = random_string(10),
	Href = ?PathParty ++ "organization/" ++ Id,
	RelatedParty = #party_ref{id = Id, href = Href,
			role = "Supplier", name = "ACME Inc."},
	fill_party(N - 1, [RelatedParty | Acc]).

fill_category_ref(N) ->
	fill_category_ref(N, []).
fill_category_ref(0, Acc) ->
	Acc;
fill_category_ref(N, Acc) ->
	Id = random_string(10),
	Href = ?PathParty ++ "organization/" ++ Id,
	Category = #category_ref{id = Id, href = Href,
			name = random_string(10), version = random_string(3)},
	fill_category_ref(N - 1, [Category | Acc]).

fill_candidate_ref(N) ->
	fill_candidate_ref(N, []).
fill_candidate_ref(0, Acc) ->
	Acc;
fill_candidate_ref(N, Acc) ->
	Id = random_string(10),
	Href = ?PathParty ++ "organization/" ++ Id,
	Candidate = #candidate_ref{id = Id, href = Href,
			name = random_string(10), version = random_string(3)},
	fill_candidate_ref(N - 1, [Candidate | Acc]).

fill_related_ref(N) ->
	fill_related_ref(N, []).
fill_related_ref(0, Acc) ->
	Acc;
fill_related_ref(N, Acc) ->
	Id = random_string(10),
	Type = random_string(5),
	Href = ?PathParty ++ "organization/" ++ Id,
	Related = #specification_rel{id = Id, href = Href,
			role = "Supplier", name = "ACME Inc.", class_type = Type,
	start_date = 1548720000000, end_date = 1577836740000},
	fill_related_ref(N - 1, [Related | Acc]).

fill_spec_char(N) ->
	fill_spec_char(N, []).
fill_spec_char(0, Acc) ->
	Acc;
fill_spec_char(N, Acc) ->
	SpecChar = #specification_char{name = random_string(10),
			description = random_string(20), value_type = random_string(5)},
	fill_spec_char(N - 1, [SpecChar | Acc]).

fill_resource_char(N) ->
	fill_resource_char(N, []).
fill_resource_char(0, Acc) ->
	Acc;
fill_resource_char(N, Acc) ->
	CharSchema = ?PathInventory ++ "schema/resourceInventoryManagement#/definitions/v3/schema/geranNrm#/definitions/BtsSiteMgrList",
	Characteristic = #resource_char{name = random_string(10),
			class_type = random_string(5),
			schema = CharSchema, value = random_string(15)},
	fill_resource_char(N - 1, [Characteristic | Acc]).

get_name(1) ->
	"RAN";
get_name(2) ->
	"EPC";
get_name(3) ->
	"Core".

-spec encode_base64url(Value) -> EncodedValue
	when
		Value :: string(),
		EncodedValue :: list().
%% @doc Encode a value using base64url encoding.
encode_base64url(Value)
		when is_list(Value) ->
	EncodedValue = base64:encode_to_string(Value),
	StrippedValue = string:strip(EncodedValue, both, $=),
	sub_chars_en(StrippedValue, []).

%% @hidden
sub_chars_en([$/ | T], Acc) ->
	sub_chars_en(T, [$_ | Acc]);
sub_chars_en([$+ | T], Acc) ->
	sub_chars_en(T, [$- | Acc]);
sub_chars_en([H | T], Acc) ->
	sub_chars_en(T, [H | Acc]);
sub_chars_en([], Acc) ->
	lists:reverse(Acc).

set_inet_mod() ->
	{ok, EnvObj} = application:get_env(inets, services),
	[{httpd, Services}] = EnvObj,
	NewModTuple = replace_mod(lists:keyfind(modules, 1, Services), []),
	NewServices = lists:keyreplace(modules, 1, Services, NewModTuple),
	ok = application:set_env(inets, services, [{httpd, NewServices}]).

replace_mod({modules, Mods}, Acc) ->
	replace_mod1(Mods, Acc).
%% @hidden
replace_mod1([mod_auth | T], Acc) ->
	replace_mod1(T, [mod_oauth | Acc]);
replace_mod1([mod_oauth | T], Acc) ->
	replace_mod1(T, [mod_auth | Acc]);
replace_mod1([H | T], Acc) ->
	replace_mod1(T, [H | Acc]);
replace_mod1([], Acc) ->
	{modules, lists:reverse(Acc)}.

%% @hidden
party_role(RoleName) ->
	RoleType = "PartyRole",
	StartDate = "2021-08-17T00:00Z",
	EndDate = "2022-12-31T00:00Z",
	#{"@type" => RoleType,
		"name" => RoleName,
		"validFor" => #{
				"startDateTime" => StartDate,
				"endDateTime" => EndDate
		}
	}.

%% @hidden
is_role(#{"id" := Id, "href" := Href, "name" := Name,
		"@type" := RoleType, "validFor" := #{"startDateTime" := SD,
		"endDateTime" := ED}}) when is_list(Id),
		is_list(Href), is_list(Name), is_list(RoleType),
		is_list(SD), is_list(ED) ->
	true;
is_role(_) ->
	false.
