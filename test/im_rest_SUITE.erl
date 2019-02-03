%%% im_rest_SUITE.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018-2019 SigScale Global Inc.
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
-copyright('Copyright (c) 2018-2019 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

-include("im.hrl").
-include_lib("common_test/include/ct.hrl").

-define(PathCatalog, "/resourceCatalogManagement/v3/").
-define(PathInventory, "/resourceInventoryManagement/v3/").
-define(PathFunction, "/resourceFunctionActivationConfiguration/v2/").
-define(PathParty, "/partyManagement/v2/").

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
init_per_testcase(_TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
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
	[map_to_catalog, catalog_to_map, post_catalog, get_catalogs, get_catalog].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

map_to_catalog() ->
	[{userdata, [{doc, "Decode Catalog map()"}]}].

map_to_catalog(_Config) ->
	CatalogId = random_string(12),
	CatalogHref = ?PathCatalog ++ "catalog/" ++ CatalogId,
	CatalogName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceCatalog",
	Schema = ?PathCatalog ++ "schema/swagger.json#/definitions/ResourceCatalog",
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	CategoryId = random_string(10),
	CategoryHref = ?PathCatalog ++ "category/" ++ CategoryId,
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
			status = active, related_party = [RP],
			category = [C]} = im_rest_res_catalog:catalog(Map),
	true = is_integer(StartDate),
	true = is_integer(EndDate),
	#related_party_ref{id = PartyId, href = PartyHref} = RP,
	#category_ref{id = CategoryId, href = CategoryHref,
			name = CategoryName, version = Version} = C.

catalog_to_map() ->
	[{userdata, [{doc, "Encode Catalog map()"}]}].

catalog_to_map(_Config) ->
	CatalogId = random_string(12),
	CatalogHref = ?PathCatalog ++ "catalog/" ++ CatalogId,
	CatalogName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceCatalog",
	Schema = ?PathCatalog ++ "schema/swagger.json#/definitions/ResourceCatalog",
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	CategoryId = random_string(10),
	CategoryHref = ?PathCatalog ++ "category/" ++ CategoryId,
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
			related_party = [#related_party_ref{id = PartyId,
					href = PartyHref,
					role = "Supplier",
					name = "ACME Inc.",
					start_date = 1548720000000,
					end_date = 1577836740000}],
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
	CollectionUrl = HostUrl ++ ?PathCatalog ++ "catalog",
	CatalogName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceCatalog",
	Schema = ?PathCatalog ++ "schema/swagger.json#/definitions/ResourceCatalog",
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	CategoryId = random_string(10),
	CategoryHref = ?PathCatalog ++ "category/" ++ CategoryId,
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
	{?PathCatalog ++ "catalog/" ++ ID, _} = httpd_util:split_path(URI),
	{ok, #catalog{id = ID, name = CatalogName,
			description = Description, version = Version,
			class_type = ClassType, base_type = "Catalog",
			schema = Schema, related_party = [RP],
			category = [C]}} = im:get_catalog(ID),
	#related_party_ref{id = PartyId, href = PartyHref} = RP,
	#category_ref{id = CategoryId, href = CategoryHref,
			name = CategoryName, version = Version} = C.

get_catalogs() ->
	[{userdata, [{doc, "GET Catalog collection"}]}].

get_catalogs(Config) ->
	ok = fill_catalog(5),
	HostUrl = ?config(host_url, Config),
	CollectionUrl = HostUrl ++ ?PathCatalog ++ "catalog",
	Accept = {"accept", "application/json"},
	Request = {CollectionUrl, [Accept, auth_header()]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, ResponseBody} = Result,
	{_, "application/json"} = lists:keyfind("content-type", 1, Headers),
	ContentLength = integer_to_list(length(ResponseBody)),
	{_, ContentLength} = lists:keyfind("content-length", 1, Headers),
	{ok, Catalogs} = zj:decode(ResponseBody),
	true = lists:all(fun is_catalog/1, Catalogs).

get_catalog() ->
	[{userdata, [{doc, "GET Catalog resource"}]}].

get_catalog(Config) ->
	HostUrl = ?config(host_url, Config),
	CatalogName = random_string(10),
	Description = random_string(25),
	Version = random_string(3),
	ClassType = "ResourceCatalog",
	Schema = ?PathCatalog ++ "schema/swagger.json#/definitions/ResourceCatalog",
	PartyId = random_string(10),
	PartyHref = ?PathParty ++ "organization/" ++ PartyId,
	CategoryId = random_string(10),
	CategoryHref = ?PathCatalog ++ "category/" ++ CategoryId,
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
			related_party = [#related_party_ref{id = PartyId,
					href = PartyHref,
					role = "Supplier",
					name = "ACME Inc.",
					start_date = 1548720000000,
					end_date = 1577836740000}],
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
	true = is_related_party_ref(RP),
	true = is_category_ref(C).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

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

is_related_party_ref(#{"id" := Id, "href" := Href,
		"name" := Name, "role" := Role,
		"validFor" := #{"startDateTime" := Start,
		"endDateTime" := End}}) when is_list(Id),
		is_list(Href), is_list(Name), is_list(Role),
		is_list(Start), is_list(End) ->
	im_rest:iso8601(End) > im_rest:iso8601(Start);
is_related_party_ref(_RP) ->
	false.

is_category_ref(#{"id" := Id, "href" := Href,
		"name" := Name, "version" := Version})
		when is_list(Id), is_list(Href),
		is_list(Name), is_list(Version) ->
	true;
is_category_ref(_) ->
	false.

is_catalog(#{"id" := Id, "href" := Href, "name" := Name,
		"description" := Description, "version" := Version,
		"@type" := ClassType, "@baseType" := "Catalog",
		"@schemaLocation" := Schema, "relatedParty" := RelatedParty,
		"category" := Category}) when is_list(Id),
		is_list(Href), is_list(Name), is_list(Description),
		is_list(Version), is_list(ClassType), is_list(Schema),
		is_list(RelatedParty) ->
	lists:all(fun is_related_party_ref/1, RelatedParty),
	lists:all(fun is_category_ref/1, Category);
is_catalog(_) ->
	false.

fill_catalog(0) ->
	ok;
fill_catalog(N) ->
	Schema = ?PathCatalog ++ "schema/swagger.json#/definitions/ResourceCatalog",
	Catalog = #catalog{name = random_string(10),
			description = random_string(25),
			class_type = "ResourceCatalog",
			schema = Schema,
			base_type = "Catalog",
			version = random_string(3),
			start_date = 1548720000000,
			end_date = 1577836740000,
			status = active,
			related_party = fill_related_party(3),
			category = fill_category(5)},
	{ok, _} = im:add_catalog(Catalog),
	fill_catalog(N - 1).

fill_related_party(N) ->
	fill_related_party(N, []).
fill_related_party(0, Acc) ->
	Acc;
fill_related_party(N, Acc) ->
	Id = random_string(10),
	Href = ?PathParty ++ "organization/" ++ Id,
	RelatedParty = #related_party_ref{id = Id, href = Href,
			role = "Supplier", name = "ACME Inc.",
	start_date = 1548720000000, end_date = 1577836740000},
	fill_related_party(N - 1, [RelatedParty | Acc]).

fill_category(N) ->
	fill_category(N, []).
fill_category(0, Acc) ->
	Acc;
fill_category(N, Acc) ->
	Id = random_string(10),
	Href = ?PathParty ++ "organization/" ++ Id,
	Category = #category_ref{id = Id, href = Href,
			name = random_string(10), version = random_string(3)},
	fill_category(N - 1, [Category | Acc]).

