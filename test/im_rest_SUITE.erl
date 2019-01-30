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
	[{timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	PrivDir = ?config(priv_dir, Config),
	ok = application:set_env(mnesia, dir, PrivDir),
	ok = im_test_lib:initialize_db(),
	ok = im_test_lib:start(),
	Config.

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
	[map_to_catalog, post_catalog].

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

post_catalog() ->
	[{userdata, [{doc, "Post to Catalog collection"}]}].

post_catalog(_Config) ->
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
	Body = "{\n"
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
			++ "\t\"lifecycleStatus\": \"Active\",\n"
			++ "\t\"relatedParty\": [\n"
			++ "\t\t{\n"
			++ "\t\t\t\"id\": \"" ++ PartyId ++ ",\n"
			++ "\t\t\t\"href\": \"" ++ PartyHref ++ ",\n"
			++ "\t\t\t\"role\": \"Supplier\",\n"
			++ "\t\t\t\"name\": \"ACME Inc.\",\n"
			++ "\t\t\t\"validFor\": {\n"
			++ "\t\t\t\t\"startDateTime\": \"2019-01-29T00:00\",\n"
			++ "\t\t\t\t\"endDateTime\": \"2019-12-31T23:59\",\n"
			++ "\t\t\t}\n"
			++ "\t\t}\n"
			++ "\t],\n"
			++ "\t\"category\": [\n"
			++ "\t\t{\n"
			++ "\t\t\t\"id\": \"" ++ CategoryId ++ ",\n"
			++ "\t\t\t\"href\": \"" ++ CategoryHref ++ ",\n"
			++ "\t\t\t\"name\": \"" ++ CategoryName ++ ",\n"
			++ "\t\t\t\"version\": \"" ++ Version ++ "\n"
			++ "\t\t}\n"
			++ "\t]\n"
			++ "}\n",
	true = is_list(Body),
	{skip, unimplemented}.

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

