%%% im.erl
%%% vim: ts=3
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
%%% @doc This library module implements the public API for the
%%%   {@link //sigscale_im. sigscale_im} application.
%%%
-module(im).
-copyright('Copyright (c) 2018-2019 SigScale Global Inc.').

%% export the im public API
-export([add_catalog/1, get_catalogs/0, get_catalog/1, del_catalog/1]).
-export([add_category/1, get_categories/0, get_category/1, del_category/1]).
-export([add_candidate/1, get_candidates/0, get_candidate/1, del_candidate/1]).
-export([add_specification/1, get_specifications/0, get_specification/1,
		del_specification/1]).
-export([add_resource/1, get_resources/0, get_resource/1, del_resource/1]).
-export([query_resource/7, query_resource/8]).
-export([add_user/3, get_users/0, get_user/1, del_user/1, query_users/4]).
-export([generate_password/0, generate_identity/0]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").

-define(CHUNKSIZE, 100).
%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).
-define(IDOFFSET, 63681984000).

-record(state,
      {current :: string() | undefined,
      resource = #resource{} :: resource()}).

%%----------------------------------------------------------------------
%%  The im public API
%%----------------------------------------------------------------------

-spec add_catalog(Catalog) -> Result
	when
		Result :: {ok, Catalog} | {error, Reason},
		Reason :: term().
%% @doc Create a new Resource Catalog.
add_catalog(#catalog{id = undefined,
		last_modified = undefined} = Catalog) ->
	F = fun() ->
			{Id, LM} = unique(),
			NewCatalog = Catalog#catalog{id = Id, last_modified = LM},
			ok = mnesia:write(NewCatalog),
			NewCatalog
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, NewCatalog} ->
			{ok, NewCatalog}
	end.

-spec get_catalogs() -> Result
	when
		Result :: {ok, CatalogIDs} | {error, Reason},
		CatalogIDs :: [string()],
		Reason :: term().
%% @doc Get all Resource Catalog identifiers.
get_catalogs() ->
	F = fun() ->
			mnesia:all_keys(catalog)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, CatalogIDs} ->
			CatalogIDs
	end.

-spec get_catalog(CatalogID) -> Result
	when
		CatalogID :: string(),
		Result :: {ok, Catalog} | {error, Reason},
		Catalog :: catalog(),
		Reason :: term().
%% @doc Get a Resource Catalog.
get_catalog(CatalogID) when is_list(CatalogID) ->
	F = fun() ->
			mnesia:read(catalog, CatalogID, read)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, [Catalog]} ->
			{ok, Catalog}
	end.

-spec del_catalog(CatalogID) -> Result
	when
		CatalogID :: string(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Delete a Resource Catalog.
del_catalog(CatalogID) when is_list(CatalogID) ->
	F = fun() ->
			mnesia:delete(catalog, CatalogID, write)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, ok} ->
			ok
	end.

-spec add_category(Category) -> Result
	when
		Result :: {ok, Category} | {error, Reason},
		Reason :: term().
%% @doc Create a new Resource Category.
add_category(#category{id = undefined,
		last_modified = undefined} = Category) ->
	F = fun() ->
			{Id, LM} = unique(),
			NewCategory = Category#category{id = Id, last_modified = LM},
			ok = mnesia:write(NewCategory),
			NewCategory
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, NewCategory} ->
			{ok, NewCategory}
	end.

-spec get_categories() -> Result
	when
		Result :: {ok, CategoryIDs} | {error, Reason},
		CategoryIDs :: [string()],
		Reason :: term().
%% @doc Get all Resource Category identifiers.
get_categories() ->
	F = fun() ->
			mnesia:all_keys(category)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, CategoryIDs} ->
			CategoryIDs
	end.

-spec get_category(CategoryID) -> Result
	when
		CategoryID :: string(),
		Result :: {ok, Category} | {error, Reason},
		Category :: category(),
		Reason :: term().
%% @doc Get a Resource Category.
get_category(CategoryID) when is_list(CategoryID) ->
	F = fun() ->
			mnesia:read(category, CategoryID, read)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, [Category]} ->
			{ok, Category}
	end.

-spec del_category(CategoryID) -> Result
	when
		CategoryID :: string(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Delete a Resource Category.
del_category(CategoryID) when is_list(CategoryID) ->
	F = fun() ->
			mnesia:delete(category, CategoryID, write)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, ok} ->
			ok
	end.

-spec add_candidate(Candidate) -> Result
	when
		Result :: {ok, Candidate} | {error, Reason},
		Reason :: term().
%% @doc Create a new Resource Candidate.
add_candidate(#candidate{id = undefined,
		last_modified = undefined} = Candidate) ->
	F = fun() ->
			{Id, LM} = unique(),
			NewCandidate = Candidate#candidate{id = Id, last_modified = LM},
			ok = mnesia:write(NewCandidate),
			NewCandidate
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, NewCandidate} ->
			{ok, NewCandidate}
	end.

-spec get_candidates() -> Result
	when
		Result :: {ok, CandidateIDs} | {error, Reason},
		CandidateIDs :: [string()],
		Reason :: term().
%% @doc Get all Resource Candidate identifiers.
get_candidates() ->
	F = fun() ->
			mnesia:all_keys(candidate)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, CandidateIDs} ->
			CandidateIDs
	end.

-spec get_candidate(CandidateID) -> Result
	when
		CandidateID :: string(),
		Result :: {ok, Candidate} | {error, Reason},
		Candidate :: candidate(),
		Reason :: term().
%% @doc Get a Resource Candidate.
get_candidate(CandidateID) when is_list(CandidateID) ->
	F = fun() ->
			mnesia:read(candidate, CandidateID, read)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, [Candidate]} ->
			{ok, Candidate}
	end.

-spec del_candidate(CandidateID) -> Result
	when
		CandidateID :: string(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Delete a Resource Candidate.
del_candidate(CandidateID) when is_list(CandidateID) ->
	F = fun() ->
			mnesia:delete(candidate, CandidateID, write)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, ok} ->
			ok
	end.

-spec add_specification(Specification) -> Result
	when
		Result :: {ok, Specification} | {error, Reason},
		Reason :: term().
%% @doc Create a new Resource Specification.
add_specification(#specification{id = undefined,
		last_modified = undefined} = Specification) ->
	F = fun() ->
			{Id, LM} = unique(),
			NewSpecification = Specification#specification{id = Id,
					last_modified = LM},
			ok = mnesia:write(NewSpecification),
			NewSpecification
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, NewSpecification} ->
			{ok, NewSpecification}
	end.

-spec get_specifications() -> Result
	when
		Result :: {ok, SpecificationIDs} | {error, Reason},
		SpecificationIDs :: [string()],
		Reason :: term().
%% @doc Get all Resource Specification identifiers.
get_specifications() ->
	F = fun() ->
			mnesia:all_keys(specification)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, SpecificationIDs} ->
			SpecificationIDs
	end.

-spec get_specification(SpecificationID) -> Result
	when
		SpecificationID :: string(),
		Result :: {ok, Specification} | {error, Reason},
		Specification :: specification(),
		Reason :: term().
%% @doc Get a Resource Specification.
get_specification(SpecificationID) when is_list(SpecificationID) ->
	F = fun() ->
			mnesia:read(specification, SpecificationID, read)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, [Specification]} ->
			{ok, Specification}
	end.

-spec del_specification(SpecificationID) -> Result
	when
		SpecificationID :: string(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Delete a Resource Specification.
del_specification(SpecificationID) when is_list(SpecificationID) ->
	F = fun() ->
			mnesia:delete(specification, SpecificationID, write)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, ok} ->
			ok
	end.

-spec add_resource(Resource) -> Result
	when
		Result :: {ok, Resource} | {error, Reason},
		Reason :: term().
%% @doc Create a new Resource.
add_resource(#resource{id = undefined,
		last_modified = undefined} = Resource) ->
	F = fun() ->
			{Id, LM} = unique(),
			NewResource = Resource#resource{id = Id, last_modified = LM},
			ok = mnesia:write(NewResource),
			NewResource
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, NewResource} ->
			{ok, NewResource}
	end.

-spec get_resources() -> Result
	when
		Result :: {ok, ResourceIDs} | {error, Reason},
		ResourceIDs :: [string()],
		Reason :: term().
%% @doc Get all Resource identifiers.
get_resources() ->
	F = fun() ->
			mnesia:all_keys(resource)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, ResourceIDs} ->
			ResourceIDs
	end.

-spec get_resource(ResourceID) -> Result
	when
		ResourceID :: string(),
		Result :: {ok, Resource} | {error, Reason},
		Resource :: resource(),
		Reason :: term().
%% @doc Get a Resource.
get_resource(ResourceID) when is_list(ResourceID) ->
	F = fun() ->
			mnesia:read(resource, ResourceID, read)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, [Resource]} ->
			{ok, Resource}
	end.

-spec del_resource(ResourceID) -> Result
	when
		ResourceID :: string(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Delete a Resource.
del_resource(ResourceID) when is_list(ResourceID) ->
	F = fun() ->
			mnesia:delete(resource, ResourceID, write)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, ok} ->
			ok
	end.

-spec query_resource(Cont, Size, Sort, MatchId, MatchName, MatchType,
		MatchChar) -> Result
	when
		Cont :: start | any(),
		Size :: pos_integer() | undefined,
		Sort :: [integer()],
		MatchId :: Match,
		MatchName :: Match,
		MatchType :: Match,
		MatchChar :: Match,
		Match :: {exact, string()} | {notexact, string()} | {like, [string()]},
		Result :: {Cont1, [#resource{}], Total} | {error, Reason},
		Cont1 :: eof | any(),
		Total :: non_neg_integer(),
		Reason :: term().
%% @equiv query_resource(Cont, Size, Sort, MatchId, MatchName,
%%       MatchType, MatchChar)
query_resource(Cont, Size, Sort, MatchId, MatchName, MatchType, MatchChar) ->
	query_resource(Cont, Size, Sort, MatchId, MatchName, MatchType, MatchChar, false).

-spec query_resource(Cont, Size, Sort, MatchId, MatchName, MatchType,
		MatchChar, CountOnly) -> Result
	when
		Cont :: start | any(),
		Size :: pos_integer() | undefined,
		Sort :: [integer()],
		MatchId :: Match,
		MatchName :: Match,
		MatchType :: Match,
		MatchChar :: Match,
		Match :: {exact, string()} | {notexact, string()} | {like, [string()]},
		CountOnly :: boolean(),
		Result :: {Cont1, [#resource{}], Total} | {error, Reason},
		Cont1 :: eof | any(),
		Total :: non_neg_integer(),
		Reason :: term().
%% @doc Query the resource table.
%%
%%    The result list will be sorted by the record elements listed in `Sort', in order.
query_resource(Cont, undefined, Sort, MatchId, MatchName, MatchType, MatchChar,
		CountOnly) ->
	{ok, Size} = application:get_env(rest_page_size),
	query_resource(Cont, Size, Sort, MatchId, MatchName, MatchType, MatchChar,
		CountOnly);
query_resource(Cont, Size, Sort, '_' = _MatchId, MatchName, MatchType, MatchChar,
		CountOnly) ->
	MatchHead = #resource{_ = '_'},
	query_resource1(Cont, Size, Sort, MatchHead, [], MatchName, MatchType, MatchChar,
		CountOnly);
query_resource(Cont, Size, Sort, {exact, String} = _MatchId, MatchName, MatchType,
		MatchChar, CountOnly) ->
	MatchHead = #resource{id = '$1', _ = '_'},
	MatchConditions = [{'==', '$1', String}],
	query_resource1(Cont, Size, Sort, MatchHead, MatchConditions, MatchName, MatchType,
		MatchChar, CountOnly);
query_resource(Cont, Size, Sort, {like, [String]} = _MatchId, MatchName, MatchType,
		MatchChar, CountOnly) ->
	{MatchHead, MatchConditions} = case lists:last(String) of
		$% ->
			Prefix = lists:droplast(String),
			Mh = #resource{id = Prefix ++ '_', _ = '_'},
			{Mh, []};
		_ ->
			Mh1 = #resource{id = String, _ = '_'},
			{Mh1, []}
	end,
	query_resource1(Cont, Size, Sort, MatchHead, MatchConditions, MatchName, MatchType,
		MatchChar, CountOnly);
query_resource(Cont, Size, Sort, {notexact, String} = _MatchId, MatchName, MatchType,
		MatchChar, CountOnly) when is_list(String) ->
		MatchHead = #resource{id = '$1', _ = '_'},
		MatchConditions = [{'/=', '$1', String}],
	query_resource1(Cont, Size, Sort, MatchHead, MatchConditions, MatchName, MatchType,
		MatchChar, CountOnly).
%% @hidden
query_resource1(Cont, Size, Sort, MatchHead, MatchConditions, '_' = _MatchName, MatchType,
		MatchChar, CountOnly) ->
	query_resource2(Cont, Size, Sort, MatchHead, MatchConditions, MatchType, MatchChar,
		CountOnly);
query_resource1(Cont, Size, Sort, MatchHead, MatchConditions, {exact, String} = _MatchName,
		MatchType, MatchChar, CountOnly) when is_list(String)->
		MatchHead1 = MatchHead#resource{name = '$2'},
		MatchConditions1 = [{'==', '$2', String} | MatchConditions],
	query_resource2(Cont, Size, Sort, MatchHead1, MatchConditions1, MatchType,
		MatchChar, CountOnly);
query_resource1(Cont, Size, Sort, MatchHead, MatchConditions, {Op, [String]} = _MatchName,
		MatchType, MatchChar, CountOnly) when ((Op == exact) orelse (Op == like))->
	{MatchHead1, MatchConditions1} = case lists:last(String) of
		$% when Op == like ->
			Prefix = lists:droplast(String),
			Mh = MatchHead#resource{name = '$2'},
			Mc = [{'>=', '$3', Prefix} | MatchConditions],
			{Mh, Mc};
		_ ->
			Mh1 = MatchHead#resource{name = String},
			{Mh1, MatchConditions}
	end,
	query_resource2(Cont, Size, Sort, MatchHead1, MatchConditions1, MatchType, MatchChar,
			CountOnly);
query_resource1(Cont, Size, Sort, MatchHead, MatchConditions, {notexact, String} = _MatchName,
		MatchType, MatchChar, CountOnly) when is_list(String) ->
		MatchHead1 = MatchHead#resource{name = '$2'},
		MatchConditions1 = [{'/=', '$2', String} | MatchConditions],
	query_resource2(Cont, Size, Sort, MatchHead1, MatchConditions1, MatchType, MatchChar,
		CountOnly).
%% @hidden
query_resource2(Cont, Size, Sort, MatchHead, MatchConditions, '_' = _MatchType, MatchChar,
		CountOnly) ->
	query_resource3(Cont, Size, Sort, MatchHead, MatchConditions, MatchChar, CountOnly);
query_resource2(Cont, Size, Sort, MatchHead, MatchConditions, {exact, String} = _MatchType,
		MatchChar, CountOnly) when is_list(String) ->
		MatchHead1 = MatchHead#resource{class_type = '$3'},
		MatchCondition = {'==', '$3', String},
	query_resource3(Cont, Size, Sort, MatchHead1, [MatchCondition | MatchConditions],
			MatchChar, CountOnly);
query_resource2(Cont, Size, Sort, MatchHead, MatchConditions, {Op, [String]} = _MatchType,
		MatchChar, CountOnly) when ((Op == exact) orelse (Op == like)) ->
	MatchHead1 = case lists:last(String) of
		$% ->
			Prefix = lists:droplast(String),
			MatchHead#resource{class_type = Prefix ++ '_'};
		_ ->
			MatchHead#resource{class_type = String}
	end,
	query_resource3(Cont, Size, Sort, MatchHead1, MatchConditions, MatchChar, CountOnly);
query_resource2(Cont, Size, Sort, MatchHead, MatchConditions, {notexact, String} = _MatchType,
		MatchChar, CountOnly) when is_list(String) ->
		MatchHead1 = MatchHead#resource{class_type = '$3'},
		MatchCondition = {'/=', '$3', String},
	query_resource3(Cont, Size, Sort, MatchHead1, [MatchCondition | MatchConditions],
			MatchChar, CountOnly).
%% @hidden
query_resource3(start, Size, [], #resource{_ = '_'}, [], '_', true) ->
	{eof, Size, mnesia:table_info(inventory, size)};
query_resource3(start, Size, [], #resource{_ = '_'} = MatchHead, [] = MatchConditions,
	'_', false) ->
	MatchSpec = [{MatchHead, MatchConditions, ['$_']}],
	F = fun() ->
			{mnesia:select(inventory, MatchSpec, Size, read),
					mnesia:table_info(inventory, size)}
	end,
	query_resource4(mnesia:ets(F), [], '_', false);
query_resource3(start, Size, [], MatchHead, MatchConditions, MatchChar, false) ->
	MatchSpec = [{MatchHead, MatchConditions, ['$_']}],
	F = fun() ->
		{mnesia:select(inventory, MatchSpec, Size, read), undefined}
	end,
	query_resource4(mnesia:ets(F), [], MatchChar, false);
query_resource3(start, _Size, Sort, MatchHead, MatchConditions, MatchChar, CountOnly) ->
	MatchSpec = [{MatchHead, MatchConditions, ['$_']}],
	F = fun() ->
		{mnesia:select(inventory, MatchSpec, read), undefined}
	end,
	query_resource4(mnesia:ets(F), Sort, MatchChar, CountOnly);
query_resource3(Cont, _Size, Sort, #resource{_ = '_'}, [], '_' = MatchChar, CountOnly) ->
	F = fun() ->
		{mnesia:select(Cont), mnesia:table_info(inventory, size)}
	end,
	query_resource4(mnesia:ets(F), Sort, MatchChar, CountOnly);
query_resource3(Cont, _Size, Sort, _MatchHead, _MatchConditions, MatchChar, CountOnly) ->
	F = fun() ->
		{mnesia:select(Cont), undefined}
	end,
	query_resource4(mnesia:ets(F), Sort, MatchChar, CountOnly).
%% @hidden
query_resource4({Resource, Cont}, '_', '_', '_') ->
	{Cont, Resource};
query_resource4({Resource, undefined}, _Sort, '_', true) when is_list(Resource) ->
	Total = length(Resource),
	{eof, Total, Total};
query_resource4({Resource, undefined}, Sort, '_', true) when is_list(Resource) ->
	Total = length(Resource),
	query_resource5(eof, Resource, Total, lists:reverse(Sort));
query_resource4({{Resource, Cont}, Total}, _Sort, '_', true) when is_list(Resource) ->
	{Cont, length(Resource), Total};
query_resource4({{Resource, Cont}, Total}, Sort, '_', false) ->
	query_resource5(Cont, Resource, Total, lists:reverse(Sort));
query_resource4({Resource, undefined}, _Sort, {Op, [String]}, true) when is_list(Resource),
		is_list(String), ((Op == exact) orelse (Op == like)) ->
	Fun = count_source(Op, String),
	Total = lists:foldl(Fun, 0, Resource),
	{eof, Total, Total};
query_resource4({Resource, Total}, Sort, {Op, [String]}, false) when is_list(Resource),
		is_list(String), ((Op == exact) orelse (Op == like)) ->
	Fun = filter_source(Op, String),
	query_resource5(eof, lists:filter(Fun, Resource), Total, lists:reverse(Sort));
query_resource4({{Resource, Cont}, _Total}, _Sort, {Op, [String]}, true)
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	Fun = count_source(Op, String),
	Total = lists:foldl(Fun, 0, Resource),
	{Cont, Total, Total};
query_resource4({{Resource, _Cont}, _Total}, Sort, {Op, [String]}, false)
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	Fun = filter_source(Op, String),
	query_resource5(eof, lists:filter(Fun, Resource), undefined, lists:reverse(Sort));
query_resource4({'$end_of_table', _Total}, _Sort, _MatchChar, _CountOnly) ->
	{eof, []}.
%% @hidden
query_resource5(Cont, Resource, Total, [H | T]) when H > 0 ->
	query_resource5(Cont, lists:keysort(H, Resource), Total, T);
query_resource5(Cont, Resource, Total, [H | T]) when H < 0 ->
	query_resource5(Cont, lists:reverse(lists:keysort(H, Resource)), Total, T);
query_resource5(Cont, Resource, undefined, []) ->
	{Cont, Resource};
query_resource5(Cont, Resource, Total, []) ->
	{Cont, Resource, Total}.

-spec add_user(Username, Password, Locale) -> Result
	when
		Username :: string(),
		Password :: string(),
		Locale :: string(),
		Result :: {ok, LastModified} | {error, Reason},
		LastModified :: {integer(), integer()},
		Reason :: user_exists | term().
%% @doc Add an HTTP user.
%% 	HTTP Basic authentication (RFC7617) is required with
%% 	`Username' and  `Password' used to construct the
%% 	`Authorization' header in requests.
%%
%% 	`Locale' is used to set the language for text in the web UI.
%% 	For English use `"en"', for Spanish use `"es'"..
%%
add_user(Username, Password, Locale) when is_list(Username),
		is_list(Password), is_list(Locale) ->
	add_user1(Username, Password, Locale, get_params()).
%% @hidden
add_user1(Username, Password, Locale, {Port, Address, Dir, Group}) ->
	add_user2(Username, Password, Locale,
			Address, Port, Dir, Group, im:get_user(Username));
add_user1(_, _, _, {error, Reason}) ->
	{error, Reason}.
%% @hidden
add_user2(Username, Password, Locale,
		Address, Port, Dir, Group, {error, no_such_user}) ->
	{_, LM} = unique(),
	NewUserData = [{last_modified, LM}, {locale, Locale}],
	add_user3(Username, Address, Port, Dir, Group, LM,
			mod_auth:add_user(Username, Password, NewUserData, Address, Port, Dir));
add_user2(_, _, _, _, _, _, _, {error, Reason}) ->
	{error, Reason};
add_user2(_, _, _, _, _, _, _, {ok, _}) ->
	{error, user_exists}.
%% @hidden
add_user3(Username, Address, Port, Dir, Group, LM, true) ->
	add_user4(LM, mod_auth:add_group_member(Group, Username, Address, Port, Dir));
add_user3(_, _, _, _, _, _, {error, Reason}) ->
	{error, Reason}.
%% @hidden
add_user4(LM, true) ->
	{ok, LM};
add_user4(_, {error, Reason}) ->
	{error, Reason}.

-spec get_users() -> Result
	when
		Result :: {ok, Users} | {error, Reason},
		Users :: [Username],
		Username :: string(),
		Reason :: term().
%% @doc List HTTP users.
%% @equiv  mod_auth:list_users(Address, Port, Dir)
get_users() ->
	get_users(get_params()).
%% @hidden
get_users({Port, Address, Dir, _}) ->
	mod_auth:list_users(Address, Port, Dir);
get_users({error, Reason}) ->
	{error, Reason}.

-spec get_user(Username) -> Result
	when
		Username :: string(),
		Result :: {ok, User} | {error, Reason},
		User :: #httpd_user{},
		Reason :: term().
%% @doc Get an HTTP user record.
%% @equiv mod_auth:get_user(Username, Address, Port, Dir)
get_user(Username) ->
	get_user(Username, get_params()).
%% @hidden
get_user(Username, {Port, Address, Dir, _}) ->
	mod_auth:get_user(Username, Address, Port, Dir);
get_user(_, {error, Reason}) ->
	{error, Reason}.

-spec del_user(Username) -> Result
	when
		Username :: string(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Delete an existing HTTP user.
del_user(Username) ->
	del_user(Username, get_params()).
%% @hidden
del_user(Username, {Port, Address, Dir, GroupName}) ->
	del_user(GroupName, Username, Address, Port, Dir,
			mod_auth:delete_user(Username, Address, Port, Dir));
del_user(_, {error, Reason}) ->
	{error, Reason}.
%% @hidden
del_user(GroupName, Username, Address, Port, Dir, true) ->
	case mod_auth:delete_group_member(GroupName,
			Username, Address, Port, Dir) of
		true ->
			ok;
		{error, Reason} ->
			{error, Reason}
	end;
del_user(_, _, _, _, _, {error, Reason}) ->
	{error, Reason}.

-spec query_users(Cont, Size, MatchId, MatchLocale) -> Result
	when
		Cont :: start | any(),
		Size :: pos_integer() | undefined,
		MatchId :: Match,
		MatchLocale :: Match,
		Match :: {exact, string()} | {notexact, string()} | {like, string()},
		Result :: {Cont1, [#httpd_user{}]} | {error, Reason},
		Cont1 :: eof | any(),
		Reason :: term().
%% @doc Query the user table.
query_users(Cont, undefined, MatchId, MatchLocale) ->
	{ok, Size} = application:get_env(rest_page_size),
	query_users(Cont, Size, MatchId, MatchLocale);
query_users(start, Size, '_', MatchLocale) ->
	MatchSpec = [{'_', [], ['$_']}],
	query_users1(Size, MatchSpec, MatchLocale);
query_users(start, Size, {Op, String} = _MatchId, MatchLocale)
		when is_list(String), ((Op == exact) orelse (Op == like)) ->
	MatchSpec = case lists:last(String) of
		$% when Op == like ->
			Prefix = lists:droplast(String),
			Username = {Prefix ++ '_', '_', '_', '_'},
			MatchHead = #httpd_user{username = Username, _ = '_'},
			[{MatchHead, [], ['$_']}];
		_ ->
			Username = {String, '_', '_', '_'},
			MatchHead = #httpd_user{username = Username, _ = '_'},
			[{MatchHead, [], ['$_']}]
	end,
	query_users1(Size, MatchSpec, MatchLocale);
query_users(start, Size, {notexact, String} = _MatchId, MatchLocale)
		when is_list(String) ->
	Username = {'$1', '_', '_', '_'},
	MatchHead = #httpd_user{username = Username, _ = '_'},
	MatchSpec = [{MatchHead, [{'/=', '$1', String}], ['$_']}],
	query_users1(Size, MatchSpec, MatchLocale);
query_users(Cont, _Size, _MatchId, MatchLocale) when is_tuple(Cont) ->
	F = fun() ->
			mnesia:select(Cont)
	end,
	case mnesia:ets(F) of
		{Users, Cont1} ->
			query_users2(MatchLocale, Cont1, Users);
		'$end_of_table' ->
			{eof, []}
	end;
query_users(start, Size, MatchId, MatchLocale) when is_tuple(MatchId) ->
	MatchCondition = [match_condition('$1', MatchId)],
	Username = {'$1', '_', '_', '_'},
	MatchHead = #httpd_user{username = Username, _ = '_'},
	MatchSpec = [{MatchHead, MatchCondition, ['$_']}],
	query_users1(Size, MatchSpec, MatchLocale).
%% @hidden
query_users1(Size, MatchSpec, MatchLocale) ->
	F = fun() ->
			mnesia:select(httpd_user, MatchSpec, Size, read)
	end,
	case mnesia:ets(F) of
		{Users, Cont} ->
			query_users2(MatchLocale, Cont, Users);
		'$end_of_table' ->
			{eof, []}
	end.
%% @hidden
query_users2('_' = _MatchLocale, Cont, Users) ->
	{Cont, Users};
query_users2({exact, String} = _MatchLocale, Cont, Users)
		when is_list(String) ->
	F = fun(#httpd_user{user_data = UD}) ->
			case lists:keyfind(locale, 1, UD) of
				{_, String} ->
					true;
				_ ->
					false
			end
	end,
	{Cont, lists:filter(F, Users)};
query_users2({notexact, String} = _MatchLocale, Cont, Users)
		when is_list(String) ->
	F = fun(#httpd_user{user_data = UD}) ->
			case lists:keyfind(locale, 1, UD) of
				{_, String} ->
					false;
				_ ->
					true
			end
	end,
	{Cont, lists:filter(F, Users)};
query_users2({like, String} = _MatchLocale, Cont, Users)
		when is_list(String) ->
	F = case lists:last(String) of
		$% ->
			Prefix = lists:droplast(String),
			fun(#httpd_user{user_data = UD}) ->
					case lists:keyfind(locale, 1, UD) of
						{_, Locale} ->
							lists:prefix(Prefix, Locale);
						_ ->
							false
					end
			end;
		_ ->
			fun(#httpd_user{user_data = UD}) ->
					case lists:keyfind(locale, 1, UD) of
						{_, String} ->
							true;
						_ ->
							false
					end
			end
	end,
	{Cont, lists:filter(F, Users)}.

-type password() :: [50..57 | 97..104 | 106..107 | 109..110 | 112..116 | 119..122].
-spec generate_password() -> password().
%% @equiv generate_password(12)
generate_password() ->
	generate_password(12).

-spec generate_identity() -> string().
%% @equiv generate_identity(7)
generate_identity() ->
	generate_identity(7).

%-spec import(File) -> Result
%	when
%		File :: file:filename(),
%		Result :: term().
%import(File) when is_list(File) ->
%	Options = [{event_fun, fun parse/3},
%		{event_state, #state{}}],
%	xmerl_sax_parser:file(File, Options).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec unique() -> Result
	when
		Result :: {ID, LM},
		ID :: string(),
		LM :: {TS, N},
		TS :: pos_integer(),
		N :: pos_integer().
%% @doc Generate a unique identifier and timestamp.
unique() ->
	TS = erlang:system_time(?MILLISECOND),
	N = erlang:unique_integer([positive]),
	ID = integer_to_list(TS - ?IDOFFSET) ++ integer_to_list(N),
	LM = {TS, N},
	{ID, LM}.

-spec match_condition(MatchVariable, Match) -> MatchCondition
   when
      MatchVariable :: atom(), % '$<number>'
      Match :: {exact, term()} | {notexact, term()} | {lt, term()}
            | {lte, term()} | {gt, term()} | {gte, term()},
      MatchCondition :: {GuardFunction, MatchVariable, Term},
      Term :: any(),
      GuardFunction :: '=:=' | '=/=' | '<' | '=<' | '>' | '>='.
%% @doc Convert REST query patterns to Erlang match specification conditions.
%% @hidden
match_condition(Var, {exact, Term}) ->
   {'=:=', Var, Term};
match_condition(Var, {notexact, Term}) ->
   {'=/=', Var, Term};
match_condition(Var, {lt, Term}) ->
   {'<', Var, Term};
match_condition(Var, {lte, Term}) ->
   {'=<', Var, Term};
match_condition(Var, {gt, Term}) ->
   {'>', Var, Term};
match_condition(Var, {gte, Term}) ->
   {'>=', Var, Term}.

-spec generate_password(Length) -> password()
	when
		Length :: pos_integer().
%% @doc Generate a random uniform password.
%% @private
generate_password(Length) when Length > 0 ->
	Charset = charset(),
	NumChars = length(Charset),
	Random = crypto:strong_rand_bytes(Length),
	generate_password(Random, Charset, NumChars,[]).
%% @hidden
generate_password(<<N, Rest/binary>>, Charset, NumChars, Acc) ->
	CharNum = (N rem NumChars) + 1,
	NewAcc = [lists:nth(CharNum, Charset) | Acc],
	generate_password(Rest, Charset, NumChars, NewAcc);
generate_password(<<>>, _Charset, _NumChars, Acc) ->
	Acc.

-spec generate_identity(Length) -> string()
	when
		Length :: pos_integer().
%% @doc Generate a random uniform numeric identity.
%% @private
generate_identity(Length) when Length > 0 ->
	Charset = lists:seq($0, $9),
	NumChars = length(Charset),
	Random = crypto:strong_rand_bytes(Length),
	generate_identity(Random, Charset, NumChars,[]).
%% @hidden
generate_identity(<<N, Rest/binary>>, Charset, NumChars, Acc) ->
	CharNum = (N rem NumChars) + 1,
	NewAcc = [lists:nth(CharNum, Charset) | Acc],
	generate_identity(Rest, Charset, NumChars, NewAcc);
generate_identity(<<>>, _Charset, _NumChars, Acc) ->
	Acc.

-spec charset() -> Charset
	when
		Charset :: password().
%% @doc Returns the table of valid characters for passwords.
%% @private
charset() ->
	C1 = lists:seq($2, $9),
	C2 = lists:seq($a, $h),
	C3 = lists:seq($j, $k),
	C4 = lists:seq($m, $n),
	C5 = lists:seq($p, $t),
	C6 = lists:seq($w, $z),
	lists:append([C1, C2, C3, C4, C5, C6]).

-spec get_params() -> Result
	when
		Result :: {Port :: integer(), Address :: string(),
				Directory :: string(), Group :: string()}
				| {error, Reason :: term()}.
%% @doc Returns configurations details for currently running
%% {@link //inets. httpd} service.
%% @hidden
get_params() ->
	get_params(inets:services_info()).
%% @hidden
get_params({error, Reason}) ->
	{error, Reason};
get_params(ServicesInfo) ->
	get_params1(lists:keyfind(httpd, 1, ServicesInfo)).
%% @hidden
get_params1({httpd, _, HttpdInfo}) ->
	{_, Address} = lists:keyfind(bind_address, 1, HttpdInfo),
	{_, Port} = lists:keyfind(port, 1, HttpdInfo),
	get_params2(Address, Port, application:get_env(inets, services));
get_params1(false) ->
	{error, httpd_not_started}.
%% @hidden
get_params2(Address, Port, {ok, Services}) ->
	get_params3(Address, Port, lists:keyfind(httpd, 1, Services));
get_params2(_, _, undefined) ->
	{error, inet_services_undefined}.
%% @hidden
get_params3(Address, Port, {httpd, Httpd}) ->
	get_params4(Address, Port, lists:keyfind(directory, 1, Httpd));
get_params3(_, _, false) ->
	{error, httpd_service_undefined}.
%% @hidden
get_params4(Address, Port, {directory, {Directory, Auth}}) ->
	get_params5(Address, Port, Directory,
			lists:keyfind(require_group, 1, Auth));
get_params4(_, _, false) ->
	{error, httpd_directory_undefined}.
%% @hidden
get_params5(Address, Port, Directory, {require_group, [Group | _]}) ->
	{Port, Address, Directory, Group};
get_params5(_, _, _, false) ->
	{error, httpd_group_undefined}.

%% @hidden
count_source(Op, String) ->
   case lists:last(String) of
      $% when Op == like ->
         Prefix = lists:droplast(String),
         fun(#resource{characteristic = Ext}, Acc) ->
               case lists:prefix(Prefix, Ext) of
                  true ->
                     Acc + 1;
                  false ->
                     Acc
               end
         end;
      _ ->
         fun(#resource{characteristic = Ext}, Acc) when Ext == String ->
                  Acc + 1;
               (_, Acc) ->
                  Acc
         end
   end.


%% @hidden
filter_source(Op, String) ->
   case lists:last(String) of
      $% when Op == like ->
         Prefix = lists:droplast(String),
         fun(#resource{characteristic = Ext}) ->
               lists:prefix(Prefix, Ext)
         end;
      _ ->
         fun(#resource{characteristic = Ext}) when Ext == String ->
                  true;
               (_) ->
                  false
         end
   end.

%% @hidden
parse(startDocument = _Event, _Location, State) ->
   State;
parse({startElement, _, "MO", _, [{_, _, "className", Class}, {_, _, "fdn", Fdn}]}, _, _State) ->
   #state{resource = #resource{id = Fdn, description = Class, class_type = "EQUIPMENT", base_type = "PhysicalResource"}};
parse({startElement, _, "MO", _, _}, _, #state{}) ->
   #state{};
parse({startElement, _, "attr", _, [{_, _, "name", Name}]}, _, State) ->
   State#state{current = Name};
parse({characters, Chars}, _, #state{current = "fdn", resource = R} = State) ->
   State#state{resource = R#resource{id = Chars}, current = undefined};
parse({characters, Chars}, _, #state{current = "name", resource = R} = State) ->
   State#state{resource = R#resource{name = Chars}, current = undefined};
parse({characters, _Chars}, _, #state{current = "className"} = State) ->
   State#state{current = undefined};
parse({characters, Chars}, _, #state{current = Name, resource = R} = State) ->
   Characteristics = [{Name, Chars} | R#resource.characteristic],
   State#state{resource = R#resource{characteristic = Characteristics}, current = undefined};
parse({endElement, _, "attr", _}, _, State) ->
   State#state{current = undefined};
parse({endElement, _, "MO", _}, _, #state{resource = R}) ->
   F = fun() ->
         mnesia:write(R)
   end,
   mnesia:transaction(F),
   #state{};
parse({ignorableWhitespace, _}, _, State) ->
   State;
parse(endDocument, _, State) ->
   State;
parse(_Other, _Location, State) ->
   State.
%strip(Char) ->
%	strip1(Char, []).

%strip1([$_, $B | T], Acc) ->
%	strip1(T, Acc);
%strip1([$_, $G | T], Acc) ->
%	strip1(T, Acc);
%strip1([$_, $M | T], Acc) ->
%	strip1(T, Acc);
%strip1([$_, $R | T], Acc) ->
%	strip1(T, Acc);
%strip1([$_, $W | T], Acc) ->
%	strip1(T, Acc);
%strip1([$_, $X | T], Acc) ->
%	strip1(T, Acc);
%strip1([$_ | T], Acc) ->
%	strip1(T, Acc);
%strip1([H | T], Acc) ->
%	strip1(T, [H | Acc]);
%strip1([], Acc) ->
%   lists:reverse(Acc).

