%%% im.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018 - 2024 SigScale Global Inc.
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
%%%   {@link //im. im} application.
%%%
-module(im).
-copyright('Copyright (c) 2018 - 2024 SigScale Global Inc.').

%% export the im public API
-export([add_catalog/1, del_catalog/1, get_catalog/0, get_catalog/1,
		get_catalog_name/1]).
-export([add_category/1, del_category/1, get_category/0, get_category/1,
		get_category_name/1]).
-export([add_candidate/1, del_candidate/1, get_candidate/0, get_candidate/1,
		get_candidate_name/1]).
-export([add_specification/1, del_specification/1, get_specification/0,
		get_specification/1, get_specification_name/1]).
-export([add_resource/1, del_resource/1, get_resource/0, get_resource/1,
		get_resource_name/1, httpd_logname/1]).
-export([add_user/3, del_user/1, get_user/0, get_user/1, query_users/4]).
-export([add_rule/2, delete_rule/1, get_rule/0, get_rule/1, get_pee/2]).
-export([query/5, query/6]).
-export([import/1, import/2]).
-export([generate_password/0, generate_identity/0]).
-export([merge/2, last/2]).
-export([statistics/1]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").

-define(CHUNKSIZE, 100).
%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

-define(PathCatalog, "/resourceCatalogManagement/v4/").
-define(PathInventory, "/resourceInventoryManagement/v4/").

%%----------------------------------------------------------------------
%%  The im public API
%%----------------------------------------------------------------------

-spec add_catalog(Catalog) -> Result
	when
		Result :: {ok, Catalog} | {error, Reason},
		Reason :: term().
%% @doc Create a new Resource Catalog.
add_catalog(#catalog{id = undefined, href = undefined,
		last_modified = undefined} = Catalog) ->
	F = fun() ->
			{Id, LM} = unique(),
			Href = ?PathCatalog ++ "resourceCatalog/" ++ Id,
			NewCatalog = Catalog#catalog{id = Id,
					href = Href, last_modified = LM},
			ok = mnesia:write(NewCatalog),
			NewCatalog
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, NewCatalog} ->
			{ok, NewCatalog}
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

-spec get_catalog() -> Result
	when
		Result :: {ok, CatalogIDs} | {error, Reason},
		CatalogIDs :: [string()],
		Reason :: term().
%% @doc Get all Resource Catalog identifiers.
get_catalog() ->
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
		Reason :: not_found | term().
%% @doc Get a Resource Catalog.
get_catalog(CatalogID) when is_list(CatalogID) ->
	F = fun() ->
			mnesia:read(catalog, CatalogID, read)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, [Catalog]} ->
			{ok, Catalog};
		{atomic, []} ->
			{error, not_found}
	end.

-spec get_catalog_name(CatalogName) -> Result
	when
		CatalogName :: string(),
		Result :: {ok, Catalog} | {error, Reason},
		Catalog :: category(),
		Reason :: not_found | term().
%% @doc Get a Catalog by name.
get_catalog_name(CatalogName) when is_list(CatalogName) ->
	F = fun() ->
			mnesia:index_read(catalog, CatalogName, #catalog.name)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, [Catalog]} ->
			{ok, Catalog};
		{atomic, []} ->
			{error, not_found}
	end.

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
query_users2([] = _MatchLocale, Cont, Users) ->
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

-spec query(Table, Cont, Size, Sort, MatchSpec) -> Result
	when
		Table :: atom(),
		Cont :: start | any(),
		Size :: pos_integer() | undefined,
		Sort :: [integer()] | [],
		MatchSpec :: ets:match_spec() | '_',
		Result :: {Cont1, [term()], Total} | {error, Reason},
		Cont1 :: eof | any(),
		Total :: non_neg_integer(),
		Reason :: term().
%% @equiv query(Table, Cont, Size, Sort, MatchHead, MatchConditions, false)
query(Table, Cont, Size, Sort, MatchSpec) ->
	query(Table, Cont, Size, Sort, MatchSpec, false).

-spec query(Cont, Size, Table, Sort, MatchSpec, CountOnly) -> Result
	when
		Cont :: start | any(),
		Size :: pos_integer() | undefined,
		Table :: atom(),
		Sort :: [integer()],
		MatchSpec :: ets:match_pattern() | '_',
		CountOnly :: boolean(),
		Result :: {Cont1, [term()], Total} | {error, Reason},
		Cont1 :: eof | any(),
		Total :: non_neg_integer(),
		Reason :: term().
%% @doc Query the `Table'.
%%
%%		The result list will be sorted by the record elements listed in `Sort', in order.
query(Cont, undefined, Table, Sort, MatchSpec, CountOnly)
		when is_atom(Table), is_list(Sort), is_boolean(CountOnly) ->
	{ok, Size} = application:get_env(im, rest_page_size),
	query1(Cont, Size, Table, Sort, MatchSpec, CountOnly);
query(Cont, Size, Table, Sort, MatchSpec, CountOnly)
		when is_atom(Table), is_integer(Size),
		is_list(Sort), is_boolean(CountOnly) ->
	query1(Cont, Size, Table, Sort, MatchSpec, CountOnly).
%% @hidden
query1(start, Size, Table, [], '_', true) ->
	{eof, Size, mnesia:table_info(Table, size)};
query1(start, Size, Table, [], '_', false) ->
	Arity = mnesia:table_info(Table, arity),
	MatchHead = erlang:make_tuple(Arity, '_', [{1, Table}]),
	MatchSpec = [{MatchHead, [], ['$_']}],
	F = fun() ->
			{mnesia:select(Table, MatchSpec, Size, read),
					mnesia:table_info(Table, size)}
	end,
	query2(mnesia:ets(F), [], false);
query1(start, Size, Table, [], MatchSpec, false)
		when is_integer(Size) ->
	F = fun() ->
		{mnesia:select(Table, MatchSpec, Size, read), undefined}
	end,
	query2(mnesia:ets(F), [], false);
query1(start, _Size, Table, Sort, MatchSpec, CountOnly) ->
	F = fun() ->
		{mnesia:select(Table, MatchSpec, read), undefined}
	end,
	query2(mnesia:ets(F), Sort, CountOnly);
query1(Cont, _Size, Table, Sort, '_', CountOnly) ->
	F = fun() ->
		{mnesia:select(Cont), mnesia:table_info(Table, size)}
	end,
	query2(mnesia:ets(F), Sort, CountOnly);
query1(Cont, _Size, _Table, Sort, _MatchSpec, CountOnly) ->
	F = fun() ->
		{mnesia:select(Cont), undefined}
	end,
	query2(mnesia:ets(F), Sort, CountOnly).

%% @hidden
query2({Objects, undefined}, _Sort, true)
		when is_list(Objects) ->
	Total = length(Objects),
	{eof, Total, Total};
query2({Objects, undefined}, Sort, false)
		when is_list(Objects) ->
	Total = length(Objects),
	query3(eof, Objects, Total, lists:reverse(Sort));
query2({{Objects, Cont}, Total}, _Sort, true)
		when is_integer(Total) ->
	{Cont, length(Objects), Total};
query2({{Objects, Cont}, Total}, Sort, false) ->
	query3(Cont, Objects, Total, lists:reverse(Sort));
query2({'$end_of_table', _Total},
		_Sort, _CountOnly) ->
	{eof, []}.
%% @hidden
query3(Cont, Objects, Total, [H | T]) when H > 0 ->
	query3(Cont, lists:keysort(H, Objects), Total, T);
query3(Cont, Objects, Total, [H | T]) when H < 0 ->
	query3(Cont, lists:reverse(lists:keysort(-H, Objects)), Total, T);
query3(Cont, Objects, undefined, []) ->
	{Cont, Objects};
query3(Cont, Objects, Total, []) ->
	{Cont, Objects, Total}.

-spec add_category(Category) -> Result
	when
		Result :: {ok, Category} | {error, Reason},
		Reason :: term().
%% @doc Create a new Resource Category.
add_category(#category{id = undefined,
		last_modified = undefined} = Category) ->
	F = fun() ->
			{Id, LM} = unique(),
			Href = ?PathCatalog ++ "resourceCategory/" ++ Id,
			NewCategory = Category#category{id = Id,
					href = Href, last_modified = LM},
			ok = mnesia:write(NewCategory),
			NewCategory
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, NewCategory} ->
			{ok, NewCategory}
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

-spec get_category() -> Result
	when
		Result :: {ok, CategoryIDs} | {error, Reason},
		CategoryIDs :: [string()],
		Reason :: term().
%% @doc Get all Resource Category identifiers.
get_category() ->
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
		Reason :: not_found | term().
%% @doc Get a Resource Category.
get_category(CategoryID) when is_list(CategoryID) ->
	F = fun() ->
			mnesia:read(category, CategoryID, read)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, [Category]} ->
			{ok, Category};
		{atomic, []} ->
			{error, not_found}
	end.

-spec get_category_name(CategoryName) -> Result
	when
		CategoryName :: string(),
		Result :: {ok, Category} | {error, Reason},
		Category :: category(),
		Reason :: not_found | term().
%% @doc Get a Category by name.
get_category_name(CategoryName) when is_list(CategoryName) ->
	F = fun() ->
			mnesia:index_read(category, CategoryName, #category.name)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, [Category]} ->
			{ok, Category};
		{atomic, []} ->
			{error, not_found}
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
			Href = ?PathCatalog ++ "resourceCandidate/" ++ Id,
			NewCandidate = Candidate#candidate{id = Id,
					href = Href, last_modified = LM},
			ok = mnesia:write(NewCandidate),
			NewCandidate
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, NewCandidate} ->
			{ok, NewCandidate}
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

-spec get_candidate() -> Result
	when
		Result :: {ok, CandidateIDs} | {error, Reason},
		CandidateIDs :: [string()],
		Reason :: term().
%% @doc Get all Resource Candidate identifiers.
get_candidate() ->
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
		Reason :: not_found | term().
%% @doc Get a Resource Candidate.
get_candidate(CandidateID) when is_list(CandidateID) ->
	F = fun() ->
			mnesia:read(candidate, CandidateID, read)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, [Candidate]} ->
			{ok, Candidate};
		{atomic, []} ->
			{error, not_found}
	end.

-spec get_candidate_name(CandidateName) -> Result
	when
		CandidateName :: string(),
		Result :: {ok, Candidate} | {error, Reason},
		Candidate :: candidate(),
		Reason :: not_found | term().
%% @doc Get a Candidate by name.
get_candidate_name(CandidateName) when is_list(CandidateName) ->
	F = fun() ->
			mnesia:index_read(candidate, CandidateName, #candidate.name)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, [Candidate]} ->
			{ok, Candidate};
		{atomic, []} ->
			{error, not_found}
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
			Href = ?PathCatalog ++ "resourceSpecification/" ++ Id,
			NewSpecification = Specification#specification{id = Id,
					href = Href, last_modified = LM},
			ok = mnesia:write(NewSpecification),
			NewSpecification
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, NewSpecification} ->
			{ok, NewSpecification}
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

-spec get_specification() -> Result
	when
		Result :: {ok, SpecificationIDs} | {error, Reason},
		SpecificationIDs :: [string()],
		Reason :: term().
%% @doc Get all Resource Specification identifiers.
get_specification() ->
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
		Reason :: not_found | term().
%% @doc Get a Resource Specification.
get_specification(SpecificationID) when is_list(SpecificationID) ->
	F = fun() ->
			mnesia:read(specification, SpecificationID, read)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, [Specification]} ->
			{ok, Specification};
		{atomic, []} ->
			{error, not_found}
	end.

-spec get_specification_name(SpecName) -> Result
	when
		SpecName :: string(),
		Result :: {ok, Specification} | {error, Reason},
		Specification :: specification(),
		Reason :: not_found | term().
%% @doc Get a Specification by name.
get_specification_name(SpecName) when is_list(SpecName) ->
	F = fun() ->
			mnesia:index_read(specification, SpecName, #specification.name)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, [Specification]} ->
			{ok, Specification};
		{atomic, []} ->
			{error, not_found}
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
			Href = ?PathInventory ++ "resource/" ++ Id,
			NewResource = Resource#resource{id = Id,
					href = Href, last_modified = LM},
			ok = mnesia:write(NewResource),
			NewResource
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, NewResource} ->
			{ok, NewResource}
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

-spec get_resource() -> Result
	when
		Result :: {ok, ResourceIDs} | {error, Reason},
		ResourceIDs :: [string()],
		Reason :: term().
%% @doc Get all Resource identifiers.
get_resource() ->
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
		Reason :: not_found | term().
%% @doc Get a Resource by identifier.
get_resource(ResourceID) when is_list(ResourceID) ->
	F = fun() ->
			mnesia:read(resource, ResourceID, read)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, [Resource]} ->
			{ok, Resource};
		{atomic, []} ->
			{error, not_found}
	end.

-spec get_resource_name(ResourceName) -> Result
	when
		ResourceName :: string(),
		Result :: {ok, Resource} | {error, Reason},
		Resource :: resource(),
		Reason :: not_found | term().
%% @doc Get a Resource by name.
get_resource_name(ResourceName) when is_list(ResourceName) ->
	F = fun() ->
			mnesia:index_read(resource, ResourceName, #resource.name)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
%		{atomic, [Resource]} ->
%			{ok, Resource};
		{atomic, []} ->
			{error, not_found};
		{atomic, [Resource | _]} ->
			{ok, Resource}
	end.

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

-spec get_user() -> Result
	when
		Result :: {ok, Users} | {error, Reason},
		Users :: [Username],
		Username :: string(),
		Reason :: term().
%% @doc List HTTP users.
%% @equiv  mod_auth:list_users(Address, Port, Dir)
get_user() ->
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

-spec import(File) -> Result
	when
		File :: string(),
		Result :: ok | ignore | {error, Reason},
		Reason :: term().
%% @doc @equiv import(File, [{type, '3gpp'}]).
import(File) when is_list(File) ->
	import(File, [{type, '3gpp'}, {rule, []}]).

-spec import(File, Options) -> Result
	when
		File :: string(),
		Options :: [Option],
		Option :: {type, Type} | {rule, Rule},
		Type :: '3gpp' | huawei,
		Rule :: string(),
		Result :: ok | ignore | {error, Reason},
		Reason :: term().
%% @doc Import a file in the inventory table.
import(File, Options) when is_list(File), is_list(Options) ->
	case proplists:get_value(type, Options, '3gpp') of
		'3gpp' ->
			RuleId = proplists:get_value(rule, Options),
			im_xml_cm_bulk:import(File, RuleId);
		nokia ->
			RuleId = proplists:get_value(rule, Options),
			im_xml_nokia:import(File, RuleId);
		huawei ->
			RuleId = proplists:get_value(rule, Options),
			im_xml_huawei:import(File, RuleId)
	end.

-type password() :: [50..57 | 97..104 | 106..107 | 109..110 | 112..116 | 119..122].
-spec generate_password() -> password().
%% @equiv generate_password(12)
generate_password() ->
	generate_password(12).

-spec generate_identity() -> string().
%% @equiv generate_identity(7)
generate_identity() ->
	generate_identity(7).

-spec add_rule(Rule, Description) -> Result
	when
		Rule :: rule() | '_',
		Description :: string() | '_',
		Result :: {ok, PeeRule} | {error, Reason},
		PeeRule :: pee_rule(),
		Reason :: term().
%% @doc Create PEE matching rule.
%add_rule(Rule, Description) when is_function(Rule), is_list(Description) ->
add_rule(Rule, Description) when is_list(Description) ->
	F = fun() ->
			{Id, _LM} = unique(),
			PeeRule = #pee_rule{id = Id,
					rule = Rule, description = Description},
			ok = mnesia:write(PeeRule),
			PeeRule
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, PeeRule} ->
			{ok, PeeRule}
	end.

-spec get_rule() -> Result
	when
		Result :: {ok, PeeRuleIDs} | {error, Reason},
		PeeRuleIDs :: [string()],
		Reason :: term().
%% @doc Get all Rule identifiers.
get_rule() ->
	F = fun() ->
			mnesia:all_keys(pee_rule)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, PeeRuleIDs} ->
			PeeRuleIDs
	end.

-spec get_rule(Id) -> Result
	when
		Id :: string(),
		Result :: {ok, PeeRule} | {error, Reason},
		PeeRule :: pee_rule(),
		Reason :: not_found | term().
%% @doc Get a specific rule.
get_rule(Id) when is_list(Id) ->
	F = fun() ->
			mnesia:read(pee_rule, Id, read)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, [PeeRule]} ->
			{ok, PeeRule};
		{atomic, []} ->
			{error, not_found}
	end.

-spec delete_rule(Id) -> Result
	when
		Id :: string(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Delete a specific rule.
delete_rule(Id) when is_list(Id) ->
	F = fun() ->
			mnesia:delete(pee_rule, Id, write)
	end,
	case mnesia:transaction(F) of
		{aborted, Reason} ->
			{error, Reason};
		{atomic, ok} ->
			ok
	end.

-spec get_pee(RuleId, Input) -> Result
	when
		RuleId :: string(),
		Input :: term(),
		Result :: {ok, PEEMonitoredEntities} | {error, Reason},
		PEEMonitoredEntities :: [resource()],
		Reason :: not_found | term().
%% @doc Get matching PEE CMON entity(s) for a given `Input' based on the rule.
get_pee(RuleId, Input) when is_list(RuleId), is_list(Input) ->
	case get_rule(RuleId) of
		{ok, #pee_rule{rule = Rule}} ->
			MatchSpec = Rule(Input),
			F = fun() ->
					mnesia:select(resource, MatchSpec, read)
			end,
			{ok, mnesia:ets(F)};
		{error, Reason} ->
			{error, Reason}
	end.

-spec statistics(Item) -> Result
	when
		Item :: scheduler_utilization,
		Result :: {ok, {Etag, Interval, Report}} | {error, Reason},
		Etag :: string(),
		Interval :: pos_integer(),
		Report :: [ItemResult],
		ItemResult :: {SchedulerId, Utilization},
		SchedulerId :: pos_integer(),
		Utilization :: non_neg_integer(),
		Reason :: term().
%% @doc Get system statistics.
statistics(Item) ->
	case catch gen_server:call(im_statistics, Item) of
		{Etag, Interval, Report} ->
			{ok, {Etag, Interval, Report}};
		{error, Reason} ->
			{error, Reason};
		{'EXIT', {noproc,_}} ->
			case catch supervisor:start_child(im_statistics_sup, []) of
				{ok, Child} ->
					case catch gen_server:call(Child, Item) of
						{Etag, Interval, Report} ->
							{ok, {Etag, Interval, Report}};
						{error, Reason} ->
							{error, Reason}
					end;
				{error, Reason} ->
					{error, Reason};
				{'EXIT', {noproc,_}} ->
					{error, im_down}
			end
	end.

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
	ID = integer_to_list(TS) ++ integer_to_list(N),
	LM = {TS, N},
	{ID, LM}.

-spec merge(OldRecord, NewRecord) -> MergedRecord
	when
		OldRecord :: tuple(),
		NewRecord :: tuple(),
		MergedRecord :: tuple().
%% @doc Merge two records. .
merge(OldRecord, NewRecord)
		when size(OldRecord) =:= size(NewRecord),
		element(1, OldRecord) =:= element(1, NewRecord) ->
	Size = size(NewRecord),
	merge(Size, OldRecord, NewRecord).
%% @hidden
merge(1, _, New) ->
	New;
merge(N, Old, New)
		when element(N, New) == undefined,
	element(N, Old) /= undefined ->
	merge(N - 1, Old, setelement(N, New, element(N, Old)));
merge(N, Old, New) ->
	merge(N - 1, Old, New).

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
	F = fun({directory, _}) ->
				true;
			(_) ->
				false
	end,
	get_params4(Address, Port, lists:filter(F, Httpd));
get_params3(_, _, false) ->
	{error, httpd_service_undefined}.
%% @hidden
get_params4(Address, Port, [{directory, {_Dir, []}} | T]) ->
	get_params4(Address, Port, T);
get_params4(Address, Port, [{directory, {Directory, Auth}} | _]) ->
	get_params5(Address, Port, Directory,
			lists:keyfind(require_group, 1, Auth));
get_params4(_, _, []) ->
	{error, httpd_directory_undefined}.
%% @hidden
get_params5(Address, Port, Directory, {require_group, [Group | _]}) ->
	{Port, Address, Directory, Group};
get_params5(_, _, _, false) ->
	{error, httpd_group_undefined}.

-spec httpd_logname(LogType) -> disk_log:log()
	when
		LogType :: transfer | error | security.
%% @doc Find local name of {@link //inets/httpd. httpd} disk_log.
%%
httpd_logname(LogType) ->
	{ok, Services} = application:get_env(inets, services),
	{_, HttpdConfig} = lists:keyfind(httpd, 1, Services),
	{_, ServerRoot} = lists:keyfind(server_root, 1, HttpdConfig),
	httpd_logname(LogType, ServerRoot, HttpdConfig).
%% @hidden
httpd_logname(transfer, ServerRoot, HttpdConfig) ->
	{_, LogName} = lists:keyfind(transfer_disk_log, 1, HttpdConfig),
	filename:join(ServerRoot, string:strip(LogName));
httpd_logname(error, ServerRoot, HttpdConfig) ->
	{_, LogName} = lists:keyfind(error_disk_log, 1, HttpdConfig),
	filename:join(ServerRoot, string:strip(LogName));
httpd_logname(security, ServerRoot, HttpdConfig) ->
	{_, LogName} = lists:keyfind(security_disk_log, 1, HttpdConfig),
	filename:join(ServerRoot, string:strip(LogName)).

-spec last(Log, MaxItems) -> Result
	when
		Log :: disk_log:log(),
		MaxItems :: pos_integer(),
		Result :: {NumItems, Items} | {error, Reason},
		NumItems :: non_neg_integer(),
		Items :: [term()],
		Reason :: term().
%% @doc Get the last `MaxItems' events in most recent item first order.
last(Log, MaxItems) ->
	case disk_log:chunk_step(Log, start, 0) of
		{error, end_of_log} ->
			{0, []};
		{error, Reason} ->
			{error, Reason};
		{ok, Cont1} ->
			last(Log, MaxItems, Cont1, [Cont1])
	end.
%% @hidden
last(Log, MaxItems, Cont1, [H | _] = Acc) ->
	case disk_log:chunk_step(Log, H, 1) of
		{error, end_of_log} ->
			last1(Log, MaxItems, Acc, {0, []});
		{ok, Cont1} ->
			last1(Log, MaxItems, Acc, {0, []});
		{ok, ContN} ->
			last(Log, MaxItems, Cont1, [ContN | Acc])
	end.
%% @hidden
last1(Log, MaxItems, [Cont | T], _Acc) ->
	case last2(Log, MaxItems, Cont, []) of
		{error, Reason} ->
			{error, Reason};
		{N, Items} when N < MaxItems ->
			last1(Log, MaxItems, T, {N, Items});
		{MaxItems, Items} ->
			{MaxItems, lists:flatten(Items)}
	end;
last1(_Log, _MaxItems, [], {NumItems, Items}) ->
	{NumItems, lists:flatten(Items)}.
%% @hidden
last2(Log, MaxItems, Cont, Acc) ->
	case disk_log:bchunk(Log, Cont) of
		{error, Reason} ->
			{error, Reason};
		eof ->
			last3(Log, MaxItems, Acc, 0, []);
		{Cont1, _Chunk} ->
			last2(Log, MaxItems, Cont1, [Cont | Acc])
	end.
%% @hidden
last3(Log, MaxItems, [Cont | T], NumItems, Acc) ->
	case disk_log:chunk(Log, Cont) of
		{error, Reason} ->
			{error, Reason};
		{_, Items} ->
			RevItems = lists:reverse(Items),
			NumNewItems = length(RevItems),
			case NumItems + NumNewItems of
				MaxItems ->
					NewAcc = [RevItems | Acc],
					{MaxItems, lists:reverse(NewAcc)};
				N when N > MaxItems ->
					NumHead = MaxItems - NumItems,
					{NewItems, _} = lists:split(NumHead, RevItems),
					NewAcc = [NewItems | Acc],
					{MaxItems, lists:reverse(NewAcc)};
				N ->
					NewAcc = [RevItems | Acc],
					last3(Log, MaxItems, T, N, NewAcc)
			end
	end;
last3(_Log, _MaxItems, [], NumItems, Acc) ->
	{NumItems, lists:reverse(Acc)}.

