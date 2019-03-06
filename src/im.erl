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
-export([add_catalog/1, del_catalog/1, get_catalog/0, get_catalog/1,
		get_catalog_name/1, query_catalog/4, query_catalog/1]).
-export([add_category/1, del_category/1, get_category/0, get_category/1,
		get_category_name/1, query_category/4, query_category/1]).
-export([add_candidate/1, del_candidate/1, get_candidate/0, get_candidate/1,
		get_candidate_name/1, query_candidate/4, query_candidate/1]).
-export([add_specification/1, del_specification/1, get_specification/0,
		get_specification/1, get_specification_name/1,
		query_specification/4, query_specification/1]).
-export([add_resource/1, del_resource/1, get_resource/0, get_resource/1,
		get_resource_name/1, query_resource/4, query_resource/1]).
-export([add_user/3, del_user/1, get_user/0, get_user/1,
		query_user/4, query_user/1]).
-export([import/1]).
-export([generate_password/0, generate_identity/0]).

-include("im.hrl").
-include_lib("inets/include/mod_auth.hrl").

-define(CHUNKSIZE, 100).
%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).
-define(IDOFFSET, 63681984000).

-define(PathCatalog, "/resourceCatalogManagement/v3/").
-define(PathInventory, "/resourceInventoryManagement/v3/").

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

-spec query_catalog(Continuation, Size, MatchHead, MatchConditions) -> Result
	when
		Continuation :: start | ets:continuation(),
		Size :: pos_integer() | undefined,
		MatchHead :: ets:match_pattern(),
		MatchConditions :: [tuple()],
		Result :: {NextContinuation, [#catalog{}]} | {error, Reason},
		NextContinuation:: eof | ets:continuation(),
		Reason :: term().
%% @doc Query the Resource Catalog.
query_catalog(start = _Continuation, undefined, MatchHead, MatchConditions) ->
	query_catalog(start = _Continuation, ?CHUNKSIZE, MatchHead, MatchConditions);
query_catalog(start = _Continuation, Size, MatchHead, MatchConditions)
		when is_integer(Size), Size > 0,
		(is_record(MatchHead,  catalog) orelse (MatchHead == '_')),
		is_list(MatchConditions) ->
	MatchExpression = [{MatchHead, MatchConditions, ['$_']}],
	F = fun() ->
			 mnesia:select(catalog, MatchExpression, Size, read)
	end,
	case mnesia:ets(F) of
		{error, Reason} ->
			{error, Reason};
		'$end_of_table' ->
			{eof, []};
		{Catalogs, NextContinuation} ->
			{NextContinuation, Catalogs}
	end.

-spec query_catalog(Continuation) -> Result
	when
		Continuation :: ets:continuation(),
		Result :: {NextContinuation, [#catalog{}]} | {error, Reason},
		NextContinuation:: eof | ets:continuation(),
		Reason :: term().
%% @doc Get the next results using a previous query of the Resource Catalog.
query_catalog(Continuation) ->
	F = fun() ->
			 mnesia:select(Continuation)
	end,
	case mnesia:ets(F) of
		{error, Reason} ->
			{error, Reason};
		'$end_of_table' ->
			{eof, []};
		{Catalogs, NextContinuation} ->
			{NextContinuation, Catalogs}
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

-spec query_category(Continuation, Size, MatchHead, MatchConditions) -> Result
	when
		Continuation :: start | ets:continuation(),
		Size :: pos_integer() | undefined,
		MatchHead :: ets:match_pattern(),
		MatchConditions :: [tuple()],
		Result :: {NextContinuation, [#category{}]} | {error, Reason},
		NextContinuation:: eof | ets:continuation(),
		Reason :: term().
%% @doc Query the Resource Categories.
query_category(start = _Continuation, undefined, MatchHead, MatchConditions) ->
	query_category(start = _Continuation, ?CHUNKSIZE, MatchHead, MatchConditions);
query_category(start = _Continuation, Size, MatchHead, MatchConditions)
		when is_integer(Size), Size > 0,
		(is_record(MatchHead,  category) orelse (MatchHead == '_')),
		is_list(MatchConditions) ->
	MatchExpression = [{MatchHead, MatchConditions, ['$_']}],
	F = fun() ->
			 mnesia:select(category, MatchExpression, Size, read)
	end,
	case mnesia:ets(F) of
		{error, Reason} ->
			{error, Reason};
		'$end_of_table' ->
			{eof, []};
		{Categories, NextContinuation} ->
			{NextContinuation, Categories}
	end.

-spec query_category(Continuation) -> Result
	when
		Continuation :: ets:continuation(),
		Result :: {NextContinuation, [#category{}]} | {error, Reason},
		NextContinuation:: eof | ets:continuation(),
		Reason :: term().
%% @doc Get the next results using a previous query of Resource Categories.
query_category(Continuation) ->
	F = fun() ->
			 mnesia:select(Continuation)
	end,
	case mnesia:ets(F) of
		{error, Reason} ->
			{error, Reason};
		'$end_of_table' ->
			{eof, []};
		{Categories, NextContinuation} ->
			{NextContinuation, Categories}
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

-spec query_candidate(Continuation, Size, MatchHead, MatchConditions) -> Result
	when
		Continuation :: start | ets:continuation(),
		Size :: pos_integer() | undefined,
		MatchHead :: ets:match_pattern(),
		MatchConditions :: [tuple()],
		Result :: {NextContinuation, [#candidate{}]} | {error, Reason},
		NextContinuation:: eof | ets:continuation(),
		Reason :: term().
%% @doc Query the Resource Candidates.
query_candidate(start = _Continuation, undefined, MatchHead, MatchConditions) ->
	query_candidate(start = _Continuation, ?CHUNKSIZE, MatchHead, MatchConditions);
query_candidate(start = _Continuation, Size, MatchHead, MatchConditions)
		when is_integer(Size), Size > 0,
		(is_record(MatchHead,  candidate) orelse (MatchHead == '_')),
		is_list(MatchConditions) ->
	MatchExpression = [{MatchHead, MatchConditions, ['$_']}],
	F = fun() ->
			 mnesia:select(candidate, MatchExpression, Size, read)
	end,
	case mnesia:ets(F) of
		{error, Reason} ->
			{error, Reason};
		'$end_of_table' ->
			{eof, []};
		{Categories, NextContinuation} ->
			{NextContinuation, Categories}
	end.

-spec query_candidate(Continuation) -> Result
	when
		Continuation :: ets:continuation(),
		Result :: {NextContinuation, [#candidate{}]} | {error, Reason},
		NextContinuation:: eof | ets:continuation(),
		Reason :: term().
%% @doc Get the next results using a previous query of Resource Candidates.
query_candidate(Continuation) ->
	F = fun() ->
			 mnesia:select(Continuation)
	end,
	case mnesia:ets(F) of
		{error, Reason} ->
			{error, Reason};
		'$end_of_table' ->
			{eof, []};
		{Candidates, NextContinuation} ->
			{NextContinuation, Candidates}
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

-spec query_specification(Continuation, Size, MatchHead, MatchConditions) -> Result
	when
		Continuation :: start | ets:continuation(),
		Size :: pos_integer() | undefined,
		MatchHead :: ets:match_pattern(),
		MatchConditions :: [tuple()],
		Result :: {NextContinuation, [#specification{}]} | {error, Reason},
		NextContinuation:: eof | ets:continuation(),
		Reason :: term().
%% @doc Query the Resource Specifications.
query_specification(start = _Continuation, undefined, MatchHead, MatchConditions) ->
	query_specification(start = _Continuation, ?CHUNKSIZE, MatchHead, MatchConditions);
query_specification(start = _Continuation, Size, MatchHead, MatchConditions)
		when is_integer(Size), Size > 0,
		(is_record(MatchHead,  specification) orelse (MatchHead == '_')),
		is_list(MatchConditions) ->
	MatchExpression = [{MatchHead, MatchConditions, ['$_']}],
	F = fun() ->
			 mnesia:select(specification, MatchExpression, Size, read)
	end,
	case mnesia:ets(F) of
		{error, Reason} ->
			{error, Reason};
		'$end_of_table' ->
			{eof, []};
		{Categories, NextContinuation} ->
			{NextContinuation, Categories}
	end.

-spec query_specification(Continuation) -> Result
	when
		Continuation :: ets:continuation(),
		Result :: {NextContinuation, [#specification{}]} | {error, Reason},
		NextContinuation:: eof | ets:continuation(),
		Reason :: term().
%% @doc Get the next results using a previous query of Resource Specifications.
query_specification(Continuation) ->
	F = fun() ->
			 mnesia:select(Continuation)
	end,
	case mnesia:ets(F) of
		{error, Reason} ->
			{error, Reason};
		'$end_of_table' ->
			{eof, []};
		{Specifications, NextContinuation} ->
			{NextContinuation, Specifications}
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
		{atomic, [Resource]} ->
			{ok, Resource};
		{atomic, []} ->
			{error, not_found}
	end.

-spec query_resource(Continuation, Size, MatchHead, MatchConditions) -> Result
	when
		Continuation :: start | ets:continuation(),
		Size :: pos_integer() | undefined,
		MatchHead :: ets:match_pattern(),
		MatchConditions :: [tuple()],
		Result :: {NextContinuation, [#resource{}]} | {error, Reason},
		NextContinuation:: eof | ets:continuation(),
		Reason :: term().
%% @doc Query the inventory Resources.
query_resource(start = _Continuation, undefined, MatchHead, MatchConditions) ->
	query_resource(start = _Continuation, ?CHUNKSIZE, MatchHead, MatchConditions);
query_resource(start = _Continuation, Size, MatchHead, MatchConditions)
		when is_integer(Size), Size > 0,
		(is_record(MatchHead,  resource) orelse (MatchHead == '_')),
		is_list(MatchConditions) ->
	MatchExpression = [{MatchHead, MatchConditions, ['$_']}],
	F = fun() ->
			 mnesia:select(resource, MatchExpression, Size, read)
	end,
	case mnesia:ets(F) of
		{error, Reason} ->
			{error, Reason};
		'$end_of_table' ->
			{eof, []};
		{Categories, NextContinuation} ->
			{NextContinuation, Categories}
	end.

-spec query_resource(Continuation) -> Result
	when
		Continuation :: ets:continuation(),
		Result :: {NextContinuation, [#resource{}]} | {error, Reason},
		NextContinuation:: eof | ets:continuation(),
		Reason :: term().
%% @doc Get the next results using a previous query of inventory Resources.
query_resource(Continuation) ->
	F = fun() ->
			 mnesia:select(Continuation)
	end,
	case mnesia:ets(F) of
		{error, Reason} ->
			{error, Reason};
		'$end_of_table' ->
			{eof, []};
		{Resources, NextContinuation} ->
			{NextContinuation, Resources}
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

-spec query_user(Continuation, Size, MatchHead, MatchConditions) -> Result
	when
		Continuation :: start | ets:continuation(),
		Size :: pos_integer() | undefined,
		MatchHead :: ets:match_pattern(),
		MatchConditions :: [tuple()],
		Result :: {NextContinuation, [#httpd_user{}]} | {error, Reason},
		NextContinuation:: eof | ets:continuation(),
		Reason :: term().
%% @doc Query the REST Users.
query_user(start = _Continuation, undefined, MatchHead, MatchConditions) ->
	query_user(start = _Continuation, ?CHUNKSIZE, MatchHead, MatchConditions);
query_user(start = _Continuation, Size, MatchHead, MatchConditions)
		when is_integer(Size), Size > 0,
		(is_record(MatchHead,  httpd_user) orelse (MatchHead == '_')),
		is_list(MatchConditions) ->
	MatchExpression = [{MatchHead, MatchConditions, ['$_']}],
	F = fun() ->
			 mnesia:select(httpd_user, MatchExpression, Size, read)
	end,
	case mnesia:ets(F) of
		{error, Reason} ->
			{error, Reason};
		'$end_of_table' ->
			{eof, []};
		{Users, NextContinuation} ->
			{NextContinuation, Users}
	end.

-spec query_user(Continuation) -> Result
	when
		Continuation :: ets:continuation(),
		Result :: {NextContinuation, [#httpd_user{}]} | {error, Reason},
		NextContinuation:: eof | ets:continuation(),
		Reason :: term().
%% @doc Get the next results using a previous query of REST Users.
query_user(Continuation) ->
	F = fun() ->
			 mnesia:select(Continuation)
	end,
	case mnesia:ets(F) of
		{error, Reason} ->
			{error, Reason};
		'$end_of_table' ->
			{eof, []};
		{Users, NextContinuation} ->
			{NextContinuation, Users}
	end.

-spec import(File) -> Result
	when
		File :: string(),
		Result :: ok | ignore | {error, Reason},
		Reason :: term().
%% @doc Import a file in the inventory table.
import(File) when is_list(File) ->
	im_xml_cm_bulk:import(File).

-type password() :: [50..57 | 97..104 | 106..107 | 109..110 | 112..116 | 119..122].
-spec generate_password() -> password().
%% @equiv generate_password(12)
generate_password() ->
	generate_password(12).

-spec generate_identity() -> string().
%% @equiv generate_identity(7)
generate_identity() ->
	generate_identity(7).

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


