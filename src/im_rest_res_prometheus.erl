%%% im_rest_res_prometheus.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2021 SigScale Global Inc.
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
%%% This module exports metrics for Prometheus server to "scrape".
%%%
%%% @reference <a href="https://github.com/prometheus/prometheus">Prometheus.io</a>.
%%%
-module(im_rest_res_prometheus).
-copyright('Copyright (c) 2021 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0, get_metrics/2]).

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: [string()].
%% @doc Provide list of resource representations accepted.
content_types_accepted() ->
	[].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: [string()].
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["text/plain", "application/problem+json"].

-spec get_metrics(Query, Headers) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		Headers :: [tuple()],
		Result :: {ok, Headers :: [tuple()], Body :: iolist()}
				| {error, ErrorCode :: integer()}.
%% @doc Body producing function for `GET /metrics'
%% requests.
get_metrics([] = _Query, _Headers) ->
	List = [catalog, category, candidate, specification, resource],
	Body1 = lists:map(fun get_gauge/1, List),
	Body2 = arrange_metrics(get_metrics1(ets:first(metrics),
			[]), [], [], [], [], []),
	{ok, [{content_type, "text/plain"}], Body1 ++ Body2}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
get_metrics1(resourceCatalogCreate = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf634_resourceCatalog_total{label=create} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceCatalogChange = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf634_resourceCatalog_total{label=change} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceCatalogDelete = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf634_resourceCatalog_total{label=delete} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceCatalogRead = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf634_resourceCatalog_total{label=read} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceCategoryCreate = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf634_resourceCategory_total{label=create} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceCategoryChange = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf634_resourceCategory_total{label=change} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceCategoryDelete = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf634_resourceCategory_total{label=delete} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceCategoryRead = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf634_resourceCategory_total{label=read} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceCandidateCreate = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf634_resourceCandidate_total{label=create} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceCandidateChange = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf634_resourceCandidate_total{label=change} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceCandidateDelete = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf634_resourceCandidate_total{label=delete} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceCandidateRead = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf634_resourceCandidate_total{label=read} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceSpecificationCreate = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf634_resourceSpecification_total{label=create} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceSpecificationChange = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf634_resourceSpecification_total{label=change} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceSpecificationDelete = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf634_resourceSpecification_total{label=delete} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceSpecificationRead = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf634_resourceSpecification_total{label=read} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceInventoryCreate = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf639_resourceInventory_total{label=create} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceInventoryAttributeValueChange = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf639_resourceInventory_total{label=attributeValueChange} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceInventoryStateChange = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf639_resourceInventory_total{label=stateChange} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceInventoryDelete = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf639_resourceInventory_total{label=delete} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1(resourceInventoryRead = Key, Acc) ->
	[{Key, Value}] = ets:lookup(metrics, Key),
	NewAcc = ["tmf639_resourceInventory_total{label=read} "
			++ integer_to_list(Value) ++ "\n" | Acc],
	get_metrics1(ets:next(metrics, Key), NewAcc);
get_metrics1('$end_of_table', Acc) ->
	Acc.

%% @hidden
get_gauge(catalog = T) ->
			Size = mnesia:table_info(T, size),
			"# HELP tmf634_resourceCatalog_items Size of the TMF634 Catalog "
					"collection.\n# TYPE tmf634_resourceCatalog_items gauge\n"
					"tmf634_resourceCatalog_items " ++ integer_to_list(Size) ++ "\n";
get_gauge(category = T) ->
			Size = mnesia:table_info(T, size),
			"# HELP tmf634_resourceCategory_items Size of the TMF634 Category "
					"collection.\n# TYPE tmf634_resourceCategory_items gauge\n"
					"tmf634_resourceCategory_items " ++ integer_to_list(Size) ++ "\n";
get_gauge(candidate = T) ->
			Size = mnesia:table_info(T, size),
			"# HELP tmf634_resourceCandidate_items Size of the TMF634 Candidate "
					"collection.\n# TYPE tmf634_resourceCandidate_items gauge\n"
					"tmf634_resourceCandidate_items " ++ integer_to_list(Size) ++ "\n";
get_gauge(specification = T) ->
			Size = mnesia:table_info(T, size),
			"# HELP tmf634_resourceSpecification_items Size of the TMF634 Specification "
					"collection.\n# TYPE tmf634_resourceSpecification_items gauge\n"
					"tmf634_resourceSpecification_items " ++ integer_to_list(Size) ++ "\n";
get_gauge(resource = T) ->
			Size = mnesia:table_info(T, size),
			"# HELP tmf639_resourceInventory_items Size of the TMF639 Resource "
					"collection.\n# TYPE tmf639_resourceInventory_items gauge\n"
					"tmf639_resourceInventory_items " ++ integer_to_list(Size) ++ "\n".

%% @hidden
arrange_metrics(["tmf634_resourceCatalog" ++ _ = Res | T],
		CatalogAcc, CategoryAcc, CandidateAcc, SpecAcc, ResAcc) ->
	arrange_metrics(T, [Res | CatalogAcc],
			CategoryAcc, CandidateAcc, SpecAcc, ResAcc);
arrange_metrics(["tmf634_resourceCategory" ++ _ = Res | T],
		CatalogAcc, CategoryAcc, CandidateAcc, SpecAcc, ResAcc) ->
	arrange_metrics(T, CatalogAcc, [Res | CategoryAcc],
			CandidateAcc, SpecAcc, ResAcc);
arrange_metrics(["tmf634_resourceCandidate" ++ _ = Res | T],
		CatalogAcc, CategoryAcc, CandidateAcc, SpecAcc, ResAcc) ->
	arrange_metrics(T, CatalogAcc, CategoryAcc,
			[Res | CandidateAcc], SpecAcc, ResAcc);
arrange_metrics(["tmf634_resourceSpecification" ++ _ = Res | T],
		CatalogAcc, CategoryAcc, CandidateAcc, SpecAcc, ResAcc) ->
	arrange_metrics(T, CatalogAcc, CategoryAcc,
			CandidateAcc, [Res | SpecAcc], ResAcc);
arrange_metrics(["tmf639_resourceInventory" ++ _ = Res | T],
		CatalogAcc, CategoryAcc, CandidateAcc, SpecAcc, ResAcc) ->
	arrange_metrics(T, CatalogAcc, CategoryAcc,
			CandidateAcc, SpecAcc, [Res | ResAcc]);
arrange_metrics([], CatalogAcc, CategoryAcc, CandidateAcc, SpecAcc, ResAcc) ->
	F = fun (["tmf634_resourceCatalog" ++ _ | _] = List) ->
				{true, ["# HELP tmf634_resourceCatalog_total A counter of "
						"operations on TMF634 ResourceCatalog.\n"
						"# TYPE tmf634_resourceCatalog_total counter\n" | List]};
			(["tmf634_resourceCategory" ++ _ | _] = List) ->
				{true, ["# HELP tmf634_resourceCategory_total A counter of "
						"operations on TMF634 ResourceCategory.\n"
						"# TYPE tmf634_resourceCategory_total counter\n" | List]};
			(["tmf634_resourceCandidate" ++ _ | _] = List) ->
				{true, ["# HELP tmf634_resourceCandidate_total A counter of "
						"operations on TMF634 ResourceCandidate.\n"
						"# TYPE tmf634_resourceCandidate_total counter\n" | List]};
			(["tmf634_resourceSpecification" ++ _ | _] = List) ->
				{true, ["# HELP tmf634_resourceSpecification_total A counter of "
						"operations on TMF634 ResourceSpecification.\n"
						"# TYPE tmf634_resourceSpecification_total counter\n" | List]};
			(["tmf639_resourceInventory" ++ _ | _] = List) ->
				{true, ["# HELP tmf639_resourceInventory_total A counter of "
						"operations on TMF634 ResourceInventory.\n"
						"# TYPE tmf639_resourceInventory_total counter\n" | List]};
			([]) ->
				false
	end,
	lists:flatten(lists:filtermap(F, [CatalogAcc,
			CategoryAcc, CandidateAcc, SpecAcc, ResAcc])).

