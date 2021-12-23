#!/usr/bin/env escript
%% vim: syntax=erlang

main([]) ->
	case catch mnesia:system_info(db_nodes) of
		[] ->
			case im_app:install() of
				{ok, Tables} ->
					io:fwrite("~p~n", [Tables]);
				{error, Reason} ->
					io:fwrite("error: ~w~n", [Reason]),
					erlang:halt(1)
			end;
		Nodes ->
			case mnesia:set_master_nodes(Nodes) of
				ok ->
					case im_app:install() of
						{ok, Tables} ->
							io:fwrite("~p~n", [Tables]);
						{error, Reason} ->
							io:fwrite("error: ~w~n", [Reason]),
							erlang:halt(1)
					end;
				{error, Reason} ->
					io:fwrite("error: ~w~n", [Reason]),
					erlang:halt(1)
			end
	end.
