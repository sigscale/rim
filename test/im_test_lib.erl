%%% im_test_lib.erl
%%% vim: ts=3
%%%
-module(im_test_lib).

-export([initialize_db/0, start/0, stop/0]).

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

initialize_db() ->
	case mnesia:system_info(is_running) of
		no ->
			ok = application:start(mnesia),
			initialize_db();
		S when S == starting; S == stopping ->
			receive
				after 1000 ->
					initialize_db()
			end;
		yes ->
			Tables = [httpd_group, httpd_user, resource,
					specification, candidate, category, catalog],
			case mnesia:wait_for_tables(Tables, 1000) of
				{timeout, _} ->
					ok = application:stop(mnesia),
					{ok, _} = im_app:install(),
					initialize_db();
				ok ->
					ok
			end
	end.

start() ->
	start([crypto, inets, asn1, public_key, ssl, xmerl, compiler,
		syntax_tools, im]).

start([H | T]) ->
	case application:start(H) of
		ok  ->
			start(T);
		{error, {already_started, H}} ->
			start(T);
		{error, Reason} ->
			{error, Reason}
	end;
start([]) ->
	ok.

stop() ->
	application:stop(im).

