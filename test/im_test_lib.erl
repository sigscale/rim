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
			case mnesia:wait_for_tables([inventory, catalog], 1000) of
				{timeout, _} ->
					ok = application:stop(mnesia),
					{ok, Tables} = im_app:install(),
					F = fun(T) ->
						case T of
							T when T == inventory; T == catalog;
									T == httpd_user; T == httpd_group ->
								true;
							_ ->
								false
						end
					end,
					lists:all(F, Tables),
					initialize_db();
				ok ->
					ok
			end
	end.

start() ->
	start([crypto, inets, asn1, public_key, ssl, xmerl, compiler,
		syntax_tools, sigscale_im]).

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
	application:stop(sigscale_im).

