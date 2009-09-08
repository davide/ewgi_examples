%% @author Filippo Pacini <filippo.pacini@gmail.com>
%% @copyright 2009 S.G. Consulting s.r.l.

%% @doc dispatcher middleware

-module(ewgi_dispatcher).
-author('Filippo Pacini <filippo.pacini@gmail.com>').

%% Middleware exports
-export([run/2, dispatch/1]).

-define(INCLUDE_IP, true).
-define(SESSION_TIMEOUT, 15 * 60 * 1000). %% 15 minutes

-define(SESSION_SERVER_REF, ewgi_session_server).

%% This is the first middleware that gets called by the various webservers
run(Ctx, []) ->
    dispatch(ewgi_api:path_info(Ctx), Ctx).

dispatch(Ctx) ->
    dispatch(ewgi_api:path_info(Ctx), Ctx).

dispatch("/", Ctx) ->
    ewgi_index:run(Ctx,[]);
dispatch("/hello", Ctx) ->
    ewgi_hello:run(Ctx,[]);
dispatch("/HELLO", Ctx) ->
    ewgi_to_upper:run(ewgi_hello:run(Ctx,[]), []);
dispatch("/postex", Ctx) ->
    ewgi_post:post_app_example(Ctx);
dispatch("/test.txt", Ctx) ->
    ewgi_stream_file:run(Ctx,["priv/www/test.txt"]);
dispatch("/gzhello", Ctx) ->
    ewgi_deflate:run(ewgi_hello:run(Ctx,[]), []);


dispatch("/session/cookie", Ctx) ->
    {NoSession, Session, MiddlewareError} = session_create_apps("cookie"),
    SessionStore = {ewgi_session_cookie_store, ["cookie_session_id", <<"ABCDEFGHIJKLMNOP">>, [{secure, false}]]},
    ewgi_session:run(Ctx, [MiddlewareError, NoSession, Session, ?INCLUDE_IP, ?SESSION_TIMEOUT, SessionStore]);
dispatch("/session/cookie/delete", Ctx) ->
    {NoSession, Session, MiddlewareError} = session_delete_apps(),
    SessionStore = {ewgi_session_cookie_store, ["cookie_session_id", <<"ABCDEFGHIJKLMNOP">>, [{secure, false}]]},
    ewgi_session:run(Ctx, [MiddlewareError, NoSession, Session, ?INCLUDE_IP, ?SESSION_TIMEOUT, SessionStore]);


dispatch("/session/server", Ctx) ->
    {NoSession, Session, MiddlewareError} = session_create_apps("server"),
    SessionStore = {ewgi_session_server_store, [?SESSION_SERVER_REF, "server_session_id", false]},
    ewgi_session:run(Ctx, [MiddlewareError, NoSession, Session, ?INCLUDE_IP, ?SESSION_TIMEOUT, SessionStore]);
dispatch("/session/server/delete", Ctx) ->
    {NoSession, Session, MiddlewareError} = session_delete_apps(),
    SessionStore = {ewgi_session_server_store, [?SESSION_SERVER_REF, "server_session_id", false]},
    ewgi_session:run(Ctx, [MiddlewareError, NoSession, Session, ?INCLUDE_IP, ?SESSION_TIMEOUT, SessionStore]);

dispatch(_, Ctx) ->   
    ewgi_api:response_message_body("404 Not Found", 
                                   ewgi_api:response_status({404, "Not Found"}, Ctx)).

%% Private functions
session_create_apps(Store) ->
    NoSessionApp =
	fun(Ctx) ->
		Ctx1 = ewgi_session:new_session(Ctx),
		ewgi_api:response_message_body("Created session! (please refresh the page)",
					       ewgi_api:response_status({200, "OK"}, Ctx1))
	end, 
    SessionApp =
	fun(Ctx) ->
		case ewgi_session:get_session_data(Ctx, name) of
		    undefined ->
			Name = "Bill",
			Body = ["Hello stranger! I'll call you ", Name, " from now on! (please refresh the page)"],
			Ctx1 = ewgi_session:set_session_data(Ctx, name, "Bill");
		    Name ->
			Body = ["Hello ", Name, "! Nice to see you again! (no point in reloading again, delete the session by visiting /session/", Store, "/delete)"],
			Ctx1 = Ctx
		end,
		Headers = [{"Content-type", "text/plain"}] ++ ewgi_api:response_headers(Ctx1),
		Ctx2 = ewgi_api:response_headers(Headers, Ctx1),
		Ctx3 = ewgi_api:response_status({200, "OK"}, Ctx2),
		ewgi_api:response_message_body(Body, Ctx3)
	end,
    MiddlewareErrorApp = fun middleware_error/1,
    {NoSessionApp, SessionApp, MiddlewareErrorApp}.

session_delete_apps() ->
    NoSessionApp =
	fun(Ctx) ->
		ewgi_api:response_message_body("No session to delete!",
					       ewgi_api:response_status({200, "OK"}, Ctx))
	end, 
    SessionApp =
	fun(Ctx) ->
		Ctx1 = ewgi_session:delete_session(Ctx),
		ewgi_api:response_message_body("Deleted session!",
					       ewgi_api:response_status({200, "OK"}, Ctx1))
	end,
    MiddlewareErrorApp = fun middleware_error/1,
    {NoSessionApp, SessionApp, MiddlewareErrorApp}.

middleware_error(Ctx) ->
    case ewgi_api:response_error(Ctx) of
	Other ->
	    error_logger:error_report(Other)
    end,
    ewgi_api:response_error(undefined, Ctx).

