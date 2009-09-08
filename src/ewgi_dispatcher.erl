%% @author Filippo Pacini <filippo.pacini@gmail.com>
%% @copyright 2009 S.G. Consulting s.r.l.

%% @doc dispatcher middleware

-module(ewgi_dispatcher).
-author('Filippo Pacini <filippo.pacini@gmail.com>').

%% Middleware exports
-export([run/2, dispatch/1]).

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
    ewgi_session:cookie_store_example(Ctx, create);
dispatch("/session/cookie/delete", Ctx) ->
    ewgi_session:cookie_store_example(Ctx, delete);

dispatch("/session/server", Ctx) ->
    ewgi_session:server_store_example(Ctx, create);
dispatch("/session/server/delete", Ctx) ->
    ewgi_session:server_store_example(Ctx, delete);

dispatch(_, Ctx) ->   
    ewgi_api:response_message_body("404 Not Found", 
                                   ewgi_api:response_status({404, "Not Found"}, Ctx)).
