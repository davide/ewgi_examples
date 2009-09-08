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

