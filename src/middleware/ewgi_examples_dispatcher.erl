%% @author Filippo Pacini <filippo.pacini@gmail.com>
%% @copyright 2009 S.G. Consulting s.r.l.

%% @doc dispatcher middleware

-module(ewgi_examples_dispatcher).
-author('Filippo Pacini <filippo.pacini@gmail.com>').

%% Middleware exports
-export([run/2]).

%% This is the first middleware that gets called by the various webservers
run(Ctx, []) ->
    dispatch(ewgi_api:path_info(Ctx), Ctx).

dispatch("/", Ctx) ->
    ewgi_examples_index:run(Ctx,[]);
dispatch("/hello", Ctx) ->
    ewgi_examples_hello:run(Ctx,[]);
dispatch("/HELLO", Ctx) ->
    ewgi_examples_to_upper:run(ewgi_examples_hello:run(Ctx,[]), []);
dispatch("/postex", Ctx) ->
    ewgi_post:post_app_example(Ctx);
dispatch("/test.txt", Ctx) ->
    ewgi_stream_file:run(Ctx,["priv/www/test.txt"]);
dispatch("/gzhello", Ctx) ->
    ewgi_deflate:run(ewgi_examples_hello:run(Ctx,[]), []);

dispatch("/session/cookie", Ctx) ->
    ewgi_session_cookie_store:create_example(Ctx);
dispatch("/session/cookie/delete", Ctx) ->
    ewgi_session_cookie_store:delete_example(Ctx);

dispatch("/session/server", Ctx) ->
    ewgi_session_server_store:create_example(Ctx);
dispatch("/session/server/delete", Ctx) ->
    ewgi_session_server_store:delete_example(Ctx);

dispatch("/push_stream/chunked", Ctx) ->
    ewgi_push_stream:chunked_stream_example(Ctx);
dispatch("/push_stream/non_chunked", Ctx) ->
    ewgi_push_stream:non_chunked_stream_example(Ctx);	
dispatch("/push_stream/ewgi_free", Ctx) ->
    ewgi_push_stream:ewgi_free_stream_example(Ctx);	

dispatch("/zip", Ctx) ->
    Dir = "priv/www",
    ZipType = "zip",
    Filename = "directory_contents",
    ewgi_dir_zipper:run(Ctx, [Dir, ZipType, Filename]);

dispatch("/websocket", Ctx) ->
    ewgi_websocket:websocket_example(Ctx);

dispatch([$/,$e,$w,$g,$i,$_,$s,$e,$r,$v,$e,$r|_RemPath], Ctx) ->
	DocRoot = "priv/www",
	DocRootMountPoint = "/ewgi_server",
	ewgi_server:run(Ctx, [DocRoot, DocRootMountPoint]);

dispatch(_, Ctx) ->   
    ewgi_api:response_message_body("404 Not Found", 
                                   ewgi_api:response_status({404, "Not Found"}, Ctx)).
