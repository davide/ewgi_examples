%% @author Filippo Pacini <filippo.pacini@gmail.com>
%% @copyright 2009 S.G. Consulting s.r.l.

%% @doc Web server for ewgi_examples.

-module(ewgi_examples_web).
-author('Filippo Pacini <filippo.pacini@gmail.com>').

%% Mochiweb exports
-export([start_link/2, 
         stop/0, 
         loop/2]).

%% Yaws exports
-export([out/1]).

%% External API
start_link(WebServer, Options) ->
    {ok, Pid} = start_webserver(WebServer, Options),
    link(Pid),
    {ok, Pid}.

start_webserver(mochiweb, Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
		   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]);

start_webserver(inets, Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    {Port, _Options2} = get_option(port, Options1),
    application:start(inets),
    inets:start(httpd, [{server_name,"ewgi_examples"}, {server_root,"."}, {document_root,DocRoot}, {modules,[ewgi_inets]}, {port,Port}]);

start_webserver(yaws, Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    {Port, _Options2} = get_option(port, Options1),
    AppMods = [{"/", ewgi_examples_web}],
    SL = [{servername,"ewgi_examples"}, {listen,{0,0,0,0}}, {port,Port}, {appmods,AppMods}],
    LogDir = "./log/",
    GL = [{logdir,LogDir}],
    filelib:ensure_dir(LogDir),
    ok = yaws:start_embedded(DocRoot, SL, GL),
    {ok, whereis(yaws_server)}.

stop() ->
    mochiweb_http:stop(?MODULE).

%% Mochiweb functions
loop(Req, _DocRoot) ->
    {ok, M} = application:get_env(ewgi, ewgi_entry_app_module),
    {ok, F} = application:get_env(ewgi, ewgi_entry_app_function),
    {ok, A} = application:get_env(ewgi, ewgi_entry_app_args),
    RootApp = ewgi_application:mfa_mw(M, F, A),
    ewgi_mochiweb:run(RootApp, Req).

%% Yaws' functions
out(Arg) ->
    {ok, M} = application:get_env(ewgi, ewgi_entry_app_module),
    {ok, F} = application:get_env(ewgi, ewgi_entry_app_function),
    {ok, A} = application:get_env(ewgi, ewgi_entry_app_args),
    RootApp = ewgi_application:mfa_mw(M, F, A),
    ewgi_yaws:run(RootApp, Arg).


%% Internal API
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
