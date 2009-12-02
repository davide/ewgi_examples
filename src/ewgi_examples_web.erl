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
-include_lib("yaws/include/yaws.hrl").

%% Inets exports
-export([inets_ssl_password/0]).

%% External API
start_link(WebServer, Options) ->
    {ok, Pid} = start_webserver(WebServer, Options),
    link(Pid),
    {ok, Pid}.

start_webserver(mochiweb, Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    {SSLOptions, Options2} = get_option(ssl, Options1),
    Loop = fun (Req) ->
		   ?MODULE:loop(Req, DocRoot)
           end,
    Options0 = [{name, ?MODULE}, {loop, Loop} | Options2],
    ServerOptions = 
	if	SSLOptions =:= undefined -> Options0;
		true ->
		Options0 ++ [{ssl, true}|SSLOptions]
	end,
    mochiweb_http:start(ServerOptions);

start_webserver(inets, Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    {Port, Options2} = get_option(port, Options1),
    {SSLOptions, _Options3} = get_option(ssl, Options2),
    application:start(inets),
    Options0 = [{server_name,"ewgi_examples"}, {server_root,"."}, {document_root,DocRoot}, {modules,[ewgi_inets]}, {port,Port}],
    ServerOptions =
	if	SSLOptions =:= undefined -> Options0;
		true ->
		SSLKeyMatches = [
				 {keyfile, ssl_certificate_key_file},
				 {certfile, ssl_certificate_file},
				 {cacertfile, ssl_ca_certificate_file},
				 {verify, ssl_verify_client},
				 {depth, ssl_verify_client_depth},
				 {ciphers, ssl_ciphers}
				],
		ConvertedSSLValues = 
		    lists:foldl(fun({Key1, Key2}, Acc) ->
					case proplists:get_value(Key1, SSLOptions) of
					    undefined -> Acc;
					    Value -> [{Key2, Value}|Acc]
					end
				end,
				[],
				SSLKeyMatches),
		SSLPasswordOptions =
		    case proplists:get_value(password, SSLOptions) of
			undefined -> [];
			Pwd ->
			    application:set_env(ewgi, ssl_password, Pwd),
			    [{ssl_password_callback_module, ?MODULE},
			     {ssl_password_callback_function, inets_ssl_password}]
		    end,
		Options0 ++ [{socket_type, ssl}]
		    ++ ConvertedSSLValues ++ SSLPasswordOptions
	end,
    inets:start(httpd, ServerOptions);

start_webserver(yaws, Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    {Port, Options2} = get_option(port, Options1),
    {SSLOptions, _Options3} = get_option(ssl, Options2),
    AppMods = [{"/", ?MODULE}],
    SL0 = [{servername,"ewgi_examples"}, {listen,{0,0,0,0}}, {port,Port}, {appmods,AppMods}],
    SL =
	if	SSLOptions =:= undefined -> SL0;
		true ->
		SSL = #ssl{
		  keyfile = proplists:get_value(keyfile, SSLOptions),
		  certfile = proplists:get_value(certfile, SSLOptions),
		  verify = proplists:get_value(verify, SSLOptions, 0),
		  depth = proplists:get_value(depth, SSLOptions, 1),
		  password = proplists:get_value(password, SSLOptions),
		  cacertfile = proplists:get_value(cacertfile, SSLOptions, ""),
		  ciphers = proplists:get_value(ciphers, SSLOptions),
		  cachetimeout = proplists:get_value(cachetimeout, SSLOptions)
		 },
		[{ssl, SSL} | SL0]
		%% Applying this patch:
		%% http://github.com/davide/yaws/commit/2ed673322970f6ea84f5ab9dde466e5efb89368b
		%% enables passing the SSLOptions proplist directly, like this:
		%% [{ssl, SSLOptions} | SL0]
	end,
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

%% Inets functions
inets_ssl_password() ->
    Pwd = application:get_env(ewgi, ssl_password),
    application:set_env(ewgi, ssl_password, undefined),
    Pwd.

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
