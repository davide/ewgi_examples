%% @author Filippo Pacini <filippo.pacini@gmail.com>
%% @copyright 2009 S.G. Consulting s.r.l.

%% @doc Web server for ewgi_examples.

-module(ewgi_examples_web).
-author('Filippo Pacini <filippo.pacini@gmail.com>').

%% Mochiweb exports
-export([start_link/2, 
         loop/3]).

%% Yaws exports
-export([out/1]).
-include_lib("yaws/include/yaws_api.hrl").

%% Inets exports
-export([save_inets_ssl_password/2, fetch_inets_ssl_password/1]).
-define(INETS_SSL_ETS, inets_ssl_passwords).

%% External API
start_link(WebServer, Options) ->
    {ok, Pid} = start_webserver(WebServer, Options),
    link(Pid),
    {ok, Pid}.

start_webserver(mochiweb, Options) ->
    {ServerName, Options1} = get_option(server_name, Options),
    {EwgiEntryApp, Options2} = get_option(ewgi_entry_app, Options1),
    {DocRoot, Options3} = get_option(docroot, Options2),
    {SSLOptions, OtherOptions} = get_option(ssl, Options3),

    {M, F, A} = EwgiEntryApp,
    Appl = ewgi_application:mfa_mw(M, F, A),

    Loop = fun (Req) ->
		   ?MODULE:loop(Req, Appl, DocRoot)
           end,
    ServerOptions0 = [{name, ServerName}, {loop, Loop} | OtherOptions],
    SSL = 
	if	SSLOptions =:= undefined -> [];
		true -> [{ssl, true}|SSLOptions]
	end,
    ServerOptions = ServerOptions0 ++ SSL,
    mochiweb_http:start(ServerOptions);

start_webserver(inets, Options) ->
    {ServerName, Options1} = get_option(server_name, Options),
    {EwgiEntryApp, Options2} = get_option(ewgi_entry_app, Options1),
    {Port, Options3} = get_option(port, Options2),
    {DocRoot, Options4} = get_option(docroot, Options3),
    {SSLOptions, _IgnoredOptions} = get_option(ssl, Options4),
    application:start(inets),

    {M, F, A} = EwgiEntryApp,
    Appl = ewgi_application:mfa_mw(M, F, A),

    ServerOptions0 = [{port, Port},
		      {document_root, DocRoot},
		      {server_root, "."},
		      {server_name, ServerName},
		      {modules, [ewgi_inets]},
		      %% Made accessible in #mod.config_db
		      {ewgi_entry_app, Appl}],
    SSL =
	if	SSLOptions =:= undefined -> [];
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
			    ?MODULE:save_inets_ssl_password(ServerName, Pwd),
			    [{ssl_password_callback_module, ?MODULE},
			     {ssl_password_callback_function, inets_ssl_password},
			     {ssl_password_callback_arguments, [ServerName]}]
		    end,
		[{socket_type, ssl} | ConvertedSSLValues] ++ SSLPasswordOptions
	end,
    ServerOptions = ServerOptions0 ++ SSL,
    inets:start(httpd, ServerOptions);

start_webserver(yaws, Options) ->
    {ServerName, Options1} = get_option(server_name, Options),
    {EwgiEntryApp, Options2} = get_option(ewgi_entry_app, Options1),
    {DocRoot, Options3} = get_option(docroot, Options2),
    {Port, Options4} = get_option(port, Options3),
    {SSLOptions, _IgnoredOptions} = get_option(ssl, Options4),

    {M, F, A} = EwgiEntryApp,
    Appl = ewgi_application:mfa_mw(M, F, A),

    ServerOptions0 = [{servername, ServerName},
		      {listen, {0,0,0,0}},
		      {port, Port},
		      {appmods, [{"/", ?MODULE}]},
		      %% Taking over #arg.opaque as the ewgi_entry_app carrier
		      {opaque, Appl}],
    SSL =
	if	SSLOptions =:= undefined -> [];
		true -> [{ssl, SSLOptions}]
	end,		  
    ServerOptions = ServerOptions0 ++ SSL,
    LogDir = "./log/",
    GL = [{logdir, LogDir}],
    filelib:ensure_dir(LogDir),
    ok = yaws:start_embedded(DocRoot, ServerOptions, GL),
    {ok, whereis(yaws_server)}.


%% Mochiweb functions
loop(Req, RootApp, _DocRoot) ->
    ewgi_mochiweb:run(RootApp, Req).

%% Inets functions
save_inets_ssl_password(ServerName, Password) ->
    case ets:info(?INETS_SSL_ETS) of
	undefined -> ets:new(?INETS_SSL_ETS, [named_table]);
	_ -> already_existed
    end,
    ets:insert(?INETS_SSL_ETS, {ServerName, Password}).

fetch_inets_ssl_password(ServerName) ->
    {ServerName, Password} = 
	ets:lookup(?INETS_SSL_ETS, ServerName),
    Password.

%% Yaws' functions
out(Arg) ->
    RootApp = Arg#arg.opaque,
    ewgi_yaws:run(RootApp, Arg).


%% Internal API
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
