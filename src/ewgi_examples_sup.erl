%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the ewgi_examples application.

-module(ewgi_examples_sup).
-author('author <author@example.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    {ok, ServerConfigs} = application:get_env(ewgi_examples, webservers),

    WebServers = [webserver_spec(Config) || Config <- ServerConfigs],

    SessionServer = {ewgi_session_server,
		     {ewgi_session_server, start_link, []},
		     permanent, 5000, worker, [ewgi_session_server]},

    Processes = [SessionServer | WebServers],
    {ok, {{one_for_one, 10, 10}, Processes}}.

%% Internal functions
webserver_spec(Config) ->
    {ServerGateway, Config1} = get_option(server_gateway, Config, yaws),
    {EwgiEntryApp, Config2} = get_option(ewgi_entry_app, Config1, undefined),
    {ServerName, Config3} = get_option(server_name, Config2, undefined),
    {Ip, Config4} = get_option(ip, Config3, "0.0.0.0"),
    {Port, Config5} = get_option(port, Config4, 8000),
    {DocRoot, Config6} = get_option(docroot, Config5, "priv/www"),
    {SSLOptions, OtherConfigs} = get_option(ssl, Config6, undefined),

    SSL =
	case SSLOptions of
	    undefined -> [];
	    SSLOptions -> [{ssl, SSLOptions}]
	end,

    ServerConfig =
	[{ewgi_entry_app, EwgiEntryApp},
	 {server_name, ServerName},
	 {ip, Ip},
	 {port, Port},
	 {docroot, DocRoot}] ++ SSL ++ OtherConfigs,

    {ServerName,
     {ewgi_examples_web, start_link, [ServerGateway, ServerConfig]},
     permanent, 5000, worker, dynamic}.

get_option(Option, Options, Default) ->
    {proplists:get_value(Option, Options, Default), proplists:delete(Option, Options)}.
