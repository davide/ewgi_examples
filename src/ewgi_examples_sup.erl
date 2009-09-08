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
    WebServer = case os:getenv("EWGI_WEBSERVER") of
		    "mochiweb" -> mochiweb;
		    "yaws" -> yaws;
		    "inets" -> inets;
		    _ -> mochiweb
		end,
    Ip = case os:getenv("EWGI_WEBSERVER_IP") of false -> "0.0.0.0"; Any -> Any end,   
    WebConfig = [
		 {ip, Ip},
                 {port, 8000},
                 {docroot, ewgi_examples_deps:local_path(["priv", "www"])}],
    Web = {ewgi_examples_web,
           {ewgi_examples_web, start_link, [WebServer, WebConfig]},
           permanent, 5000, worker, dynamic},

    SessionServer = {ewgi_session_server,
		     {ewgi_session_server, start_link, []},
		     permanent, 5000, worker, [ewgi_session_server]},

    Processes = [Web, SessionServer],
    {ok, {{one_for_one, 10, 10}, Processes}}.
