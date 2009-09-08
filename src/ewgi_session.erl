%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @author Davide Marquês <nesrait@gmail.com>
%% @copyright 2009 Smarkets Limited.
%%
%% @doc Session middleware with pluggable storage mechanisms.
%% Based on smak_auth_cookie by Hunter Morris.
%%
%% The loading of session data is done when the middleware is
%% initialized and the storage of session data is only done(!)
%% after calling the middleware downstream.
%%
%% In case of errors in the downstream middleware the updates
%% to session data are not persisted.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(ewgi_session).
-author('Hunter Morris <hunter.morris@smarkets.com>').
-author('Davide Marquês <nesrait@gmail.com>').

%% API
-export([run/2]).
-export([new_session/1, get_session_data/1, get_session_data/2, set_session_data/3, remove_session_data/2, delete_session/1]).

-include("smak.hrl").
-include("session.hrl").

-record(cka, {
	  store,
	  error_app,
	  include_ip = false,
          timeout = 15 * 60 * 1000, %% 15 minutes
	  session_data = [],
	  action = undefined,
	  store_config = undefined
         }).

-define(IF_SESSION(Ctx, DoWork),
	case ewgi_api:find_data(?EWGI_SESSION_CONFIG, Ctx) of
	    Cka when is_record(Cka, cka) ->
		DoWork(Ctx, Cka);
	    undefined ->
		error_logger:error_msg("Session not initialized!"),
		Ctx
	end).

%%====================================================================
%% Session API
%%====================================================================
new_session(Ctx0) ->
    F = fun(Ctx, #cka{store=Store, store_config=Cfg} = Cka0) ->
		Cfg1 = Store:new_session(Cfg),
		Cka = Cka0#cka{session_data=[], action=save_session, store_config=Cfg1},
		ewgi_api:store_data(?EWGI_SESSION_CONFIG, Cka, Ctx)
	end,
    ?IF_SESSION(Ctx0, F).

delete_session(Ctx0) ->
    F = fun(Ctx, Cka0) ->
		Cka = Cka0#cka{session_data=[], action=delete_session},
		ewgi_api:store_data(?EWGI_SESSION_CONFIG, Cka, Ctx)
       end,
    ?IF_SESSION(Ctx0, F).

%%====================================================================
%% Functions that deal with the already loaded session_data
%%====================================================================
get_session_data(Ctx0) ->
    F = fun(_, #cka{session_data=Data}) ->
	       Data
       end,
    ?IF_SESSION(Ctx0, F).

get_session_data(Ctx0, Key) ->
    F = fun(_, #cka{session_data=Data}) ->
		proplists:get_value(Key, Data)
	end,
    ?IF_SESSION(Ctx0, F).

set_session_data(Ctx0, Key, Value) ->
    F = fun(Ctx, #cka{session_data=Data} = Cka0) ->
		Data1 =
		    case proplists:is_defined(Key, Data) of
			true ->
			    Rest = proplists:delete(Key, Data),
			    [{Key, Value}|Rest];
			false ->
			    [{Key, Value}|Data]
		    end,
		Cka = Cka0#cka{session_data=Data1, action=save_session},
		ewgi_api:store_data(?EWGI_SESSION_CONFIG, Cka, Ctx)
	end, 
    ?IF_SESSION(Ctx0, F).

remove_session_data(Ctx0, Key) ->
    F = fun(Ctx, #cka{session_data=Data} = Cka0) ->
		case proplists:is_defined(Key, Data) of
		    true ->
			Data1 = proplists:delete(Key, Data),
			Cka = Cka0#cka{session_data=Data1, action=save_session},
			ewgi_api:store_data(?EWGI_SESSION_CONFIG, Cka, Ctx);
		    false ->
			Ctx
		end
	end,
    ?IF_SESSION(Ctx0, F).

%%====================================================================
%% Middleware initialization
%%====================================================================

%% @spec run(Ctx::ewgi_context(), Args::any()) -> ewgi_context()
%% @doc Initializes the cookie based session middleware.
-spec run(ewgi_context(), any()) -> ewgi_context().
run(Ctx, [ErrorApp, NoSessionApp, SessionApp, IncludeIp, Timeout, {Store, SessionStoreArgs}]) ->
    {StoreConfig, InitResult} = Store:init(Ctx, SessionStoreArgs),
    Cka = #cka{store=Store,
	       store_config=StoreConfig,
	       error_app=ErrorApp,
	       include_ip=IncludeIp,
	       timeout=Timeout},
    case InitResult of
	ok ->
	    load_session(Ctx, Cka, NoSessionApp, SessionApp);
	no_session ->
	    run_sessioned_app(Ctx, Cka, NoSessionApp);
	{error, Reason} ->
	    process_init_error(Ctx, Cka, NoSessionApp, Reason)
    end.

load_session(Ctx, Cka0, NoSessionApp, SessionApp) ->
    case (Cka0#cka.store):load_session(Ctx, Cka0#cka.store_config) of
	{error, Reason} ->
	    process_init_error(Ctx, Cka0, NoSessionApp, Reason);
	no_session ->
	    run_sessioned_app(Ctx, Cka0, NoSessionApp);
	Session when is_record(Session, session) ->
	    case validate_session(Ctx, Cka0, Session) of
		{error, Reason} ->
		    process_init_error(Ctx, Cka0, NoSessionApp, Reason);
		ValidSession ->
		    Data = ValidSession#session.data,
		    Cka = Cka0#cka{session_data=Data},
		    run_sessioned_app(Ctx, Cka, SessionApp)
	    end;
	Other ->
	    process_init_error(Ctx, Cka0, NoSessionApp, {invalid_session, Other})
    end.

validate_session(Ctx, Cka, Session) ->
    ExpireTime = Session#session.timestamp + Cka#cka.timeout,
    Expired = smak_calendar:now_utc_ts_ms() >= ExpireTime,
    if Expired ->
	    {error, session_timeout};
       true ->
	    case Cka#cka.include_ip of
		false ->
		    Session;
		true ->
		    Addr = ewgi_api:remote_addr(Ctx),
		    SavedAddr = Session#session.ip_address,
		    if SavedAddr =:= Addr ->
			    Session;
		       true ->
			    error_logger:error_msg("Saved address: ~p, New address: ~p", [SavedAddr, Addr]),
			    {error, invalid_ip_address}
		    end
	    end
    end.

%% Process the error we encountered, clear the headers and proceed to NoSessionApp.
-spec process_init_error(ewgi_context(), #cka{}, ewgi_app(), any()) -> ewgi_context().
process_init_error(Ctx, Cka, NoSessionApp, Error) ->
    Ctx1 = ewgi_api:response_error({?MODULE, Error}, Ctx),
    Ctx2 = (Cka#cka.error_app)(Ctx1),
    Ctx3 = (Cka#cka.store):delete_session(Ctx2, Cka#cka.store_config),
    run_sessioned_app(Ctx3, Cka, NoSessionApp).

run_sessioned_app(Ctx, Cka, App) ->
    %% Store session_config so that App can call session functions
    Ctx1 = ewgi_api:store_data(?EWGI_SESSION_CONFIG, Cka, Ctx),
    Ctx2 = App(Ctx1),
    save_session(Ctx2).

save_session(Ctx) ->
    case ewgi_api:find_data(?EWGI_SESSION_CONFIG, Ctx) of
	#cka{action=undefined} ->
	    Ctx;
	#cka{store_config=undefined} ->
	    %% If we're here then errors occurred. Check the error log!
	    Ctx;
	#cka{store=Store, action=delete_session, store_config=Cfg} ->
	    Store:delete_session(Ctx, Cfg);
	#cka{store=Store, action=save_session, store_config=Cfg, session_data=Data} = Cka ->
	    Timestamp = smak_calendar:now_utc_ts_ms(),
	    Session0 = #session{data=Data, timestamp=Timestamp},
	    Session = if Cka#cka.include_ip ->
			      Addr = ewgi_api:remote_addr(Ctx),
			      Session0#session{ip_address=Addr};
			 true ->
			      Session0
		      end,
	    case Store:store_session(Ctx, Cfg, Session) of
		{error, Reason} ->
		    Ctx1 = ewgi_api:response_error({?MODULE, {error_storing_data, Reason}}, Ctx),
		    (Cka#cka.error_app)(Ctx1);
		Ctx1 ->
		    Ctx1
	    end
    end.
