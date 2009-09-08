%% @author Davide Marquês <nesrait@gmail.com>
%% @copyright 2009 Davide Marquês <nesrait@gmail.com>
%%
%% @doc Interface for server-side session stores.
%%
%% This store serves as an interface for stores that manage
%% client sessions on the server side using session_id's to
%% between differenciate the sessions.
%%
%% Currently cookies are needed for the client to hold the session_id.
%% BUT/TODO: we could also encode the session_id on the urls so it might
%% be a good idea to swap cookie_headers/4 for a inject_session_data_on_response/?
%% function defined on the ewgi_session module.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(ewgi_session_server_store).
-author('Davide Marquês <nesrait@gmail.com>').

%% Session Store API
-export([init/2, load_session/2, new_session/1, delete_session/2, store_session/3]).

-import(smak_cookie, [cookie_headers/4, cookie_safe_encode/1, cookie_safe_decode/1]).

-include("smak.hrl").
-include("session.hrl").

-define(DEFAULT_COOKIE_NAME, "session_id").
-define(SESSION_SERVER_MODULE, ewgi_session_server).

-record(store_config, {
	  server_id = undefined,
          cookie_name = ?DEFAULT_COOKIE_NAME,
	  secure_cookie = false,
	  session_id = undefined
	 }).

%%====================================================================
%% Session Store API
%%====================================================================
-spec init(ewgi_context(), any()) -> {#store_config{},ok} | {#store_config{},no_session} | {#store_config{},{error,any()}}.
init(Ctx, [ServerId, CookieName, SecureCookie] = _Args) when is_list(CookieName) ->
    Cfg = #store_config{server_id=ServerId, cookie_name=CookieName, secure_cookie=SecureCookie},
    case ewgi_api:get_header_value("cookie", Ctx) of
	undefined ->
	    {Cfg, no_session};
	Cookies ->
	    CookieValues = smak_cookie:parse_cookie(Cookies),
	    case proplists:get_value(Cfg#store_config.cookie_name, CookieValues) of
		undefined ->
		    {Cfg, no_session};
		SidB64 ->
		    BinSid = cookie_safe_decode(SidB64),
		    case (catch(binary_to_term(BinSid))) of
			Sid when is_list(Sid) ->
			    {Cfg#store_config{session_id=Sid}, ok};
			_ ->
			    {Cfg, {error, {?MODULE, cookie_tampered}}}
		    end
	    end
    end;
init(_Ctx, Args) ->
    {error, {?MODULE, invalid_args, Args}}.

-spec load_session(ewgi_context(), #store_config{}) -> no_session | #session{} | {'error', any()}.
load_session(_Ctx, #store_config{server_id=Server, session_id=Sid}) ->
    case Sid of
	undefined -> no_session;
	_ ->
	    case ?SESSION_SERVER_MODULE:get_session(Server, Sid) of
		undefined -> no_session;
		Session -> Session
	    end
    end.

new_session(#store_config{server_id=Server, session_id=Sid} = Cfg) ->
    NewId = ?SESSION_SERVER_MODULE:new_session(Server, Sid),
    Cfg#store_config{session_id=NewId}.

-spec store_session(ewgi_context(), #store_config{}, #session{}) -> ewgi_context().
store_session(Ctx, #store_config{server_id=Server,
				      session_id=Sid,
				      cookie_name=CookieName,
				      secure_cookie=Sec},
		   Session) ->
    ?SESSION_SERVER_MODULE:save_session(Server, Sid, Session),
    SidB64 = cookie_safe_encode(term_to_binary(Sid)),
    cookie_headers(Ctx, CookieName, SidB64, Sec).

delete_session(Ctx, #store_config{server_id=Server, session_id=Sid, cookie_name=CookieName}) ->
    case Sid of
	undefined -> ok;
	_ -> ?SESSION_SERVER_MODULE:delete_session(Server, Sid)
    end,
    cookie_headers(Ctx, CookieName, [], false).
