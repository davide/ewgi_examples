%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @author Davide Marquês <nesrait@gmail.com>
%% @copyright 2009 Smarkets Limited.
%%
%% @doc Cookie-based session store.
%% Based on smak_auth_cookie by Hunter Morris.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(ewgi_session_cookie_store).
-author('Hunter Morris <hunter.morris@smarkets.com>').
-author('Davide Marquês <nesrait@gmail.com>').

%% Session Store API
-export([init/2, load_session/2, new_session/1, delete_session/2, store_session/3]).

-import(smak_cookie, [cookie_headers/4, cookie_safe_encode/1, cookie_safe_decode/1]).

-include("smak.hrl").
-include("session.hrl").

-define(DEFAULT_COOKIE_NAME, "session_id").
-define(DEFAULT_ENCODER, ewgi_crypto).

-record(store_config, {
	  key = undefined,
          cookie_name = ?DEFAULT_COOKIE_NAME,
          encoder = ?DEFAULT_ENCODER,
          maxlength = 4096,
          secure = false
	 }).

%%====================================================================
%% Session Store API
%%====================================================================
-spec init(ewgi_context(), any()) -> {#store_config{},ok} | {#store_config{},no_session} | {#store_config{},{error,any()}}.
init(_Ctx, [CookieName, Key, Options]) when is_binary(Key), is_list(CookieName), is_list(Options) ->
    Cfg = #store_config{key=Key, cookie_name=CookieName},
    case populate_record(Options, Cfg) of
	{error, _} = Error ->
	    {Cfg, Error};
	Cfg1 ->
	    {Cfg1, ok}
    end;
init(_Ctx, Args) ->
    {error, {?MODULE, invalid_args, Args}}.
    
-spec load_session(ewgi_context(), #store_config{}) -> no_session | #session{} | {'error', any()}.
load_session(Ctx, Cfg) ->
    case ewgi_api:get_header_value("cookie", Ctx) of
	undefined ->
	    no_session;
	Cookies ->
	    CookieValues = smak_cookie:parse_cookie(Cookies),
	    case proplists:get_value(Cfg#store_config.cookie_name, CookieValues) of
		undefined ->
		    no_session;
		SessionCookie ->
		    case decode_cookie_contents(SessionCookie, Cfg) of
			{error,_} = Error ->
			    Error;
			Session ->
			    Session
		    end
	    end
	end.

new_session(Cfg) ->
    Cfg.

delete_session(Ctx, #store_config{cookie_name=CookieName}) ->
    cookie_headers(Ctx, CookieName, [], false).

-spec store_session(ewgi_context(), #store_config{}, #session{}) -> ewgi_context().
store_session(Ctx, #store_config{cookie_name=CookieName,
				encoder=Encoder,
				key=Key,
				maxlength=M,
				secure=Sec},
		   Session) ->
    case Encoder:encode(Key, term_to_binary(Session), M) of
	{error, _} = Error ->
	    Error;
	EncryptedVal ->
	    CookieVal = cookie_safe_encode(EncryptedVal),
	    cookie_headers(Ctx, CookieName, CookieVal, Sec)
    end.


%%====================================================================
%% Internal functions
%%====================================================================
-spec decode_cookie_contents(string(), #store_config{}) -> #session{} | {'error', any()}.
decode_cookie_contents(Cookie0, Cfg) ->
    case cookie_safe_decode(Cookie0) of
        <<>> ->
	    {error, {decode_cookie_contents, no_data}};
        Cookie when is_binary(Cookie) ->
	    case (Cfg#store_config.encoder):decode(Cfg#store_config.key, Cookie) of
		{error, _} = Error ->
		    Error;
		Content when is_binary(Content) ->
		    case (catch(binary_to_term(Content))) of
			Session when is_record(Session, session) ->
			    Session;
			Error ->
			    {error, {decode_cookie_contents, Error}}
		    end
	    end
    end.

-spec populate_record(proplist(), #store_config{}) -> #store_config{} | {'error', {'invalid_option', any()}}.
populate_record(Options, Cfg) ->
    lists:foldl(fun r_option/2, Cfg, Options).

-spec r_option({atom(), any()}, #store_config{}) -> #store_config{} | {'error', {'invalid_option', any()}}.
r_option(_, PrevError) when not is_record(PrevError, store_config) ->
	PrevError;
r_option({encoder, E}, Cfg) ->
    Cfg#store_config{encoder=E};
r_option({maxlength, M}, Cfg) when is_integer(M), M > 0 ->
    Cfg#store_config{maxlength=M};
r_option({secure, Secure}, Cfg) when is_boolean(Secure) ->
    Cfg#store_config{secure=Secure};
r_option(O, _) ->
    {error, {invalid_option, O}}.
