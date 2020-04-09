-module(vernemq_ldap_auth).

-include_lib("eldap/include/eldap.hrl").
-include_lib("vernemq_dev/include/vernemq_dev.hrl").

-behaviour(auth_on_register_hook).
-behaviour(auth_on_register_m5_hook).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Plugin Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    {ok, _} = application:ensure_all_started(vernemq_ldap_auth),
    vernemq_ldap_auth_cli:register(),
    ok.

stop() ->
    application:stop(vernemq_ldap_auth).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VerneMQ Hooks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


auth_on_register(_Peer, _SubscriberId, User, Password, _CleanSession) ->
    authenticate(User, Password).

auth_on_register_m5(_Peer, _SubscriberId, User, Password, _CleanStart, _Properties) ->
    authenticate(User, Password).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

authenticate(undefined, _) ->
    next;
authenticate(_, undefined) ->
    next;
authenticate(User, Password) ->
    ok.




