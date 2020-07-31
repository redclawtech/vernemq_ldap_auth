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

eldap_error({ldap_search_failed, Reason}) ->
    io_lib:format("LDAP search returned error: ~s", [eldap_error(Reason)]);
eldap_error({connect_failed, _}) ->
    "Can't connect to the LDAP server";
eldap_error({start_tls_failed, _}) ->
    "Failed to use StartTLS extension";
eldap_error({ldap_url_parse_error, URL, Error}) ->
    io_lib:format("Failed to parse LDAP url ~p (~s)", [URL, eldap_error(Error)]);
eldap_error({dn_search_failed, Reason}) ->
    io_lib:format("LDAP search for user distinguished name failed with reason: ~s",
                  [eldap_error(Reason)]);
eldap_error(dn_not_found) ->
    "LDAP distinguished name not found";
eldap_error(not_unique_username) ->
    "Search returned more than one entry for given username";
eldap_error({invalid_filter, Filter, Reason}) ->
    io_lib:format("Invalid LDAP filter ~p (~s)", [Filter, Reason]);
eldap_error({username_to_dn_map_failed, R}) ->
    io_lib:format("Failed to map username to LDAP distinguished name: ~s", [eldap_error(R)]);
eldap_error({invalid_scheme, S}) ->
    io_lib:format("Invalid scheme ~p", [S]);
eldap_error(malformed_url) ->
    "Malformed LDAP URL";
eldap_error({invalid_dn, DN}) ->
    io_lib:format("Invalid LDAP distinguished name \"~s\"", [DN]);
eldap_error({invalid_scope, Scope}) ->
    io_lib:format("Invalid LDAP scope: ~p, possible values are one, base or sub", [Scope]);
eldap_error(user_placeholder) ->
    "%u placeholder is not allowed in nested groups search";
eldap_error(max_depth) ->
    "Nested search max depth has been reached";
eldap_error({invalid_groups_query, Query, Reason}) ->
    io_lib:format("Invalid LDAP query: \"~s\". ~s", [Query, eldap_error(Reason)]);
eldap_error({bind_failed, DN, Bind}) ->
    io_lib:format("Bind failed for \"~s\". ~s", [DN, eldap_error(Bind)]);
eldap_error(invalidCredentials) ->
    "Invalid username or password";
eldap_error(anonymous_auth) ->
    "Anonymous bind is not supported by LDAP server";
eldap_error(unwillingToPerform) ->
    "LDAP server cannot process the request because of server-defined "
    "restrictions";
eldap_error(invalidDNSyntax) ->
    "Invalid LDAP distinguished name syntax";
eldap_error(Error) ->
    io_lib:format("~p", [Error]).
