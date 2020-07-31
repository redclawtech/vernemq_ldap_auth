-module(vernemq_ldap_utils).

-include_lib("eldap/include/eldap.hrl").

open_connection([], _Port, _SSL, _Timeout, _Opts) ->
    {error, "connection failed"};
open_connection([Host | OtherHosts], Port, SSL, Timeout, Opts) ->
    SSLOpts = case SSL of
                true ->
                    [{ssl, true}, {sslopts, ssl_options(Host, Opts)}];
                false ->
                    []
              end,
    %% Note: timeout option sets timeout for any ldap operation.
    case eldap:open([Host], [{port, Port}, {timeout, Timeout} | SSLOpts]) of
      {ok, Handle} ->
          {ok, Handle, Host};
      {error, _} ->
          open_connection(OtherHosts, Port, SSL, Timeout, Opts)
    end.

eldap_search(Handle, SearchProps) ->
    case eldap:search(Handle, SearchProps) of
      {ok, #eldap_search_result{entries = Entries, referrals = Refs}} ->
          Refs == [] orelse
            lager:error("LDAP search continuations are not supported yet, ignoring: ~p", [Refs]),
          lager:debug("LDAP search res ~p: ~p", [SearchProps, Entries]),
          {ok, Entries};
      {ok, {referral, Refs}} ->
          lager:error("LDAP referrals are not supported, ignoring: ~p", [Refs]),
          {ok, []};
      {error, Reason} ->
          lager:error("LDAP search failed ~p: ~p", [SearchProps, Reason]),
          {error, Reason}
    end.
