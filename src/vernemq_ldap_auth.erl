-module(vernemq_ldap_auth).

-include_lib("eldap/include/eldap.hrl").
-include_lib("vernemq_dev/include/vernemq_dev.hrl").

%% Number of times to retry LDAP operations.
-define(LDAP_RETRIES, 10).




