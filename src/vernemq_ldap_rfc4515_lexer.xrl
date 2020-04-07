Definitions.

STRING = [A-Za-z0-9\-\.\s\\%]+

Rules.

\:[dD][nN]    : {token, {list_to_atom(string:to_lower(Chars)), Line}}.
\~=           : {token, {list_to_atom(Chars), Line}}.
>=            : {token, {list_to_atom(Chars), Line}}.
<=            : {token, {list_to_atom(Chars), Line}}.
\:=           : {token, {list_to_atom(Chars), Line}}.
=\*           : {token, {list_to_atom(Chars), Line}}.
[()&\|!=\:\*] : {token, {list_to_atom(Chars), Line}}.
{STRING}      : {token, {str, Line, Chars}}.

Erlang code.
