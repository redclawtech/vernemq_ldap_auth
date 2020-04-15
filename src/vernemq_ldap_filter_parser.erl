-module(vernemq_ldap_filter_parser).

-export([parse/1]).

-spec parse(string()) -> {ok, any()} | {error, any()}.
parse(FilterString) ->
    case tokens(FilterString) of
        {ok, Tokens} ->
            try vernemq_ldap_rfc4515_parser:parse(Tokens) of
                {ok, Filter} ->
                    {ok, Filter};
                {error, {_Line, Module, Error}} ->
                    {error, Module:format_error(Error)}
            catch
                _:_ -> {error, "ldap filter syntax error"}
            end;
        {error, Error} ->
            {error, Error}
    end.


tokens(String) -> tokens(String, []).
tokens("", Res) -> {ok, lists:reverse(Res)};
tokens(String, Res) ->
    case vernemq_ldap_rfc4515_lexer:token([], String) of
        {done, {ok, Token, Line}, Rest} ->
            case is_equals(Token) of
                true ->
                    {Tokens2, Rest2} = parse_value(Rest, Line),
                    tokens(Rest2, Tokens2 ++ [Token | Res]);
                false ->
                    tokens(Rest, [Token | Res])
            end;
        {done, {eof, _}, _Rest} ->
            {error, "unexpected end of file"};
        {done, {error, {_Line, Module, Error}, _}, _} ->
            {error, Module:format_error(Error)};
        {more, _} ->
            {error, "incomplete"}
    end.

is_equals({'~=', _}) -> true;
is_equals({'>=', _}) -> true;
is_equals({'<=', _}) -> true;
is_equals({'=', _}) -> true;
is_equals({':=', _}) -> true;
is_equals({'=*', _}) -> true;
is_equals(_) -> false.

parse_value(Str, Line) ->
    {Str2, Rest} =
        case string:split(Str, ")") of
            [V, R] -> {V, [$) | R]};
            [V] -> {V, ""}
        end,
    StrTokens = [{str, Line, T} || T <- string:split(Str2, "*", all)],
    Tokens = [T || T <- lists:join({'*', Line}, StrTokens),
                   T =/= {str, Line, ""}],
    {lists:reverse(Tokens), Rest}.
