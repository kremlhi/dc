-module(dc).

-export([eval/1, eval/2, evalop/2, tokens/1, intr/0, test/0]).

-define(is_regop(X), X == $s; X == $l; X == $L; X == $<; X == $=; X == $>).
-define(is_ws(X), X == $\t; X == $\n; X == $\r; X == $\s).
            
eval(S) ->
    eval(S, {[],dict:new()}).

eval(S, St) ->
    lists:foldl(fun evalop/2, St, tokens(S)).

evalop("+", {[Y, X | T],D}) -> {[X + Y | T],D};
evalop("-", {[Y, X | T],D}) -> {[X - Y | T],D};
evalop("*", {[Y, X | T],D}) -> {[X * Y | T],D};
evalop("/", {[Y, X | T],D}) -> {[X / Y | T],D};
evalop("%", {[Y, X | T],D}) -> {[X rem Y | T],D};
evalop("_", {[X | T],D}) -> {[-X | T],D};
evalop("^", {[Y, X | T],D}) -> {[math:pow(X, Y) | T],D};
evalop("v", {[X | T],D}) -> {[math:pow(X, 0.5) | T],D};
evalop("c", {_,D}) -> {[],D};
evalop("d", {[X | T],D}) -> {[X, X | T],D};
evalop("r", {[Y, X | T],D}) -> {[X, Y | T],D};
evalop("e", {St,D}) -> {[lists:sum(St)],D};
evalop("q", _) -> init:stop();
evalop("p", {[H | T],D}) ->
    print([H]),
    {[H | T],D};
evalop("f", {St,D}) ->
    print(St),
    {St,D};
evalop("z", {St,D}) -> {[length(St) | St],D};
evalop("x", {[H | T],D}) ->
    eval(H, {T,D});
evalop([$<, Reg], {[Y, X | T],D}) ->
    condi(Y < X, Reg, {T,D});
evalop([$=, Reg], {[Y, X | T],D}) ->
    condi(Y = X, Reg, {T,D});
evalop([$>, Reg], {[Y, X | T],D}) ->
    condi(Y > X, Reg, {T,D});
evalop([$s, Reg], {[H | T],D}) ->
    {ok,V} = case dict:find([Reg], D) of
                 error -> {ok,[undefined]};
                 X -> X end,
    {T,dict:store([Reg], [H | tl(V)], D)};
evalop([$S, Reg], {[H | T],D}) ->
    {T,dict:append([Reg], H, D)};
evalop([$l, Reg], {St,D}) ->
    {[hd(dict:fetch([Reg], D)) | St],D};
evalop([$L, Reg], {St,D}) ->
    L = dict:fetch([Reg], D),
    {[hd(L) | St],dict:store([Reg], tl(L), D)};
evalop(S = [$[ | _], {St,D}) ->
    {[data(S) | St],D};
evalop(N, {St,D}) when is_integer(N); is_float(N) ->
    {[N | St],D}.

print([]) ->
    ok;
print([H | T]) when H == ""; is_integer(hd(H)) ->
    io:format("~s~n", [H]),
    print(T);
print([H | T]) ->
    io:format("~w~n", [H]),
    print(T).

data([$[ | T]) ->
    {S,"]"} = lists:split(length(T) - 1, T),
    S.

condi(true, Reg, Xs) ->
    eval([$l, Reg | "x"], Xs);
condi(false, _, Xs) ->
    Xs.

tokens([]) -> [];
tokens(L = [H | _]) when H >= $0, H =< $9 ->
    {N,T} = case string:to_float(L) of
                {error,no_float} ->
                    string:to_integer(L);
                X -> X end,
    [N | tokens(T)];
tokens([H | T]) when ?is_ws(H) ->
    tokens(T);
tokens(L = [$[ | _]) ->
    {S,T} = brackets(L, 0, []),
    [S | tokens(T)];
tokens([H, Reg | T]) when ?is_regop(H) ->
    [[H, Reg] | tokens(T)];
tokens([H | T]) ->   
    [[H] | tokens(T)].

brackets([$[ | T], N, Acc) ->
    brackets(T, N + 1, [$[ | Acc]);
brackets([$] | T], 1, Acc) ->
    {lists:reverse([$] | Acc]), T};
brackets([$] | T], N, Acc) ->
    brackets(T, N - 1, [$] | Acc]);
brackets([X | T], N, Acc) ->
    brackets(T, N, [X | Acc]).

intr() ->
    intr({[],dict:new()}).

intr(St) ->
    case io:get_line("") of
        eof ->
            init:stop();
        S -> St1 = lists:foldl(fun pokemon_evalop/2, St, tokens(S)),
             intr(St1) end.

pokemon_evalop(X, Xs) ->
    try evalop(X, Xs)
    catch error:Reason ->
            io:format(standard_error, "error: ~p: ~p~n", [X, Reason]),
            Xs end.

test() ->
    {St1,_} = eval("10[1-dd0<a]salax"),
    St1 = [0 | lists:seq(0, 9)],
    
    {St2,_} = eval("[sasb lbla lbla+]sf 0 1[lfx z10>l]slllx"),
    St2 = [34, 21, 13, 8, 5, 3, 2, 1, 1, 0],
    
    ok. 
