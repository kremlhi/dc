-module(dc).

-export([eval/1, eval/2, evalop/2, tokens/1, intr/0, test/0]).

-define(is_regop(X),
        X == $s; X == $S; X == $l; X == $L;
        X == $<; X == $=; X == $>).
-define(is_ws(X), X == $\s; X == $\t; X == $\r; X == $\n).

-type element() :: string() | number().
-type stack() :: [element()].
-type state() :: {stack(),dict()}.
-type evalop_return() :: state().
-type eval_return() :: evalop_return().

-spec eval(string()) -> eval_return().

eval(S) ->
    eval(S, {[],dict:new()}).

-spec eval(string(), state()) -> eval_return().

eval(S, State) ->
    lists:foldl(fun evalop/2, State, tokens(S)).


-spec evalop(Cmd | Value, state()) -> evalop_return() when
      Cmd :: nonempty_string(),
      Value :: element().

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
evalop("q", _) -> erlang:halt();
evalop("Q", {[X | _],_}) -> erlang:halt(X);
evalop("p", {[X | T],D}) ->
    print(X),
    {[X | T],D};
evalop("f", {St,D}) ->
    lists:foreach(fun print/1, St),
    {St,D};
evalop("z", {St,D}) -> {[length(St) | St],D};
evalop("x", {[X | T],D}) ->
    eval(X, {T,D});
evalop([$< | Reg], {[Y, X | T],D}) ->
    condi(Y < X, Reg, {T,D});
evalop([$= | Reg], {[Y, X | T],D}) ->
    condi(Y == X, Reg, {T,D});
evalop([$> | Reg], {[Y, X | T],D}) ->
    condi(Y > X, Reg, {T,D});
evalop([$s | Reg], {[X | T],D}) ->
    St1 = case dict:find(Reg, D) of
              {ok,[_ | T1]} -> T1;
              _ -> [] end,
    {T,dict:store(Reg, [X | St1], D)};
evalop([$S | Reg], {[X | T],D}) ->
    {T,dict:append(Reg, X, D)};
evalop([$l | Reg], {St,D}) ->
    {[hd(dict:fetch(Reg, D)) | St],D};
evalop([$L | Reg], {St,D}) ->
    [H | T] = dict:fetch(Reg, D),
    {[H | St],dict:store(Reg, T, D)};
evalop(S = [$[ | _], {St,D}) ->
    {[remove_brackets(S) | St],D};
evalop(N, {St,D}) when is_number(N) ->
    {[N | St],D}.


-spec print(string() | number()) -> ok.

print(S) when S == ""; is_integer(hd(S)) ->
    io:format("~s~n", [S]);
print(N) when is_number(N) ->
    io:format("~w~n", [N]).


-spec remove_brackets(nonempty_string()) -> string().

remove_brackets([$[ | T]) ->
    {S,"]"} = lists:split(length(T) - 1, T),
    S.


-spec condi(boolean(), nonempty_string(), state()) -> state().

condi(true, Reg, State) ->
    eval("l" ++ Reg ++ "x", State);
condi(false, _, State) ->
    State.


-spec tokens(string()) -> [element()].

tokens("") -> "";
tokens(L = [H | _]) when H >= $0, H =< $9 ->
    {N,T} = case string:to_float(L) of
                {error,no_float} ->
                    string:to_integer(L);
                X -> X end,
    [N | tokens(T)];
tokens([H | T]) when ?is_ws(H) ->
    tokens(T);
tokens(L = [$[ | _]) ->
    {S,T} = split_brackets(L, 0, []),
    [S | tokens(T)];
tokens([H, Reg | T]) when ?is_regop(H) ->
    [[H, Reg] | tokens(T)];
tokens([H | T]) ->   
    [[H] | tokens(T)].


-spec split_brackets(string(), non_neg_integer(), string()) -> Tuple when
      Tuple :: {string(),string()}.

split_brackets(S, N, Acc) when N < 0 ->
    erlang:error(badarg, [S, N, Acc]);
split_brackets([$[ | T], N, Acc) ->
    split_brackets(T, N + 1, [$[ | Acc]);
split_brackets([$] | T], 1, Acc) ->
    {lists:reverse([$] | Acc]),T};
split_brackets([$] | T], N, Acc) ->
    split_brackets(T, N - 1, [$] | Acc]);
split_brackets([X | T], N, Acc) ->
    split_brackets(T, N, [X | Acc]).


-spec intr() -> no_return().

intr() ->
    intr({[],dict:new()}).

-spec intr(state()) -> no_return().

intr(St) ->
    case io:get_line("") of
        eof -> erlang:halt();
        S -> St1 = lists:foldl(fun pokemon_evalop/2, St, tokens(S)),
             intr(St1) end.


-spec pokemon_evalop(string(), state()) -> evalop_return().

pokemon_evalop(S, State) ->
    try evalop(S, State)
    catch error:Reason ->
            io:format(standard_error, "error: ~p: ~p~n", [S, Reason]),
            State end.


-spec test() -> ok.

test() ->
    {St1,_} = eval("10[1-dd0<a]salax sb"),
    St1 = lists:seq(0, 9),

    {St2,_} = eval("[sbsa lalb lalb+]sf 0 1[lfx z10>l]slllx"),
    St2 = [34, 21, 13, 8, 5, 3, 2, 1, 1, 0],

    {St3,_} = eval("[la1+dsa*dla10>y]sy 0sa1 lyx sa"),
    St3 = [3628800, 362880, 40320, 5040, 720, 120, 24, 6, 2, 1],
    
    {St4,_} = eval("1saLasa"),
    St4 = [],
    
    {St5,_} = eval("zdslsn[Saz0<x]sxlxx"
                   " [LaSb lld1-sl 1<y]sylyx"
                   " [Lb lnd1-sn 1<z]szlzx", {St1,dict:new()}),
    St5 = lists:seq(9, 0, -1),
    
    ok.
