#!/usr/bin/env escript
-module(main).
-mode(compile).

read_lines() ->
    case io:get_line("") of
        eof -> [];
        L -> [L | read_lines()]
    end.

to_pair(Line) ->
    P = fun (S) -> {Result, _} = string:to_integer(S), Result end,
    [A, B, C, D | _] = lists:map(P, string:tokens(Line, ",-")),
    {{A, B}, {C, D}}.

contained({{A, B}, {C, D}}) when A =< C andalso B >= D -> true;
contained({{A, B}, {C, D}}) when A >= C andalso B =< D -> true;
contained(_) -> false.

overlaps({{A, B}, {C, D}}) when A =< D andalso B >= C -> true;
overlaps(_) -> false.

main(_) ->
    Lines = read_lines(),
    Pairs = lists:map(fun to_pair/1, Lines),
    P1 = length(lists:filter(fun contained/1, Pairs)),
    io:fwrite("Part 1: ~p~n", [P1]),
    P2 = length(lists:filter(fun (P) -> contained(P) orelse overlaps(P) end, Pairs)),
    io:fwrite("Part 2: ~p~n", [P2]).
