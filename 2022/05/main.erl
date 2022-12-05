#!/usr/bin/env escript
-module(main).
-mode(compile).

read_lines() ->
    case io:get_line("") of
        eof -> [];
        L -> [string:chomp(L) | read_lines()]
    end.

parse_commands(Lines) ->
    ParseCmd = fun (Line) ->
        Parts = lists:map(fun string:to_integer/1, string:split(Line, " ", all)),
        [_, {Nth, _}, _, {From, _}, _, {To, _}] = Parts,
        {Nth, From, To}
    end,
    lists:map(ParseCmd, Lines).

parse_crates(Lines) ->
    [Line1 | _] = Lines,
    StackCount = (string:len(Line1) + 1) div 4,
    LetterFn = fun (I, M, Line) ->
        Stack = maps:get(I, M, []),
        LetterIndex = (I - 1) * 4 + 1,
        case string:slice(Line, LetterIndex, 1) of
            " " -> M;
            Let -> M#{I => Stack ++ [Let]}
        end
    end,
    LineFn = fun (Line, M) ->
        lists:foldl(
            fun (Index, Acc) -> LetterFn(Index, Acc, Line) end,
            M,
            lists:seq(1, StackCount)
        )
    end,
    lists:foldl(LineFn, #{}, Lines).

parse_input(Lines) ->
    {CrateLines1, CmdLines1} = lists:splitwith(
        fun ([]) -> false; (_) -> true end, 
        Lines
    ),
    CrateLines = lists:droplast(CrateLines1),
    [_ | CmdLines] = CmdLines1,
    {parse_crates(CrateLines), parse_commands(CmdLines)}.

run_command({N, From, To}, Stacks, KeepOrder) ->
    FromStack = maps:get(From, Stacks),
    ToStack = maps:get(To, Stacks),
    {ToBeMoved, Rest} = lists:split(N, FromStack),
    ToBeMoved2 = case KeepOrder of
        true -> ToBeMoved;
        false -> lists:reverse(ToBeMoved)
    end,
    Stacks#{From => Rest, To => ToBeMoved2 ++ ToStack}.

solve_part1(Crates, Commands) ->
    FinalCrates = lists:foldl(
        fun (E, Acc) -> run_command(E, Acc, false) end,
        Crates,
        Commands
    ),
    Answer = maps:values(maps:map(fun (_, [L | _]) -> L end, FinalCrates)),
    io:fwrite("Part 1: ~s~n", [Answer]).

solve_part2(Crates, Commands) ->
    FinalCrates = lists:foldl(
        fun (E, Acc) -> run_command(E, Acc, true) end,
        Crates,
        Commands
    ),
    Answer = maps:values(maps:map(fun (_, [L | _]) -> L end, FinalCrates)),
    io:fwrite("Part 2: ~s~n", [Answer]).

main(_) ->
    Lines = read_lines(),
    {Crates, Commands} = parse_input(Lines),
    solve_part1(Crates, Commands),
    solve_part2(Crates, Commands).
