-module(test_optimistic).
%the full path was needed for me to run eqc on my machine
%-include_lib("C:/Program Files/erl10.4/lib/eqc-2.01.0/include/eqc.hrl").
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([mkopr/2, good_opr/0, prop_opr_accurate/1, prop_server_commit/0, prop_isolated/0,
         test_all/0, test_everything/0, my_tests/0]).

mkopr(Opr, Args) ->
    case Opr of
        incr -> fun(View) -> 
                    V = maps:get(Args, View),
                    {success, #{Args => V+1}}
                end;
        swap -> fun(View) ->
                    {A, B} = Args,
                    New_A = maps:get(B, View),
                    New_B = maps:get(A, View),
                    {success, #{A => New_A, B => New_B}}
                end
    end.

good_opr() ->
    ?LET(Keys, oneof([atoms(), atom3()]),
    case length(Keys) of
        1 -> [A] = Keys,
             {Keys, Keys, {call, test_optimistic, mkopr, [incr, A]}};
        2 -> [A, B] = Keys,
             {Keys, Keys, {call, test_optimistic, mkopr, [swap, {A, B}]}}
    end).

atoms() -> [atom(), atom2()].

atom() ->
    elements([a,b,c,d]).

atom2() ->
    elements([e,f,g,h]).

atom3() ->
    elements([[a], [b], [c], [d], [e], [f], [g], [h]]).

prop_opr_accurate(OprGen) -> ?FORALL(
    {Reads, Writes, Opr}, OprGen,
    begin
        View = maps:from_list(lists:map(fun(A) -> {A, 1} end, Reads)),
        Fun = eval(Opr),
        Output = try Fun(View) of
                    {_, Change} -> Change
                catch
                    _:_ -> aborted
                end,
        case Output of
            aborted -> equals(Output, aborted);
            _ -> equals(maps:keys(Output),
                Writes)
        end
    end).

prop_server_commit() -> ?FORALL(
    {Reads, _, Opr}, good_opr(),
    begin
        State = #{a => 1, b => 2, c => 3, d => 4,
                  e => 5, f => 6, g => 7, h => 8},
        {ok, S} = optimistic:start(State),
        Fun = eval(Opr),
        {ok, OR} = optimistic:operation(S, Reads, Fun),
        {ok, Result} = optimistic:commit(OR),
        {ok, New_State} = optimistic:stop(S),
        case Reads of
            [A] -> equals({Result, maps:get(A, State) + 1},
                        {success, maps:get(A, New_State)});
            [A, B] -> equals({Result, maps:get(A, State), maps:get(B, State)},
                             {success, maps:get(B, New_State), maps:get(A, New_State)})
        end
    end).

prop_isolated() -> ?FORALL(
    {{Reads1, Writes1, Opr1},{Reads2, Writes2, Opr2}}, {good_opr(), good_opr()},
    ?IMPLIES(sets:is_disjoint(sets:from_list(Writes1), sets:from_list(Writes2)),
    begin
        State = #{a => 1, b => 2, c => 3, d => 4,
                  e => 5, f => 6, g => 7, h => 8},
        {ok, S} = optimistic:start(State),
        Fun1 = eval(Opr1),
        Fun2 = eval(Opr2),
        {ok, OR1} = optimistic:operation(S, Reads1, Fun1),
        {ok, OR2} = optimistic:operation(S, Reads2, Fun2),
        optimistic:commit(OR1),
        optimistic:commit(OR2),
        {ok, New_State} = optimistic:stop(S),
        {ok, S2} = optimistic:start(State),
        Fun3 = eval(Opr1),
        Fun4 = eval(Opr2),
        {ok, OR3} = optimistic:operation(S2, Reads1, Fun3),
        optimistic:commit(OR3),
        {ok, OR4} = optimistic:operation(S2, Reads2, Fun4),
        optimistic:commit(OR4),
        {ok, New_State2} = optimistic:stop(S2),
        equals(New_State, New_State2)
    end)).

test_all() ->
    quickcheck(prop_opr_accurate(good_opr())),
    quickcheck(prop_server_commit()),
    quickcheck(prop_isolated()),
    my_tests().

test_everything() ->
    test_all().


% My tests

my_tests() ->
    eunit:test([
        conflicting_opr(),
        too_late_opr(),
        abort_commit_opr(),
        delete_commit_nonconflict_opr(),
        delete_commit_conflict_opr(),
        reset_commit_opr(),
        commit_twice_opr(),
        failed_opr()
    ], [verbose]).

conflicting_opr() ->
    {"run and commit two operations that conflicts",
    fun() ->
        State = #{a => 1, b => 2, c => 3, d => 4,
                  e => 5, f => 6, g => 7, h => 8},
        {ok, S} = optimistic:start(State),
        {ok, OR1} = optimistic:operation(S, [], fun(_) -> {success, #{a => 2}} end),
        {ok, OR2} = optimistic:operation(S, [], fun(_) -> {success, #{a => 2}} end),
        optimistic:commit(OR1),
        Result = optimistic:commit(OR2),
        ?assertMatch(aborted, Result)
    end}.

too_late_opr() ->
    {"calls abort after commit have completed",
    fun() ->
        State = #{a => 1, b => 2, c => 3, d => 4,
                  e => 5, f => 6, g => 7, h => 8},
        {ok, S} = optimistic:start(State),
        {ok, OR1} = optimistic:operation(S, [], fun(_) -> {success, #{a => 2}} end),
        optimistic:commit(OR1),
        Result = optimistic:abort(OR1),
        ?assertMatch(too_late, Result)
    end}.

abort_commit_opr() ->
    {"calls commit after abort have completed",
    fun() ->
        State = #{a => 1, b => 2, c => 3, d => 4,
                  e => 5, f => 6, g => 7, h => 8},
        {ok, S} = optimistic:start(State),
        {ok, OR1} = optimistic:operation(S, [], fun(_) -> {success, #{a => 2}} end),
        optimistic:abort(OR1),
        Result = optimistic:commit(OR1),
        ?assertMatch(aborted, Result)
    end}.

delete_commit_nonconflict_opr() ->
    {"deletes a key that operation writes to",
    fun() ->
        State = #{a => 1, b => 2, c => 3, d => 4,
                  e => 5, f => 6, g => 7, h => 8},
        {ok, S} = optimistic:start(State),
        {ok, OR1} = optimistic:operation(S, [], fun(_) -> {success, #{a => 2}} end),
        optimistic:delete(S, [b]),
        {ok, Result} = optimistic:commit(OR1),
        ?assertMatch(success, Result)
    end}.

delete_commit_conflict_opr() ->
    {"deletes a key that operation writes to",
    fun() ->
        State = #{a => 1, b => 2, c => 3, d => 4,
                  e => 5, f => 6, g => 7, h => 8},
        {ok, S} = optimistic:start(State),
        {ok, OR1} = optimistic:operation(S, [], fun(_) -> {success, #{a => 2}} end),
        optimistic:delete(S, [a]),
        Result = optimistic:commit(OR1),
        ?assertMatch(aborted, Result)
    end}.

reset_commit_opr() ->
    {"reset the shared state and then try to commit",
    fun() ->
        State = #{a => 1, b => 2, c => 3, d => 4,
                    e => 5, f => 6, g => 7, h => 8},
            {ok, S} = optimistic:start(State),
            {ok, OR1} = optimistic:operation(S, [], fun(_) -> {success, #{a => 2}} end),
            optimistic:reset(S, State),
            Result = optimistic:commit(OR1),
            ?assertMatch(aborted, Result)
    end}.

commit_twice_opr() ->
    {"checks that commit is idempotent",
    fun() ->
        State = #{a => 1, b => 2, c => 3, d => 4,
                    e => 5, f => 6, g => 7, h => 8},
            {ok, S} = optimistic:start(State),
            {ok, OR1} = optimistic:operation(S, [], fun(_) -> {success, #{a => 2}} end),
            optimistic:commit(OR1),
            {ok, Result} = optimistic:commit(OR1),
            ?assertMatch(success, Result)
    end}.

failed_opr() ->
    {"checks that an operation that fails makes it abort",
    fun() ->
        State = #{a => 1, b => 2, c => 3, d => 4,
                    e => 5, f => 6, g => 7, h => 8},
            {ok, S} = optimistic:start(State),
            {ok, OR1} = optimistic:operation(S, [], fun(_) ->
                throw(crash), {success, #{a => 2}} end),
            optimistic:commit(OR1),
            Result = optimistic:commit(OR1),
            ?assertMatch(aborted, Result)
    end}.