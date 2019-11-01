-module(optimistic).

-export([start/1, stop/1, reset/2, delete/2, operation/3, commit/1, abort/1]).
-export([init/1, handle_call/3]).
-behaviour(gen_server).

start(State) -> gen_server:start(optimistic,[State],[]).

stop(_) -> not_implemented.
reset(_,_) -> not_implemented.
delete(_, _) -> not_implemented.

operation(PID, Reads, OFun) -> {ok, gen_server:call(PID, {new_op, Reads, OFun}, infinity)}.


commit(_) -> not_implemented.
abort(_) -> not_implemented.


supervisor(Reads, Fun) -> not_implemented.

%callback functions
%state tuple is {shared state, map for OR, Counter for OR}
init(Arg) -> {ok, {Arg, maps:new()}}.

%value of OR map can be:
%{ongoing, PID, list of read keys, list of dirty keys)
%aborted
%{commited, Result}

handle_call({new_op, Reads, OFun}, _From, {State, OR_map}) ->
    View = maps:with(Reads, State),
    PID = spawn(fun() -> supervisor(View, OFun) end),
    New_OR_map = maps:put(PID, {ongoing, PID, Reads, []}, OR_map),
    {reply, PID, {State, New_OR_map}}.