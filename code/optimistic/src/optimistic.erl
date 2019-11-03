-module(optimistic).

-export([start/1, stop/1, reset/2, delete/2, operation/3, commit/1, abort/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-behaviour(gen_server).

start(State) -> gen_server:start(optimistic,[State],[]).

stop(S) -> gen_server:call(S, stop, infinity).

reset(S, State) -> gen_server:call(S, {reset, State}, infinity).

delete(S, Keys) -> gen_server:call(S, {delete, Keys}, infinity).

operation(S, Reads, OFun) -> {ok, gen_server:call(S, {new_op, Reads, OFun}, infinity)}.

commit(OR) -> request_reply(OR, commit).

abort(OR) -> request_reply(OR, abort).

request_reply(PID, Request) ->
    Ref = make_ref(),
    Me = self(),
    PID ! {Request, Ref, Me},
    receive
        {Ref, Result} -> Result
    end.

supervisor(View, OFun, S) ->
    process_flag(trap_exit, true),
    SuperID = self(),
    Worker_ID = spawn_link(fun() -> SuperID ! {result, self(), OFun(View)} end),
    loop_supervisor(Worker_ID, S, ongoing, nothing, []).

%status codes
%ongoing: OR is not complete
%finished: OR has completed and waiting on abort/commit
%aborted: abort has been called before commit
%committed: commit completed

%CommitList is a list of users that have run commit on the OR but havent recieved a response yet
loop_supervisor(WID, S, Status, Result, CommitList) ->
    receive
        {'EXIT', WID, {{nocatch, _}, _}} ->
            gen_server:call(S, abort, infinity),
            respond(aborted, CommitList),
            loop_supervisor(WID, S, aborted, Result, []);
        {result, WID, {Res, Change}} ->
            case Status of
                ongoing ->
                    case CommitList of
                        [] -> loop_supervisor(WID, S, finished, {Res, Change}, CommitList);
                        _ -> Response = gen_server:call(S, {commit, Change}, infinity),
                             case Response of
                                 aborted -> respond(aborted, CommitList),
                                            loop_supervisor(WID, S, aborted, {Res, Change}, []);
                                 committed -> respond({ok, Res}, CommitList),
                                              loop_supervisor(WID, S, committed, {Res, Change}, [])
                             end
                    end;
                aborted -> loop_supervisor(WID, S, Status, Result, CommitList)
            end;
        % abortall is when the optimistic server aborts all ongoing operations in reset or stop
        abortall ->
            case Status of
                ongoing -> exit(WID, kill),
                           respond(aborted, CommitList),
                           loop_supervisor(WID, S, aborted, Result, []);
                finished -> loop_supervisor(WID, S, aborted, Result, []);
                _ -> loop_supervisor(WID, S, Status, Result, [])
            end;
        stop ->
            case Status of
                ongoing -> exit(WID, kill);
                _ -> nothing
            end;
        {abort, Ref, Client_ID} ->
            case Status of
                committed -> Client_ID ! {Ref, too_late},
                             loop_supervisor(WID, S, Status, Result, []);
                aborted -> Client_ID ! {Ref, aborted},
                           loop_supervisor(WID, S, aborted, Result, CommitList);
                finished -> gen_server:call(S, abort, infinity),
                     Client_ID ! {Ref, aborted},
                     loop_supervisor(WID, S, aborted, Result, CommitList);
                ongoing -> exit(WID, kill), %stops the worker as the result isnt needed
                     respond(aborted, CommitList),
                     gen_server:call(S, abort, infinity),
                     Client_ID ! {Ref, aborted},
                     loop_supervisor(WID, S, aborted, Result, [])
            end;
        {commit, Ref, Client_ID} ->
            case Status of
                ongoing -> loop_supervisor(WID, S, Status, Result, [{Client_ID, Ref}| CommitList]);
                finished -> C = gen_server:call(S, {commit, element(2, Result)}, infinity),
                            case C of
                                aborted -> Client_ID ! {Ref, aborted};
                                _ -> Client_ID ! {Ref, {ok, element(1, Result)}}
                            end,
                            loop_supervisor(WID, S, C, Result, []);
                aborted -> Client_ID ! {Ref, aborted},
                           loop_supervisor(WID, S, Status, Result, CommitList);
                committed -> Client_ID ! {Ref, {ok, element(1, Result)}},
                             loop_supervisor(WID, S, Status, Result, CommitList)
            end
    end.


respond(_Message, []) -> nothing;
respond(Message, [{From, Ref}| Tail]) -> From ! {Ref, Message},
                                         respond(Message, Tail).

%callback functions
%state tuple is {shared state, map for OR}
init([Arg]) -> {ok, {Arg, maps:new(), []}}.

%key of OR map is the PID of the OR process
%value of OR map is: {list of read keys, list of dirty keys)
% key, value pair is removed from the map when aborted of committed


handle_call({new_op, Reads, OFun}, _From, {State, OR_map, Completed}) ->
    View = maps:with(Reads, State),
    Me = self(),
    PID = spawn(fun() -> supervisor(View, OFun, Me) end),
    New_OR_map = maps:put(PID, {Reads, []}, OR_map),
    {reply, PID, {State, New_OR_map, Completed}};

handle_call(abort, {PID, _}, {State, OR_map, Completed}) ->
    New_OR_map = maps:remove(PID, OR_map),
    {reply, aborted, {State, New_OR_map, [PID | Completed]}};


handle_call({commit, Change}, {PID, _}, {State, OR_map, Completed}) ->
    Write = maps:keys(Change),
    {Reads, Dirty} = maps:get(PID, OR_map),
    Overlap = sets:intersection(sets:from_list(Write ++ Reads), sets:from_list(Dirty)),
    case sets:is_empty(Overlap) of
        true -> Reduced_OR_map = maps:remove(PID, OR_map),
                Fun = fun(_K, {Reads2, Dirty2}) -> {Reads2, Dirty2 ++ Write} end,
                New_OR_map = maps:map(Fun, Reduced_OR_map),
                New_State = maps:merge(State, Change),
                {reply, committed, {New_State, New_OR_map, [PID | Completed]}};
        _ -> New_OR_map = maps:remove(PID, OR_map),
             {reply, aborted, {State, New_OR_map, [PID | Completed]}}
    end;

handle_call({delete, Keys}, _From, {State, OR_map, Completed}) ->
    New_State = maps:without(Keys, State),
    Fun = fun(_K, {Reads, Dirty}) -> {Reads, Dirty ++ Keys} end,
    New_OR_map = maps:map(Fun, OR_map),
    {reply, ok, {New_State, New_OR_map, Completed}};

handle_call({reset, New_state}, _From, {_State, OR_map, Completed}) ->
    Keys = maps:keys(OR_map),
    Fun = fun(Key) -> Key ! abortall end,
    lists:for_each(Fun, Keys),
    {reply, ok, {New_state, maps:new(), Keys ++ Completed}};

handle_call(stop, _From, {State, OR_map, Completed}) ->
    Keys = maps:keys(OR_map),
    Fun = fun(Key) -> Key ! stop end,
    lists:foreach(Fun, Keys ++ Completed),
    {stop, normal, {ok, State}, {State, OR_map, Completed}}.

handle_cast(_Request, State) ->
    {noreply, State}.