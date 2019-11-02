-module(optimistic).

-export([start/1, stop/1, reset/2, delete/2, operation/3, commit/1, abort/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-behaviour(gen_server).

start(State) -> gen_server:start(optimistic,[State],[]).

stop(_) -> not_implemented.
reset(_,_) -> not_implemented.
delete(_, _) -> not_implemented.

operation(PID, Reads, OFun) -> {ok, gen_server:call(PID, {new_op, Reads, OFun}, infinity)}.


commit(OR) ->
    Ref = make_ref(),
    Me = self(),
    OR ! {commit, Ref, Me},
    receive
        {Ref, Result} -> Result
    end.

abort(_) -> not_implemented.


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
                                 committed -> Response({ok, Res}),
                                              loop_supervisor(WID, S, committed, {Res, Change}, [])
                             end
                    end;
                aborted -> loop_supervisor(WID, S, Status, Result, CommitList)
            end;
        % abortall is when the optimistic server aborts all ongoing operations in reset or stop
        abortall -> loop_supervisor(WID, S, aborted, Result, []);
        {abort, Ref, Client_ID} -> 
            case Status of
                committed -> Client_ID ! {Ref, too_late};
                aborted -> Client_ID ! {Ref, aborted},
                           loop_supervisor(WID, S, aborted, Result, CommitList);
                finished -> gen_server:call(S, abort, infinity),
                     Client_ID ! {Ref, aborted},
                     loop_supervisor(WID, S, aborted, Result, CommitList);
                ongoing -> stop(WID), %stops the worker as the result isnt needed
                     gen_server:call(S, abort, infinity),
                     Client_ID ! {Ref, aborted},
                     loop_supervisor(WID, S, aborted, Result, CommitList)
            end;
        {commit, Ref, Client_ID} ->
            case Status of
                ongoing -> loop_supervisor(WID, S, Status, Result, [{Client_ID, Ref}| CommitList]);
                finished -> C = gen_server:call(S, {commit, element(2, Result)}, infinity),
                            case C of
                                aborted -> Client_ID ! {Ref, aborted};
                                _ -> Client_ID ! {Ref, {ok, element(1, Result)}}
                            end;
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
init([Arg]) -> {ok, {Arg, maps:new()}}.

%key of OR map is the PID of the OR process
%value of OR map is: {list of read keys, list of dirty keys)
% key, value pair is removed from the map when aborted of committed


handle_call({new_op, Reads, OFun}, _From, {State, OR_map}) ->
    View = maps:with(Reads, State),
    Me = self(),
    PID = spawn(fun() -> supervisor(View, OFun, Me) end),
    New_OR_map = maps:put(PID, {Reads, []}, OR_map),
    {reply, PID, {State, New_OR_map}};

handle_call(abort, _From, {State, OR_map}) ->
    not_implemented;


handle_call({commit, Change}, {PID, _}, {State, OR_map}) ->
    Write = maps:keys(Change),
    {Reads, Dirty} = maps:get(PID, OR_map),
    Overlap = sets:intersection(sets:from_list(Write ++ Reads), sets:from_list(Dirty)),
    case sets:is_empty(Overlap) of
        true -> Reduced_OR_map = maps:remove(PID, OR_map),
                Fun = fun(_K, {Reads2, Dirty2}) -> {Reads2, Dirty2 ++ Write} end,
                New_OR_map = maps:map(Fun, Reduced_OR_map),
                New_State = maps:merge(Change, State),
                {reply, committed, {New_State, New_OR_map}};
        _ -> New_OR_map = maps:remove(PID, OR_map),
             {reply, aborted, {State, New_OR_map}}
    end.

handle_cast(_Request, State) ->
    {noreply, State}.