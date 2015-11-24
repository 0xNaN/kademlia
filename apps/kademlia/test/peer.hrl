-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, F}).
-define(pass, ?_assert(true)).
-define(fail, ?_assert(false)).

start() ->
    Id = 1,
    PeerPid = peer:start(Id),
    PeerPid.

peer_suite_test_() ->
    [?setup(fun should_store_data/1),
    ?setup(fun should_overwrite_data_with_same_key/1),
    ?setup(fun should_answer_with_pong_to_a_ping/1),
    ?setup(fun should_return_its_id/1)].

should_store_data(PeerPid) ->
    Key = mykey,
    Value = "myvalue",

    peer:store({Key, Value}, PeerPid),
    RetrievalData = peer:find_value_of(Key, PeerPid),
    [?_assertEqual(Value, RetrievalData)].

should_overwrite_data_with_same_key(PeerPid) ->
    Key = mykey,
    FirstValue = "myvalue",
    SecondValue = "updated",

    peer:store({Key, FirstValue} , PeerPid),
    peer:store({Key, SecondValue}, PeerPid),
    RetrievalData = peer:find_value_of(Key, PeerPid),
    [?_assertEqual(SecondValue, RetrievalData)].

should_answer_with_pong_to_a_ping(PeerPid) ->
    peer:ping(PeerPid, self()),
    receive
        {pong, PeerPid} ->
            [?pass];
        _ ->
            [?fail]
    end.

should_return_its_id(PeerPid) ->
    Id = peer:id_of(PeerPid),
    [?_assertEqual(1, Id)].
