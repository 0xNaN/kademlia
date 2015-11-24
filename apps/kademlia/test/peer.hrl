-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun teardown/1, F}).
-define(pass, ?_assert(true)).
-define(fail, ?_assert(false)).

start() ->
    Id = 1,
    meck:new(kbucket, [non_strict]),
    KbucketPid = self(),
    PeerPid = peer:start(Id, KbucketPid),
    {PeerPid, KbucketPid}.

teardown({_, _}) ->
    meck:validate(kbucket),
    meck:unload(kbucket).

peer_suite_test_() ->
    [?setup(fun should_store_data/1),
    ?setup(fun should_overwrite_data_with_same_key/1),
    ?setup(fun should_answer_with_pong_to_a_ping/1),
    ?setup(fun should_update_kbucket_if_receive_a_pong/1),
    ?setup(fun should_contact_the_kbucket_for_its_closest_peer_to_a_key/1),
    ?setup(fun should_return_its_id/1)].

should_store_data({PeerPid, _}) ->
    FakePeer = self(),
    Key = mykey,
    Value = "myvalue",

    meck:expect(kbucket, put, fun(_) -> ok end),
    peer:store({Key, Value}, PeerPid),
    RetrievalData = peer:find_value_of(Key, PeerPid),
    [?_assertEqual(Value, RetrievalData),
     ?_assert(meck:called(kbucket, put, [FakePeer]))].

should_overwrite_data_with_same_key({PeerPid, _}) ->
    FakePeer = self(),
    Key = mykey,
    FirstValue = "myvalue",
    SecondValue = "updated",
    meck:expect(kbucket, put, fun(_) -> ok end),

    peer:store({Key, FirstValue} , PeerPid),
    peer:store({Key, SecondValue}, PeerPid),
    RetrievalData = peer:find_value_of(Key, PeerPid),

    [?_assertEqual(SecondValue, RetrievalData),
     ?_assert(meck:called(kbucket, put, [FakePeer]))].

should_answer_with_pong_to_a_ping({PeerPid, _}) ->
    FakePeer = self(),
    meck:expect(kbucket, put, fun(_) -> ok end),
    peer:ping(PeerPid),
    receive
        {pong, PeerPid} ->
            [?_assert(meck:called(kbucket, put, [FakePeer]))];
        _ ->
            [?fail]
    end.

should_update_kbucket_if_receive_a_pong({PeerPid, _}) ->
    FakePeer = self(),
    meck:expect(kbucket, put, fun(_) -> ok end),
    peer:pong(PeerPid),
    % since we check that the peer called kbucket:put but doesn't
    % wait an answer
    timer:sleep(10),
    [?_assert(meck:called(kbucket, put, [FakePeer]))].

should_contact_the_kbucket_for_its_closest_peer_to_a_key({PeerPid, KbucketPid}) ->
    FakePeer = self(),
    Key = 2,
    meck:expect(kbucket, closest_peers, fun(_, _) -> [1, 2] end),
    meck:expect(kbucket, put, fun(_) -> ok end),

    peer:find_closest_peers(Key, PeerPid),
    receive
        {PeerPid, Peers} ->
            [?_assertEqual([1, 2], Peers),
             ?_assert(meck:called(kbucket, put, [FakePeer])),
             ?_assert(meck:called(kbucket, closest_peers, [KbucketPid, Key]))]
    end.

should_return_its_id({PeerPid, _}) ->
    Id = peer:id_of(PeerPid),
    [?_assertEqual(1, Id)].
