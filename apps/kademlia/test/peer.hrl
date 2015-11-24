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
    ?setup(fun should_return_its_id/1)].

should_store_data({PeerPid, _}) ->
    Key = mykey,
    Value = "myvalue",

    peer:store({Key, Value}, PeerPid),
    RetrievalData = peer:find_value_of(Key, PeerPid),
    [?_assertEqual(Value, RetrievalData)].

should_overwrite_data_with_same_key({PeerPid, _}) ->
    Key = mykey,
    FirstValue = "myvalue",
    SecondValue = "updated",

    peer:store({Key, FirstValue} , PeerPid),
    peer:store({Key, SecondValue}, PeerPid),
    RetrievalData = peer:find_value_of(Key, PeerPid),
    [?_assertEqual(SecondValue, RetrievalData)].

should_answer_with_pong_to_a_ping({PeerPid, KbucketPid}) ->
    FakePeer = self(),
    meck:expect(kbucket, put, fun(_, _) -> ok end),
    peer:ping(PeerPid, FakePeer),
    receive
        {pong, PeerPid} ->
            [?_assert(meck:called(kbucket, put, [FakePeer, KbucketPid]))];
        _ ->
            [?fail]
    end.

should_update_kbucket_if_receive_a_pong({PeerPid, KbucketPid}) ->
    FakePeer = self(),
    meck:expect(kbucket, put, fun(_, _) -> ok end),
    peer:pong(PeerPid, FakePeer),
    [?_assert(meck:called(kbucket, put, [FakePeer, KbucketPid]))].


should_return_its_id({PeerPid, _}) ->
    Id = peer:id_of(PeerPid),
    [?_assertEqual(1, Id)].
