-include_lib("eunit/include/eunit.hrl").
-include_lib("test_macro.hrl").

start() ->
    meck:new(kbucket, [non_strict]),
    KbucketPid = self(),
    Id = 1,
    PeerPid = peer:start(Id, KbucketPid),
    {PeerPid, KbucketPid}.

teardown({_, _}) ->
    ?assert(meck:validate(kbucket)),
    meck:unload(kbucket).

peer_suite_test_() ->
    [?setup(fun should_store_data/1),
     ?setup(fun should_overwrite_data_with_same_key/1),
     ?setup(fun should_answer_with_pong_to_a_ping/1),
     ?setup(fun should_update_kbucket_if_receive_a_pong/1),
     ?setup(fun should_contact_the_kbucket_for_its_closest_peer_to_a_key/1),
     ?setup(fun should_returns_its_closest_peers_when_no_key_is_found/1),
     ?setup(fun should_not_returns_the_contact_of_the_caller/1),
     ?setup(fun should_return_its_id/1)].

should_store_data({PeerPid, KbucketPid}) ->
    FakePeer = self(),
    {Key, Value} = {mykey, "myvalue"},

    meck:expect(kbucket, put, ?two_any_args(?return(ok))),
    peer:store({Key, Value}, PeerPid),
    peer:find_value_of(Key, PeerPid),

    ?receiving({PeerPid, ResponseValue},
                    [?_assertEqual(Value, ResponseValue),
                     ?_assertEqual(2, meck:num_calls(kbucket, put, [KbucketPid, FakePeer]))]).

should_overwrite_data_with_same_key({PeerPid, KbucketPid}) ->
    FakePeer = self(),
    {Key, FirstValue} = {mykey, "myvalue"},
    SecondValue = "updated",

    meck:expect(kbucket, put, ?two_any_args(?return(ok))),
    peer:store({Key, FirstValue}, PeerPid),
    peer:store({Key, SecondValue}, PeerPid),
    peer:find_value_of(Key, PeerPid),

    ?receiving({PeerPid, ResponseValue},
                    [?_assertEqual(SecondValue, ResponseValue),
                     ?_assertEqual(3, meck:num_calls(kbucket, put, [KbucketPid, FakePeer]))]).

should_answer_with_pong_to_a_ping({PeerPid, KbucketPid}) ->
    FakePeer = self(),

    meck:expect(kbucket, put, ?two_any_args(?return(ok))),
    peer:ping(PeerPid),

    ?receiving({pong, PeerPid},
                    [?_assert(meck:called(kbucket, put, [KbucketPid, FakePeer]))]).

should_update_kbucket_if_receive_a_pong({PeerPid, KbucketPid}) ->
    FakePeer = self(),

    meck:expect(kbucket, put, ?two_any_args(?return(ok))),
    peer:pong(PeerPid),
    timer:sleep(10),

    [?_assert(meck:called(kbucket, put, [KbucketPid, FakePeer]))].

should_contact_the_kbucket_for_its_closest_peer_to_a_key({PeerPid, KbucketPid}) ->
    FakePeer = self(),
    KeyToSearch = 2,

    meck:expect(kbucket, closest_peers, ?two_any_args(?return([1, 2]))),
    meck:expect(kbucket, put, ?two_any_args(?return(ok))),
    peer:find_closest_peers(KeyToSearch, PeerPid),

    ?receiving({PeerPid, Peers},
                    [?_assertEqual([1, 2], Peers),
                     ?_assert(meck:called(kbucket, put, [KbucketPid, FakePeer])),
                     ?_assert(meck:called(kbucket, closest_peers, [KbucketPid, KeyToSearch]))]).

should_returns_its_closest_peers_when_no_key_is_found({PeerPid, KbucketPid})->
    FakePeer = self(),
    UnknownKey = 2,

    meck:expect(kbucket, closest_peers, ?two_any_args(?return([1, 2]))),
    meck:expect(kbucket, put, ?two_any_args(?return(ok))),
    peer:find_value_of(UnknownKey, PeerPid),

    ?receiving({PeerPid, Peers},
                    [?_assertEqual([1, 2], Peers),
                     ?_assertEqual(1, meck:num_calls(kbucket, put, [KbucketPid, FakePeer])),
                     ?_assert(meck:called(kbucket, closest_peers, [KbucketPid, UnknownKey]))]).

should_not_returns_the_contact_of_the_caller({PeerPid, _}) ->
    FakePeer = self(),
    KeyToSearch = 1,

    meck:expect(kbucket, closest_peers, ?two_any_args(?return([2, FakePeer]))),
    meck:expect(kbucket, put, ?two_any_args(?return(ok))),
    peer:find_closest_peers(KeyToSearch, PeerPid),
    ?receiving({PeerPid, Peers}, [?_assertEqual([2], Peers)]).

should_return_its_id({PeerPid, _}) ->
    Id = peer:id_of(PeerPid),
    [?_assertEqual(1, Id)].
