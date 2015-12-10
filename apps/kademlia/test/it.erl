-module(it).

-include_lib("test_macro.hrl").
-include_lib("eunit/include/eunit.hrl").

should_find_the_closest_peer_for_a_given_key_test() ->
    K = 3,
    Peer        = peer:start(2#0100, K),
    MiddlePeer  = peer:start(2#0001, K),
    ClosestPeer = peer:start(2#0010, K),

    % check the links between peers to simulate a join
    %
    %    Peer <--> MiddlePeer <--> ClosestPeer
    %
    peer:check_link(Peer, MiddlePeer),
    peer:check_link(MiddlePeer, ClosestPeer),
    Key = 3,
    ClosestPeers = peer:iterative_find_peers(Peer, Key),
    ?assertEqual([ClosestPeer, MiddlePeer, Peer], ClosestPeers).

should_find_the_closest_peer_for_a_given_key_in_a_complex_network_test() ->
    K = 3,
    PeerA = peer:start(2#0001, K),
    PeerB = peer:start(2#0010, K),

    PeerE = peer:start(2#1101, K),
    PeerF = peer:start(2#1110, K),
    PeerG = peer:start(2#1111, K),
    PeerH = peer:start(2#1010, K),
    PeerZ = peer:start(2#0111, K),

    peer:check_link(PeerA, PeerB),  % B join with A

    peer:check_link(PeerG, PeerA),  % G join through A
    peer:check_link(PeerG, PeerB),  %   find its key
                                    % know all so stop

    peer:check_link(PeerH, PeerG),  % H join through G
    peer:check_link(PeerH, PeerA),  %   find its key
    peer:check_link(PeerH, PeerB),  %   ...

    peer:check_link(PeerZ, PeerH),  % join through H
    peer:check_link(PeerZ, PeerA),  %    find its key
    peer:check_link(PeerZ, PeerB),
                                    % find close bucket but no new peers

    peer:check_link(PeerF, PeerE),   % F join with E

    peer:check_link(PeerF, PeerZ),   % F join through Z
    peer:check_link(PeerF, PeerH),   %   find its key
    peer:check_link(PeerF, PeerG),   %   try to find `15` to fill kbucket 0 (empty)

    Key = 3,
    ClosestPeers = peer:iterative_find_peers(PeerE, Key),
    ?assertEqual([PeerB, PeerA, PeerZ], ClosestPeers).

join_should_update_kbucket_of_bootstrap_peer_test() ->
    K = 3,
    {KbucketA, PeerA} = new_peer(1, K),
    {KbucketB, PeerB} = new_peer(2, K),

    peer:join(PeerA, PeerB),

    ?assertEqual([PeerB], kbucket:get(KbucketA, 1)),
    ?assertEqual([PeerA], kbucket:get(KbucketB, 1)).

a_joining_peer_should_know_its_closest_neighbours_test() ->
    K = 3,
    {KbucketA, PeerA} = new_peer(1, K),
    {KbucketB, PeerB} = new_peer(9, K),
    {KbucketC, PeerC} = new_peer(10, K),
    {KbucketD, PeerD} = new_peer(2, K),

    peer:join(PeerA, PeerB),
    peer:join(PeerC, PeerB),
    peer:join(PeerD, PeerC),

    ?assertEqual([PeerD],        kbucket:get(KbucketA, 1)),
    ?assertEqual([PeerB, PeerC], kbucket:get(KbucketA, 3)),

    ?assertEqual([PeerC],        kbucket:get(KbucketB, 1)),
    ?assertEqual([PeerA, PeerD], kbucket:get(KbucketB, 3)),

    ?assertEqual([PeerB],        kbucket:get(KbucketC, 1)),
    ?assertEqual([PeerA, PeerD], kbucket:get(KbucketC, 3)),

    ?assertEqual([PeerA],        kbucket:get(KbucketD, 1)),
    ?assertEqual([PeerC, PeerB], kbucket:get(KbucketD, 3)).

should_store_a_key_on_closest_peers_test() ->
    K = 3,
    FakePeer = {self(), 100},

    Key     = key,
    HashKey = 16#A62F2225BF70BFACCBC7F1EF2A397836717377DE,
    Value   = "value",

    % distance with HashKey
    % PeerA = 4, PeerB = 7, PeerC = 14, PeerD = ~2^159
    PeerA = peer:start(16#A62F2225BF70BFACCBC7F1EF2A397836717377DA, K),
    PeerB = peer:start(16#A62F2225BF70BFACCBC7F1EF2A397836717377D9, K),
    PeerC = peer:start(16#A62F2225BF70BFACCBC7F1EF2A397836717377D0, K),
    PeerD = peer:start(16#F62F2225BF70BFACCBC7F1EF2A397836717377DE, K),

    peer:join(PeerA, PeerC),
    peer:join(PeerB, PeerC),
    peer:join(PeerD, PeerB),
    timer:sleep(1000),

    peer:iterative_store(PeerA, {Key, Value}),
    timer:sleep(500),

    peer:find_value_of(PeerA, HashKey, FakePeer),
    ?receiving({PeerA, ResponseA}, ?assertEqual({found, Value}, ResponseA)),

    peer:find_value_of(PeerC, HashKey, FakePeer),
    ?receiving({PeerC, ResponseB}, ?assertEqual({found, Value}, ResponseB)),

    peer:find_value_of(PeerB, HashKey, FakePeer),
    ?receiving({PeerB, ResponseC}, ?assertEqual({found, Value}, ResponseC)),

    peer:find_value_of(PeerD, HashKey, FakePeer),
    ?receiving({PeerD, ResponseD}, ?assertEqual([PeerA, PeerB, PeerC], ResponseD)).

should_find_a_value_stored_in_a_network_test() ->
    K = 3,
    Key     = key,
    Value   = "value",

    % distance with HashKey
    % PeerA = 4, PeerB = 7, PeerC = 14, PeerD = ~2^159
    PeerA = peer:start(16#A62F2225BF70BFACCBC7F1EF2A397836717377DA, K),
    PeerB = peer:start(16#A62F2225BF70BFACCBC7F1EF2A397836717377D9, K),
    PeerC = peer:start(16#A62F2225BF70BFACCBC7F1EF2A397836717377D0, K),
    PeerD = peer:start(16#F62F2225BF70BFACCBC7F1EF2A397836717377DE, K),

    peer:join(PeerA, PeerC),
    peer:join(PeerB, PeerC),
    peer:join(PeerD, PeerB),
    timer:sleep(1000),

    peer:iterative_store(PeerA, {Key, Value}),
    timer:sleep(500),

    ?assertEqual(Value, peer:iterative_find_value(PeerD, Key)).

new_peer(Id, K) ->
    Kbucket = kbucket:start(K, 4),
    Peer = peer:start(Id, Kbucket),
    Kbucket ! {set_peer, Peer},
    {Kbucket, Peer}.

