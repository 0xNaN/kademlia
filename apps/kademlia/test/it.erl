-module(it).
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


new_peer(Id, K) ->
    Kbucket = kbucket:start(K),
    Peer = peer:start(Id, Kbucket),
    Kbucket ! {set_peer, Peer},
    {Kbucket, Peer}.
