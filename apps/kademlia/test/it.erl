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
