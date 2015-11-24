-module(peer).
-export([start/1,
         start/2,
         peer/3,
         store/2,
         find_value_of/2,
         find_closest_peers/2,
         pong/1,
         ping/1,
         id_of/1]).

start(Id) ->
    spawn(fun() -> peer(Id, #{}, kbucket:start(Id)) end).
start(Id, KbucketPid) ->
    spawn(fun() -> peer(Id, #{}, KbucketPid) end).

store({Key, Value}, PeerPid) ->
    PeerPid ! {store, self(), {Key, Value}},
    ok.

find_value_of(Key, PeerPid) ->
    PeerPid ! {find_value, self(), Key},
    receive
        {PeerPid, Value} -> Value
    end.

find_closest_peers(Key, PeerPid) ->
    PeerPid ! {find_closest_peers, self(), Key},
    ok.

ping(ToPeerPid) ->
    ToPeerPid ! {ping, self()},
    ok.

pong(ToPeerPid) ->
    ToPeerPid ! {pong, self()},
    ok.

id_of(PeerPid) ->
    PeerPid ! {id, self()},
    receive
        {PeerPid, Id} ->
            Id
    end.

peer(Id, Map, KbucketPid) ->
    receive
        {store, FromPeer, {Key, Value}} ->
            kbucket:put(KbucketPid, FromPeer),
            NewMap = Map#{Key => Value},
            peer(Id, NewMap, KbucketPid);

        {ping, FromPeer} ->
            kbucket:put(KbucketPid, FromPeer),
            pong(FromPeer),
            peer(Id, Map, KbucketPid);

        {pong, FromPeer} ->
            kbucket:put(KbucketPid, FromPeer),
            peer(Id, Map, KbucketPid);

        {find_value, FromPeer, Key} ->
            kbucket:put(KbucketPid, FromPeer),
            #{Key := Value} = Map,
            FromPeer ! {self(), Value},
            peer(Id, Map, KbucketPid);

        {find_closest_peers, FromPeer, Key} ->
            kbucket:put(KbucketPid, FromPeer),
            ClosestPeers = kbucket:closest_peers(KbucketPid, Key),
            FromPeer ! {self(), ClosestPeers},
            peer(Id, Map, KbucketPid);

        {id, FromPeer} ->
            FromPeer ! {self(), Id},
            peer(Id, Map, KbucketPid);

        _ -> peer(Id, Map, KbucketPid)
    end.

-ifdef(TEST).
-include_lib("../test/peer.hrl").
-endif.
