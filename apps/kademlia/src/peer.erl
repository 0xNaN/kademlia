-module(peer).
-export([start/1,
         start/2,
         peer/3,
         store/2,
         find_value_of/2,
         pong/2,
         ping/2,
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

ping(ToPeerPid, FromPeerPid) ->
    ToPeerPid ! {ping, FromPeerPid},
    ok.

pong(ToPeerPid, FromPeerPid) ->
    ToPeerPid ! {pong, FromPeerPid},
    ok.

id_of(PeerPid) ->
    PeerPid ! {id, self()},
    receive
        {PeerPid, Id} ->
            Id
    end.

peer(Id, Map, KbucketPid) ->
    %TODO: each of these ops should update the bucket
    receive
        {store, _, {Key, Value}} ->
            NewMap = Map#{Key => Value},
            peer(Id, NewMap, KbucketPid);

        {ping, FromPeer} ->
            kbucket:put(FromPeer, KbucketPid),
            pong(FromPeer, self()),
            peer(Id, Map, KbucketPid);

        {pong, FromPeer} ->
            kbucket:put(FromPeer, KbucketPid),
            peer(Id, Map, KbucketPid);

        {find_value, FromPeer, Key} ->
            #{Key := Value} = Map,
            FromPeer ! {self(), Value},
            peer(Id, Map, KbucketPid);

        {id, FromPeer} ->
            FromPeer ! {self(), Id},
            peer(Id, Map, KbucketPid);

        _ -> peer(Id, Map, KbucketPid)
    end.

-ifdef(TEST).
-include_lib("../test/peer.hrl").
-endif.
