-module(peer).

-export([start/1, start/2]).
-export([peer/3]).
-export([store/2]).
-export([find_value_of/2]).
-export([find_closest_peers/2]).
-export([pong/1]).
-export([ping/1]).
-export([id_of/1]).

start(Id) ->
    spawn(fun() -> peer(Id, #{}, kbucket:start(Id)) end).
start(Id, KbucketPid) ->
    spawn(fun() -> peer(Id, #{}, KbucketPid) end).

store({Key, Value}, PeerPid) ->
    PeerPid ! {store, self(), {Key, Value}},
    ok.

find_value_of(Key, PeerPid) ->
    PeerPid ! {find_value, self(), Key},
    ok.

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
            case maps:is_key(Key, Map) of
                true ->
                    #{Key := Value} = Map,
                    FromPeer ! {self(), Value};
                false ->
                    ClosestPeers = kbucket:closest_peers(KbucketPid, Key),
                    FromPeer ! {self(), ClosestPeers}
            end,
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
