-module(peer).

-export([start/2]).
-export([loop/1]).
-export([store/3]).
-export([find_value_of/3]).
-export([find_closest_peers/3]).
-export([pong/2]).
-export([ping/2]).

-record(peer, {id, repository, kbucket}).


start(Id, KbucketPid) when is_pid(KbucketPid) ->
    Peer = #peer{id = Id, repository = #{}, kbucket = KbucketPid},
    spawn(fun() -> loop(Peer) end);
start(Id, K) ->
    KbucketPid = kbucket:start(Id, K),
    Peer = #peer{id = Id, repository = #{}, kbucket = KbucketPid},
    spawn(fun() -> loop(Peer) end).

store(PeerPid, {Key, Value}, FromPeerPid) ->
    PeerPid ! {store, FromPeerPid, {Key, Value}},
    ok.

find_value_of(PeerPid, Key, FromPeerPid) ->
    PeerPid ! {find_value, FromPeerPid, Key},
    ok.

find_closest_peers(PeerPid, Key, FromPeerPid) ->
    PeerPid ! {find_closest_peers, FromPeerPid, Key},
    ok.

ping(PeerPid, FromPeerPid) ->
    PeerPid ! {ping, FromPeerPid},
    ok.

pong(PeerPid, FromPeerPid) ->
    PeerPid ! {pong, FromPeerPid},
    ok.

loop(#peer{kbucket = Kbucket, id = Id, repository = Repository} = Peer) ->
    receive
        {store, FromPeer, {Key, Value}} ->
            kbucket:put(Kbucket, FromPeer),
            NewRepository = Repository#{Key => Value},
            NewPeer = Peer#peer{repository = NewRepository},
            loop(NewPeer);
        {ping, FromPeer} ->
            kbucket:put(Kbucket, FromPeer),
            pong(FromPeer, self()),
            loop(Peer);
        {pong, FromPeer} ->
            kbucket:put(Kbucket, FromPeer),
            loop(Peer);
        {find_value, FromPeer, Key} ->
            kbucket:put(Kbucket, FromPeer),
            ResponseValue = handle_find_value(FromPeer, Key, Peer),
            FromPeer ! {self(), ResponseValue},
            loop(Peer);
        {find_closest_peers, FromPeer, Key} ->
            kbucket:put(Kbucket, FromPeer),
            FilteredClosestPeers = handle_find_closest_peers(FromPeer, Kbucket, Key),
            FromPeer ! {self(), FilteredClosestPeers},
            loop(Peer);
        _ ->
            loop(Peer)
    end.

handle_find_closest_peers(FromPeer, Kbucket, Key) ->
    ClosestPeers = kbucket:closest_peers(Kbucket, Key),
    lists:delete(FromPeer, ClosestPeers).

handle_find_value(FromPeer, Key, #peer{repository = Repository, kbucket = Kbucket}) ->
    case maps:is_key(Key, Repository) of
        true ->
            #{Key := Value} = Repository,
            Value;
        false ->
            handle_find_closest_peers(FromPeer, Kbucket, Key)
    end.

-ifdef(TEST).
-include_lib("../test/peer.hrl").
-endif.
