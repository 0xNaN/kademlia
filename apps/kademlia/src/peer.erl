-module(peer).

-export([start/1, start/2]).
-export([loop/1]).
-export([store/2]).
-export([find_value_of/2]).
-export([find_closest_peers/2]).
-export([pong/1]).
-export([ping/1]).

-record(peer, {id, repository, kbucket}).

start(Id) ->
    KbucketPid = kbucket:start(Id),
    Peer = #peer{id = Id, repository = #{}, kbucket = KbucketPid},
    spawn(fun() -> loop(Peer) end).
start(Id, KbucketPid) ->
    Peer = #peer{id = Id, repository = #{}, kbucket = KbucketPid},
    spawn(fun() -> loop(Peer) end).

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

loop(#peer{kbucket = Kbucket, id = Id, repository = Repository} = Peer) ->
    receive
        {store, FromPeer, {Key, Value}} ->
            kbucket:put(Kbucket, FromPeer),
            NewRepository = Repository#{Key => Value},
            NewPeer = Peer#peer{repository = NewRepository},
            loop(NewPeer);
        {ping, FromPeer} ->
            kbucket:put(Kbucket, FromPeer),
            pong(FromPeer),
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
