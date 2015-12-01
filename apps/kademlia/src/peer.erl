-module(peer).

-export([start/2]).
-export([loop/1]).
-export([check_link/2]).

-define (TIMEOUT_PONG, 100).

-record(peer, {id, repository, kbucket}).


start(Id, KbucketPid) when is_pid(KbucketPid) ->
    Peer = #peer{id = Id, repository = #{}, kbucket = KbucketPid},
    PeerPid = spawn(fun() -> loop(Peer) end),
    {PeerPid, Id};
start(Id, K) ->
    KbucketPid = kbucket:start(K),
    Peer = #peer{id = Id, repository = #{}, kbucket = KbucketPid},
    PeerPid = spawn(fun() -> loop(Peer) end),
    PeerContact = {PeerPid, Id},
    KbucketPid ! {set_peer, PeerContact},
    PeerContact.

check_link({PeerPid, _} = Peer, WithPeer) ->
    PeerPid ! {check_link, self(), WithPeer},
    receive
        {Peer, ok} ->
            ok;
        {Peer, ko} ->
            ko
    end.

store({PeerPid, _}, {Key, Value}, FromPeer) ->
    PeerPid ! {store, FromPeer, {Key, Value}},
    ok.

find_value_of({PeerPid, _}, Key, FromPeer) ->
    PeerPid ! {find_value, FromPeer, Key},
    ok.

find_closest_peers({PeerPid, _}, Key, FromPeer) ->
    PeerPid ! {find_closest_peers, FromPeer, Key},
    ok.

ping({PeerPid, _}, FromPeer) ->
    PeerPid ! {ping, FromPeer},
    ok.

pong({PeerPid, _}, FromPeer) ->
    PeerPid ! {pong, FromPeer},
    ok.

loop(#peer{kbucket = Kbucket, id = Id, repository = Repository} = Peer) ->
    MyContact = {self(), Id},
    receive
        {store, FromContact, {Key, Value}} ->
            kbucket:put(Kbucket, FromContact),
            NewRepository = Repository#{Key => Value},
            NewPeer = Peer#peer{repository = NewRepository},
            loop(NewPeer);
        {ping, FromContact} ->
            kbucket:put(Kbucket, FromContact),
            pong(FromContact, MyContact),
            loop(Peer);
        {pong, FromContact} ->
            handle_pong(Kbucket, FromContact),
            loop(Peer);
        {find_value, {FromPid, _} = FromContact, Key} ->
            kbucket:put(Kbucket, FromContact),
            ResponseValue = handle_find_value(FromContact, Key, Peer),
            FromPid ! {self(), ResponseValue},
            loop(Peer);
        {find_closest_peers, {FromPid, _} = FromContact, Key} ->
            kbucket:put(Kbucket, FromContact),
            FilteredClosestPeers = handle_find_closest_peers(FromContact, Kbucket, Key),
            FromPid ! {self(), FilteredClosestPeers},
            loop(Peer);
        {check_link, From, ToContact} ->
            ping(ToContact, MyContact),
            receive
                {pong, ToContact} ->
                    handle_pong(Kbucket, ToContact),
                    From ! {MyContact, ok}
            after ?TIMEOUT_PONG ->
                    From ! {MyContact, ko}
            end,
            loop(Peer);
        _ ->
            loop(Peer)
    end.

handle_pong(Kbucket, FromContact) ->
    kbucket:put(Kbucket, FromContact).

handle_find_closest_peers(FromContact, Kbucket, Key) ->
    ClosestPeers = kbucket:closest_contacts(Kbucket, Key),
    lists:delete(FromContact, ClosestPeers).

handle_find_value(FromContact, Key, #peer{repository = Repository, kbucket = Kbucket}) ->
    case maps:is_key(Key, Repository) of
        true ->
            #{Key := Value} = Repository,
            Value;
        false ->
            handle_find_closest_peers(FromContact, Kbucket, Key)
    end.

-ifdef(TEST).
-compile([export_all]).
-include_lib("../test/peer.hrl").
-endif.
