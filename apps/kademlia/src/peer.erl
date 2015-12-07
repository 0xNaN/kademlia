-module(peer).

-export([start/2]).
-export([loop/1]).
-export([check_link/2]).
-export([iterative_find_peers/2]).

-define (TIMEOUT_REQUEST, 100).
-define (ALPHA, 3).

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

iterative_find_peers({PeerPid, _} = Peer, Key) ->
    PeerPid ! {iterative_find_peers, self(), Key},
    receive
        {Peer, Result} ->
            Result
    end.

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

syn_find_closest_peers(CandidatePeer, Key, FromPeer) ->
    find_closest_peers(CandidatePeer, Key, FromPeer),
    receive
        %% XXX for consistency should tag with Contact
        {CandidatePeer, Result} ->
            Result
        after ?TIMEOUT_REQUEST ->
            []
    end.

ping({PeerPid, _}, FromPeer) ->
    PeerPid ! {ping, FromPeer},
    ok.

pong({PeerPid, _}, FromPeer) ->
    PeerPid ! {pong, FromPeer},
    ok.

join({PeerPid, _} = Peer, BootstrapPeer) ->
    PeerPid ! {join, self(), BootstrapPeer},
    receive
        {Peer, ok} ->
            ok
    end.

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
            FromPid ! {MyContact, ResponseValue},
            loop(Peer);
        {find_closest_peers, {FromPid, _} = FromContact, Key} ->
            kbucket:put(Kbucket, FromContact),
            FilteredClosestPeers = handle_find_closest_peers(FromContact, Kbucket, Key),
            FromPid ! {MyContact, FilteredClosestPeers},
            loop(Peer);
        {check_link, From, ToContact} ->
            Result = handle_check_link(Kbucket, MyContact, ToContact),
            From ! {MyContact, Result},
            loop(Peer);
        {iterative_find_peers, From, Key} ->
            Result = handle_iterative_find_peers(Peer, MyContact, Key),
            From ! {MyContact, Result},
            loop(Peer);
        {join, From, BootstrapPeer} ->
            kbucket:put(Kbucket, BootstrapPeer),
            KClosest = peer:iterative_find_peers(BootstrapPeer, Id),
            lists:foreach(fun(Neighbor) -> handle_check_link(Kbucket, MyContact, Neighbor) end, KClosest),
            From ! {MyContact, ok},
            loop(Peer);
        _ ->
            loop(Peer)
    end.

handle_check_link(Kbucket, MyContact, ToContact) ->
    ping(ToContact, MyContact),
    receive
        {pong, ToContact} ->
            handle_pong(Kbucket, ToContact),
            ok
    after ?TIMEOUT_REQUEST ->
            ko
    end.

handle_iterative_find_peers(#peer{kbucket = Kbucket}, MyContact, Key) ->
    case kbucket:closest_contacts(Kbucket, Key) of
        [] ->
            [MyContact];

        LocalClosestPeers ->
            ClosestSoFar = hd(LocalClosestPeers),
            do_iterative_find_peers(Kbucket, MyContact, ClosestSoFar, LocalClosestPeers, Key)
    end.

do_iterative_find_peers(Kbucket, MyContact, ClosestSoFar, ClosestPeers, Key) ->
    SelectedContacts = lists:sublist(ClosestPeers, ?ALPHA),
    ContactsFound = lists:flatmap(fun(Contact) ->
                                      syn_find_closest_peers(Contact, Key, MyContact)
                                  end, SelectedContacts),
    AllKnownClosestContacts = append_unique(ContactsFound, SelectedContacts),
    KClosestContacts = kbucket:k_closest_to(Kbucket, Key, AllKnownClosestContacts),
    CandidateClosest = hd(KClosestContacts),
    case kbucket:is_closest(CandidateClosest, ClosestSoFar, Key) of
        true ->
            do_iterative_find_peers(Kbucket, MyContact, CandidateClosest, KClosestContacts, Key);
        false ->
            kbucket:k_closest_to(Kbucket, Key, [MyContact | KClosestContacts])
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

append_unique(FirstList, SecondList) ->
    List = lists:append(FirstList, SecondList),
    sets:to_list(sets:from_list(List)).

-ifdef(TEST).
-compile([export_all]).
-include_lib("../test/peer.hrl").
-endif.
