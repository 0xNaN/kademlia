-module(peer).

-export([start/2]).
-export([loop/1]).
-export([check_link/2]).
-export([iterative_find_peers/2]).

-define (TIMEOUT_REQUEST, 100).
-define (ALPHA, 3).
-define (KEY_LENGTH, 160).

-record(peer, {mycontact, repository, kbucket}).

start(Id, KbucketPid) when is_pid(KbucketPid) ->
    PeerPid = spawn(fun() -> loop(build_peer(Id, KbucketPid)) end),
    {PeerPid, Id};
start(Id, K) ->
    KbucketPid = kbucket:start(K, ?KEY_LENGTH),
    PeerPid = spawn(fun() -> loop(build_peer(Id, KbucketPid)) end),
    PeerContact = {PeerPid, Id},
    KbucketPid ! {set_peer, PeerContact},
    PeerContact.

build_peer(Id, Kbucket) ->
    Peer = #peer{repository = #{}, kbucket = Kbucket, mycontact = {self(), Id}},
    Peer.

iterative_find_peers({PeerPid, _} = Peer, Key) ->
    PeerPid ! {iterative_find_peers, self(), Key},
    receive
        {Peer, Result} ->
            Result
    end.

iterative_store({PeerPid, _} = Peer, {Key, Value}) ->
    PeerPid ! {iterative_store, self(), {Key, Value}},
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

loop(#peer{kbucket = Kbucket, repository = Repository, mycontact = MyContact} = Peer) ->
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
            handle_pong(Peer, FromContact),
            loop(Peer);
        {find_value, {FromPid, _} = FromContact, Key} ->
            kbucket:put(Kbucket, FromContact),
            ResponseValue = handle_find_value(Peer, FromContact, Key),
            FromPid ! {MyContact, ResponseValue},
            loop(Peer);
        {find_closest_peers, {FromPid, _} = FromContact, Key} ->
            kbucket:put(Kbucket, FromContact),
            FilteredClosestPeers = handle_find_closest_peers(Peer, FromContact, Key),
            FromPid ! {MyContact, FilteredClosestPeers},
            loop(Peer);
        {check_link, From, ToContact} ->
            Result = handle_check_link(Peer, MyContact, ToContact),
            From ! {MyContact, Result},
            loop(Peer);
        {iterative_find_peers, From, Key} ->
            Result = handle_iterative_find_peers(Peer, Key),
            From ! {MyContact, Result},
            loop(Peer);
        {iterative_store, From, {Key, Value}} ->
            handle_iterative_store(Peer, {Key, Value}),
            From ! {MyContact, ok},
            loop(Peer);
        {join, From, BootstrapPeer} ->
            kbucket:put(Kbucket, BootstrapPeer),
            handle_join(Peer, BootstrapPeer),
            From ! {MyContact, ok},
            loop(Peer);
        _ ->
            loop(Peer)
    end.

handle_iterative_store(#peer{mycontact = MyContact } = Peer, {Key, Value}) ->
    HashedKey = crypto:hash(sha, atom_to_list(Key)),
    %% XXX: NumberKey should be removed when ID become binary
    <<NumberKey:?KEY_LENGTH>> = HashedKey,
    ClosestPeers = handle_iterative_find_peers(Peer, NumberKey),
    lists:foreach(fun(Contact) -> peer:store(Contact, {NumberKey, Value}, MyContact) end, ClosestPeers).

handle_join(#peer{kbucket = Kbucket, mycontact = MyContact} = Peer, BootstrapPeer) ->
    {_, Id} = MyContact,
    MyKClosest = peer:iterative_find_peers(BootstrapPeer, Id),
    lists:foreach(fun(Neighbor) ->
                      handle_check_link(Peer, MyContact, Neighbor)
                  end, MyKClosest),
    kbucket:refresh(Kbucket).

handle_check_link(Peer, MyContact, ToContact) ->
    ping(ToContact, MyContact),
    receive
        {pong, ToContact} ->
            handle_pong(Peer, ToContact),
            ok
    after ?TIMEOUT_REQUEST ->
            ko
    end.

handle_iterative_find_peers(#peer{kbucket = Kbucket, mycontact = MyContact} = Peer, Key) ->
    case kbucket:closest_contacts(Kbucket, Key) of
        [] ->
            [MyContact];

        LocalClosestPeers ->
            ClosestSoFar = hd(LocalClosestPeers),
            do_iterative_find_peers(Peer, ClosestSoFar, LocalClosestPeers, Key)
    end.

do_iterative_find_peers(#peer{kbucket = Kbucket, mycontact = MyContact} = Peer, ClosestSoFar, ClosestPeers, Key) ->
    SelectedContacts = lists:sublist(ClosestPeers, ?ALPHA),
    ContactsFound = lists:flatmap(fun(Contact) ->
                                      syn_find_closest_peers(Contact, Key, MyContact)
                                  end, SelectedContacts),
    AllKnownClosestContacts = append_unique(ContactsFound, SelectedContacts),
    KClosestContacts = kbucket:k_closest_to(Kbucket, Key, AllKnownClosestContacts),
    CandidateClosest = hd(KClosestContacts),
    case kbucket:is_closest(CandidateClosest, ClosestSoFar, Key) of
        true ->
            do_iterative_find_peers(Peer, CandidateClosest, KClosestContacts, Key);
        false ->
            kbucket:k_closest_to(Kbucket, Key, [MyContact | KClosestContacts])
    end.

handle_pong(#peer{kbucket = Kbucket}, FromContact) ->
    kbucket:put(Kbucket, FromContact).

handle_find_closest_peers(#peer{kbucket = Kbucket}, FromContact, Key) ->
    ClosestPeers = kbucket:closest_contacts(Kbucket, Key),
    lists:delete(FromContact, ClosestPeers).

handle_find_value(#peer{repository = Repository} = Peer, FromContact, Key) ->
    case maps:is_key(Key, Repository) of
        true ->
            #{Key := Value} = Repository,
            Value;
        false ->
            handle_find_closest_peers(Peer, FromContact, Key)
    end.

append_unique(FirstList, SecondList) ->
    List = lists:append(FirstList, SecondList),
    sets:to_list(sets:from_list(List)).

-ifdef(TEST).
-compile([export_all]).
-include_lib("../test/peer.hrl").
-endif.
