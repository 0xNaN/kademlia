-module(peer).

-export([start/3]).
-export([loop/1]).
-export([iterative_find_peers/2]).
-export([iterative_store/2]).
-export([iterative_find_value/2]).
-export([find_value_of/3]).
-export([find_closest_peers/3]).
-export([check_link/2]).
-export([join/2]).
-export([store/3]).

-include("peer.hrl").
-define (TIMEOUT_REQUEST, 1000).
-define (ALPHA, 3).
-define (KEY_LENGTH, 160).


start(Id, KbucketPid, Alpha) when is_pid(KbucketPid) ->
    PeerPid = spawn(fun() -> loop(build_peer(Id, KbucketPid, Alpha)) end),
    {PeerPid, Id};
start(Id, K, Alpha) ->
    KbucketPid = kbucket:start(K, ?KEY_LENGTH),
    PeerPid = spawn(fun() -> loop(build_peer(Id, KbucketPid, Alpha)) end),
    PeerContact = {PeerPid, Id},
    KbucketPid ! {set_peer, PeerContact},
    PeerContact.

build_peer(Id, Kbucket, Alpha) ->
    Peer = #peer{repository = #{}, kbucket = Kbucket, mycontact = {self(), Id}, alpha = Alpha},
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

iterative_find_value({PeerPid, _} = Peer, Key) ->
    PeerPid ! {iterative_find_value, self(), Key},
    receive
        {Peer, Value} ->
            Value
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

find_closest_peers({PeerPid, _}, Key, FromPeer) ->
    PeerPid ! {find_closest_peers, FromPeer, Key, self()},
    ok.

find_value_of({PeerPid, _}, Key, FromPeer) ->
    PeerPid ! {find_value, FromPeer, Key, self()},
    ok.

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
            log:peer(Peer, log:contact_to_field(FromContact, "from"), "STORE ~p ~p", [Key, Value]),
            kbucket:put(Kbucket, FromContact),
            NewRepository = Repository#{Key => Value},
            NewPeer = Peer#peer{repository = NewRepository},
            loop(NewPeer);
        {ping, FromContact} ->
            log:peer(Peer, log:contact_to_field(FromContact, "from"), "PING"),
            kbucket:put(Kbucket, FromContact),
            pong(FromContact, MyContact),
            loop(Peer);
        {pong, FromContact} ->
            log:peer(Peer, log:contact_to_field(FromContact, "from"), "PONG"),
            handle_pong(Peer, FromContact),
            loop(Peer);
        {find_closest_peers, FromContact, Key, From} ->
            log:peer(Peer, log:contact_to_field(FromContact, "from"), "FIND_CLOSEST_PEERS ~p", [Key]),
            kbucket:put(Kbucket, FromContact),
            FilteredClosestPeers = handle_find_closest_peers(Peer, FromContact, Key),
            From ! {MyContact, FilteredClosestPeers},
            loop(Peer);
        {find_value, FromContact, Key, From} ->
            log:peer(Peer, log:contact_to_field(FromContact, "from"), "FIND_VALUE ~p", [Key]),
            kbucket:put(Kbucket, FromContact),
            ResponseValue = handle_find_value(Peer, FromContact, Key),
            From ! {MyContact, ResponseValue},
            loop(Peer);
        {check_link, From, ToContact} ->
            log:peer(Peer, log:contact_to_field(ToContact, "to") ++ log:pid_to_field(From, "from"), "CHECK_LINK ~p",[self()]),
            Result = handle_check_link(Peer, MyContact, ToContact),
            From ! {MyContact, Result},
            loop(Peer);
        {iterative_find_peers, From, Key} ->
            log:peer(Peer, log:pid_to_field(From, "from"), "ITERATIVE_FIND_PEERS ~p", [Key]),
            Result = handle_iterative_find_peers(Peer, Key),
            From ! {MyContact, Result},
            loop(Peer);
        {iterative_store, From, {Key, Value}} ->
            log:peer(Peer, log:pid_to_field(From, "from"), "ITERATIVE_STORE ~p ~p", [Key, Value]),
            HashedKey = hash_key(Key),
            handle_iterative_store(Peer, {HashedKey, Value}),
            From ! {MyContact, ok},
            loop(Peer);
        {iterative_find_value, From, Key} ->
            log:peer(Peer, log:pid_to_field(From, "from"), "ITERATIVE_FIND_VALUE ~p", [Key]),
            Value = handle_iterative_find_value(Peer, hash_key(Key)),
            From ! {MyContact, Value},
            loop(Peer);
        {join, From, BootstrapPeer} ->
            log:peer(Peer, log:pid_to_field(From, "from"), "JOIN ~p", [BootstrapPeer]),
            kbucket:put(Kbucket, BootstrapPeer),
            handle_join(Peer, BootstrapPeer),
            From ! {MyContact, ok},
            loop(Peer);
        _ ->
            loop(Peer)
    end.

handle_iterative_store(#peer{mycontact = MyContact} = Peer, {Key, Value}) ->
    ClosestPeers = handle_iterative_find_peers(Peer, Key),
    lists:foreach(fun(Contact) -> peer:store(Contact, {Key, Value}, MyContact) end, ClosestPeers).

handle_join(#peer{kbucket = Kbucket, mycontact = MyContact} = Peer, BootstrapPeer) ->
    {_, Id} = MyContact,
    MyKClosest = peer:iterative_find_peers(BootstrapPeer, Id),
    lists:foreach(fun(Neighbor) ->
                      handle_check_link(Peer, MyContact, Neighbor)
                  end, lists:delete(MyContact, MyKClosest)),
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

handle_iterative_find_peers(#peer{kbucket = Kbucket, mycontact = MyContact, alpha = Alpha}, Key) ->
    network:find_peers(MyContact, Kbucket, Key, Alpha).

handle_iterative_find_value(#peer{kbucket = Kbucket, mycontact = MyContact, alpha = Alpha}, Key) ->
    network:find_value(MyContact, Kbucket, Key, Alpha).

handle_pong(#peer{kbucket = Kbucket}, FromContact) ->
    kbucket:put(Kbucket, FromContact).

handle_find_closest_peers(#peer{kbucket = Kbucket}, FromContact, Key) ->
    ClosestPeers = kbucket:closest_contacts(Kbucket, Key),
    lists:delete(FromContact, ClosestPeers).

handle_find_value(#peer{repository = Repository} = Peer, FromContact, Key) ->
    case maps:is_key(Key, Repository) of
        true ->
            #{Key := Value} = Repository,
            {found, Value};
        false ->
            handle_find_closest_peers(Peer, FromContact, Key)
    end.

hash_key(Key) ->
    HashedKey = crypto:hash(sha, atom_to_list(Key)),
    %% XXX: NumberKey should be removed when ID become binary
    <<NumberKey:?KEY_LENGTH, _/bitstring>> = HashedKey,
    NumberKey.

-ifdef(TEST).
-include_lib("../test/peer.hrl").
-endif.
