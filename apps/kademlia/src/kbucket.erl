-module(kbucket).
-export([start/2]).
-export([loop/1]).
-export([put/2]).
-export([set_peer/2]).
-export([is_closest/3]).
-export([k_closest_to/3]).
-export([closest_contacts/2]).
-export([refresh/1]).

-define (LOG(From, To, Msg, Args), lager:info("[KBUCKET] ~p <- ~p: ~p ~p", [To, From, Msg, Args])).

-type contact() :: {pid(), integer()} .

-record(kbucket, {peer, k, contacts, keylength}).

start(K, Keylength) ->
    Kbucket = #kbucket{k = K, keylength = Keylength, contacts = #{}},
    spawn(fun() -> loop(Kbucket) end).

put(KbucketPid, Peer) ->
    KbucketPid ! {put, self(), Peer},
    ok.

get(KbucketPid, BucketIndex) ->
    KbucketPid ! {get, self(), BucketIndex},
    receive
        {KbucketPid, Bucket} -> Bucket
    end.

set_peer(KbucketPid, PeerContact) ->
    KbucketPid ! {set_peer, PeerContact},
    ok.

closest_contacts(KbucketPid, Key) ->
    KbucketPid ! {closest_contacts, self(), Key},
    receive
        {KbucketPid, Contacts} -> Contacts
    end.

k_closest_to(KbucketPid, Key, Contacts) ->
    KbucketPid ! {k_closest_to, self(), Key, Contacts},
    receive
        {KbucketPid, Result} ->
            Result
    end.

refresh(Kbucket) ->
    Kbucket ! {refresh, self()},
    receive
        {Kbucket, ok} ->
            ok
    end.

loop(#kbucket{keylength = Keylength, peer = Peer, contacts = Contacts} = Kbucket) ->
    receive
        {put, FromPeer, Contact} when Contact =/= Peer ->
            ?LOG(FromPeer, self(), "PUT", Contact),
            NewKbucket = handle_put(Kbucket, Contact),
            loop(NewKbucket);
        {closest_contacts, FromPeer, Key} ->
            ?LOG(FromPeer, self(), "CLOSEST_CONTACTS", Key),
            ClosestContacts = handle_closest_contacts(Kbucket, Key),
            FromPeer ! {self(), ClosestContacts},
            loop(Kbucket);
        {get, FromPeer, BucketIndex} ->
            ?LOG(FromPeer, self(), "GET", BucketIndex),
            FromPeer ! {self(), bucket(BucketIndex, Contacts)},
            loop(Kbucket);
        {set_peer, PeerContact} ->
            NewKbucket = Kbucket#kbucket{peer = PeerContact},
            loop(NewKbucket);
        {k_closest_to, From, Key, ListOfContacts} ->
            Result = handle_k_closest_to(Kbucket, Key, ListOfContacts),
            From ! {self(), Result},
            loop(Kbucket);
        {refresh, From} ->
            ?LOG(From, self(), "REFRESH", ""),
            handle_refresh(Kbucket, Keylength),
            From ! {self(), ok},
            loop(Kbucket);
        _ ->
            loop(Kbucket)
    end.

handle_k_closest_to(#kbucket{k = K}, Key, ListOfContacts) ->
    SortedContacts = sort_on(Key, ListOfContacts),
    lists:sublist(SortedContacts, K).

handle_refresh(#kbucket{peer = Peer}, Keylength) ->
    lists:foreach(fun(Index) ->
                          %% NOTE: maybe we can call only handle_refresh on a new
                          %% process
                     spawn(kbucket, refresh_bucket, [self(), Index, Peer])
                  end, lists:seq(0, Keylength - 1)).

handle_closest_contacts(#kbucket{k = K, contacts = Contacts}, Key) ->
    SortedContacts = sort_on(Key, contacts_to_list(Contacts)),
    lists:sublist(SortedContacts, K).

handle_put(#kbucket{contacts = Contacts} = Kbucket, Contact) ->
    {BucketIndex, Bucket} = bucket_for(Kbucket, Contact),
    NewBucket = put_on(Bucket, Contact, Kbucket),
    NewContacts = Contacts#{BucketIndex => NewBucket},
    Kbucket#kbucket{contacts = NewContacts}.

bucket_for(#kbucket{contacts = Contacts, peer = {_, MyId}}, {_, ContactId}) ->
    DestinationBucketIndex = bucket_index(distance(MyId, ContactId)),
    {DestinationBucketIndex, bucket(DestinationBucketIndex, Contacts)}.

put_on([LeastContact | PartialBucket] = Bucket, Contact, #kbucket{k = K, peer = Peer}) when length(Bucket) =:= K ->
    case peer:check_link(LeastContact, Peer) of
        ok -> Bucket;
        ko -> put_on(PartialBucket, Contact, K)
    end;
put_on(Bucket, Contact, _) ->
    CleanedBucket = lists:delete(Contact, Bucket),
    lists:append(CleanedBucket, [Contact]).

refresh_bucket(Kbucket, BucketIndex, {_, PeerId} = Peer) ->
    Key = gen_key_within(BucketIndex, PeerId),
    ClosestPeers = peer:iterative_find_peers(Peer, Key),
    [kbucket:put(Kbucket, Contact) || Contact <- lists:delete(Peer, ClosestPeers)].

bucket(BucketIndex, Contacts) ->
    case maps:is_key(BucketIndex, Contacts) of
        true -> #{BucketIndex := Bucket} = Contacts,
                Bucket;
        _    -> []
    end.

contacts_to_list(Contacts) ->
    AllContacts = lists:map(fun(Index) -> bucket(Index, Contacts) end, maps:keys(Contacts)),
    lists:flatten(AllContacts).

sort_on(Key, Contacts) ->
    lists:sort(fun(PeerA, PeerB) -> is_closest(PeerA, PeerB, Key) end, Contacts).

distance(FromPeerId, ToPeerId) ->
    FromPeerId bxor ToPeerId.

is_closest({_, PeerAId}, {_, PeerBId}, Key) ->
    kbucket:distance(Key, PeerAId) < kbucket:distance(Key, PeerBId).

bucket_index(Distance) ->
    trunc(math:log2(Distance)).

gen_key_within(BucketIndex, Id) ->
    trunc(math:pow(2, BucketIndex)) bxor Id.

-ifdef(TEST).
-compile([export_all]).
-include_lib("../test/kbucket.hrl").
-endif.
