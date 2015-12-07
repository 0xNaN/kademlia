-module(kbucket).
-export([start/2]).
-export([loop/1]).
-export([put/2]).
-export([set_peer/2]).
-export([is_closest/3]).
-export([k_closest_to/3]).
-export([closest_contacts/2]).

-type contact() :: {pid(), integer()} .

-record(kbucket, {peer, k, contacts, keylength}).

start(K, Keylength) ->
    Kbucket = #kbucket{k = K, keylength = Keylength, contacts = #{}},
    spawn(fun() -> loop(Kbucket) end).

put(KbucketPid, PeerId) ->
    KbucketPid ! {put, PeerId},
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

loop(#kbucket{k = K} = Kbucket) ->
    receive
        {put, Contact} ->
            NewKbucket = handle_put(Contact, Kbucket),
            loop(NewKbucket);
        {closest_contacts, FromPeer, Key} ->
            ClosestContacts = handle_closest_contacts(Key, Kbucket),
            FromPeer ! {self(), ClosestContacts},
            loop(Kbucket);
        {get, FromPeer, BucketIndex} ->
            FromPeer ! {self(), bucket(BucketIndex, Kbucket)},
            loop(Kbucket);
        {set_peer, PeerContact} ->
            NewKbucket = Kbucket#kbucket{peer = PeerContact},
            loop(NewKbucket);
        {k_closest_to, From, Key, Contacts} ->
            SortedContacts = sort_on(Key, Contacts),
            Result = lists:sublist(SortedContacts, K),
            From ! {self(), Result},
            loop(Kbucket);
        _ ->
            loop(Kbucket)
    end.

handle_closest_contacts(Key, Kbucket) ->
    SortedContacts = sort_on(Key, all_contacts(Kbucket)),
    lists:sublist(SortedContacts, Kbucket#kbucket.k).

handle_put({_, PeerId} = Contact, #kbucket{contacts = Contacts, peer = {_, Id}} = Kbucket) ->
    BucketIndex = bucket_index(distance(Id, PeerId)),
    Bucket = bucket(BucketIndex, Kbucket),
    NewContacts = Contacts#{BucketIndex => put_on(Bucket, Contact, Kbucket)},
    Kbucket#kbucket{contacts=NewContacts}.

put_on([LeastContact | PartialBucket] = Bucket, Contact, #kbucket{k = K, peer = Peer}) when length(Bucket) =:= K ->
    case peer:check_link(LeastContact, Peer) of
        ok -> Bucket;
        ko -> put_on(PartialBucket, Contact, K)
    end;
put_on(Bucket, Contact, _) ->
    CleanedBucket = lists:delete(Contact, Bucket),
    lists:append(CleanedBucket, [Contact]).

bucket(BucketIndex, #kbucket{contacts = Contacts}) ->
    case maps:is_key(BucketIndex, Contacts) of
        true -> #{BucketIndex := Bucket} = Contacts,
                Bucket;
        _    -> []
    end.

all_contacts(Kbucket) ->
    Indexes = maps:keys(Kbucket#kbucket.contacts),
    AllContacts = lists:map(fun(Index) -> bucket(Index, Kbucket) end, Indexes),
    lists:flatten(AllContacts).

sort_on(Key, Contacts) ->
    lists:sort(fun(PeerA, PeerB) -> is_closest(PeerA, PeerB, Key) end, Contacts).

distance(FromPeerId, ToPeerId) ->
    FromPeerId bxor ToPeerId.

is_closest({_, PeerAId}, {_, PeerBId}, Key) ->
    kbucket:distance(Key, PeerAId) < kbucket:distance(Key, PeerBId).

bucket_index(Distance) ->
    trunc(math:log2(Distance)).

-ifdef(TEST).
-compile([export_all]).
-include_lib("../test/kbucket.hrl").
-endif.
