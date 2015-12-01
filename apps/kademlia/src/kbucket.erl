-module(kbucket).
-export([start/1]).
-export([loop/1]).
-export([put/2]).
-export([set_peer/2]).
-export([closest_contacts/2]).

-type contact() :: {pid(), integer()} .

-record(kbucket, {peer, k, contacts}).

start(K) ->
    Kbucket = #kbucket{k=K, contacts=#{}},
    spawn(fun() -> loop(Kbucket) end).

put(KbucketPid, PeerId) ->
    KbucketPid ! {put, PeerId},
    ok.

get(KbucketPid, Distance) ->
    KbucketPid ! {get, self(), Distance},
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

loop(Kbucket) ->
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
    lists:sort(fun({_, FirstId}, {_, SecondId}) ->
                       distance(Key, FirstId) =< distance(Key, SecondId)
               end, Contacts).

distance(FromPeerId, ToPeerId) ->
    FromPeerId bxor ToPeerId.

bucket_index(Distance) ->
    trunc(math:log2(Distance)).

-ifdef(TEST).
-compile([export_all]).
-include_lib("../test/kbucket.hrl").
-endif.
