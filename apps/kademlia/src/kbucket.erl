-module(kbucket).
-export([start/2]).
-export([kbucket/3]).
-export([put/2]).
-export([get/2]).
-export([bucket_index/1]).
-export([distance/2]).

-define (TIMEOUT_PONG, 100).

-type contact() :: {pid(), integer()} .

start(OwningPeerId, K) ->
    spawn(fun() -> kbucket(OwningPeerId, K, #{}) end).

distance(FromPeerId, ToPeerId) ->
    FromPeerId bxor ToPeerId.

bucket_index(Distance) ->
    trunc(math:log2(Distance)).

put(KbucketPid, PeerId) ->
    KbucketPid ! {put, PeerId},
    ok.

get(KbucketPid, Distance) ->
    KbucketPid ! {get, self(), Distance},
    receive
        {KbucketPid, Bucket} -> Bucket
    end.

bucket(BucketIndex, Contacts) ->
    case maps:is_key(BucketIndex, Contacts) of
        true -> #{BucketIndex := Bucket} = Contacts,
                Bucket;
        _    -> []
    end.

put_contact(Contact, [LeastContact | PartialBucket] = Bucket , K) when length(Bucket) =:= K ->
    {PeerPid, _} = LeastContact,
    peer:ping(PeerPid),
    receive
        {pong, PeerPid} ->
            Bucket
    after ?TIMEOUT_PONG ->
        put_contact(Contact, PartialBucket, K)
    end;
put_contact(Contact, Bucket, _) ->
    CleanedBucket = lists:delete(Contact, Bucket),
    lists:append(CleanedBucket, [Contact]).

kbucket(OwningPeerId, K, Contacts) ->
    receive
        {put, {PeerPid, PeerId} = Contact} ->
            Distance = distance(OwningPeerId, PeerId),
            BucketIndex = bucket_index(Distance),
            Bucket = bucket(BucketIndex, Contacts),
            NewBucket = put_contact(Contact, Bucket, K),
            kbucket(OwningPeerId, K, Contacts#{BucketIndex => NewBucket});

        {get, FromPeer, BucketIndex} ->
            FromPeer ! {self(), bucket(BucketIndex, Contacts)},
            kbucket(OwningPeerId, K, Contacts);

        _ ->
            kbucket(OwningPeerId, K, Contacts)
    end.

-ifdef(TEST).
-include_lib("../test/kbucket.hrl").
-endif.
