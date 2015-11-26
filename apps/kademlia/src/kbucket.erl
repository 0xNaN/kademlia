-module(kbucket).
-export([start/2]).
-export([kbucket/3]).
-export([put/2]).
-export([get/2]).
-export([bucket_index/1]).
-export([distance/2]).

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

kbucket(OwningPeerId, K, Contacts) ->
    receive
        {put, PeerId} ->
            Distance = distance(OwningPeerId, PeerId),
            BucketIndex = bucket_index(Distance),
            Bucket = bucket(BucketIndex, Contacts),
            NewBucket = lists:append(Bucket, [PeerId]),
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
