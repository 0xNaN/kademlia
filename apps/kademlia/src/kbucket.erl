-module(kbucket).
-export([start/2]).
-export([kbucket/3]).
-export([put/2]).
-export([get/2]).
-export([distance/2]).

start(OwningPeerId, K) ->
    spawn(fun() -> kbucket(OwningPeerId, K, #{}) end).

distance(FromPeerId, ToPeerId) ->
    FromPeerId bxor ToPeerId.

put(KbucketPid, PeerId) ->
    KbucketPid ! {put, PeerId},
    ok.

get(KbucketPid, Distance) ->
    KbucketPid ! {get, self(), Distance},
    receive
        {KbucketPid, Bucket} -> Bucket
    end.

kbucket(OwningPeerId, K, Contacts) ->
    receive
        {put, PeerId} ->
            Distance = distance(OwningPeerId, PeerId),
            BucketIndex = list_to_integer(float_to_list(math:log2(Distance), [{decimals, 0}])),
            kbucket(OwningPeerId, K, Contacts#{BucketIndex => [PeerId]});
        {get, FromPeer, Distance} ->
            ResultBucket = case maps:is_key(Distance, Contacts) of
                true -> #{Distance := Bucket} = Contacts,
                        Bucket;
                _    -> []
            end,
            FromPeer ! {self(), ResultBucket},
            kbucket(OwningPeerId, K, Contacts);
        _ ->
            kbucket(OwningPeerId, K, Contacts)
    end.

-ifdef(TEST).
-include_lib("../test/kbucket.hrl").
-endif.
