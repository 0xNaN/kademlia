-include_lib("eunit/include/eunit.hrl").

should_compute_the_distance_of_two_id_test() ->
    FirstId  = 2#0001,
    SecondId = 2#0010,
    ?assertEqual(3, kbucket:distance(FirstId, SecondId)).

should_start_a_kbucket_process_test() ->
    OwningId = 2#0001,
    K = 3,
    KbucketPid = kbucket:start(OwningId, K),
    ?assert(erlang:is_pid(KbucketPid)).

should_put_a_contact_on_its_correct_bucket_test() ->
    K = 1,
    OwnerPeerId = 2#0001,
    KbucketPid = kbucket:start(OwnerPeerId, K),

    PeerId = 2#00011,
    ok = kbucket:put(KbucketPid, PeerId),
    ?assertEqual([PeerId], kbucket:get(KbucketPid, 2)).

should_returns_an_empty_list_for_an_unknown_distance_test() ->
    K = 1,
    OwnerPeerId = 2#0001,
    KbucketPid = kbucket:start(OwnerPeerId, K),

    ?assertEqual([], kbucket:get(KbucketPid, 100)).

