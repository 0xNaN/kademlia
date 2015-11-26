-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun teardown/1, F}).

start() ->
    OwningId = 2#1101,
    K = 3,
    KbucketPid = kbucket:start(OwningId, K),
    KbucketPid.

teardown(_) ->
    void.

peer_suite_test_() ->
    [?setup(fun should_start_a_kbucket_process/1),
     ?setup(fun should_create_a_bucket_if_not_exists/1),
     ?setup(fun should_returns_an_empty_list_for_an_unknown_distance/1)].

should_compute_the_distance_of_two_id_test() ->
    FirstId  = 2#0001,
    SecondId = 2#0010,
    ?assertEqual(3, kbucket:distance(FirstId, SecondId)).

should_start_a_kbucket_process(KbucketPid) ->
    [?_assert(erlang:is_pid(KbucketPid))].

should_create_a_bucket_if_not_exists(KbucketPid) ->
    PeerId = 2#1111,
    ok = kbucket:put(KbucketPid, PeerId),
    [?_assertEqual([PeerId], kbucket:get(KbucketPid, 1))].

should_returns_an_empty_list_for_an_unknown_distance(KbucketPid) ->
    [?_assertEqual([], kbucket:get(KbucketPid, 100))].
