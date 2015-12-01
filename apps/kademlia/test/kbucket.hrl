-include_lib("eunit/include/eunit.hrl").
-include_lib("test_macro.hrl").

-define(PEER_ID, 2#1101).

start() ->
    meck:new(peer),
    meck:expect(peer, start, fun(Id, Kbucket) -> {self(), Id} end),
    K = 3,
    KbucketPid = kbucket:start(K),
    kbucket:set_peer(KbucketPid, peer:start(?PEER_ID, KbucketPid)),
    KbucketPid.

teardown(_) ->
    meck:unload(peer).

peer_suite_test_() ->
    [?setup(fun should_start_a_kbucket_process/1),
     ?setup(fun should_create_a_bucket_if_not_exists/1),
     ?setup(fun should_append_a_contacts_with_the_same_bucket_index/1),
     ?setup(fun should_update_an_already_present_contacts/1),
     ?setup(fun ping_the_least_seen_contact_when_a_bucket_is_full_and_remove_if_doesnt_respond/1),
     ?setup(fun ping_the_least_seen_contact_when_a_bucket_is_full_and_mantain_it_if_respond/1),
     ?setup(fun should_returns_an_empty_list_for_an_unknown_distance/1),
     ?setup(fun should_fill_the_results_with_other_contacts_if_the_closest_bucket_is_not_full/1),
     ?setup(fun should_returns_up_to_k_contacts_closest_to_a_key/1)].

should_compute_the_distance_of_two_id_test() ->
    ?assertEqual(3, kbucket:distance(2#0001, 2#0010)).

should_returns_the_correct_bucket_index_of_a_distance_test() ->
    ?assertEqual(0, kbucket:bucket_index(1)),
    ?assertEqual(1, kbucket:bucket_index(2)),
    ?assertEqual(1, kbucket:bucket_index(3)),
    ?assertEqual(2, kbucket:bucket_index(4)),
    ?assertEqual(2, kbucket:bucket_index(5)),
    ?assertEqual(2, kbucket:bucket_index(6)),
    ?assertEqual(2, kbucket:bucket_index(7)),
    ?assertEqual(3, kbucket:bucket_index(8)),
    ?assertEqual(7, kbucket:bucket_index(130)).

should_start_a_kbucket_process(KbucketPid) ->
    [?_assert(erlang:is_pid(KbucketPid))].

should_create_a_bucket_if_not_exists(KbucketPid) ->
    PeerContact = {self(), 2#1111},
    ok = kbucket:put(KbucketPid, PeerContact),
    [?_assertEqual([PeerContact], kbucket:get(KbucketPid, 1))].

should_returns_an_empty_list_for_an_unknown_distance(KbucketPid) ->
    [?_assertEqual([], kbucket:get(KbucketPid, 100))].

should_append_a_contacts_with_the_same_bucket_index(KbucketPid) ->
    PeerContact    = {self(), 2#1111}, % distance of 2 from OwningId
    AnotherContact = {self(), 2#1110}, %distance of 3 from OwningId

    ok = kbucket:put(KbucketPid, PeerContact),
    ok = kbucket:put(KbucketPid, AnotherContact),

    [?_assertEqual([PeerContact, AnotherContact], kbucket:get(KbucketPid, 1))].

should_update_an_already_present_contacts(KbucketPid) ->
    PeerContact    = {self(), 2#1111},
    AnotherContact = {self(), 2#1110},
    ok = kbucket:put(KbucketPid, PeerContact),
    ok = kbucket:put(KbucketPid, AnotherContact),

    ok = kbucket:put(KbucketPid, PeerContact),

    [?_assertEqual([AnotherContact, PeerContact], kbucket:get(KbucketPid, 1))].

ping_the_least_seen_contact_when_a_bucket_is_full_and_remove_if_doesnt_respond(KbucketPid) ->
    % these defines fakes contacts.
    % the IDs are such that its BucketIndex are the
    % same if stored in a peer with 2#1101 ID.
    FourPeerContact  = peer:start(2#1001, 3),
    FivePeerContact  = {self(), 2#1000},
    SixPeerContact   = {self(), 2#1011},
    SevenPeerContact = {self(), 2#1010},

    ok = kbucket:put(KbucketPid, FourPeerContact),
    ok = kbucket:put(KbucketPid, FivePeerContact),
    ok = kbucket:put(KbucketPid,  SixPeerContact),

    meck:expect(peer, check_link,  ?two_any_args(?return(ko))),
    ok = kbucket:put(KbucketPid, SevenPeerContact),

    [?_assertEqual([FivePeerContact, SixPeerContact, SevenPeerContact],
                   kbucket:get(KbucketPid, 2))].

ping_the_least_seen_contact_when_a_bucket_is_full_and_mantain_it_if_respond(KbucketPid) ->
    FourPeerContact  = peer:start(2#1001, 3),
    FivePeerContact  = {self(), 2#1000},
    SixPeerContact   = {self(), 2#1011},
    SevenPeerContact = {self(), 2#1010},

    ok = kbucket:put(KbucketPid, FourPeerContact),
    ok = kbucket:put(KbucketPid, FivePeerContact),
    ok = kbucket:put(KbucketPid,  SixPeerContact),

    meck:expect(peer, check_link, ?two_any_args(?return(ok))),
    ok = kbucket:put(KbucketPid, SevenPeerContact),

    [?_assertEqual([FourPeerContact, FivePeerContact, SixPeerContact],
                   kbucket:get(KbucketPid, 2))].

should_returns_up_to_k_contacts_closest_to_a_key(KbucketPid) ->
    ThreePeerContact  = {self(), 2#1001},
    TwoPeerContact    = {self(), 2#1000},
    OnePeerContact    = {self(), 2#1011},

    ok = kbucket:put(KbucketPid,    OnePeerContact),
    ok = kbucket:put(KbucketPid,    TwoPeerContact),
    ok = kbucket:put(KbucketPid,  ThreePeerContact),

    [?_assertEqual([OnePeerContact, TwoPeerContact, ThreePeerContact],
                   kbucket:closest_contacts(KbucketPid, 2#1010))].

should_fill_the_results_with_other_contacts_if_the_closest_bucket_is_not_full(KbucketPid) ->
    FivePeerContact   = {self(), 2#1111},
    ThreePeerContact  = {self(), 2#1001},
    TwoPeerContact    = {self(), 2#1000},

    ok = kbucket:put(KbucketPid,   TwoPeerContact),
    ok = kbucket:put(KbucketPid, ThreePeerContact),
    ok = kbucket:put(KbucketPid,  FivePeerContact),

    [?_assertEqual([TwoPeerContact, ThreePeerContact, FivePeerContact],
                   kbucket:closest_contacts(KbucketPid, 2#1010))].
