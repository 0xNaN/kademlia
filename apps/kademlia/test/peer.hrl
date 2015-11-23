-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, F}).

start() ->
    Id = 1,
    PeerPid = peer:start(Id),
    PeerPid.

peer_suite_test_() ->
    [?setup(fun should_store_data/1),
    ?setup(fun should_overwrite_data_with_same_key/1)].

should_store_data(PeerPid) ->
    Key = mykey,
    Value = "myvalue",

    peer:store({Key, Value}, PeerPid),
    RetrievalData = peer:find_value_of(Key, PeerPid),
    [?_assertEqual(Value, RetrievalData)].

should_overwrite_data_with_same_key(PeerPid) ->
    Key = mykey,
    FirstValue = "myvalue",
    SecondValue = "updated",

    peer:store({Key, FirstValue} , PeerPid),
    peer:store({Key, SecondValue}, PeerPid),
    RetrievalData = peer:find_value_of(Key, PeerPid),
    [?_assertEqual(SecondValue, RetrievalData)].
