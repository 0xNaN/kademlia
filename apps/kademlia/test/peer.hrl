-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, F}).

start() ->
    Id = 1,
    PeerPid = peer:start(Id),
    PeerPid.

peer_suite_test_() ->
    ?setup(fun should_store_data/1).

should_store_data(PeerPid) ->
    Key = mykey,
    Value = "myvalue",

    peer:store({Key, Value}, PeerPid),
    RetrievalData = peer:find_value_of(Key, PeerPid),
    [?_assertEqual(Value, RetrievalData)].
