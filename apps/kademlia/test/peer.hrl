-include_lib("eunit/include/eunit.hrl").


should_store_data_test() ->
    Id = 1,
    Key = mykey,
    Value = "myvalue",

    PeerPid = peer:start(Id),
    peer:store({Key, Value}, PeerPid),
    RetrievalData = peer:find_value_of(Key, PeerPid),
    ?assertEqual(Value, RetrievalData).
