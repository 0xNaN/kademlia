-module(peer).
-export([start/1,
         peer/2,
         store/2,
         find_value_of/2]).

start(Id) ->
    spawn(fun() ->
              peer(Id, #{})
          end).

store({Key, Value}, PeerPid) ->
    PeerPid ! {store, self(), {Key, Value}},
    ok.

find_value_of(Key, PeerPid) ->
    PeerPid ! {find_value, self(), Key},
    receive
        {PeerPid, Value} -> Value
    end.

peer(Id, Map) ->
    receive
        {store, _, {Key, Value}} ->
            NewMap = Map#{Key => Value},
            peer(Id, NewMap);

        {find_value, FromPeer, Key} ->
            #{Key := Value} = Map,
            FromPeer ! {self(), Value},
            peer(Id, Map);

        _ -> peer(Id, Map)
    end.

-ifdef(TEST).
-include_lib("../test/peer.hrl").
-endif.
