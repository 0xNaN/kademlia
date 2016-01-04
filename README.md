An Erlang implementation of kademlia
=====

To run all tests and download all dependecies:
-----

    $ rebar3 eunit --module=peer,kbucket,it

Warning: some tests may fail since there are some `sleep()` to wait until some procedures end. This is a known issue and will be addressed in future developments, try to run several times all tests.

Setting up a little network
-----
    $ rebar3 as test shell
    
    1> K = 20. % Kademlia K parameter
    20
    2> Alpha = 3. % Kademlia Alpha parameter
    3
    3> A = peer:start(123, K, Alpha).
    {<0.193.0>,123}
    4> B = peer:start(100, K, Alpha).
    {<0.199.0>,100}
    5> C = peer:start(50, K, Alpha).
    {<0.204.0>,50}
    6>
    6> peer:join(A, B).
    ....
    7> peer:join(B, C).
    ....
    8> peer:iterative_store(A, {mykey, "a value string"}).
    ....
    ok.
    9> Value = peer:iterative_find_value(C, mykey).
    ....
    10> Value.
    "a value string".
    11> peer:iterative_store(B, {mylist, [1, 2, 3]}).
    ....
    ok.
    12> List = peer:iterative_find_value(A, mylist).
    ....
    11> List.
    [1, 2, 3]

For each message will be created a log.
By default the logs are displayed on the console, to modify this behaviour check out `sys.config` file and comment
out what you do not want:

    [
      {'kademlia', []},
      {lager, [
        {handlers, [
            {lager_logstash_backend, [
                   {level, info},
                   {output, {tcp, "localhost", 9125}},
                   {format, json},
                   {json_encoder, jsx}
            ]},
            {lager_console_backend, error},
            {lager_console_backend, info}
    %%        {lager_file_backend, [{file, "log/error.log"}, {level, error}]},
    %%        {lager_file_backend, [{file, "log/consoles.log"}, {level, info}]}
        ]}
       ]}
    ].
All the logs are sent to the local port 9125, waiting to be consumed by a logstash daemon.
The project provides [vagrant-elk-box](https://github.com/comperiosearch/vagrant-elk-box), an already configured vagrant-box to display the logs through an ELK stack.
