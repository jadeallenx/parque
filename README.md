parque
======

This is a trading game engine written in Erlang as an educational tool, to help introduce
a concrete implementation of standard Erlang idioms and concepts.

Prerequisites
-------------
You will need the following to play around with this project:

* Erlang (See [Erlang Solutions](https://www.erlang-solutions.com/downloads/download-erlang-otp) for Mac OS X, Windows and most popular Linux binaries)
* GNU make

Compiling
---------
    $ git clone https://github.com/mrallen1/parque
    $ cd parque
    $ make all

Start REPL
----------
    $ erl -pa ebin -s parque

Create a player process
-----------------------
    1> {ok, PlayerPid} = parque:create_player('SomeName').

Create ports
------------
    2> Ports = ['Havana', 'Cozumel', 'Kingston'].
    3> [ parque:create_port(X) || X <- Ports ].

List goods at ports
-------------------
To get a listing of the goods, quantities and prices available at each port, use this:

    4> [ parque_port:list(X) || X <- Ports ].

The output is formatted in a list of tuples of `[{GoodName, {QuantityAvailable, CurrentPrice}}]`
