-module(parque_port_sup).

-behaviour(supervisor).

-export([
    start_link/0, 
    init/1,
    start_port/2
]).

start_port(Name, Goods) ->
    supervisor:start_child(?MODULE, [Name, Goods]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 3, 60},
         [{?MODULE,
           {parque_port, start_link, []},
           permanent, 2000, worker, [parque_port]}
         ]}}.
