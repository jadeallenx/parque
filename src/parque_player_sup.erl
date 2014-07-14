-module(parque_player_sup).

-behaviour(supervisor).

-export([
    start_link/0, 
    init/1,
    start_player/3
]).

start_player(Name, Cash, Capacity) ->
    supervisor:start_child(?MODULE, [Name, Cash, Capacity]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 3, 60},
         [{?MODULE,
           {parque_player, start_link, []},
           permanent, 1000, worker, [parque_player]}
         ]}}.
