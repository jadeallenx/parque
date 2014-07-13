-module(parque).

-export([
    create_player/1,
    create_port/1
]).

create_player(Name) ->
    parque_sup:start_player(Name, make_value(100), make_value(100, 10)).

create_port(Name) ->
    parque_sup:start_port(Name, select_goods(make_value(5))).
    
% private functions
make_value(N) ->
    random:uniform(N).

make_value(N, R) ->
    V = random:uniform(N),
    case V rem R of
        0 -> V;
        L -> V + (R - L)
    end.

select_goods(N) ->
    AllGoods = get_goods(),

    lists:foldl(
        fun({Name, MinP, MaxP, MaxQ}, Acc) ->
            orddict:store(Name, {make_price(MinP, MaxP), make_value(MaxQ)}, Acc)
        end,
        orddict:new(),
        [ lists:nth(AllGoods, V) || V <- 
            [ make_value(length(AllGoods) + 1) || _ <- lists:seq(1,N) ]
        ]
    ).
            
make_price(Min, Max) ->
    case make_value(Max) of 
        V when V < Min ->
            Min + V;
        V -> V
    end.

get_goods() ->
    case file:consult("priv/goods.config") of
        {error, Error} ->
            error_logger:error_msg("ERROR: ~p", [Error]),
            [];
        {ok, [Goods]} ->
            Goods
    end.
    
