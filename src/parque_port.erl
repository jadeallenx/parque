-module(parque_port).

-behaviour(gen_server).

-export([
    start_link/2,
    state/1,
    arrive/2,
    leave/2,
    buy/4,
    sell/4,
    list/1
]).

% gen_server callbacks
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-record(state, {
    name,
    location = {0,0},
    goods = orddict:new(),
    traders = [],
    has_shipyard,
    crime,
    corruption,
    buy_affinity,
    sell_affinity
}).

%% Public API

start_link(Port, Goods) ->
    gen_server:start({local, Port}, ?MODULE, [Port, Goods], []).

state(Port) ->
    gen_server:call(Port, {state}).

arrive(Port, Who) ->
    gen_server:call(Port, {arrive, Who}).

leave(Port, Who) ->
    gen_server:call(Port, {leave, Who}).

buy(Port, Who, What, Qty) ->
    gen_server:call(Port, {buy, Who, What, Qty}).

sell(Port, Who, What, Qty) ->
    gen_server:call(Port, {sell, Who, What, Qty}).

list(Port) ->
    gen_server:call(Port, {list}).

%% gen_server callbacks
init([Port, Goods]) ->
    {ok, #state{
        name = Port,
        goods = Goods
    }}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({state}, _From, State) ->
    {reply, State, State};

handle_call({arrive, Who}, _From, State = #state{name = Name, traders = Traders}) ->
    NewTraders = case lists:member(Who, Traders) of
        true ->
            error_logger:error_msg("~p is already at ~p!", [Who, Name]),
            Traders;
        false ->
            error_logger:info_msg("~p arrived at ~p.", [Who, Name]),
            notify(Traders, {arrived, Name, Who}),
            [Who | Traders]
    end,
    {reply, ok, State#state{traders = NewTraders}};

handle_call({leave, Who}, _From, State = #state{name = Name, traders = Traders}) ->
    NewTraders = case lists:member(Who, Traders) of
        false ->
            error_logger:error_msg("~p isn't at ~p!", [Who, Name]),
            Traders;
        true ->
            error_logger:info_msg("~p left ~p.", [Who, Name]),
            notify(Traders, {departed, Name, Who}),
            lists:delete(Who, Traders)
    end,
    {reply, ok, State#state{traders = NewTraders}};

handle_call({buy, Who, What, Qty}, _From, State = #state{ name = Name, goods = Goods, traders = Traders }) ->
    {Return, NewState} = 
        case orddict:find(What, Goods) of
            {ok, {QtyAvailable, Price}} when QtyAvailable >= Qty -> 
                case parque_player:afford(Who, (Qty*Price)) of
                    true ->
                        NewQty = QtyAvailable - Qty,
                        NewPrice = set_new_price(NewQty, QtyAvailable, Price),
                        notify(Traders, {bought, Name, Who, What, Qty, Price, NewQty, NewPrice}),
                        error_logger:info_msg("~p bought ~p ~p at ~p per unit (~p total).", [Who, Qty, What, Price, (Qty*Price)]),
                        {{ok, {Qty, Price}}, 
                            State#state{ goods = orddict:store(What, {NewQty, NewPrice}, Goods) }};
                    false ->
                        error_logger:error_msg("~p tried to buy ~p ~p at ~p per unit (~p total) but didn't have enough cash.", [Who, Qty, What, Price, (Qty*Price)]),
                        {{error, not_enough_cash}, State}
                end;
            {ok, {QtyAvailable, Price}} -> 
                error_logger:error_msg("~p tried to buy ~p ~p at ~p per unit (~p total) but there's only ~p units of ~p available.", [Who, Qty, What, Price, (Qty*Price), QtyAvailable, What]),
                {{error, {not_enough_qty, QtyAvailable}}, State};
            error -> 
                error_logger:error_msg("~p tried to buy ~p ~p but there's no such good as ~p.", [Who, Qty, What, What]),
                {{error, {no_such_good, What}}, State}
        end,
    {reply, Return, NewState};

handle_call({sell, Who, What, Qty}, _From, State = #state{ goods = Goods, traders = Traders, name = Name }) ->
    {Return, NewState} = 
        case orddict:find(What, Goods) of
            {ok, {QtyAvailable, Price}} -> 
                NewQty = QtyAvailable + Qty,
                NewPrice = set_new_price(NewQty, QtyAvailable, Price),
                notify(Traders, {sold, Name, Who, What, Qty, Price, NewQty, NewPrice}),
                error_logger:info_msg("~p sold ~p ~p at ~p per unit (~p total).", [Who, Qty, What, Price, (Qty*Price)]),
                {{ok, {Qty, Price}}, State#state{ goods = orddict:store(What, {NewQty, NewPrice}, Goods) }};
            error -> 
                error_logger:error_msg("~p tried to sell ~p ~p but there's no such good as ~p at ~p.", [Who, Qty, What, What, Name]),
                {{error, {no_such_good, What}}, State}
        end,
    {reply, Return, NewState};
 
handle_call({list}, _From, State = #state{ goods = Goods }) ->
    {reply, orddict:to_list(Goods), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private functions
notify(Whom, Msg) ->
    lists:foreach(fun(Pid) -> gen_server:cast(Pid, Msg) end, Whom).

set_new_price(0, _OldQty, OldPrice) ->
    OldPrice * 2;
set_new_price(NewQty, 0, OldPrice) ->
    %%% XXX FIXME: This random price change might be a littttle crazy.
    OldPrice + random:uniform(NewQty);
set_new_price(NewQty, OldQty, OldPrice) ->
    Change = 1 + ( ( OldQty - NewQty ) / OldQty ),
    round(OldPrice * Change).
