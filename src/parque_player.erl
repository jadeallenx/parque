-module(parque_player).

-behaviour(gen_server).

% public api
-export([
    start_link/3,
    state/1,
    move/2,
    buy/3,
    sell/3
]).

% required gen_server callbacks
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
    location = {0, 0},
    port,
    cash = 0,
    capacity = 0,
    inventory = orddict:new()
}).

% api
start_link(Player, Cash, Capacity) ->
    gen_server:start({local, Player}, ?MODULE, [Player, Cash, Capacity], []).

state(Who) ->
    gen_server:call(Who, {state}).

move(Who, Where) ->
    case whereis(Where) of
        undefined ->
            error_logger:error_msg("There is no port named ~p", [Where]);
        _Pid ->
            gen_server:call(Who, {move, Where})
    end.

buy(Who, What, Qty) ->
    gen_server:call(Who, {buy, What, Qty}).

sell(Who, What, Qty) ->
    gen_server:call(Who, {sell, What, Qty}).

%% gen_server callbacks
init([Player, Cash, Capacity]) ->
    {ok, #state{
        name = Player,
        cash = Cash,
        capacity = Capacity
    }}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({state}, _From, State) ->
    {reply, State, State};

handle_call({move, Where}, _From, State = #state{ port = undefined }) ->
    parque_port:arrive(Where, self()),
    {reply, ok, State#state{ port = Where }};

handle_call({move, Where}, _From, State = #state{ port = Port }) ->
    parque_port:leave(Port, self()),
    parque_port:arrive(Where, self()),
    {reply, ok, State#state{ port = Where }};

handle_call({buy, _What, _Qty}, _From, State = #state{ port = undefined }) ->
    {reply, {error, not_in_port}, State};

handle_call({buy, What, Qty}, _From, State = #state{ name = Name, port = Port, capacity = Capacity, cash = Cash, inventory = Inv }) ->
    {Reply, NewState} = case Qty > Capacity of
        true ->
            {{error, no_capacity}, State};
        false ->
            case parque_port:get_price(Port, What, Qty) of
                {error, Error} ->
                    {{error, Error}, State};
                {ok, Amount} when Amount >= Cash ->
                    {{error, not_enough_cash}, State};
                _ ->
                    case parque_port:buy(Port, Name, What, Qty) of
                        {ok, {Qty, Price}} ->
                            NewCash = Cash - (Qty*Price),
                            NewCapacity = Capacity - Qty,
                            NewInv = update_inventory(What, Qty, Inv),
                            {ok, State#state{ capacity = NewCapacity, cash = NewCash, inventory = NewInv}};
                        Err ->
                            {Err, State}
                    end
            end
    end,
    {reply, Reply, NewState};

handle_call({sell, _What, _Qty}, _From, State = #state{ port = undefined }) ->
    {reply, {error, not_in_port}, State};

handle_call({sell, What, Qty}, _From, State = #state{ name = Name, port = Port, capacity = Capacity, cash = Cash, inventory = Inv}) ->
    {Reply, NewState} = case check_inventory(What, Qty, Inv) of
        false ->
            {{error, {no_inventory, What}}, State};
        true ->
            case parque_port:sell(Port, Name, What, Qty) of
                {ok, {Qty, Price}} ->
                    NewCash = Cash + (Qty * Price),
                    NewCapacity = Capacity + Qty,
                    NewInv = update_inventory(What, (Qty * -1), Inv),
                    {ok, State#state{ capacity = NewCapacity, cash = NewCash, inventory = NewInv}};
                Err ->
                    {Err, State}
            end
    end,
    {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({arrived, Port, Who}, State) ->
    error_logger:info_msg("~p arrived in ~p", [Who, Port]),
    {noreply, State};

handle_cast({departed, Port, Who}, State) ->
    error_logger:info_msg("~p departed ~p", [Who, Port]),
    {noreply, State};

handle_cast({bought, Port, Who, What, Qty, Price, NewQty, NewPrice}, State) ->
    error_logger:info_msg("~p bought ~p units of ~p for ~p (~p total) in ~p. Now available: ~p for ~p", [Who, Qty, What, Price, (Qty*Price), Port, NewQty, NewPrice]),
    {noreply, State};

handle_cast({sold, Port, Who, What, Qty, Price, NewQty, NewPrice}, State) ->
    error_logger:info_msg("~p sold ~p units of ~p for ~p (~p total) at ~p. Now available: ~p for ~p", [Who, Qty, What, Price, (Qty*Price), Port, NewQty, NewPrice]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private functions
update_inventory(What, Qty, Inv) ->
    case orddict:find(What, Inv) of
        error ->
            orddict:store(What, Qty, Inv);
        {ok, Current} ->
            orddict:store(What, Current+Qty, Inv)
    end.

check_inventory(What, Qty, Inv) ->
    case orddict:find(What, Inv) of
        error -> false;
        {ok, Current} -> Current >= Qty
    end.
