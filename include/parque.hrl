%% parque header file

-record(warehouse, {
    location,
    size,
    goods = []
    }).

-record(port, {
    name,
    buy_affinity,
    sell_affinity,
    goods = [],
    warehouses = [],
    has_shipyard,
    corruption,
    crime
    }).

-record(ship, {
    boat,
    location,
    size,
    condition,
    guns,
    cargo = []
    }).

-record(good, {
    name,
    contraband
    }).

-record(player, {
    name,
    money,
    ships = []
    }).

