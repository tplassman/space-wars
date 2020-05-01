-module(war).

-import(lists, [seq/2, split/2]).

-export([start/0]).

-define(Limit, 100).

start() ->
    Deck = build_deck(),
    ShuffledDeck = shuffle_deck(Deck),
    Midpoint = length(ShuffledDeck) div 2,
    {Rebels, Empires} = split(Midpoint, ShuffledDeck),
    battle(Rebels, Empires, [], 1).

build_deck() ->
    Ranks = lists:seq(1, 13),
    Suits = [clubs, diamonds, hearts, spades],
    Cards = [{Rank, Suit} || Rank <- Ranks , Suit <- Suits],
    Cards.

shuffle_deck(Deck) ->
    Cards = [S || {_, S} <- lists:sort([{rand:uniform(), Card} || Card <- Deck])],
    Cards.

battle(Rebels, _, _, _) when length(Rebels) == 0 ->
    io:fwrite("The Evil Empire won the war\n");
battle(_, Empires, _, _) when length(Empires) == 0 ->
    io:fwrite("The Rebel Scum won the war\n");
battle(_, _, _, N) when N > ?Limit ->
    io:fwrite("The war rages on after ~p battles\n", [?Limit]);
battle(Rebels, Empires, Spoils, N) ->
    [RebelBattling | RebelsWatching] = Rebels,
    {RebelBattlingRank, RebelBattlingSuit} = RebelBattling,
    [EmpireBattling | EmpiresWatching] = Empires,
    {EmpireBattlingRank, EmpireBattlingSuit} = EmpireBattling,
    BattleSpoils = Spoils ++ [RebelBattling, EmpireBattling],

    if
        length(Spoils) == 0 ->
            io:fwrite("Battle #~p\n", [N]);
        length(Spoils) > 0 ->
            io:fwrite("War for spoils:\n~p\n", [Spoils])
    end,

    io:fwrite("The Rebels throw ~p of ~p\n", [RebelBattlingRank, RebelBattlingSuit]),
    io:fwrite("The Empire throw ~p of ~p\n", [EmpireBattlingRank, EmpireBattlingSuit]),

    if
        RebelBattlingRank > EmpireBattlingRank ->
            io:fwrite("The Rebel Scum won battle #~p\n", [N]),
            io:fwrite("Rebel Troops: ~p\n", [length(RebelsWatching ++ BattleSpoils)]),
            io:fwrite("Empire Troops: ~p\n", [length(EmpiresWatching)]),
            io:fwrite("================================\n"),
            battle(RebelsWatching ++ BattleSpoils, EmpiresWatching, [], N + 1);
        RebelBattlingRank < EmpireBattlingRank ->
            io:fwrite("The Evil Empire won battle #~p\n", [N]),
            io:fwrite("Rebel Troops: ~p\n", [length(RebelsWatching)]),
            io:fwrite("Empire Troops: ~p\n", [length(EmpiresWatching ++ BattleSpoils)]),
            io:fwrite("================================\n"),
            battle(RebelsWatching, EmpiresWatching ++ BattleSpoils, [], N + 1);
        length(RebelsWatching) < 4 ->
            io:fwrite("The Rebels cannot sustain War.\n"),
            battle([], EmpiresWatching ++ RebelsWatching, [], N);
        length(EmpiresWatching) < 4 ->
            io:fwrite("The Empire cannot sustain War.\n"),
            battle(RebelsWatching ++ EmpiresWatching, [], [], N);
        EmpireBattlingRank == EmpireBattlingRank ->
            {RebelSpoils, RebelsLeft} = split(3, RebelsWatching),
            {EmpireSpoils, EmpiresLeft} = split(3, EmpiresWatching),
            WarSpoils = BattleSpoils ++ RebelSpoils ++ EmpireSpoils,
            io:fwrite("War begins at battle #~p\n", [N]),
            io:fwrite("================================\n"),
            battle(RebelsLeft, EmpiresLeft, WarSpoils, N)
    end.
