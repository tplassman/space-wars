-module(war).

-import(lists, [seq/2, sort/1, split/2]).
-import(rand, [uniform/0]).

-export([start/0]).

-define(Limit, 1000).

start() ->
    Deck = build_deck(),
    ShuffledDeck = shuffle_deck(Deck),
    Midpoint = length(ShuffledDeck) div 2,
    {Rebels, Empires} = split(Midpoint, ShuffledDeck),
    battle(Rebels, Empires, [], 1).

build_deck() ->
    Ranks = lists:seq(2, 14),
    Suits = [clubs, diamonds, hearts, spades],
    [{Rank, Suit} || Rank <- Ranks , Suit <- Suits].

shuffle_deck(Deck) ->
    [Card || {_, Card} <- sort([{uniform(), Card} || Card <- Deck])].

battle(Rebels, _, _, N) when length(Rebels) == 0 ->
    io:fwrite("The Evil Empire won the war after ~p battles\n", [N]);
battle(_, Empires, _, N) when length(Empires) == 0 ->
    io:fwrite("The Rebel Scum won the war after ~p battles\n", [N]);
battle(_, _, _, N) when N > ?Limit ->
    io:fwrite("The war rages on after ~p battles\n", [?Limit]);
battle([RebelBattling | RebelsWatching], [EmpireBattling | EmpiresWatching], Spoils, N) ->
    {RebelBattlingRank, RebelBattlingSuit} = RebelBattling,
    {EmpireBattlingRank, EmpireBattlingSuit} = EmpireBattling,
    BattleSpoils = Spoils ++ [RebelBattling, EmpireBattling],

    if
        length(Spoils) == 0 -> io:fwrite("Battle #~p\n", [N]);
        length(Spoils) > 0 -> io:fwrite("War for spoils:\n~p\n", [Spoils])
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
        RebelBattlingRank == EmpireBattlingRank ->
            {RebelSpoils, RebelsLeft} = split(3, RebelsWatching),
            {EmpireSpoils, EmpiresLeft} = split(3, EmpiresWatching),
            io:fwrite("War begins at battle #~p\n", [N]),
            io:fwrite("================================\n"),
            battle(RebelsLeft, EmpiresLeft, BattleSpoils ++ RebelSpoils ++ EmpireSpoils, N)
    end.
