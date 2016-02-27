-module(example_tsp).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').
-behaviour(digeno_callback).

%% digeno callback functions
-export([get_config/0,
         generate/0,
         evaluate/1,
         fitness/2,
         dead_on_arrival/2,
         mutate/1,
         combine/2,
         format/1,
         format_result/1]).

%% other exports
-export([city_name/1]).

%% This is a module defining a Genetic Optimisation problem.

%% The optimisation problem demonstrated in this module is the
%% well-known Travelling Salesman Problem (TSP). The input to TSP is a
%% distance (cost) matrix between a number of cities (edges). The goal
%% is to find a cycle in the graph that yields minimal distance (cost)
%% when used to traverse each city exactly once.

%% Instances are permutations of the order of cities, represented as a
%% list of integers.

%% We use the distance matrix between the following 21 Swedish cities
%% as our example problem:

city_name(1)  -> "Falun";
city_name(2)  -> "Gävle";
city_name(3)  -> "Göteborg";
city_name(4)  -> "Halmstad";
city_name(5)  -> "Härnösand";
city_name(6)  -> "Jönköping";
city_name(7)  -> "Kalmar";
city_name(8)  -> "Karlskrona";
city_name(9)  -> "Karlstad";
city_name(10) -> "Linköping";
city_name(11) -> "Luleå";
city_name(12) -> "Malmö";
city_name(13) -> "Nyköping";
city_name(14) -> "Stockholm";
city_name(15) -> "Umeå";
city_name(16) -> "Uppsala";
city_name(17) -> "Visby";
city_name(18) -> "Västerås";
city_name(19) -> "Växjö";
city_name(20) -> "Örebro";
city_name(21) -> "Östersund".

%% To make the problem more realistic, we store the complete distance
%% matrix (in kilometers) for the actual driving distance between any
%% two city, taking into account the existing public road network in
%% Sweden.

%% The matrix is represented by the function city_dist(A, B).

-define(NUM_CITIES, 21).

%% digeno callbacks

get_config() -> [{population_size, 1000},
                 {converg_detect, auto},
                 {display_decimator, 1000}].

generate() -> utils:permutation(lists:seq(1, ?NUM_CITIES)).

mutate(P) ->
    %% Swap two randomly chosen cities
    I1 = random:uniform(?NUM_CITIES),
    I2 = random:uniform(?NUM_CITIES),
    if I1 =:= I2 -> mutate(P);
       true ->
            City1 = lists:nth(I1, P),
            City2 = lists:nth(I2, P),
            P0 = utils:change_item(I1, City2, P),
            utils:change_item(I2, City1, P0)
    end.

combine(P1, P2) ->
    %% Choose a random point to cut P1.
    %% Take the items before the cut as they are;
    %% take the rest of the items but in the order they appear in P2.
    Idx = 1 + random:uniform(?NUM_CITIES-2),
    P1Rest = lists:nthtail(Idx, P1),
    lists:sublist(P1, Idx) ++ [I || I <- P2, lists:member(I, P1Rest)].

evaluate([FirstCity|_] = P) ->
    F = fun(City, {undefined, AccDist}) -> {City, AccDist};
           (City, {PrevCity, AccDist})  -> {City, city_dist(City, PrevCity) + AccDist}
        end,
    {LastCity, PathDistance} = lists:foldl(F, {undefined, 0}, P),
    PathDistance + city_dist(FirstCity, LastCity).

dead_on_arrival(_P, _Result) -> false.

fitness(_P, 0) -> infinity;
fitness(_P, Result) -> 1.0 / Result.

format(P) ->
    %% Since it's a cycle, it can start anywhere. So print it rotated
    %% to start with the same index.  This hides 'changes' found that
    %% are just rotations of the same permutation, and thus equivalent.
    string:join([integer_to_list(Pi) || Pi <- rotate(P)], " ").

format_result(Result) -> io_lib:format("~B", [Result]).

%% private functions

rotate(P) ->
    %% Rotate cycle to start with Stockholm
    Idx = length(lists:takewhile(fun(E) -> E /= 14 end, P)),
    rotate(P, Idx).

rotate(P, Idx) ->
    lists:nthtail(Idx, P) ++ lists:sublist(P, Idx).

%% Driving distance between major Swedish cities

city_dist(1,  2) ->  91;
city_dist(1,  3) -> 461;
city_dist(1,  4) -> 572;
city_dist(1,  5) -> 344;
city_dist(1,  6) -> 392;
city_dist(1,  7) -> 537;
city_dist(1,  8) -> 568;
city_dist(1,  9) -> 234;
city_dist(1, 10) -> 332;
city_dist(1, 11) -> 824;
city_dist(1, 12) -> 681;
city_dist(1, 13) -> 266;
city_dist(1, 14) -> 230;
city_dist(1, 15) -> 556;
city_dist(1, 16) -> 192;
city_dist(1, 17) -> 444;
city_dist(1, 18) -> 147;
city_dist(1, 19) -> 509;
city_dist(1, 20) -> 179;
city_dist(1, 21) -> 367;

city_dist(2,  3) -> 518;
city_dist(2,  4) -> 673;
city_dist(2,  5) -> 258;
city_dist(2,  6) -> 493;
city_dist(2,  7) -> 582;
city_dist(2,  8) -> 660;
city_dist(2,  9) -> 347;
city_dist(2, 10) -> 368;
city_dist(2, 11) -> 738;
city_dist(2, 12) -> 782;
city_dist(2, 13) -> 272;
city_dist(2, 14) -> 172;
city_dist(2, 15) -> 470;
city_dist(2, 16) -> 107;
city_dist(2, 17) -> 386;
city_dist(2, 18) -> 144;
city_dist(2, 19) -> 609;
city_dist(2, 20) -> 239;
city_dist(2, 21) -> 391;

city_dist(3,  4) -> 140;
city_dist(3,  5) -> 771;
city_dist(3,  6) -> 147;
city_dist(3,  7) -> 341;
city_dist(3,  8) -> 350;
city_dist(3,  9) -> 247;
city_dist(3, 10) -> 276;
city_dist(3, 11) -> 1251;
city_dist(3, 12) -> 274;
city_dist(3, 13) -> 371;
city_dist(3, 14) -> 472;
city_dist(3, 15) -> 983;
city_dist(3, 16) -> 453;
city_dist(3, 17) -> 450;
city_dist(3, 18) -> 377;
city_dist(3, 19) -> 234;
city_dist(3, 20) -> 282;
city_dist(3, 21) -> 781;

city_dist(4,  5) -> 924;
city_dist(4,  6) -> 182;
city_dist(4,  7) -> 243;
city_dist(4,  8) -> 213;
city_dist(4,  9) -> 384;
city_dist(4, 10) -> 308;
city_dist(4, 11) -> 1404;
city_dist(4, 12) -> 137;
city_dist(4, 13) -> 404;
city_dist(4, 14) -> 505;
city_dist(4, 15) -> 1136;
city_dist(4, 16) -> 569;
city_dist(4, 17) -> 384;
city_dist(4, 18) -> 488;
city_dist(4, 19) -> 132;
city_dist(4, 20) -> 393;
city_dist(4, 21) -> 869;

city_dist(5,  6) -> 745;
city_dist(5,  7) -> 834;
city_dist(5,  8) -> 912;
city_dist(5,  9) -> 599;
city_dist(5, 10) -> 620;
city_dist(5, 11) -> 481;
city_dist(5, 12) -> 1034;
city_dist(5, 13) -> 523;
city_dist(5, 14) -> 424;
city_dist(5, 15) -> 213;
city_dist(5, 16) -> 359;
city_dist(5, 17) -> 637;
city_dist(5, 18) -> 396;
city_dist(5, 19) -> 861;
city_dist(5, 20) -> 491;
city_dist(5, 21) -> 218;

city_dist(6,  7) -> 214;
city_dist(6,  8) -> 234;
city_dist(6,  9) -> 242;
city_dist(6, 10) -> 129;
city_dist(6, 11) -> 1225;
city_dist(6, 12) -> 291;
city_dist(6, 13) -> 224;
city_dist(6, 14) -> 325;
city_dist(6, 15) -> 956;
city_dist(6, 16) -> 390;
city_dist(6, 17) -> 303;
city_dist(6, 18) -> 308;
city_dist(6, 19) -> 119;
city_dist(6, 20) -> 213;
city_dist(6, 21) -> 729;

city_dist(7,  8) ->  84;
city_dist(7,  9) -> 444;
city_dist(7, 10) -> 238;
city_dist(7, 11) -> 1314;
city_dist(7, 12) -> 285;
city_dist(7, 13) -> 313;
city_dist(7, 14) -> 414;
city_dist(7, 15) -> 1046;
city_dist(7, 16) -> 479;
city_dist(7, 17) -> 201;
city_dist(7, 18) -> 401;
city_dist(7, 19) -> 109;
city_dist(7, 20) -> 359;
city_dist(7, 21) -> 967;

city_dist(8,  9) -> 465;
city_dist(8, 10) -> 304;
city_dist(8, 11) -> 1398;
city_dist(8, 12) -> 206;
city_dist(8, 13) -> 397;
city_dist(8, 14) -> 498;
city_dist(8, 15) -> 1129;
city_dist(8, 16) -> 563;
city_dist(8, 17) -> 284;
city_dist(8, 18) -> 483;
city_dist(8, 19) -> 105;
city_dist(8, 20) -> 389;
city_dist(8, 21) -> 1050;

city_dist(9, 10) -> 215;
city_dist(9, 11) -> 1079;
city_dist(9, 12) -> 517;
city_dist(9, 13) -> 243;
city_dist(9, 14) -> 309;
city_dist(9, 15) -> 811;
city_dist(9, 16) -> 281;
city_dist(9, 17) -> 475;
city_dist(9, 18) -> 205;
city_dist(9, 19) -> 362;
city_dist(9, 20) -> 110;
city_dist(9, 21) -> 542;

city_dist(10, 11) -> 1100;
city_dist(10, 12) -> 418;
city_dist(10, 13) -> 100;
city_dist(10, 14) -> 200;
city_dist(10, 15) -> 832;
city_dist(10, 16) -> 265;
city_dist(10, 17) -> 291;
city_dist(10, 18) -> 187;
city_dist(10, 19) -> 219;
city_dist(10, 20) -> 121;
city_dist(10, 21) -> 753;

city_dist(11, 12) -> 1513;
city_dist(11, 13) -> 1003;
city_dist(11, 14) -> 904;
city_dist(11, 15) -> 270;
city_dist(11, 16) -> 839;
city_dist(11, 17) -> 1114;
city_dist(11, 18) -> 877;
city_dist(11, 19) -> 1340;
city_dist(11, 20) -> 969;
city_dist(11, 21) -> 579;

city_dist(12, 13) -> 513;
city_dist(12, 14) -> 614;
city_dist(12, 15) -> 1245;
city_dist(12, 16) -> 678;
city_dist(12, 17) -> 453;
city_dist(12, 18) -> 597;
city_dist(12, 19) -> 201;
city_dist(12, 20) -> 502;
city_dist(12, 21) -> 1023;

city_dist(13, 14) -> 103;
city_dist(13, 15) -> 735;
city_dist(13, 16) -> 168;
city_dist(13, 17) -> 271;
city_dist(13, 18) -> 130;
city_dist(13, 19) -> 341;
city_dist(13, 20) -> 134;
city_dist(13, 21) -> 656;

city_dist(14, 15) -> 636;
city_dist(14, 16) ->  69;
city_dist(14, 17) -> 210;
city_dist(14, 18) -> 105;
city_dist(14, 19) -> 441;
city_dist(14, 20) -> 199;
city_dist(14, 21) -> 555;

city_dist(15, 16) -> 571;
city_dist(15, 17) -> 846;
city_dist(15, 18) -> 608;
city_dist(15, 19) -> 1072;
city_dist(15, 20) -> 704;
city_dist(15, 21) -> 363;

city_dist(16, 17) -> 283;
city_dist(16, 18) ->  77;
city_dist(16, 19) -> 506;
city_dist(16, 20) -> 174;
city_dist(16, 21) -> 490;

city_dist(17, 18) -> 319;
city_dist(17, 19) -> 254;
city_dist(17, 20) -> 366;
city_dist(17, 21) -> 769;

city_dist(18, 19) -> 425;
city_dist(18, 20) ->  95;
city_dist(18, 21) -> 528;

city_dist(19, 20) -> 330;
city_dist(19, 21) -> 850;

city_dist(20, 21) -> 546;

city_dist(A, B) when A > B -> city_dist(B, A).
