%%%Authur: Sharkz Wang
%%%E-mail: sharkz.wang@gmail.com
%%%Program: Knight's Tour
%%%Brief: Implementation of knight's tour problem, 
%%%       simply for practicing Erlang.
%%%Built Date: 2013/08/23
%%%Version: 1
%%%
%%%Exports: solve({point, X, Y}, Size)
%%%         returns a list of traces of the knight's each move
%%%         starting from the given point X, Y.

-module(knight_tour).
-export([solve/2]).

%%Brief: returns a list of traces of the knight's each move
%%       starting from the given point X, Y.
%%Parameter: 
%%       {point, X, Y}: the point where the knight starts.
%%       Size: the width of the board on which the knight move.
solve({point, X, Y}, Size) when is_integer(Size), Size > 0,
        is_integer(X), 0 =< X, X < Size, is_integer(Y), 0 =< Y, Y < Size ->
    VisitBoard = array:new(Size, {default, array:new(Size, {default, false})}),
    tour({point, X, Y}, VisitBoard, [], Size).
    
solve({none}, _, Acc, _) ->
    lists:reverse(Acc);   
solve({point, X, Y}, VisitBoard, Acc, Size) when is_integer(Size), Size > 0,
       is_integer(X), 0 =< X, X < Size, is_integer(Y), 0 =< Y, Y < Size ->
    NextMove = next_move({point, X, Y}, VisitBoard, Size),
    solve(NextMove, set_elem(X, Y, true, VisitBoard), [{point, X, Y}|Acc], Size).

%%Brief: returns which point the knight should go. {none} if no further moves can be done.
%%Algorithm: an greedy-based approach, which will alway choose the point with the highest weight
%%           among all the possible move. the weight of the given point X, Y is determined by
%%           how many possible moves can the knight make from the point X, Y. additionally,
%%           because the knight cannot visit the same point twice, the possible move of X, Y will be
%%           (all adjacent points to point X, Y on the board) ^ (all points on the board not visited yet).
next_move({point, X, Y}, VisitBoard, Size) when is_integer(Size), Size > 0,
       is_integer(X), 0 =< X, X < Size, is_integer(Y), 0 =< Y, Y < Size ->
    PossibMove = [[get_weight({point, P, Q}, VisitBoard, Size), {point, P, Q}] || 
            {point, P, Q} <- get_possible_move({point, X, Y}, VisitBoard, Size)],
    case PossibMove of
        [] -> {none};
        _  -> [_, NextMove] = lists:min(PossibMove),
              NextMove
    end.

%%Brief: simply wrapping a one-dimensional array getter into a 2d one.
get_elem(Row, Col, Array) when is_integer(Row), is_integer(Col) ->
    array:get(Col, array:get(Row, Array)).

%%Brief: simply wrapping a one-dimensional array setter into a 2d one.
set_elem(Row, Col, Elem, Array) when is_integer(Row), is_integer(Col) ->
    array:set(Row, array:set(Col, Elem, array:get(Row, Array)), Array).

%%Brief: returns a list of points, among all the 9 adjacent points to point X, Y, 
%%       whose coordinates fall into the boundary of the board.
%%Parameter: 
%%       {point, X, Y}: the point where the knight starts.
%%       Size: the width of the board on which the knight move.
get_neighbor({point, X, Y}, Size) when is_integer(Size), Size > 0,
        is_integer(X), 0 =< X, X < Size, is_integer(Y), 0 =< Y, Y < Size ->
    Candidate = [{point, X-2, Y-1}, {point, X-2, Y+1}, {point, X-1, Y+2}, {point, X+1, Y+2}, {point, X+2, Y+1},
            {point, X+2, Y-1}, {point, X+1, Y-2}, {point, X-1, Y-2}],
    [{point, P, Q} || {point, P, Q} <- Candidate, is_valid({point, P, Q}, Size)].

%%Brief: returns a list of points, among all the 9 adjacent points to the given point X, Y, 
%%       whose coordinates fall into the boundary of the board, and who haven't been visited yet.
%%Parameter: 
%%       {point, X, Y}: the point where the knight starts.
%%       VisitBoard: a 2d array having same size as the board of kight's tour.
%%                   value of each element is boolean storing whether knight visited this point.
%%       Size: the width of the board on which the knight move.
get_possible_move({point, X, Y}, VisitBoard, Size) when is_integer(Size), Size > 0,
        is_integer(X), 0 =< X, X < Size, is_integer(Y), 0 =< Y, Y < Size ->
    [{point, P, Q} || {point, P, Q} <- get_neighbor({point, X, Y}, Size), get_elem(P, Q, VisitBoard) =:= false].

%%Brief: returns boolean of whether the given point X, Y fall into the boundary of the board.
%%Parameter: 
%%       {point, X, Y}: the point where the knight starts.
%%       Size: the width of the board on which the knight move.
is_valid({point, X, Y}, Size) when is_integer(Size), Size > 0 ->
        is_integer(X) andalso 0 =< X andalso X < Size andalso 
                is_integer(Y) andalso 0 =< Y andalso Y < Size.

%%Brief: returns how many points can the knight go from the given point X, Y, i.e., 
%%       the weight of the point.
%%Parameter: 
%%       {point, X, Y}: the point where the knight starts.
%%       VisitBoard: a 2d array having same size as the board of kight's tour.
%%                   value of each element is boolean storing whether knight visited this point.
%%       Size: the width of the board on which the knight move.
get_weight({point, X, Y}, VisitBoard, Size) when is_integer(Size), Size > 0,
       is_integer(X), 0 =< X, X < Size, is_integer(Y), 0 =< Y, Y < Size ->
    length(get_possible_move({point, X, Y}, VisitBoard, Size)).
