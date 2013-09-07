%%%Authur: Sharkz Wang
%%%E-mail: sharkz.wang@gmail.com
%%%Program: Knight's Tour
%%%Brief: Implementation of knight's tour problem, 
%%%       simply for practicing Erlang.
%%%Built Date: 2013/08/23
%%%Last Revised Date: 2013/09/07
%%%Version: 2
%%%
%%%Exports: solve({point, X, Y}, Size)
%%%         Brief: solve the knight's tour problem on board with the specified size.
%%%         Return: returns a list consisting of the traces of knight's each move.
%%%         Parameter:
%%%             {point, X, Y}: the starting point X, Y.
%%%             Size: the size of the board on which the knight tours.

-module(knight_tour).
-export([solve/2]).

-vsn(2).

-define(is_valid_size(Size), is_integer(Size) andalso Size > 0).
-define(is_valid_point(X, Y, Size), 
        is_integer(X) andalso 0 =< X andalso X < Size andalso
        is_integer(Y) andalso 0 =< Y andalso Y < Size).

%%%Brief: solve knight's tour problem on board with the specified size.
%%%Return: a list consisting of the traces of knight's each move.
%%%Parameter:
%%%    {point, X, Y}: the starting point X, Y.
%%%    Size: the size of the board on which the knight tours.
solve({point, X, Y} = Pt, Size) 
        when ?is_valid_size(Size), ?is_valid_point(X, Y, Size) ->
    VisitBoard = array:new(Size, {default, array:new(Size, {default, false})}),
    solve(Pt, VisitBoard, [], Size).
    
solve(none, _, Acc, _) ->
    lists:reverse(Acc);   
solve({point, X, Y} = Pt, VisitBoard, Acc, Size)
        when ?is_valid_size(Size), ?is_valid_point(X, Y, Size) ->
    NextMove = next_move(Pt, VisitBoard, Size),
    solve(NextMove, set_elem(X, Y, true, VisitBoard), [Pt|Acc], Size).

%%Brief: determines the most efficient move of the given point X, Y.
%%Return: point that the knight should go, or atom 'none' if no further moves can be done.
%%Algorithm: an greedy-based approach, which will alway choose the point with the highest weight
%%           among all the possible moves. the weight of the given point X, Y is determined by
%%           how many possible moves can the knight make from the point X, Y. additionally,
%%           because the knight cannot visit the same point twice, the possible move of X, Y will be
%%           (all adjacent points to point X, Y on the board) ^ (all points on the board not visited yet).
next_move({point, X, Y} = Pt, VisitBoard, Size)
        when ?is_valid_size(Size), ?is_valid_point(X, Y, Size) ->
    PossibMove = [{R, get_weight(R, VisitBoard, Size)} ||
            R <- get_possible_move(Pt, VisitBoard, Size)],
    case PossibMove of
        [] -> none;
        _ -> {NextMove, _Weight} = min_elem(
                    fun({_, WeightX}, {_, WeightY}) -> WeightX =< WeightY end, PossibMove),
             NextMove
    end.

%%Brief: find the minimun element of the given list.
%%Return: minimun element of the given list.
%%Parameter:
%%        F: ordering function F.
min_elem(F, [H|T]) ->
    min_elem(F, T, H).

min_elem(_, [], Min) -> Min;
min_elem(F, [H|T], Min) -> 
    case F(H, Min) of
        true -> min_elem(F, T, H);
        false -> min_elem(F, T, Min)
    end.

%%Brief: simply wrapping a one-dimensional array getter into a 2d one.
get_elem(Row, Col, Array) when is_integer(Row), is_integer(Col) ->
    array:get(Col, array:get(Row, Array)).

%%Brief: simply wrapping a one-dimensional array setter into a 2d one.
set_elem(Row, Col, Elem, Array) when is_integer(Row), is_integer(Col) ->
    array:set(Row, array:set(Col, Elem, array:get(Row, Array)), Array).

%%Brief: get all adjacent point of the given point X, Y.
%%Return: a list of points, among all the 9 adjacent points to point X, Y, 
%%        whose coordinates fall into the boundary of the board.
get_neighbor({point, X, Y}, Size) 
        when ?is_valid_size(Size), ?is_valid_point(X, Y, Size) ->
    Candidate = [{point, X-2, Y-1}, {point, X-2, Y+1}, {point, X-1, Y+2}, {point, X+1, Y+2}, {point, X+2, Y+1},
            {point, X+2, Y-1}, {point, X+1, Y-2}, {point, X-1, Y-2}],
    [R || {point, _P, _Q} = R <- Candidate, is_valid(R, Size)].

%%Brief: get all available moves from the given point X, Y.
%%Return: a list of points, among all the 9 adjacent points to the given point X, Y, 
%%        whose coordinates fall into the boundary of the board, and who haven't been visited yet.
%%Parameter: 
%%       VisitBoard: a 2d array having the same size as the board on which the knight moves.
%%                   value of each element is boolean whose value is determined by
%%                   whether knight has visited this point.
get_possible_move({point, X, Y} = Pt, VisitBoard, Size) 
        when ?is_valid_size(Size), ?is_valid_point(X, Y, Size) ->
    [R || {point, P, Q} = R <- get_neighbor(Pt, Size), get_elem(P, Q, VisitBoard) =:= false].

%%Brief: test if the given point X, Y has a valid coordinate on the board.
%%Return: boolean of whether the given point X, Y fall into the boundary of the board.
is_valid({point, X, Y}, Size) when ?is_valid_size(Size) ->
    ?is_valid_point(X, Y, Size).

%%Brief: carry out the weight of the given point.
%%Return: how many points can the knight go from the given point X, Y, i.e., 
%%        the weight of the point.
get_weight({point, X, Y} = Pt, VisitBoard, Size) 
        when ?is_valid_size(Size), ?is_valid_point(X, Y, Size) ->
    length(get_possible_move(Pt, VisitBoard, Size)).
