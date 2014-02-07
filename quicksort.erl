-module(quicksort).
-export([sort/1]).

-spec sort([term()]) -> [term()].
sort(L) ->
    lists:flatten(quicksort(L)).

-spec quicksort([term()]) -> [term()].
quicksort([]) -> [];
quicksort([Pivot|T]) ->
    {Left, Right} = lists:foldl(
        fun(Element, {Less, Greater}) ->
            case Element > Pivot of
                true ->
                    {Less, [Element|Greater]};
                _ ->
                    {[Element|Less], Greater}
            end
        end,
        {[],[]},
        T),
    [quicksort(Left), Pivot, quicksort(Right)].