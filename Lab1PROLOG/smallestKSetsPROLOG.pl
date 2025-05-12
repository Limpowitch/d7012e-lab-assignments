contiguous_sublist_indexed(List, Slice, Start, End) :-  % Example, [1,2,3] (first instance, no rollback)
    append(Prefix, Tail, List),                         % Prefix = [], Tail = [1,2,3]
    length(Prefix, PreLen),
    Start is PreLen + 1,
    append(Slice, _, Tail),                             % Append _ to Slice. Slice = [], Tail = [1,2,3]. Rollback applies here
    length(Slice, Len),
    End is PreLen + Len.

all_sublists_indexed(List, Triplets) :-
    findall(
        slice_info(Slice, Start, End),
        (   
            contiguous_sublist_indexed(List, Slice, Start, End),
            Slice \= []                                 % We make sure to remove empty sublists
        ),
        Triplets
    ).    

all_subset_sums(IndexedSublists, SumSlices) :-
    findall(
        sum_slice(Sum, Slice, Start, End),
        (
            member(slice_info(Slice, Start, End), IndexedSublists), 
            sumlist(Slice, Sum)                         % We check if member, if yes, we sum all elements in the Slice
        ),
        SumSlices                                       % We return SumSlices at the end
    ).

sort_slices_by_sum(SumSlices, Sorted) :-
    maplist(
      [sum_slice(Sum, Slice, Start, End), Pair]>>(      % We convert the sumSlice to a key-value pair
        Pair = Sum - sum_slice(Sum, Slice, Start, End)
      ),
      SumSlices,
      Pairs                                             % Returns all pairs
    ),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).                  % We first sort by key-values (Sum), then drop the key and return the value

print_rows([]).
print_rows([sum_slice(Sum, Slice, Start, End) | Rest]) :-
    format('~w ~w ~w ~w~n',
        [Sum, Start, End, Slice]),
    print_rows(Rest).

take(0, _List, []) :- !.
take(K, List, Prefix) :-                              
    length(Prefix, K),        
    append(Prefix, _, List).  

main_calculculations(List, Result) :-
    all_sublists_indexed(List, IndexedSublists),
    all_subset_sums(IndexedSublists, SumSlices),
    sort_slices_by_sum(SumSlices, Result).

main_print(List, K) :-
    main_calculculations(List, All),        
    take(K, All, TopK),                                 % Makes sure we only take the first K sorted elements
    writeln('size i j sublist'),
    print_rows(TopK).