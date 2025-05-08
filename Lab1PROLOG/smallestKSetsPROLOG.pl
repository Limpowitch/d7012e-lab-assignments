
% Our core compound term to pass around all core information
% sum_slice(Sum, Slice, Start, End) 

contiguous_sublist_indexed(List, Slice, Start, End) :-
    append(Prefix, Tail, List),
    length(Prefix, PreLen),
    Start is PreLen + 1,
    append(Slice, _, Tail),
    length(Slice, Len),
    End is PreLen + Len.

all_sublists_indexed(List, Triplets) :-
    findall(
        slice_info(Slice, Start, End),
        (   
            contiguous_sublist_indexed(List, Slice, Start, End),
            Slice \= []
        ),
        Triplets
    ).    

all_subset_sums(SliceInfos, SumSlices) :-
    findall(
        sum_slice(Sum, Slice, Start, End),
        (member(slice_info(Slice, Start, End), SliceInfos),
            sumlist(Slice, Sum)
        ),
        SumSlices
    ).

sort_slices_by_sum(SumSlices, Sorted) :-
    maplist(
        [sum_slice(Sum, Slice, Start, End), Sum-sum_slice(Sum, Slice, Start, End)]>>true,
        SumSlices,
        Pairs
    ),
    keysort(Pairs,SortedPairs),
    pairs_values(SortedPairs, Sorted).

print_rows([]).
print_rows([sum_slice(Sum, Slice, Start, End) | Rest]) :-
    format('~w ~w ~w ~w~n',
        [Sum, Start, End, Slice]),
    print_rows(Rest).

take(0, _List, []) :- !.
take(K, [X|Xs], [X|Ys]) :-
    K > 0,                
    K1 is K - 1,          
    take(K1, Xs, Ys).     
take(_K, [], []).        

main_calculculations(List, Result) :-
    all_sublists_indexed(List, IndexedSublists),
    all_subset_sums(IndexedSublists, SumSlices),
    sort_slices_by_sum(SumSlices, Result).

main_print(List, K) :-
    main_calculculations(List, All),        
    take(K, All, TopK),
    writeln('size i j sublist'),
    print_rows(TopK).