# Clustering-TS-subsequences

Considering a TS dataset, the TS subsequence clustering aim to group the similar patterns of the several TS samples. Because the TS dataset has multiple TS samples,
a pattern can be associated with multiple other co-occurring patterns. This project shows clustering based on subsequently co-occurring patterns in TS dataset.

We formulate the subsequence clustering on TS dataset, as a dense subgraph mining problem for a weighted directed graph. The weighted directed graph can capture dependencies
between the subsequences using directed edges and the similarity between the subsequences using the edge weights. The dense sub-graphs formed from the subsequently co-occurring
patterns from the weighted directed graph is defined using the concept of quasi-clique.

For atypical pattern identification, a subsequence that is followed by minimum number of other unique patterns is considered atypical.

This project can help the utility providers in balancing the demand with the supply under generation uncertainty caused due to renewable energy integration in the grid.
The stable buildings identified based on regularity measure, are likely to participate in DR programs through long term contractual agreement. Introducing promotional offers to the 
stable consumer set who can convince their neighbors towards energy management will further help in load reduction.

 More information about the method is given in https://www.sciencedirect.com/science/article/abs/pii/S0306261922000599
