# Find Max Pairing In Bipartite Graph

A library used for finding a maximal pairing in a bipartite graph. The Pairing algorithm is only supported for String vertices and Strings `"$z~"` and `"$s~"` are reserved for source and sink respectively.

## Algorithms

This library uses the **Ford-Falkerson algorithm** where the next augmenting path is found using the BFS algorithm and edge can also be augmented in the opposite direction.

This makes the Algorithm correct for any network and although it can be quite slow for a general network, **for bipartite networks** with source and sink each connected to one partite and each capacity is set to 1, the algorithm is O(n\*m) (where n is number of vertices and m is number of edges).

# Run And Results

```
# OPTIONAL: Update and install deps.
cabal update
cabal install --only-dependencies
cabal build

# on WSL
# runs tests
cabal run
```

## Tests

Tests are done on this problem. You get a chessboard with holes (tales, where a piece can't stand) and you want to find the maximum number of rooks and their placements such that no two rooks attack each other.

You can solve this problem by creating a bipartite graph of rows and columns where each edge represents a valid rook placement. By finding the Maximum Pairing in this graph, we can solve this problem.

## Test Results

```
Up to date
RUN TESTS
Test BFS - Najdi nenasicenou cestu Prosel

Test Ford-Falk - klasicky diamant Prosel
Test Ford-Falk - sit s spetnymi hranami ze stoku Prosel

Test Parovani - veze na normalni sachovnici Prosel
Test Parovani - veze na sachovnici bez diagonaly Prosel
Test Parovani - veze na sachovnici ne ctverec Prosel
Test Parovani - veze na sachovnici jen cerna pole Prosel
Test Parovani - veze na sachovnici jen cerna pole a s dirami Prosel
```
