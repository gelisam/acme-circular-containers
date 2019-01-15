# acme-circular-containers

(work in progress: this Readme documents the aspirations of this package, not its current status)

Most immutable data structures have a spine, meaning that when you modify an element, only the nodes on the path from the root to the modified element need to be reallocated. The data structures in this library are spineless, so all the nodes need to be reallocated.

## Acme

The Acme category is for joke packages, so as you might guess from that and from the paragraph above, spineless immutable data structures are rarely a good idea. Nevertheless, some serious packages do provide important spineless immutable data structures, such as [Arrays](http://hackage.haskell.org/package/array), for those few situations in which the read operations sufficiently outnumber the write operations that the improvement of reads from O(log n) to O(1) compensates for the deterioration of writes from O(log n) to O(n).

## Circular

Unlike Arrays, which are spineless because all the data is in a single node, the data structures in this package consist of several nodes. The nodes point at each other, giving you O(1) access to their neighbours, and those pointers are allowed to form cycles. This differs from spine-based data structures, in which parent nodes point to their child nodes but not vice-versa.

## Containers

The data structures provided by this package are circular versions of some of the data structure provided by the [containers](http://hackage.haskell.org/package/containers) package:

### Graph

A circular version of [`Data.Graph`](http://hackage.haskell.org/package/containers/docs/Data-Graph.html); or rather of [`Data.Graph.Wrapper`](http://hackage.haskell.org/package/graph-wrapper/docs/Data-Graph-Wrapper.html), a wrapper which associates data to the edges and vertices.

Many beginners struggle to implement graphs in Haskell, because the obvious representation is to have each vertex point at their neighbours, but that representation is spineless, unlike every other data structure they are likely to have defined up to this point. The normal solution is to store the set of nodes and the set of edges separately, each in their own spine-based data structure, something like `Graph { nodes :: Set Int, edges :: Set (Int, Int) }`. Here, however, we do create one node per vertex, and each node does point to its vertex's neighbours.

Note that since `Data.Graph` and `Data.Graph.Wrapper` are already using arrays, this representation does not really offer a different tradeoff with respect to read and write operations, it's just a proof of concept demonstrating that we can represent graphs in this way if we really want to.

### Sequence

A circular version of [`Data.Sequence`](http://hackage.haskell.org/package/containers/docs/Data-Sequence.html).

The imperative version of `Seq` would be a doubly-linked list: we can efficiently add and remove elements to both ends of the sequence, and we can efficiently concatenate two sequences, but we don't have easy access to the elements in the middle of the sequence, we have to walk to them one step at a time. Our circular version is literally a doubly-linked list; starting from either end, we can walk in either direction one step at a time, but of course we cannot efficiently make any modification. Unlike `Seq`, walking towards the middle elements doesn't peel off the outer elements, so from one of the inner nodes, we can efficiently access the nodes to its left and right.

### Tree

A circular version of [`Data.Tree`](http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Tree.html).

In the circular version, the parent node is not peeled off when we walk into a child node, so from one of the inner nodes, we can efficiently access its child nodes (as usual) but also its parent node.
