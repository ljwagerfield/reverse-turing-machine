package common

import cats.implicits._

import scala.collection.mutable

object TreeUtils {

  /**
    * Performs an iterative deepening depth-first search (IDDFS).
    * - Order:  nodes are yielded in the same order as a breadth-first search (BFS).
    * - Memory: `O(N)` instead of `O(C^N)` from BFS, where N is the depth of the traversal.
    * - Time:   Worse than DFS and BFS, since it requires re-traversal of nodes, whose
    *           children may be expensive to calculate
    */
  def iddfs[A](node: A)(children: A => IterableOnce[A]): LazyList[A] = {
    val stack             = mutable.Stack(NodeWithLevel(node, 0))
    var level             = 0
    var nodesFoundAtLevel = false
    LazyList.from(
      new Iterator[Option[A]] {
        def hasNext: Boolean = stack.nonEmpty
        def next(): Option[A] = {
          val NodeWithLevel(descendant, descendantLevel) = stack.pop()
          val result =
            if (descendantLevel === level) {
              nodesFoundAtLevel = true
              Some(descendant)
            }
            else {
              stack.prependAll(children(descendant).iterator.map(NodeWithLevel(_, descendantLevel + 1)))
              None
            }

          if (stack.isEmpty && nodesFoundAtLevel) {
            level             = level + 1
            nodesFoundAtLevel = false
            stack.push(NodeWithLevel(node, 0)) // Start again from root.
          }

          result
        }
      }.collect { case Some(x) => x }
    )
  }

  def bfs[A](node: A)(children: A => IterableOnce[A]): LazyList[A] = {
    val queue = mutable.Queue(node)
    LazyList.from(
      new Iterator[A] {
        def hasNext: Boolean = queue.nonEmpty
        def next(): A = {
          val result = queue.dequeue()
          queue.enqueueAll(children(result))
          result
        }
      }
    )
  }

  def dfs[A](node: A)(children: A => IterableOnce[A]): LazyList[A] = {
    val stack = mutable.Stack(node)
    LazyList.from(
      new Iterator[A] {
        def hasNext: Boolean = stack.nonEmpty
        def next(): A = {
          val result = stack.pop()
          stack.prependAll(children(result))
          result
        }
      }
    )
  }

  // Used instead of Tuple2 to prevent boxing of 'level' (provides some performance advantages!).
  private case class NodeWithLevel[A](node: A, level: Int)

}
