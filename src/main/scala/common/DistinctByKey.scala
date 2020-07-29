package common

import scala.collection.concurrent.TrieMap

/**
  * Bijective map where the key is guaranteed to have been derived from the value.
  */
case class DistinctByKey[V](items: List[V]) {
  private val cache: TrieMap[HasKey[_, V], Map[_, V]] = TrieMap.empty

  def value[K](implicit hasKey: HasKey[K, V]): Map[K, V] =
    cache.getOrElseUpdate(hasKey, items.groupMapReduce(hasKey.key)(identity)((a, _) => a)).asInstanceOf[Map[K, V]]

}

object DistinctByKey {
  def apply[V](values: V*): DistinctByKey[V] = apply(values.toList)
}
