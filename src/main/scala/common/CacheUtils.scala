package common

import scala.collection.concurrent.TrieMap

object CacheUtils {

  def memoize[In, Out](f: In => Out): In => Out = {
    val map = new TrieMap[In, Out]()
    input => map.getOrElseUpdate(input, f(input))
  }

}
