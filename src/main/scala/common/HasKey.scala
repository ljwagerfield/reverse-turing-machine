package common

trait HasKey[K, V] {
  def key: V => K
}
