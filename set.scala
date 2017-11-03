
import stainless.lang._
import stainless.collection._

object s {

  implicit class SetWrapper[T](val set: Set[T]) {

    @inline
    def forall(p: T => Boolean): Boolean = stainless.lang.forall { (e: T) =>
      set.contains(e) ==> p(e)
    }

    @inline
    def exists(p: T => Boolean): Boolean =
      !set.forall((e: T) => !p(e))

    @inline
    def toList: List[T] = choose { (xs: List[T]) =>
      xs.content == set && forall { (x: T) =>
        set.contains(x) ==> (xs.filter(_ == x).size == 1)
      }
    }
  }

  def add_exists[A](set: Set[A], x: A, p: A => Boolean): Boolean = {
    require(p(x))
    (set + x) exists p
  } holds

  def add_toList[A](set: Set[A], x: A): Boolean = {
    require(!set.toList.contains(x))
    (set + x).toList.contains(x)
  } holds

  def toList_nodup[A](set: Set[A], x: A): Boolean = {
    (set + x + x).toList.filter(_ == x).size == 1
  } holds

}
