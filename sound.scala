
import stainless.lang._

object sound {

  def hello[A](x: A): String = "Hello " + x.toString

  def test = {
    assert(hello(true) == "Hello true")
  }

}
