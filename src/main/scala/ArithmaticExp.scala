trait AE

case class Num(value: Int) extends AE
case class Add(left: AE, right: AE) extends AE
case class Subtract(left: AE, right: AE) extends AE


object ArithmaticExp {

  def evaluateAE(exp: AE): Int = exp match {
    case Num(n) => n
    case Add(left, right) => evaluateAE(left) + evaluateAE(right)
    case Subtract(left, right) => evaluateAE(left) - evaluateAE(right)
  }

  def eval(e: AE): Int =
    if (e.isInstanceOf[Num]) e.asInstanceOf[Num].value
    else if (e.isInstanceOf[Add]) {
      val e0 = e.asInstanceOf[Add]
      eval(e0.left) + eval(e0.right)
    } else {
      val e0 = e.asInstanceOf[Subtract]
      eval(e0.left) - eval(e0.right)
    }
  def main(args: Array[String]): Unit = {
    println("Hello World")
    assert(evaluateAE(Subtract(Add(Num(3), Num(7)), Num(5))) == 5)
  }
}
