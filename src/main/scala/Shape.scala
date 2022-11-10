trait Shape
case class Triangle(a:Int,b:Int,c:Int) extends Shape
case class Square(x:Int) extends Shape
case class Rectangle(a:Int,b:Int) extends Shape
object HelloWorld {

  def perimeter(sh:Shape):Int = sh match {
    case Triangle(a, b, c)=> a+b+c
    case Square(a)=>4*a
    case Rectangle(a,b)=> 2*(a+b)
  }
  def test()={
    "hello".substring(0, 4)
    var x: Int = 10
    val y: Int = 18
    println(x + y)

    def add(x: Int, y: Int): Int = {
      x + y
      x - y
      x * y
    }

    println(add(180, 35))

    print(if (true) 10 else 15)

    List(1, 2, 3)
    if (true) 10 else 15

    var lst = List(1, 2, 3)
    10 ::lst
    println(lst)
  }
  def headorzero(list:List[Int])={
    list match {
      case Nil => -1
      case h::t=>t
    }

  }
  case class Student(name: String, height: Int)

  def names(l: List[Student]): List[String]= l match {
    case Nil=>Nil
    case h::t=> h.name:: names(t)
  }
  def tall(l: List[Student]): List[Student] = l match {
    case Nil=>Nil
    case h::t=> if(h.height>10) h::tall(t) else tall(t)
  }
  def incmylist(lst:List[Int]):List[Int] = lst match {
    case Nil=>Nil
    case h::t => h+1 :: incmylist(t)
  }
  def oddlist(lst:List[Int]):List[Int]= lst match {
    case Nil=>Nil
    case h::t => if(h%2==1) h::oddlist(t) else oddlist(t)
  }

  def length(l: List[Student]): Int = l match {
    case Nil=>0
    case h::t => 1+length(t)
  }


  def main(args: Array[String]) {
    println("Hello, world!") // prints Hello World
    val t = Triangle(10, 20, 5)
    val s = Square(20)
    println(t.b == s.x)
    println(perimeter(t))
    println(perimeter(s))
    test()

    val x=headorzero(List(1,2,3))
    println(headorzero(List()))
    var px=(1,"wahid",true)
    (1,"wahid",true)
    println(px._3)
    var lst=List(1,2,3)
    lst.appendedAll(List(12,23))
    println(lst)
    println(incmylist(lst))
    lst=oddlist(lst)
    lst=incmylist(lst)
    println(lst)

    var lststuddent=List(Student("wahid",10),Student("Malisha",20),Student("Avy",16))
    println(names(lststuddent))
    println(tall(lststuddent))

    println(length(lststuddent))
  }
}


