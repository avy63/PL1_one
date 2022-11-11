/*
Syntax of "Arithmatic expression with id"
aeid :: = num
        | "(" aeid "+" aeid ")"
        | "(" aeid "-" aeid ")"
        | "{" "val" id "=" aeid ";" aeid "}"
        | id
 */

trait AEID
type Env = Map[String, Int]
case class Num(value: Int) extends AEID
case class Add(left: AEID, right: AEID) extends AEID
case class Subtract(left: AEID, right: AEID) extends AEID
case class Val(x:String, i:AEID,b:AEID) extends  AEID
case class Id(x:String) extends  AEID


object ArithmaticExpID {
  def evaluateAEID(exp: AEID, env: Env): Int = exp match {
    case Num(n) => n
    case Add(left, right) => evaluateAEID(left,env) + evaluateAEID(right,env)
    case Subtract(left, right) => evaluateAEID(left,env) - evaluateAEID(right,env)
    case Val(x, i, b) => evaluateAEID(b, env + (x -> evaluateAEID(i, env)))
    case Id(x) => env(x)
  }

}
