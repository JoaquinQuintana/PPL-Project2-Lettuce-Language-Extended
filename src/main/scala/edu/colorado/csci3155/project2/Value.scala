package edu.colorado.csci3155.project2

import scala.util.Failure

/* Define the values used in our interpreter */
sealed trait Value
case class NumValue(f: Double) extends Value
case class FigValue(c: MyCanvas) extends Value
case class Closure(x: String, e: Expr, env: Environment) extends Value
case class BoolValue(b: Boolean) extends Value

/* Create some utility functions to operate on values.  */
object ValueOps {

    //def line (v:Value): Value

    //see if we can handle plus here
    def plus(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => NumValue(f1 + f2)
        case (FigValue(obj1), FigValue(obj2)) => FigValue(obj1.overlap(obj2))
        case _ => throw new IllegalArgumentException("plus Failed: Provided type not defined in sealed trait Value")
    }

    def multi(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => NumValue(f1 * f2)
        case (FigValue(obj1), FigValue(obj2)) => FigValue(obj1.placeRight(obj2))
        case _ => throw new IllegalArgumentException("multi Failed: Provided type not defined in sealed trait Value")
    }

    def div(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(v1), NumValue(v2)) => NumValue(v1 / v2)
        case (FigValue(obj1), FigValue(obj2)) => FigValue(obj1.placeTop(obj2))
        case (FigValue(obj1), NumValue(v1)) => FigValue(obj1.rotate(v1))
        case _ => throw new IllegalArgumentException("div Failed: Provided type not defined in sealed trait Value")
    }

    def minus(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => NumValue(f1 - f2)
        case _ => throw new IllegalArgumentException("Cannot subtract figures, numbers and closures")
    }

    def geq(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => BoolValue(f1 >= f2)
        case _ => throw new IllegalArgumentException("Cannot compare non numeric expressions with the geq comparator")
    }

    def gt(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => BoolValue(f1 > f2)
        case _ => throw new IllegalArgumentException("Cannot compare non numeric expressions with the eq comparator")
    }

    def equal(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => BoolValue(f1 == f2)
        case (BoolValue(b1), BoolValue(b2)) => BoolValue(b1 == b2)
        case _ =>  throw new IllegalArgumentException("Cannot compare non numeric/boolean expressions with the eq comparator")
    }

    def notEqual(v1: Value, v2: Value): Value = {
        val v = equal(v1, v2)
        v match {
            case BoolValue(b) => BoolValue(!b)
            case _ => throw new IllegalArgumentException("Internal error: something is really wrong") // This should never happen
        }
    }
}