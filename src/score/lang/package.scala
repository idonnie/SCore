package score

import org.junit.Assert.assertEquals
import org.junit.Assert

package object lang {

  /**
   * Allows conditional invocation of a method
   */
  class When[F](fun: F) {  
     def when(cond: F => Boolean)(tail: F => F) = if (cond(fun)) tail(fun) else fun
  }
  implicit def whenever[F](fun: F): When[F] = new When[F](fun) 

  
  /**
	* JUnit assertEquals with smart error reporting
	* 
	* USAGE let(a === b)
	*/
  def let[L, R](lrVal: (ConsciousAssert[L], ConsciousAssert[R])) = {
	val (lVal, rVal) = (lrVal._1.value, lrVal._2.value)
	val lStr: String = lVal.toString()
	val rStr: String = rVal.toString()
	if (!(lVal == rVal)) {
	  // DEBUG
	  println(lVal.getClass() + "|" + rVal.getClass())
	  Assert.fail("" + lStr + " != " + rStr)
	}
  }        		   
  class ConsciousAssert[L](val value: L) {
	def ===[R](rValue: ConsciousAssert[R]): (ConsciousAssert[L], ConsciousAssert[R])  = {
	  (this, rValue)
	}
  }
  implicit def any2ConsciousAssert[T](value: T): ConsciousAssert[T] = 
	new ConsciousAssert(value) 
  
  /**
   * Calls JUnit Assert.fail() in a case if 
   * 1) code execution succeed;
   * 2) failed with Throwable that differ from method parameter type
   */ 
  def intercept[T >: Throwable](code: => Unit) {
    try {
      code
    } catch {
      case e: T => return;
    }
    Assert.fail()
  }  	
	
}