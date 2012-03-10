package score

import score.money.FixedPointExport
import java.math.BigInteger

package object math extends AnyRef with FixedPointExport {
  
  /**
   * Golden ratio
   */
  val PHI = scala.math.sqrt(1.25) + 0.5    
    
  /**
   * When comparing two Double-s, this can be used as computational error
   */
  private val minDeltaDouble: (Double, Double, Double) => Double = (value, delta, div) => {
    val nextDelta = delta / div
    if (value - (value * nextDelta) == value) delta else minDeltaDouble(value, nextDelta, div)      
  }      
  val ERR_DOUBLE = 5.0 * minDeltaDouble(1.0, 0.000001, 10.0) 

  /**
   * When comparing two Float-s, this can be used as computational error 
   */
  private val minDeltaFloat: (Float, Float, Float) => Float = (value, delta, div) => {
    val nextDelta = delta / div
    if (value - (value * nextDelta) == value) delta else minDeltaFloat(value, nextDelta, div)      
  }        
  val ERR_FLOAT = 5.0f * minDeltaFloat(1.0f, 0.000001f, 10.0f)
  
  /**
   * General-purpose constants
   */
  val ZERO_BIG = BigDecimal(0, MC) 
  
  /**
   * Hello World for quick performance test 
   */
  def fib(n: Int): BigDecimal = {
    if (n >= 0) {        
      def fib0(a: BigDecimal, b: BigDecimal, n: Int): BigDecimal = 
        if (n == 0) a else fib0(a + b, a, n - 1)       
      fib0(0, 1, n)      
    } else { 
      throw new IllegalArgumentException("" + n)
    }
  }
  
}