package score.money

import score.money 

import java.math.{MathContext => JMathContext}
import java.math.{RoundingMode => JRoundingMode}

import scala.math.BigDecimal.RoundingMode


/**
 * These constants are owned by both score.money.FixedPoint and score.math,
 * and this trait only exists to prevent possible circular dependencies
 */
trait FixedPointExport {
  
  /**
   * Integral part constraints (actual values fit Long)
   */
  private[money] val INT_LEN_MIN = 0
  private[money] val INT_LEN_MAX = Long.MaxValue.toString().length()     

  /**
   * Fractional part constraints (actual values fit Long, and < 10^FRAC_LEN_MAX)
   */ 
  private[money] val FRAC_LEN_MIN = 0
  private[money] val FRAC_LEN_MAX = Long.MaxValue.toString().length() - 1
  
  /**
   * Precision and format parameters for calculations that conform conventions of {@code score.money} library  
   */
  val SCALE = FRAC_LEN_MAX
  val ROUNDING_MODE = RoundingMode.HALF_UP
  val PRECISION = INT_LEN_MAX + FRAC_LEN_MAX
  val ROUNDING_MODE_MC = JRoundingMode.valueOf(ROUNDING_MODE.toString)
  val MC = new JMathContext(PRECISION, ROUNDING_MODE_MC)  

}
