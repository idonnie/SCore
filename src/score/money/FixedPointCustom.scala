package score.money 

import score.money


import java.math.{BigDecimal => JBigDecimal}
import java.math.{MathContext => JMathContext}
import java.math.{RoundingMode => JRoundingMode}
import java.math.RoundingMode.{CEILING => JCEILING}
import java.math.RoundingMode.{DOWN => JDOWN}
import java.math.RoundingMode.{FLOOR => JFLOOR}
import java.math.RoundingMode.{HALF_DOWN => JHALF_DOWN}
import java.math.RoundingMode.{HALF_EVEN => JHALF_EVEN}
import java.math.RoundingMode.{HALF_UP => JHALF_UP}
import java.math.RoundingMode.{UNNECESSARY => JUNNECESSARY}
import java.math.RoundingMode.{UP => JUP}
import java.math.{RoundingMode => JRoundingMode}

import scala.math.BigDecimal.RoundingMode.CEILING
import scala.math.BigDecimal.RoundingMode.DOWN
import scala.math.BigDecimal.RoundingMode.FLOOR
import scala.math.BigDecimal.RoundingMode.HALF_DOWN
import scala.math.BigDecimal.RoundingMode.HALF_EVEN
import scala.math.BigDecimal.RoundingMode.HALF_UP
import scala.math.BigDecimal.RoundingMode.RoundingMode
import scala.math.BigDecimal.RoundingMode.UNNECESSARY
import scala.math.BigDecimal.RoundingMode.UP
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode

import score.money.FixedPoint._

/**
 * 160-bit fixed point value (scale and rounding mode stored as {@code val format: Int})
 */
object FixedPointCustom {
  
  object Format {
        
    def calc(scaleArg: Int, roundingModeIdArg: Int): Int =    
      ((roundingModeIdArg & 07) << 5) | 
      (scaleArg & 037) 
    def calc(scaleArg: Int, roundingMode: RoundingMode): Int = 
      calc(scaleArg, FixedPoint.Format.roundingMode_id(roundingMode))
      
  }

  trait Format extends FixedPoint.Format {
         
    override def scale = format & 037
    override def roundingMode = FixedPoint.Format.id_roundingMode((format >>> 5) & 07)
    override def roundingModeJava = FixedPoint.Format.roundingMode_jRoundingMode(roundingMode)
 
  }      

}

import score.money.FixedPointCustom._

protected class FixedPointCustom protected (intArg: Long, fracArg: Long, scaleArg: Int, roundingModeIdArg: Int) 
  extends FixedPoint(intArg: Long, fracArg: Long)  with FixedPointCustom.Format {
  
  require(scaleArg >= FRAC_LEN_MIN && scaleArg <= FRAC_LEN_MAX)    
  require(roundingModeIdArg >= 0 && roundingModeIdArg < FixedPoint.Format.roundingMode_id.size)      
      
  protected val format = FixedPointCustom.Format.calc(scaleArg, roundingModeIdArg)
  
  protected def this(intFracPair: (Long, Long), scaleArg: Int, roundingMode: RoundingMode) = 
      this(intFracPair._1, intFracPair._2, scaleArg, FixedPoint.Format.roundingMode_id(roundingMode))
 
    
}
