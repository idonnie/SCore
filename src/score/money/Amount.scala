package score.money

import java.math.{BigDecimal => JBigDecimal}
import java.math.{MathContext => JMathContext}
import java.math.{RoundingMode => JRoundingMode}
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode
import score.money.Amount._
import score.lang.RareException

object Amount {
  
  val SCALE = Format.SCALE
  val ROUNDING_MODE = Format.ROUNDING_MODE
  
  def apply(value: BigDecimal, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Amount(value, scale, roundingMode) 
  def apply(value: JBigDecimal, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Amount(value, scale, roundingMode)             
  def apply(value: String, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Amount(value, scale, roundingMode)        
  def apply(value: Double, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Amount(value, scale, roundingMode) 
  def apply(value: Long, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Amount(value, scale, roundingMode)          
  def apply(value: BigDecimal, scale: Int) = 
    new Amount(value, scale) 
  def apply(value: JBigDecimal, scale: Int) = 
    new Amount(value, scale)          
  def apply(value: String, scale: Int) = 
    new Amount(value, scale)        
  def apply(value: Double, scale: Int) = 
    new Amount(value, scale)   
  def apply(value: Long, scale: Int) = 
    new Amount(value, scale)     
  def apply(value: BigDecimal) = 
    new Amount(value) 
  def apply(value: JBigDecimal) = 
    new Amount(value)          
  def apply(value: String) = 
    new Amount(value)        
  def apply(value: Double) = 
    new Amount(value)
  def apply(value: Long) = 
    new Amount(value)
  
  object Format {
  
    val SCALE = 2
    val ROUNDING_MODE = RoundingMode.HALF_UP  
    val ROUNDING_MODE_JAVA = FixedPoint.Format.roundingMode_jRoundingMode(ROUNDING_MODE)  
  
    val storedFormat = 
      FixedPointCustom.Format.calc(SCALE, ROUNDING_MODE) 
      
  }
  
  import Format._

  trait Format extends FixedPoint.Format {
        
    protected val format = storedFormat
    val scale = SCALE
    val roundingMode = ROUNDING_MODE
    val roundingModeJava = ROUNDING_MODE_JAVA    
    
  } 
  
}



class Amount protected (intFracPair: (Long, Long), scaleArg: Int, roundingMode: RoundingMode)
  extends FixedPoint(intFracPair: (Long, Long)) with Amount.Format {
  
  def this (value: BigDecimal, scale: Int, roundingMode: RoundingMode) =
    this(FixedPoint.fp(value), scale, roundingMode)
  def this (value: JBigDecimal, scale: Int, roundingMode: RoundingMode) =
    this(FixedPoint.fp(value), scale, roundingMode)    
  def this (value: String, scale: Int, roundingMode: RoundingMode) =
    this(FixedPoint.fp(value), scale, roundingMode)
  def this (value: Double, scale: Int, roundingMode: RoundingMode) =
    this(FixedPoint.fp(value), scale, roundingMode)
  def this (value: Long, scale: Int, roundingMode: RoundingMode) =
    this((value, 0L), scale, roundingMode)              
  def this (value: BigDecimal, scale: Int) =
    this(FixedPoint.fp(value), scale, ROUNDING_MODE)
  def this (value: JBigDecimal, scale: Int) =
    this(FixedPoint.fp(value), scale, ROUNDING_MODE)    
  def this (value: String, scale: Int) =
    this(FixedPoint.fp(value), scale, ROUNDING_MODE)
  def this (value: Double, scale: Int) =
    this(FixedPoint.fp(value), scale, ROUNDING_MODE)
  def this (value: Long, scale: Int) =
    this((value, 0L), scale, ROUNDING_MODE)              
  def this (value: BigDecimal) =
    this(FixedPoint.fp(value), SCALE, ROUNDING_MODE)
  def this (value: JBigDecimal) =
    this(FixedPoint.fp(value), SCALE, ROUNDING_MODE)    
  def this (value: String) =
    this(FixedPoint.fp(value), SCALE, ROUNDING_MODE)
  def this (value: Double) =
    this(FixedPoint.fp(value), SCALE, ROUNDING_MODE)  
  def this (value: Long) =
    this((value, 0L), SCALE, ROUNDING_MODE)          
  
  def unary_+ : Amount = this   
  def unary_- : Amount = {
    val intPart = ((int1.toLong << 32) | (int0.toLong & 0x00000000ffffffffL))
    if (intPart == Long.MinValue) throw RareException("Could not negate, integral part == " + intPart)
    else {
      val fracPart = ((frac1.toLong << 32) | (frac0.toLong & 0x00000000ffffffffL))
      new Amount((-intPart, -fracPart), scale, roundingMode) 
    }     
  }  
  def abs: Amount = {
    val intPart = ((int1.toLong << 32) | (int0.toLong & 0x00000000ffffffffL))
    val fracPart = ((frac1.toLong << 32) | (frac0.toLong & 0x00000000ffffffffL))
    if ((intPart.signum | fracPart.signum) != -1) this
    else new Amount((-intPart, -fracPart), scale, roundingMode)
  }  
  def +(that: Amount): Amount = new Amount(underlying + that.underlying)
  def -(that: Amount): Amount = new Amount(underlying - that.underlying)  
  def *(that: Coef): Amount = new Amount(underlying * that.underlying)   
  def *(that: Percent): Amount = new Amount(underlying * (that.underlying / 100))    
  def /(that: Coef): Amount = new Amount(underlying / that.underlying)  
  def /(that: Percent): Amount = new Amount((underlying * 100) / that.underlying)  
    
}