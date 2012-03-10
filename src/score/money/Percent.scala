package score.money

import java.math.{BigDecimal => JBigDecimal}
import java.math.{MathContext => JMathContext}
import java.math.{RoundingMode => JRoundingMode}
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode
import score.money.Percent._
import score.lang.RareException

object Percent {
  
  val SCALE = Format.SCALE
  val ROUNDING_MODE = Format.ROUNDING_MODE
  
  def apply(value: BigDecimal, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Percent(value, scale, roundingMode) 
  def apply(value: JBigDecimal, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Percent(value, scale, roundingMode)             
  def apply(value: String, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Percent(value, scale, roundingMode)        
  def apply(value: Double, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Percent(value, scale, roundingMode)        
  def apply(value: Long, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Percent(value, scale, roundingMode)          
  def apply(value: BigDecimal, scale: Int) = 
    new Percent(value, scale) 
  def apply(value: JBigDecimal, scale: Int) = 
    new Percent(value, scale)          
  def apply(value: String, scale: Int) = 
    new Percent(value, scale)        
  def apply(value: Double, scale: Int) = 
    new Percent(value, scale) 
  def apply(value: Long, scale: Int) = 
    new Percent(value, scale)          
  def apply(value: BigDecimal) = 
    new Percent(value) 
  def apply(value: JBigDecimal) = 
    new Percent(value)          
  def apply(value: String) = 
    new Percent(value)        
  def apply(value: Double) = 
    new Percent(value) 
  def apply(value: Long) = 
    new Percent(value)          
    
  object Format {
  
    val SCALE = 4
    val ROUNDING_MODE = RoundingMode.HALF_UP  
    val ROUNDING_MODE_JAVA = FixedPoint.Format.roundingMode_jRoundingMode(ROUNDING_MODE)  
  
    val storedFormat = 
      FixedPointCustom.Format.calc(SCALE, ROUNDING_MODE) 
  
  }

  import Format._

  trait Format extends FixedPoint.Format {
        
    override protected val format = storedFormat
    override val scale = SCALE
    override val roundingMode = ROUNDING_MODE
    override val roundingModeJava = ROUNDING_MODE_JAVA    
    
  }   
  
}



class Percent(intFracPair: (Long, Long), scaleArg: Int, roundingMode: RoundingMode)
  extends FixedPoint(intFracPair: (Long, Long)) with Percent.Format {
  
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
    
  def unary_+ : Percent = this   
  def unary_- : Percent = {
    val intPart = ((int1.toLong << 32) | (int0.toLong & 0x00000000ffffffffL))
    if (intPart == Long.MinValue) throw RareException("Could not negate, integral part == " + intPart)
    else {
      val fracPart = ((frac1.toLong << 32) | (frac0.toLong & 0x00000000ffffffffL))
      new Percent((-intPart, -fracPart), scale, roundingMode) 
    }     
  }  
  def abs: Percent = {
    val intPart = ((int1.toLong << 32) | (int0.toLong & 0x00000000ffffffffL))
    val fracPart = ((frac1.toLong << 32) | (frac0.toLong & 0x00000000ffffffffL))
    if ((intPart.signum | fracPart.signum) != -1) this
    else new Percent((-intPart, -fracPart), scale, roundingMode)
  }  
  def +(that: Percent): Percent = new Percent(underlying + that.underlying)
  def -(that: Percent): Percent = new Percent(underlying - that.underlying)  
  def *(that: Coef): Coef = new Coef((underlying / 100) * that.underlying)   
  def *(that: Percent): Percent = new Percent(underlying * that.underlying / 10000)    
  def *(that: Amount): Coef = new Coef((underlying / 100) * that.underlying)      
  def /(that: Coef): Coef = new Coef((underlying / 100) / that.underlying)  
  
}