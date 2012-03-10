package score.money

import java.math.{BigDecimal => JBigDecimal}
import java.math.{MathContext => JMathContext}
import java.math.{RoundingMode => JRoundingMode}
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode
import score.money.Coef._
import score.lang.RareException

object Coef {
  
  val SCALE = Format.SCALE
  val ROUNDING_MODE = Format.ROUNDING_MODE
      
  def apply(value: BigDecimal, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Coef(value, scale, roundingMode) 
  def apply(value: JBigDecimal, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Coef(value, scale, roundingMode)             
  def apply(value: String, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Coef(value, scale, roundingMode)        
  def apply(value: Double, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Coef(value, scale, roundingMode)      
  def apply(value: Long, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Coef(value, scale, roundingMode)          
  def apply(value: BigDecimal, scale: Int) = 
    new Coef(value, scale) 
  def apply(value: JBigDecimal, scale: Int) = 
    new Coef(value, scale)          
  def apply(value: String, scale: Int) = 
    new Coef(value, scale)        
  def apply(value: Double, scale: Int) = 
    new Coef(value, scale)   
  def apply(value: Long, scale: Int) = 
    new Coef(value, scale)            
  def apply(value: BigDecimal) = 
    new Coef(value) 
  def apply(value: JBigDecimal) = 
    new Coef(value)          
  def apply(value: String) = 
    new Coef(value)        
  def apply(value: Double) = 
    new Coef(value)  
  def apply(value: Long) = 
    new Coef(value)  
  
  object Format {
  
    val SCALE = 6
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
  
    implicit val ord = new Ordering[Any] { def compare(x: Any, y: Any): Int = -1}
     implicit val ord2 = new Ordering[ScalaObject] { def compare(x: ScalaObject, y: ScalaObject): Int = -1}
  
  
}



class Coef(intFracPair: (Long, Long), scaleArg: Int, roundingMode: RoundingMode)
  extends FixedPoint(intFracPair: (Long, Long)) with Coef.Format {
  
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
  
  def unary_+ : Coef = this   
  def unary_- : Coef = {
    val intPart = ((int1.toLong << 32) | (int0.toLong & 0x00000000ffffffffL))
    if (intPart == Long.MinValue) throw RareException("Could not negate, integral part == " + intPart)
    else {
      val fracPart = ((frac1.toLong << 32) | (frac0.toLong & 0x00000000ffffffffL))
      new Coef((-intPart, -fracPart), scale, roundingMode) 
    }     
  }  
  def abs: Coef = {
    val intPart = ((int1.toLong << 32) | (int0.toLong & 0x00000000ffffffffL))
    val fracPart = ((frac1.toLong << 32) | (frac0.toLong & 0x00000000ffffffffL))
    if ((intPart.signum | fracPart.signum) != -1) this
    else new Coef((-intPart, -fracPart), scale, roundingMode)
  }  
  def +(that: Coef): Amount = new Amount(underlying + that.underlying)
  def -(that: Coef): Amount = new Amount(underlying - that.underlying)  
  def *(that: Coef): Coef = new Coef(underlying * that.underlying)   
  def *(that: Percent): Coef = new Coef(underlying * (that.underlying / 100))    
  def *(that: Amount): Coef = new Coef(underlying * that.underlying)      
  def /(that: Coef): Coef = new Coef(underlying / that.underlying)  
  def /(that: Percent): Coef = new Coef((underlying * 100) / that.underlying)   
    
}