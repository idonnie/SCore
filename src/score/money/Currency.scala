package score.money

import java.math.{BigDecimal => JBigDecimal}
import java.math.{MathContext => JMathContext}
import java.math.{RoundingMode => JRoundingMode}
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode
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
import score.money.Currency._
import score.lang.RareException

object Currency {
  
  val SCALE = Format.SCALE
  val ROUNDING_MODE = Format.ROUNDING_MODE
    
  def apply(value: BigDecimal, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Currency(value, scale, roundingMode)  
  def apply(value: JBigDecimal, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Currency(value, scale, roundingMode)             
  def apply(value: String, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Currency(value, scale, roundingMode)        
  def apply(value: Double, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Currency(value, scale, roundingMode) 
  def apply(value: Long, scale: Int, roundingMode: RoundingMode.RoundingMode) = 
    new Currency(value, scale, roundingMode)          
  def apply(value: BigDecimal, scale: Int) = 
    new Currency(value, scale) 
  def apply(value: JBigDecimal, scale: Int) = 
    new Currency(value, scale)          
  def apply(value: String, scale: Int) = 
    new Currency(value, scale)        
  def apply(value: Double, scale: Int) = 
    new Currency(value, scale) 
  def apply(value: Long, scale: Int) = 
    new Currency(value, scale)          
  def apply(value: BigDecimal) = 
    new Currency(value) 
  def apply(value: JBigDecimal) = 
    new Currency(value)          
  def apply(value: String) = 
    new Currency(value)        
  def apply(value: Double) = 
    new Currency(value)
  def apply(value: Long) = 
    new Currency(value) 
    
  object Format {
  
    val SCALE = FixedPoint.SCALE
    val ROUNDING_MODE = FixedPoint.ROUNDING_MODE
      
  }
  
  trait Format extends FixedPointCustom.Format
  
}

class Currency (intFracPair: (Long, Long), scaleArg: Int, roundingMode: RoundingMode)
  extends FixedPointCustom(intFracPair: (Long, Long), scaleArg: Int, roundingMode: RoundingMode) {
  
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
    
  def unary_+ : Currency = this   
  def unary_- : Currency = {
    val intPart = ((int1.toLong << 32) | (int0.toLong & 0x00000000ffffffffL))
    if (intPart == Long.MinValue) throw RareException("Could not negate, integral part == " + intPart)
    else {
      val fracPart = ((frac1.toLong << 32) | (frac0.toLong & 0x00000000ffffffffL))
      new Currency((-intPart, -fracPart), scale, roundingMode) 
    }     
  }  
  def abs: Currency = {
    val intPart = ((int1.toLong << 32) | (int0.toLong & 0x00000000ffffffffL))
    val fracPart = ((frac1.toLong << 32) | (frac0.toLong & 0x00000000ffffffffL))
    if ((intPart.signum | fracPart.signum) != -1) this
    else new Currency((-intPart, -fracPart), scale, roundingMode)
  }  
  def +(that: Currency): Currency = new Currency(underlying + that.underlying)
  def -(that: Currency): Currency = new Currency(underlying - that.underlying)  
  def *(that: Currency): Currency = new Currency(underlying * that.underlying)   
  def /(that: Currency): Currency = new Currency(underlying / that.underlying)  

}