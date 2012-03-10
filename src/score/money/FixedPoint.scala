package score.money 

import java.math.RoundingMode.{CEILING => JCEILING}

import java.math.RoundingMode.{DOWN => JDOWN}
import java.math.RoundingMode.{FLOOR => JFLOOR}
import java.math.RoundingMode.{HALF_DOWN => JHALF_DOWN}
import java.math.RoundingMode.{HALF_EVEN => JHALF_EVEN}
import java.math.RoundingMode.{HALF_UP => JHALF_UP}
import java.math.RoundingMode.{UNNECESSARY => JUNNECESSARY}
import java.math.RoundingMode.{UP => JUP}
import java.math.{BigDecimal => JBigDecimal}
import java.math.{MathContext => JMathContext}
import java.math.{RoundingMode => JRoundingMode}
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
import scala.math.ScalaNumber
import scala.math.ScalaNumericConversions

import score.money

/**
 * 128-bit fixed point value
 */
object FixedPoint extends AnyRef with FixedPointExport {
      
  /**
   * Long constraints for integral part
   */  
  private[money] val INT_VAL_MIN = Long.MinValue  
  private[money] val INT_VAL_MAX = Long.MaxValue   
  
  /**
   * Long constraints for fractional part
   */  
  private[this] val p10: (Long, Int) => Long = (v: Long, p: Int) =>  if (p == 0) v else p10(v * 10, p - 1)   
  private[money] val FRAC_VAL_FROM = p10(-1, FRAC_LEN_MAX)
  private[money] val FRAC_VAL_UNTIL = -FRAC_VAL_FROM
  
  /**
   * BigDecimal constraints for integral part
   */
  private[money] val INT_VAL_MIN_BIG = BigDecimal(INT_VAL_MIN, MC) 
  private[money] val INT_VAL_MAX_BIG = BigDecimal(INT_VAL_MAX, MC)
  private[this] val INT_VAL_MIN_JBIG = new JBigDecimal(INT_VAL_MIN, MC) 
  private[this] val INT_VAL_MAX_JBIG = new JBigDecimal(INT_VAL_MAX, MC)

  /**
   * BigDecimal constraints for fractional part
   */
  private[money] val FRAC_VAL_FROM_BIG = BigDecimal(FRAC_VAL_FROM, MC)
  private[money] val FRAC_VAL_UNTIL_BIG = BigDecimal(FRAC_VAL_UNTIL, MC)
  private[this] val FRAC_VAL_UNTIL_JBIG = new JBigDecimal(FRAC_VAL_UNTIL, MC)
  
  /**
   * Double constraints for integral part
   */  
  private[money] val INT_VAL_MIN_DOUBLE = INT_VAL_MIN.toDouble
  private[money] val INT_VAL_MAX_DOUBLE = INT_VAL_MAX.toDouble  
  
  /**
   * Double constraints for fractional part
   */
  private[money] val FRAC_VAL_FROM_DOUBLE = FRAC_VAL_FROM.toDouble  
  private[money] val FRAC_VAL_UNTIL_DOUBLE = FRAC_VAL_UNTIL.toDouble  
  
  def fp(value: BigDecimal): (Long, Long) = {
    if (value.mc.getPrecision() >= PRECISION) {
    
      // integral part must fit Long
      val intValBig = value.setScale(0, RoundingMode.DOWN)
      
      require(intValBig >= INT_VAL_MIN_BIG && intValBig <= INT_VAL_MAX_BIG)
      val intValBeforeRound = intValBig.longValue()
      
      // fractional part must fit (FRAC_VAL_FROM_BIG, FRAC_VAL_UNTIL_BIG)
      val fracValBig = ((value - intValBig) * FRAC_VAL_UNTIL_BIG).setScale(0, BigDecimal.RoundingMode.DOWN)
      require(fracValBig >= FRAC_VAL_FROM_BIG && fracValBig <= FRAC_VAL_UNTIL_BIG)    
      val fracValRounded = fracValBig.longValue
    
      // treat rounding and overflow
      val intVal = if (fracValRounded < FRAC_VAL_UNTIL) intValBeforeRound
        else if (intValBeforeRound > 0 && intValBeforeRound < INT_VAL_MAX) intValBeforeRound + 1
        else if (intValBeforeRound < 0 && intValBeforeRound > INT_VAL_MIN) intValBeforeRound - 1
        else throw new IllegalArgumentException("Overflow: " + (intValBeforeRound, fracValRounded))    
      val fracVal = if (fracValRounded < FRAC_VAL_UNTIL) fracValRounded else 0
          
      (intVal, fracVal)
      
    } else fp(BigDecimal(value.bigDecimal, MC))
  }
  
  def fp(value: JBigDecimal): (Long, Long) = fp(BigDecimal(value, MC))  
    
  def fp(value: String): (Long, Long) = fp(BigDecimal(value, MC))

  def fp(value: Double): (Long, Long) = {
    
    // not NaN, not Infinity
    require((!value.isNaN) && (!value.isInfinity))
        
    // integral part must fit Long
    val intValDouble = if (value > 0.0) Math.floor(value) else if (value < 0.0) Math.ceil(value) else 0.0
    require(intValDouble >= INT_VAL_MIN_DOUBLE && intValDouble <= INT_VAL_MAX_DOUBLE)    
    val intValBeforeRound = intValDouble.toLong
    
    
    import scala.Predef
    
    // fractional part must fit (FRAC_VAL_FROM_DOUBLE, FRAC_VAL_UNTIL_DOUBLE)
    val fracValDouble = (value - intValDouble) * FRAC_VAL_UNTIL_DOUBLE
    require(fracValDouble > FRAC_VAL_FROM_DOUBLE && fracValDouble < FRAC_VAL_UNTIL_DOUBLE)        
    val fracValRounded = double2Double(if (fracValDouble > 0.0) Math.floor(fracValDouble) 
        else if (fracValDouble < 0.0) Math.ceil(fracValDouble) else 0.0).longValue()
      
    // treat rounding and overflow
    val intVal = if (fracValRounded < FRAC_VAL_UNTIL) intValBeforeRound
      else if (intValBeforeRound > 0 && intValBeforeRound < INT_VAL_MAX) intValBeforeRound + 1
      else if (intValBeforeRound < 0 && intValBeforeRound > INT_VAL_MIN) intValBeforeRound - 1
      else throw new IllegalArgumentException("Overflow: " + (intValBeforeRound, fracValRounded))    
    val fracVal = if (fracValRounded < FRAC_VAL_UNTIL) fracValRounded 
    else 0

    (intVal, fracVal)
  }   
    
  object Format {
        
    protected[money] val roundingMode_id = { 
      Map[RoundingMode, Int](  
        HALF_UP -> 0
        ,HALF_EVEN -> 1
        ,UNNECESSARY -> 2
        ,HALF_DOWN -> 3
        ,UP -> 4
        ,DOWN -> 5
        ,CEILING -> 6
        ,FLOOR -> 7
      )
    }
    
    protected[money] val id_roundingMode: Map[Int, RoundingMode] = 
      roundingMode_id.map(el => (el._2, el._1))
  
    protected[money] val roundingMode_jRoundingMode = {
      Map[RoundingMode, JRoundingMode](  
        HALF_UP -> JHALF_UP
        ,HALF_EVEN -> JHALF_EVEN
        ,UNNECESSARY -> JUNNECESSARY
        ,HALF_DOWN -> JHALF_DOWN
        ,UP -> JUP
        ,DOWN -> JDOWN
        ,CEILING -> JCEILING
        ,FLOOR -> JFLOOR
      )    
    }      
  
  }

  trait Format  {
    
    protected def format: Int 
    def scale: Int 
    def roundingMode: RoundingMode 
    def roundingModeJava: JRoundingMode 

  }
  
  implicit def int2FixedPoint(i: Int): FixedPoint = { new Currency(i.toLong) }
  implicit def long2FixedPoint(l: Long): FixedPoint = { new Currency(l) }
  implicit def jBigDecimal2FixedPoint(j: JBigDecimal): FixedPoint = { new Currency(j) }
  implicit def bigDecimal2FixedPoint(b: BigDecimal): FixedPoint = { new Currency(b) }
  implicit def double2FixedPoint(d: Double): FixedPoint = { new Currency(d) }   
  
}

import score.money.FixedPoint.{long2FixedPoint => _, _}

abstract class FixedPoint protected (intArg: Long, fracArg: Long) 
  extends /*AnyRef*/  ScalaNumber  with Ordered[FixedPoint] with Format  with ScalaNumericConversions with Serializable  {
  
  require(fracArg > FRAC_VAL_FROM && fracArg < FRAC_VAL_UNTIL)
  require((intArg >= 0 && fracArg >= 0) || (intArg <= 0 && fracArg <= 0))  

  // pack integral and fractional parts inside: int0, int1, frac0, frac1
  val int0 = intArg.toInt
  val int1 = (intArg >>> 32).toInt
  val frac0 = fracArg.toInt
  val frac1 = (fracArg >>> 32).toInt 

  def signum: Int = { 
    ((int1.toLong << 32) | (int0.toLong & 0x00000000ffffffffL)).signum | 
    ((frac1.toLong << 32) | (frac0.toLong & 0x00000000ffffffffL)).signum   
  }
  
  protected def this(intFracPair: (Long, Long)) = 
    this(intFracPair._1, intFracPair._2)
 
  def toValueString = {
    val int = ((int1.toLong << 32) | (int0.toLong & 0x00000000ffffffffL)).toString
    val frac = ((frac1.toLong << 32) | (frac0.toLong & 0x00000000ffffffffL)).abs.toString
    (if (signum < 0 && (! int.startsWith("-"))) "-" else "") +
    (if (frac == "0") int 
      else {
        val withZeroes = int + "." + "0" * (scale - frac.length) + frac
        withZeroes.substring(0, withZeroes.findLastIndexOf(_ != '0') + 1)  
      }
    )    
  }
    
  override def toString = {   
    val rounded = if (scale == FRAC_LEN_MAX) toValueString 
    else BigDecimal(toValueString, MC).bigDecimal.setScale(scale, roundingModeJava).toPlainString 
    if (scale == 0) rounded 
    else {      
      val dotPos = rounded.indexOf('.')
      if (dotPos != -1) rounded + "0" * ((scale - (rounded.length - (dotPos + 1))) max 0) 
      else rounded + "." + "0" * scale
    }    
  }
  
  def canEqual(other: Any): Boolean = 
    other.isInstanceOf[FixedPoint] ||
    other.isInstanceOf[BigDecimal] ||
    other.isInstanceOf[Double] ||
    other.isInstanceOf[Float] ||
    other.isInstanceOf[Int] ||
    other.isInstanceOf[Long] ||
    other.isInstanceOf[Short] ||
    other.isInstanceOf[Byte] ||
    other.isInstanceOf[Char] 
    
  override def equals(other: Any): Boolean =  
    other match {
      case that: FixedPoint => 
        (that canEqual this) && 
        (int0 == that.int0) && (frac1 == that.frac1) && 
        (int1 == that.int1) && (frac0 == that.frac0) &&
        (format == that.format)
      case that: BigDecimal =>
        underlying.equals(that)
      case _ => unifiedPrimitiveEquals(other)
    }  
  
  override def hashCode = {
    var hash = 37 + (int0 ^ int1)
    hash = (hash << 5) + (hash << 3) + hash
    hash += frac1 ^ frac0
    hash = (hash << 5) + (hash << 3) + hash
    hash += format
    hash
  }
  
  override def compare(that: FixedPoint) = {
    val intThis = (int1.toLong << 32) | (int0.toLong & 0x00000000ffffffffL)
    val intThat = (that.int1.toLong << 32) | (that.int0.toLong & 0x00000000ffffffffL)
    if (intThis < intThat) -1 
    else if (intThis > intThat) 1
    else {
      val fracThis = (frac1.toLong << 32) | (frac0.toLong & 0x00000000ffffffffL)
      val fracThat = (that.frac1.toLong << 32) | (that.frac0.toLong & 0x00000000ffffffffL)
      if (fracThis < fracThat) -1
      else if (fracThis > fracThat) 1
      else 0
    }
  }
  
  /** ScalaNumber
   */
  def underlying = BigDecimal(toString, MC) 
  protected def isWhole() = (((frac1.toLong << 32) | (frac0.toLong & 0x00000000ffffffffL)) == 0L)  
  
  /** ScalaNumericConversions
   */
  override def intValue = int0
  override def longValue = ((int1.toLong << 32) | (int0.toLong & 0x00000000ffffffffL))
  override def floatValue = underlying.toFloat
  override def doubleValue = underlying.toDouble  
  
  override def toByte = intValue.toByte
  override def toShort = intValue.toShort
  override def toInt = intValue
  override def toLong = longValue
  override def toFloat = floatValue
  override def toDouble = doubleValue 
  
}
