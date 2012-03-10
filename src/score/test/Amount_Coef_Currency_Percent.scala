package score.test

import scala.Array.canBuildFrom
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal
import scala.math.Pi
import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.assertFalse
import org.junit.Test
import score.money.{Amount => @@}
import score.money.{Percent => %%}
import score.money.{Coef => kk}
import score.money.{Currency => Big}
import score.money.FixedPoint
import score.math
import score.lang._

class Amount_Coef_Currency_Percent {
	
  private def toString_toStringF[T <: FixedPoint: ClassManifest](testToString: Array[(String, String, T)]) {
	val (expectToString, expectToStringF, arg) = testToString.unzip3 

	// filter out elements of arg array associated with nulls in toString test -
	// because it's impossible to complete all toString test cases, when using Double constructor of Big
	val (_: Int, indexesOfNull: List[Int], dropNullToString: List[String]) = 
      expectToString.foldLeft((0, List[Int](), List[String]()))((acc, el) => 
        (acc._1 + 1, if (el.ne(null)) acc._2 else acc._2.::(acc._1), if (el.ne(null)) acc._3.::(el) else acc._3))
    val (_: Int, dropNullArg: List[T]) = arg.foldLeft((0, List[T]()))((acc, el) => 
        (acc._1 + 1, if (! indexesOfNull.contains(acc._1)) acc._2.::(el) else acc._2 ))
    
    assertArrayEquals(
      dropNullToString.reverse.toArray.map((el) => { val obj: AnyRef = el ; obj})
	  , dropNullArg.reverse.toArray.map((el) => { val obj: AnyRef = el.toValueString ; obj})
	)
	assertArrayEquals(
	  expectToStringF.toArray.map((el) => { val obj: AnyRef = el ; obj})
	  , arg.toArray.map((el) => { val obj: AnyRef = el.toString ; obj})
	)   	
  }
  

  @Test def classCurrency() { 
	  	  
	  toString_toStringF(Array[(String, String, Big)](			  

			       // (0)
			  	   ("0", "0.000000000000000000", Big(BigDecimal("0.0")))
			      ,("0", "0.000000000000000000", Big("0.0"))
			      ,("0", "0.000000000000000000", Big(0.0))
			       // (+0)
			  	  ,("0", "0.000000000000000000", Big(BigDecimal("+0.0")))
			      ,("0", "0.000000000000000000", Big("+0.0"))
			      ,("0", "0.000000000000000000", Big(+0.0))	
			       // (-0)
			  	  ,("0", "0.000000000000000000", Big(BigDecimal("-0.0")))
			      ,("0", "0.000000000000000000", Big("-0.0"))
			      ,("0", "0.000000000000000000", Big(-0.0))				      
			       // (+0.000000000000000000)
			  	  ,("0", "0.000000000000000000", Big(BigDecimal("+0.000000000000000000")))
			      ,("0", "0.000000000000000000", Big("+0.000000000000000000"))
			      ,("0", "0.000000000000000000", Big(+0.000000000000000000))
			       // (-0.000000000000000000)
			  	  ,("0", "0.000000000000000000", Big(BigDecimal("-0.000000000000000000")))
			      ,("0", "0.000000000000000000", Big("-0.000000000000000000"))
			      ,("0", "0.000000000000000000", Big(-0.000000000000000000))
			       // (+0E-18)
			  	  ,("0", "0.000000000000000000", Big(BigDecimal("+0E-18")))
			      ,("0", "0.000000000000000000", Big("+0E-18"))
			      ,("0", "0.000000000000000000", Big(+0E-18))
			       // (-0E-18)
			  	  ,("0", "0.000000000000000000", Big(BigDecimal("-0E-18")))
			      ,("0", "0.000000000000000000", Big("-0E-18"))
			      ,("0", "0.000000000000000000", Big(-0E-18))
			      
			       // 21
			      
			       // small (+) => (0)
			  	  ,("0", "0.000000000000000000", Big(BigDecimal("0.00000000000000000049")))
			      ,("0", "0.000000000000000000", Big("0.00000000000000000049"))
			      ,("0", "0.000000000000000000", Big(0.00000000000000000049))   
			       // small (-) => (0)
			  	  ,("0", "0.000000000000000000", Big(BigDecimal("-0.00000000000000000049")))
			      ,("0", "0.000000000000000000", Big("-0.00000000000000000049"))
			      ,("0", "0.000000000000000000", Big(-0.00000000000000000049))   
			       // small (+) that (if rounded), could be => (+), but => (0)
			  	  ,("0", "0.000000000000000000", Big(BigDecimal("0.00000000000000000050")))
			      ,("0", "0.000000000000000000", Big("0.00000000000000000050"))
			      ,("0", "0.000000000000000000", Big(0.00000000000000000050))   
			       // small (-) that (if rounded), could be => (-), but => (0)
			  	  ,("0", "0.000000000000000000", Big(BigDecimal("-0.00000000000000000050")))
			      ,("0", "0.000000000000000000", Big("-0.00000000000000000050"))
			      ,("0", "0.000000000000000000", Big(-0.00000000000000000050))   
			       // small (+) that almost identical to small (+), but still => (0)
			  	  ,("0", "0.000000000000000000", Big(BigDecimal("0.00000000000000000099")))
			      ,("0", "0.000000000000000000", Big("0.00000000000000000099"))
			      ,("0", "0.000000000000000000", Big(0.00000000000000000099))   
			       // small (-) that almost identical to small (-), but still => (0)
			  	  ,("0", "0.000000000000000000", Big(BigDecimal("-0.00000000000000000099")))
			      ,("0", "0.000000000000000000", Big("-0.00000000000000000099"))
			      ,("0", "0.000000000000000000", Big(-0.00000000000000000099))   
			      
			       // 39 
			      
			       // small (+) => small (+)
			  	  ,("0.000000000000000001", "0.000000000000000001", Big(BigDecimal("0.000000000000000001")))
			      ,("0.000000000000000001", "0.000000000000000001", Big("0.000000000000000001"))
			      ,("0.000000000000000001", "0.000000000000000001", Big(0.000000000000000001))   
			       // small (-) => small (-)
			  	  ,("-0.000000000000000001", "-0.000000000000000001", Big(BigDecimal("-0.000000000000000001")))
			      ,("-0.000000000000000001", "-0.000000000000000001", Big("-0.000000000000000001"))
			      ,("-0.000000000000000001", "-0.000000000000000001", Big(-0.000000000000000001))   
			       // almost two small (+) => single small (+)
			  	  ,("0.000000000000000001", "0.000000000000000001", Big(BigDecimal("0.00000000000000000199")))
			      ,("0.000000000000000001", "0.000000000000000001", Big("0.00000000000000000199"))
			      ,("0.000000000000000001", "0.000000000000000001", Big(0.00000000000000000199))   
			       // almost two small (-) => single small (-)
			  	  ,("-0.000000000000000001", "-0.000000000000000001", Big(BigDecimal("-0.00000000000000000199")))
			      ,("-0.000000000000000001", "-0.000000000000000001", Big("-0.00000000000000000199"))
			      ,("-0.000000000000000001", "-0.000000000000000001", Big(-0.00000000000000000199)) 
			       // (+) with fractional part exactly on Int bound
			  	  ,("0.000000004294967296", "0.000000004294967296", Big(BigDecimal("0.000000004294967296")))
			      ,("0.000000004294967296", "0.000000004294967296", Big("0.000000004294967296"))
			      ,("0.000000004294967296", "0.000000004294967296", Big(0.000000004294967296))   
			       // (-) with fractional part exactly on Int bound
			  	  ,("-0.000000004294967296", "-0.000000004294967296", Big(BigDecimal("-0.000000004294967296")))
			      ,("-0.000000004294967296", "-0.000000004294967296", Big("-0.000000004294967296"))
			      ,("-0.000000004294967296", "-0.000000004294967296", Big(-0.000000004294967296))   			      
			       // arbitrary (+) with fractional part fitting 2 Int-s (0.12345678987)
			  	  ,("0.12345678987", "0.123456789870000000", Big(BigDecimal("0.12345678987")))
			      ,("0.12345678987", "0.123456789870000000", Big("0.12345678987"))
			      ,("0.12345678987", "0.123456789870000000", Big(0.12345678987))   
			       // arbitrary (-) with fractional part fitting 2 Int-s (-0.12345678987)		  
			  	  ,("-0.12345678987", "-0.123456789870000000", Big(BigDecimal("-0.12345678987")))
			      ,("-0.12345678987", "-0.123456789870000000", Big("-0.12345678987"))
			      ,("-0.12345678987", "-0.123456789870000000", Big(-0.12345678987))   
			       // (0.999999999999999999)
			  	  ,("0.999999999999999999", "0.999999999999999999", Big(BigDecimal("0.999999999999999999")))
			      ,("0.999999999999999999", "0.999999999999999999", Big("0.999999999999999999"))
			      ,("1", "1.000000000000000000", Big(0.999999999999999999))   
			       // (-0.999999999999999999)
			  	  ,("-0.999999999999999999", "-0.999999999999999999", Big(BigDecimal("-0.999999999999999999")))
			      ,("-0.999999999999999999", "-0.999999999999999999", Big("-0.999999999999999999"))
			      ,("-1", "-1.000000000000000000", Big(-0.999999999999999999))   
			      
			       // 69 
			      
			       // (3.141592653589793) 
			       // IMPORTANT to properly construct Big from Double-s, use toStringF and exact scale (15 in our example for Pi)
			      ,("3.141592653589793", "3.141592653589793000", Big(BigDecimal("3.141592653589793")))
			      ,("3.141592653589793", "3.141592653589793000", Big("3.141592653589793"))
			      ,(null, "3.141592653589793", Big(3.141592653589793, 15))   
			       // (-3.141592653589793)
			       // IMPORTANT to properly construct Big from Double-s, use toStringF and exact scale (15 in our example for Pi)
			  	  ,("-3.141592653589793", "-3.141592653589793000", Big(BigDecimal("-3.141592653589793")))
			      ,("-3.141592653589793", "-3.141592653589793000", Big("-3.141592653589793"))
			      ,(null, "-3.141592653589793", Big(-3.141592653589793, 15))   
			       // (+) with integral part exactly on Int bound (4294967296.11011101)
			  	   // IMPORTANT to properly construct Big from Double-s, use toStringF and exact scale			      
			  	  ,("4294967296.11011101", "4294967296.110111010000000000", Big(BigDecimal("4294967296.11011101")))
			      ,("4294967296.11011101", "4294967296.110111010000000000", Big("4294967296.11011101"))
			      ,(null, "4294967296.110111", Big(4294967296.110111, 6))   
			       // (-) with integral part exactly on Int bound (-4294967296.11011101)
			  	   // IMPORTANT to properly construct Big from Double-s, use toStringF and exact scale
			      ,("-4294967296.11011101", "-4294967296.110111010000000000", Big(BigDecimal("-4294967296.11011101")))
			      ,("-4294967296.11011101", "-4294967296.110111010000000000", Big("-4294967296.11011101"))
			      ,(null, "-4294967296.110111", Big(-4294967296.110111, 6))   
			       // (+) maximal (9223372036854775807.999999999999999999) 
			       // IMPORTANT for many significant digits we use score.math.MC
			  	  ,("9223372036854775807.999999999999999999", "9223372036854775807.999999999999999999", Big(BigDecimal("9223372036854775807.999999999999999999", math.MC)))
			      ,("9223372036854775807.999999999999999999", "9223372036854775807.999999999999999999", Big("9223372036854775807.999999999999999999"))
			      // IMPORTANT significant loss of precision! Avoid using Double-s 
			      ,(null, "9223372036854775807.000000", Big(9223372036854775807.999999, 6))   
			       // (-) minimal (-9223372036854775807) 
			       // IMPORTANT for many significant digits we use Math0.MC
			  	  ,("-9223372036854775808.999999999999999999", "-9223372036854775808.999999999999999999", Big(BigDecimal("-9223372036854775808.999999999999999999", math.MC)))
			      ,("-9223372036854775808.999999999999999999", "-9223372036854775808.999999999999999999", Big("-9223372036854775808.999999999999999999"))
			      ,(null, "-9223372036854775808.000000000000000000", Big(-9223372036854775808.999999999999999999))   
			      
			    )) 
			    
    // equals 		    
    assertTrue(3.141592653589793 == Big(3.141592653589793, 15))
	assertTrue(-3.141592653589793 == Big(-3.141592653589793, 15))
	
	// Ordered trait
    assertTrue(0 < Big("100.1")) 
    assertTrue(-0.05 <= Big("100.1")) 
    assertTrue(Big("100.1") > 0) 
    assertTrue(Big("100.1") >= -0.05 )        
    assertFalse(Big(Int.MaxValue.toLong + 11) <= Big(BigDecimal(2147483657L)))
    assertFalse(Big(Int.MaxValue.toLong + 9) >= Big(BigDecimal(2147483657L)))	    
       
    // Operations
    assertEquals(+Big("2.2"), Big("2.2"))	    
    assertEquals(-Big("2.2"), Big("-2.2"))	    
    assertEquals(Big("-2.2").abs, Big("2.2"))	    
    assertEquals(Big("2.2") + Big("2.2"), Big("4.4"))	    
    assertEquals(Big("2.2") - Big("2.2"), 0)	    
    assertEquals(Big("2") * Big("2"), Big("4"))	    
    assertEquals(Big("4") / Big("2"), Big("2"))	    
			
    // Amount => @@, Coef => kk, Percent :)
    
    // Error!
    // (@@(2) + @@(3)) * @@(3)
    // But:
    (@@(2.20) + @@(3.40)) * kk(3/2)
    
    // Passes
    (%%(23.0000) + %%(31.5000)) * @@(300000.00)
    // Error because there is no sense to divide percent by amount
    // (%%(2) + %%(3)) / @@(3)
    
    // Passes
    (kk(2) * kk(3)) / (%%(3) + %%(4)) * @@(3)
    // Error because there is no sense to divide by amount, at all
    // (kk(2) * kk(3)) / (%%(3) + %%(4)) / @@(3)
    
    // E.t.c.
    
  }
 
}