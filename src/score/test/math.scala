package score.test

import scala.Array.canBuildFrom

import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Test

import score.math
import score.math._

class math {

  @Test def phi() {    
    assertEquals(1.0, PHI - 1.0 / PHI, ERR_DOUBLE)
  }
  
  @Test def fib() {
    val argArr = Array[Int](0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    val expect = Array[Int](0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    
    assertArrayEquals(
      argArr.map(math.fib _).map((el) => { val obj: AnyRef = el ; obj}),
      expect.map((el) => { val obj: AnyRef = BigDecimal(el) ; obj})
    )
    
  }
  
}