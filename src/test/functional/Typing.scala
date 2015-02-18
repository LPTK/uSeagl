package functional

import org.scalatest.junit.JUnitSuite
import org.junit.Assert.assertEquals
import org.junit.Test
import java.io.PrintStream
import java.io.ByteArrayOutputStream
import org.junit.Ignore
import org.junit.Before

/**
 * TODO:
 * 
 *   List with get
 *   
 *   mutrec funs
 *   
 *   polymrec fun
 *   
 *   
 *   region inference stuff...
 *   
 *   
 *   c = new Cell(nil)
     id[Cell[Int]](c)
     id[Cell[Unit]](c) // should unif-fail
 *   
 *   
 *   
 *   
 */
class Typing extends JUnitSuite {
  
  
  @Test def test {
    assertEquals(2, 1+1)
  }
  
  
}

