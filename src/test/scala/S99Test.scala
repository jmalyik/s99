import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

/**
 * @author magic
 */
@RunWith(classOf[JUnitRunner])
class N99ScalaTest extends FunSuite {
  
   test("S99.P01") {
     assert(S99.last(List(1, 1, 2, 3, 5, 8)) == 8)
     println("P01 OK")
   }

   test("S99.P02") {
     assert(S99.penultimate(List(1, 1, 2, 3, 5, 8)) == 5)
     println("P02 OK")
   }

   test("S99.P03") {
     assert(S99.nth(2, List(1, 1, 2, 3, 5, 8)) == 2)
     println("P03 OK")
   }
   
   test("S99.P04") {
     assert(S99.length(List(1, 1, 2, 3, 5, 8)) == 6)
     println("P04 OK")
   }
   
   test("S99.P05") {
     assert(S99.reverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1))
     println("P05 OK")
   }
   
   test("S99.P06") {
     assert(S99.isPalindrome(List(1, 2, 3, 2, 1)) == true)
     assert(S99.isPalindrome(List(1, 2, 3, 3, 1)) == false)
     println("P06 OK")
   }
   
   test("S99.P07") {
     assert(S99.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
     println("P07 OK")
   }
   
   test("S99.P08") {
     assert(S99.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
     println("P08 OK")
   }
   
   test("S99.P09") {
     assert(S99.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
     println("P09 OK")
   }
   
   test("S99.P10") {
     assert(S99.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
     println("P10 OK")
   }

   test("S99.P11") {
     assert(S99.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
     println("P11 OK")
   }
   
   test("S99.P12") {
     assert(S99.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
     println("P12 OK")
   }
   
   test("S99.P13") {
     assert(S99.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
     println("P13 OK")
   }

   test("S99.P14") {
     assert(S99.duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
     println("P14 OK")
   }
   
   test("S99.P15") {
     assert(S99.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
     println("P15 OK")
   }
   
   test("S99.P16") {
     assert(S99.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
     println("P16 OK")
   }
   
   test("S99.P17") {
     assert(S99.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
     println("P17 OK")
   }
}