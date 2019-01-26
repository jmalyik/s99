import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.exceptions.TestFailedException

/**
 * @author magic
 */
@RunWith(classOf[JUnitRunner])
class N99ScalaTest extends FunSuite {
  
  def myAssert[T](a:T, b:T):Unit={
    try{
      assert(a == b)
    }catch{
      // printing the assert error to the log
      case e: TestFailedException => {
        println(e.getMessage)
        throw e
      }
    }
  }
  
   test("S99.P01") {
     myAssert(S99.last(List(1, 1, 2, 3, 5, 8)), 8)
     println("P01 OK")
   }

   test("S99.P02") {
     myAssert(S99.penultimate(List(1, 1, 2, 3, 5, 8)), 5)
     println("P02 OK")
   }

   test("S99.P03") {
     myAssert(S99.nth(2, List(1, 1, 2, 3, 5, 8)), 2)
     println("P03 OK")
   }
   
   test("S99.P04") {
     myAssert(S99.length(List(1, 1, 2, 3, 5, 8)), 6)
     println("P04 OK")
   }
   
   test("S99.P05") {
     myAssert(S99.reverse(List(1, 1, 2, 3, 5, 8)), List(8, 5, 3, 2, 1, 1))
     println("P05 OK")
   }
   
   test("S99.P06") {
     myAssert(S99.isPalindrome(List(1, 2, 3, 2, 1)), true)
     myAssert(S99.isPalindrome(List(1, 2, 3, 3, 1)), false)
     println("P06 OK")
   }
   
   test("S99.P07") {
     myAssert(S99.flatten(List(List(1, 1), 2, List(3, List(5, 8)))), List(1, 1, 2, 3, 5, 8))
     println("P07 OK")
   }
   
   test("S99.P08") {
     myAssert(S99.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)), List('a, 'b, 'c, 'a, 'd, 'e))
     println("P08 OK")
   }
   
   test("S99.P09") {
     myAssert(S99.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)), List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
     println("P09 OK")
   }
   
   test("S99.P10") {
     myAssert(S99.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)), List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
     println("P10 OK")
   }

   test("S99.P11") {
     myAssert(S99.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)), List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
     println("P11 OK")
   }
   
   test("S99.P12") {
     myAssert(S99.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))), List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
     println("P12 OK")
   }
   
   test("S99.P13") {
     myAssert(S99.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)), List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
     println("P13 OK")
   }

   test("S99.P14") {
     myAssert(S99.duplicate(List('a, 'b, 'c, 'c, 'd)), List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
     println("P14 OK")
   }
   
   test("S99.P15") {
     myAssert(S99.duplicateN(3, List('a, 'b, 'c, 'c, 'd)), List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
     println("P15 OK")
   }
   
   test("S99.P16") {
     myAssert(S99.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)), List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
     println("P16 OK")
   }
   
   test("S99.P17") {
     myAssert(S99.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)), (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
     println("P17 OK")
   }
   
   test("S99.P18") {
     myAssert(S99.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)), List('d, 'e, 'f, 'g))
     println("P18 OK")
   }
   
   test("S99.P19-1") {
     myAssert(S99.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
     println("P19-1 OK")
   }
   
   test("S99.P19-2") {
     myAssert(S99.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)), List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
     println("P19-2 OK")
   }
   
   test("S99.P20") {
     myAssert(S99.removeAt(1, List('a, 'b, 'c, 'd)), (List('a, 'c, 'd),'b))
     println("P20 OK")
   }
   
   test("S99.P21") {
     myAssert(S99.insertAt('new, 1, List('a, 'b, 'c, 'd)), List('a, 'new, 'b, 'c, 'd))
     println("P21 OK")
   }   
   
   test("S99.P22") {
     myAssert(S99.range(4,9), List(4, 5, 6, 7, 8, 9))
     println("P22 OK")
   }
   
   test("S99.P23") {
     val inp = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
     val ret = S99.randomSelect(3, inp)
     myAssert(ret.size, 3)
     for(r <- ret){
       assert(inp.contains(r))
     }
     println("P23 OK")
   }   

   test("S99.P24") {
     val inp = List.range(1, 6)
     val ret = S99.lotto(5, 5)
     myAssert(ret.size, 5)
     for(r <- ret){
       try{
         assert(inp.contains(r))
       }catch{
         case e:TestFailedException => {
           println(e.getMessage())
           throw e
         }
       }
     }
     println("P24 OK")
   }
   
   test("S99.P25-1") {
     val inp = List.range(1, 6)
     val ret = S99.randomPermute(inp)
     myAssert(ret.sorted, inp)
     println("P25-1 OK")
   }
   
   test("S99.P25-2") {
     val inp = List.range(1, 6)
     val ret = S99.randomPermute(inp)
     myAssert(ret.sorted, inp)
     println("P25-2 OK")
   }

   test("S99.P26") {
     val ret = S99.combinations(2, List('a, 'b, 'c))
     myAssert(ret, List(List('a,'b), List('a,'c), List('b,'c)))
     myAssert(ret.size, 3)
     val ret2 = S99.combinations(3, List('a, 'b, 'c, 'd))
     myAssert(ret2, List(
         List('a, 'b, 'c), List('a,'b, 'd),
         List('a, 'c, 'd),
         List('b, 'c, 'd)
         ))
     myAssert(ret2.size, 4)
     println("P26 OK")
   }

   test("S99.P27") {
     val inp = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
     val ret = S99.group3(inp)
     myAssert(ret.size, 1260) // 9! / (3! * 7!) * 7! / (3! * 4!)
     println("P27 OK")
   }
}