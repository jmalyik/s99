/**
 * http://aperiodic.net/phil/scala/s-99/
 * 
 * S-99: Ninety-Nine Scala Problems
 * 
 * @author magic
 * 
 */
object S99 {
  
  /**
   * P01
   */
  def last[T](inp:List[T]):T= inp.last
 
  /**
   * P02
   */
  def penultimate[T](inp:List[T]):T= inp.drop(inp.size - 2).head
  
  /**
   * P03
   */
  def nth[T](k:Int, inp:List[T]):T= inp.drop(k).head

  /**
   * P04
   */
  def length[T](inp:List[T]):Int= inp.size
  
  /**
   * P05
   */
  
  def reverse[T](in:List[T]):List[T]= in match {
    case x::xs => reverse(xs) ++ List(x)
    case List(x) => List(x)
    case Nil => Nil
  }
    
  /**
   * P06
   */
  def isPalindrome(inp:List[Int]):Boolean={
    reverse(inp) == inp
  }
  
  /**
   * P07
   */
  def flatten(inp:List[Any]):List[Any]= {
    inp.flatMap(xs => {xs match {
      case a:Int => List(a)
      case a:List[Any] => flatten(a)
    }})
  }
    
  /**
   * P08 v1
   */
  def compress[T](inp:List[T]):List[T]={
    def concat[T](in:List[T], t:T):List[T]={
      if(in.isEmpty)
        List(t)
      else
        if(in.last == t)
          in
        else
          in ++ List(t)
    }
    
    def compressIn[T](in:List[T], inp:List[T]):List[T]= inp match {
      case x::xs => compressIn(concat(in, x), xs)
      case List(x) => concat(in, x)
      case Nil => in
    }
    compressIn(List(), inp)
  }
      
  /**
   * P08 v2
   */
  def compress2[T](inp:List[T]):List[T]={
    inp.foldLeft(List[T]())((b,a) => {
      if(b.isEmpty || b.last != a)
        b ++ List(a)
      else
        b
    })
  }
      
  /**
   * P09
   */
  def pack[T](inp:List[T]):List[List[T]]= {
    inp.foldLeft(List[List[T]]())((acc, item) => {
      if(acc.isEmpty)
        List(List(item))
      else
        if(acc.last.last == item)
          acc.take(acc.size - 1) ++ List(acc.last ++ List(item))
        else
          acc ++ List(List(item))
    }) 
  }
  
  /**
   * P10
   */
  
  def encode[T](inp:List[T]):List[(Int,T)]={
    pack(inp).map(l => (l.size, l.head))  
  }
  
  /**
   * P11
   */
  def encodeModified[T](inp:List[T]):List[Any]={
    pack(inp).map(l => {
      if(l.size == 1)
        l.head
      else
        (l.size, l.head)
    })  
  }
  
  /**
   * P12 
   */
  def decode[T](inp:List[(Int,T)]):List[T]={
    def add(inp:List[T], tuple: (Int,T)):List[T]= tuple._1 match {
      case 0 => return inp
      case _ => return add(inp ++ List(tuple._2), (tuple._1 - 1, tuple._2))
    }
    inp.foldLeft(List[T]())((acc, pair) => add(acc, pair))
  }
  
  /**
   * P13 
   */
  def encodeDirect[T](inp:List[T]):List[(Int,T)]={
    pack(inp).map(l => (l.size, l.head))  
  }
  
  /**
   * P14
   */
  def duplicate[T](inp:List[T]):List[T]={
    inp.map(x=>List(x,x)).flatten
  }
  
  /**
   * P15
   */
  def duplicateN[T](n:Int, inp:List[T]):List[T]={
    def add[T](n:Int, item:T, in:List[T]):List[T]= n match {
     case 0 => in
     case _ => add(n - 1, item, in ++ List(item))
    }    
    inp.map(x => add(n, x, List[T]())).flatten
  }

  /**
   * P16
   */
  def drop[T](n:Int, inp:List[T]):List[T]={
    inp.zipWithIndex.filter(item => (item._2 - 2) % n != 0).map(item => item._1)   
  }
  
  /**
   * P17
   */
  def split[T](n:Int, inp:List[T]):(List[T],List[T])={
    //inp.splitAt(n)
    def split[T](n:Int, inp:List[T], out:(List[T],List[T])):(List[T],List[T])= n match {
      case 0 => (out._1, out._2 ++ inp)
      case _ => split(n - 1, inp.tail, (out._1 ++ List(inp.head),out._2))
    }
    split(n, inp, (List[T](),List[T]()))
  }
  
  /**
   * P18
   */
  def slice[T](from:Int, to:Int, inp:List[T]):List[T]={
    inp.drop(from).take(to - from)
  }
  
  /**
   * P19
   */
  def rotate[T](n:Int, inp:List[T]):List[T]= {
    if(n > 0){
      rotate(n - 1, inp.tail ++ List(inp.head))
    }else if(n < 0){
      rotate(n + 1, List(inp.last) ++ inp.take(inp.size - 1))
    }else{
      inp
    }
  }
  
  /**
   * P20
   */
  def removeAt[T](pos:Int, inp:List[T]):(List[T], T)={
    val splitted = inp.splitAt(pos)
    (splitted._1 ++ splitted._2.tail, splitted._2.head)
  }
}  