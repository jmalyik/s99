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
  
  /**
   * P21
   */
  def insertAt[T](newItem:T, pos:Int, inp:List[T]):List[T]={
    val splitted = inp.splitAt(pos)
    splitted._1 ++ List(newItem) ++ splitted._2
  }
  
  /**
   * P22
   */
  def range(start:Int, end:Int):List[Int]={
     if(start < end){
       start :: range(start + 1, end)
     }else{
       List(start)
     }
  }
  
  /**
   * P23
   */
  def randomSelect[T](n:Int, inp:List[T]):List[T]= n match {
    case 0 => Nil
    case _ => {
      val r = scala.util.Random
      val res = removeAt(r.nextInt(inp.size), inp)
      List(res._2) ++ randomSelect(n - 1, res._1)
    }
  }
  
  /**
   * P24
   */
  def lotto(n:Int, end:Int):List[Int]= {
    val range = List.range(1, end + 1)
    randomSelect(n, range)
  }
  
  /**
   * P25 v1
   */
  def randomPermute[T](inp:List[T]):List[T]= {
      randomSelect(inp.size, inp)
  }
  /**
   * P25 v2
   */
  def randomPermute2[T](inp:List[T]):List[T]= inp match {
    case Nil => Nil
    case List(x) => List(x)
    case x::xs => {
      val selected = randomSelect(1, inp)
      val slices = inp.splitAt(inp.indexOf(selected.head))
       selected ++ randomPermute(slices._1 ++ slices._2.tail)
    }
  }
  
  /**
   * P26
   */
  class Tree[T](var level:Int, val items:List[T], var inp:List[T]){
      def getDirectChildren():List[Tree[T]]={
        inp.zipWithIndex.map(indexed => {
          // inp.take(indexed._2) ++ inp.drop(indexed._2 + 1) => drop the nth item
          new Tree(level + 1, items ++ List(indexed._1), inp.take(indexed._2) ++ inp.drop(indexed._2 + 1))
        })
      }
      
      def getChildrenAtLevel(childLevel: Int):List[Tree[T]]={
        if(level == (childLevel - 1)){
          getDirectChildren()
        }else{
          getDirectChildren().map(_.getChildrenAtLevel(childLevel)).flatten
        }
      }
      
      def sortedItems():List[T]={
        items.sortBy(_.toString)
      }
  }
  
  def combinations[T](n:Int, inp:List[T]):List[List[T]]= {
    inp.zipWithIndex.map(indexed =>  
      new Tree(1, List(indexed._1), inp.take(indexed._2) ++ inp.drop(indexed._2 + 1))
    ).map(treeNode => treeNode.getChildrenAtLevel(n))
    .flatten.map(n => n.sortedItems).toSet.toList
  }
  
  /**
   * P27
   */
  def group3(inp:List[String]):List[List[List[String]]]= {
   val listOfFirstTwoGroups:List[(List[String], List[List[String]])] = 
     combinations(2, inp).map(groupA => (groupA, combinations(3, inp.filterNot(groupA.contains(_)))))
   val listOfFirstTwoGroupsFlatten:List[(List[String], List[String])] = listOfFirstTwoGroups.flatMap(t1 => 
     t1._2.map(secondGroup => (t1._1, secondGroup))
   )
   listOfFirstTwoGroupsFlatten.map(t2 => List(t2._1) ++ List(t2._2) ++ 
       List(inp.filterNot(inpItem => t2._1.contains(inpItem) || t2._2.contains(inpItem))))
  }
  
  def group(config:List[Int], inp:List[String]):List[List[List[String]]]= {
   val listOfFirstTwoGroups:List[(List[String], List[List[String]])] = 
     combinations(config(0), inp).map(groupA => (groupA, combinations(config(1), inp.filterNot(groupA.contains(_)))))
   val listOfFirstTwoGroupsFlatten:List[(List[String], List[String])] = listOfFirstTwoGroups.flatMap(t1 => 
     t1._2.map(secondGroup => (t1._1, secondGroup))
   )
   listOfFirstTwoGroupsFlatten.map(t2 => List(t2._1) ++ List(t2._2) ++ 
       List(inp.filterNot(inpItem => t2._1.contains(inpItem) || t2._2.contains(inpItem))))
  }
}  