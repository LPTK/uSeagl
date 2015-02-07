package common

object Monads {
  
  type Monad[+T, This[_]] = {
    def map[U](f: T => U): This[U]
    def flatMap[U](f: T => This[U]): This[U]
  }
  
  
  implicit class MonadHelper[T, M[T] <: Monad[T, M]](val mon: M[T]) extends AnyVal {
    //self: Monad[T, M] =>
    def foreach(f: T => Unit) { mon.map(f) }
  }
  
  /** Useful to emulate a container monad with exactly one element: */
  case class Single[+T](obj: T) {
    def flatMap[S](f: T=>Single[S]) = Single(f(obj).obj)
    def map[S](f: T=>S) = Single(f(obj))
    def get = obj
  }
  
  
  
  
//  /** A composable monad */
////  type CompMonad[+T, This[_]] = Monad[T, This] {
////    def test: Int
////  }
//  abstract class CompMonad[+T,  MA[_] <: Monad[T, MA],  MB[_] <: Monad[T, MB]]
//  (ma: MA[T], mb: MB[T]) {
//    
//    def unitA[T](x: T): MA[T]
//    def unitB[T](x: T): MB[T]
//    
//    def getA[T](x: MA[T]): T
//    
////    def trans[Tr[_],T](x: Tr[MA[Tr[T]]]): MA[Tr[Tr[T]]]
//    def nest[Tr[_],T](x: MA[Tr[T]]): Tr[MA[T]]
//    def extract[Tr[_],T](x: Tr[MA[T]]): MA[Tr[T]]
//    
//    
//    
////    def map[U](f: T => U): MA[MB[U]] = unit1(unit2(x))
//    def map[U](f: T => U): MA[MB[U]] = unitA(mb map f)
//    
////    def flatMap[U](f: T => MA[MB[U]]) = unitA(mb flatMap {x => getA(f(x))})
//    def flatMap[U](f: T => MA[MB[U]]) = {
//      val v = mb flatMap {x =>
//        val w = nest(f(x))
//        w
//      }
//      extract(v)
//    }
//    
//  }
  
  
}