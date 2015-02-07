package common

case class CyclicDependency() extends Exception("Illegal cyclic access")

class Cyclic[T](expr: Cyclic[T] => T) extends Unique {
  private val _value = expr(this)
  def value =
    if (_value == null) throw CyclicDependency()
    else _value
  
//  def map(f: T => T) = 
}

object Cyclic {
  
//  implicit def plain2cyclic[T](t: T) = new Cyclic[T](_ => t)
  
//  implicit def cyclic2plain[T](c: Cyclic[T]) = c.value
  
}






