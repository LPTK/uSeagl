package common

case class CyclicDependency() extends Exception("Illegal cyclic access")

//class Cyclic[+T](expr: Cyclic[T] => T, toStr: T => String = ((_:T) => "[Cyclic value]")) extends Unique {
class Cyclic[+T](expr: Cyclic[T] => T, toStr: T => String = ((t:T) => t.toString)) extends Unique {
  def this(x: T) = this(_ => x)
  
  private val _value = expr(this)
  def value =
    if (!wasComputerYet) throw CyclicDependency()
    else _value
  
//  def map(f: T => T) = 
    
  def wasComputerYet = _value != null
  
  override def toString =
    if (_value == null) "[Cyclic in resolution]"
    else toStr(value)
}

object Cyclic {
  
//  def apply[T](x: T) = new Cyclic[T](_ => x)
  def unapply[T](x: Cyclic[T]) = Some(x.value)
  
//  implicit def plain2cyclic[T](t: T) = new Cyclic[T](_ => t)
  
  implicit def cyclic2plain[T](c: Cyclic[T]) = c.value
  
}






