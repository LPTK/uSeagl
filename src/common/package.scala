package object common {
  
  
  
  
  
  implicit class TypeSafeEquatable[T](private val value : T) extends AnyVal {
//    private def value = _value_
    def ===[U >: T <: T] (that: U) = value == that
    def ==~[U <: T] (that: U) = value == that
    def ~== (that: T) = value == that
    
    def =/=[U >: T <: T] (that: U) = value != that
    def =/~[U <: T] (that: T) = value != that
    def ~/= (that: T) = value != that
    
  }
  
  
  
  
  
  def Id[T] = (a:T) => a
  
  
  
  
  
  
  
  def Pokémon[T](value: => T) = try {
    Left(value)
  } catch {
    case e: Throwable => Right(e) // gotta catch 'em all
  }
  
  
}