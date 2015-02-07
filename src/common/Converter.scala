package common

import Monads._

trait Converter {
  
  type Result[+T] <: Monad[T, Result]
  def unit[T](a: => T): Result[T]
  
}

trait DirectConverter extends Converter {
  
  final type Result[+T] = Single[T]
  final def unit[T](a: => T): Result[T] = Single(a)
  
}

trait FallibleConverter extends Converter {
  
  final def unit[T](a: => T) = succeed(a)
  def succeed[T](a: => T): Result[T]
  def fail[T](e: Throwable): Result[T]
  
}







