package regex

object Tests extends App {
  
  import RegexRegions._
  
  println(Cat('x, Or(Or('a, Or('b1, 'b2) ), 'c) ))
  
  println(Nil includes Nil, true)
  println(Nil includes 'x, false)
  println('x includes Nil, false)
  
  println(Cat(Or('x, 'b), 'y) includes Cat('x, 'y), true)
  println(Cat('x, 'y) includes Cat(Or('x, 'b), 'y), false)
  
  
  println(Cat('x, 'y) includes Cat('x, 'y), true)
  println(Cat(Rep('x), 'y) includes Cat('x, 'y), true)
  println(Cat(Rep('x), 'y) includes Cat('x, Rep('y)), false)
  
  
  println(Or(Cat('x, 'b), Cat('x, 'c)) includes Cat('x, Or('b,'c)), true)
  
  
  println(Cat('x, 'y).consume)
  println(Cat(Rep('x), 'y).consume)
  
  println
  
  implicit def plain2full(r: Regex) = Full(r)
  
  println(Nil -- Nil, Empty)
  println(Or('x, 'y) -- Sym('x), Sym('y))
  println(Cat(Or(Cat('x,'a), 'y),'z) -- Sym('x), Cat('y,'z))
  
  
  
  
}


