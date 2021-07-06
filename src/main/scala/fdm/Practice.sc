case class Person(name: String, age: Int)
case class A(n: Unit = 2, d: Int)
A(A((), 3), 2)

val X = (2, "Hi")
val Y = ("Hi", 2)

X == Y