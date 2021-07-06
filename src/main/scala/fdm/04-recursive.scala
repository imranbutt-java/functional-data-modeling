package fdm

/**
 * Scala data types constructed from enums and case classes may be *recursive*: that is, a top-
 * level definition may contain references to values of the same type.
 */
object recursive {

  /**
   * EXERCISE 1
   *
   * Create a recursive data type that models a user of a social network, who has friends; and
   * whose friends may have other friends, and so forth.
   */
  final case class User(name: String, hobbies: List[String], friends: () => Set[User])

  /**
   * EXERCISE 2
   *
   * Create a recursive data type that models numeric operations on integers, including addition,
   * multiplication, and subtraction.
   */
  sealed trait NumericExpression
  object NumericExpression {
    final case class Literal(value: Int)                                              extends NumericExpression
    final case class Addition(exp1: NumericExpression, exp2: NumericExpression)       extends NumericExpression
    final case class Multiplication(exp1: NumericExpression, exp2: NumericExpression) extends NumericExpression
    final case class Subtraction(exp1: NumericExpression, exp2: NumericExpression)    extends NumericExpression
  }

  /**
   * EXERCISE 3
   *
   * Create a `EmailTrigger` recursive data type which models the conditions in which to trigger
   * sending an email. Include common triggers like on purchase, on shopping cart abandonment, etc.
   */
  sealed trait EmailTrigger

  object EmailTrigger {
    case object OnPurchase                                        extends EmailTrigger
    final case class And(left: EmailTrigger, right: EmailTrigger) extends EmailTrigger
    final case class Or(left: EmailTrigger, right: EmailTrigger)  extends EmailTrigger
  }
}

/**
 * As Scala is an eager programming language, in which expressions are evaluated eagerly, generally
 * from left to right, top to bottom, the tree-like data structures created with case classes and
 * enumerations do not contain cycles. However, with some work, you can model cycles. Cycles are
 * usually for fully general-purpose graphs.
 */
object cyclically_recursive {
  final case class Snake(food: Snake)

  /**
   * EXERCISE 1
   *
   * Create a snake that is eating its own tail. In order to do this, you will have to use a
   * `lazy val`.
   */
  lazy val snake: Snake = Snake(snake)

  /**
   * EXERCISE 2
   *
   * Create two employees "Tim" and "Tom" who are each other's coworkers. You will have to change
   * the `coworker` field from `Employee` to `() => Employee` (`Function0`), also called a "thunk",
   * and you will have to use a `lazy val` to define the employees.
   */
  final case class Employee(name: String, coworker: () => Employee)

  lazy val Tim: Employee = Employee("Tim", () => Tom)
  lazy val Tom: Employee = Employee("Tom", () => Tim)

  /**
   * EXERCISE 3
   *
   * Develop a List-like recursive structure that is sufficiently lazy, it can be appended to
   * itself.
   */
  sealed trait LazyList[+A] extends Iterable[A]
  object LazyList {

    case object Empty extends LazyList[Nothing] {
      override def iterator: Iterator[Nothing] = Iterator.empty
    }

    final case class Cons[A](a: A, list: LazyList[A]) extends LazyList[A] {
      override def iterator: Iterator[A] = Iterator(a) ++ list.iterator
    }

    def apply[A](el: A): LazyList[A] = LazyList.Cons(el, LazyList.Empty)

    // The syntax `=>` means a "lazy parameter". Such parameters are evaluated wherever they are
    // referenced "by name".
    def concat[A](left: => LazyList[A], right: => LazyList[A]): LazyList[A] =
      fold[A, LazyList[A]](left)(right)((list, a) => LazyList.Cons(a, list))

    def fold[A, B](list: => LazyList[A])(initial: B)(f: (B, A) => B): B = list match {
      case Empty         => initial
      case Cons(a, list) => fold(list)(f(initial, a))(f)
    }
  }

  lazy val infiniteList: LazyList[Int] = LazyList.concat(LazyList(1), infiniteList)
}
