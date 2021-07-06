package fdm

import java.net.Inet4Address
import java.sql.Timestamp

/**
 * Sometimes we don't want to take the time to model data precisely. For example, we might want to
 * model an email address with a string, even though most strings are not valid email addresses.
 *
 * In such cases, we can save time by using a smart constructor, which lets us ensure we model
 * only valid data, but without complicated data types.
 */
object smart_constructors {

  object newtypes {
    type User

    final case class UserId(value: Long) {
      def render = s"uid:${value}"
    }
    object UserId {
      def fromString(value: String): Option[UserId] = ???
    }

    final case class ProductId(value: Long)
    final case class OrderId(value: Long)

    def lookupUser(userId: UserId): Option[User] = ???

    def completeTransaction(userId: UserId, productId: ProductId, orderId: OrderId) = ???

    val oid = OrderId(123L)
    val pid = ProductId(123L)
    val uid = UserId(123L)

    completeTransaction(uid, pid, oid)

  }

  sealed abstract case class Email private (value: String)
  object Email {
    def fromString(email: String): Option[Email] =
      if (email.matches("""/\w+@\w+.com""")) Some(new Email(email) {}) else None
  }

  /**
   * EXERCISE 1
   *
   * Create a smart constructor for `NonNegative` which ensures the integer is always non-negative.
   */
  sealed abstract case class NonNegative private (value: Int)
  object NonNegative {
    def from(value: Int): Option[NonNegative] = if (value > 0) Some(new NonNegative(value) {}) else None
  }

  /**
   * EXERCISE 2
   *
   * Create a smart constructor for `Age` that ensures the integer is between 0 and 120.
   */
  sealed abstract case class Age private (value: Int)

  object Age {
    def apply(value: Int): Option[Age] = Option.when(value >= 0 && value <= 120)(new Age(value) {})
  }

  /**
   * EXERCISE 3
   *
   * Create a smart constructor for password that ensures some security considerations are met.
   */
  sealed abstract case class Password private (value: String)
}

object applied_smart_constructors {

  /**
   * EXERCISE 1
   *
   * Identify the weaknesses in this data type, and use smart constructors (and possibly other
   * techniques) to correct them.
   */
  final case class BankAccount(id: String, name: String, balance: Double, opened: java.time.Instant)

  /**
   * EXERCISE 2
   *
   * Identify the weaknesses in this data type, and use smart constructors (and possibly other
   * techniques) to correct them.
   */
  case class Person(age: Age, name: String, salary: Salary)
  sealed abstract case class Age(a: Int)
  object Age {
    def fromInt(a: Int): Option[Age] = if (a >= 0 && a < 120) Some(new Age(a) {}) else None
  }

  sealed abstract case class Salary(a: Double)
  object Salary {
    def fromDouble(sal: Double): Option[Salary] = if (sal < 0) None else Some(new Salary(sal) {})
  }

  /**
   * EXERCISE 3
   *
   * Identify the weaknesses in this data type, and use smart constructors (and possibly other
   * techniques) to correct them.
   */
  final case class SecurityEvent(machine: String, timestamp: java.time.Instant, eventType: EventType)
  final case class InvalidEventType()

  sealed trait EventType
  object EventType {

    final case class PortScanning() extends EventType

    final case class DenialOfService() extends EventType

    final case class InvalidLogin() extends EventType

    def fromInt(value: Int): Either[InvalidEventType, EventType] = value match {
      case 0 => Right(PortScanning())
      case 1 => Right(DenialOfService())
      case 2 => Right(InvalidLogin())
      case _ => Left(InvalidEventType())
    }
  }
}
