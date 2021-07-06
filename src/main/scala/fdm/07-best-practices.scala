package fdm

import java.time.{ Instant, LocalDateTime }

/**
 * Some anti-patterns emerge when doing data modeling in Scala. Learning to spot and refactor such
 * anti-patterns is a very valuable skill that will keep data models precise and easy to maintain
 * in the face of changing business requirements.
 *
 */
/**
 * One anti-pattern common in object-oriented data modeling technique is to define different
 * interfaces for different aspects of a data model, relying on inheritance (which is a form of
 * type intersection) to combine multiple aspects into a new type. While this approach is flexible,
 * it's often _too_ flexible, allowing combinations that don't make sense, and making it hard to
 * deal with combinations in a principled way.
 *
 * In this case, refactoring to enumerations (sealed traits _without_ overlaps) and case classes
 * can not only disallow combinations that don't make sense, but give us a principled way to
 * handle all the combinations that do make sense.
 */
object eliminate_intersection {

  final case class Event(eventId: String, eventType: EventType, timeStamp: Option[java.time.Instant])
  sealed trait EventType
  object EventType {
    final case class UserEvent(userId: String)     extends EventType
    final case class DeviceEvent(deviceId: String) extends EventType
  }

  object adt {

    /**
     * EXERCISE 1
     *
     * Create a pure case class / enumeration model of `Event`, which permits events which are
     * user events OR device events (but NOT both), and which permits events that have timestamps
     * or lack timestamps; but which always have event ids.
     */
    final case class EventId private (value: String)
    final case class UserId private (value: String)
    sealed abstract case class UserAge private (value: Int)
    object UserAge {
      def create(value: Int): Either[String, UserAge] =
        if (value >= 18 && value <= 120) Right(new UserAge(value) {}) else Left("Wrong user age")
    }
    final case class DeviceId private (value: String)

    final case class EventTime private (value: Instant)
    sealed trait EventType
    object EventType {
      final case class UserEvent private (userId: UserId, age: UserAge) extends EventType
      final case class DeviceEvent(userId: DeviceId)                    extends EventType
    }
    import EventType._

    final case class EventError(reason: String) extends EventType
    final case class Event(eventId: EventId, eventType: EventType, timeStamp: Option[Instant] = None)

    val event = for {
      age   <- UserAge.create(20)
      event = Event(EventId("1"), UserEvent(UserId("1"), age))
    } yield event

  }
}

/**
 * Another anti-pattern is when all cases of an enumeration share the same field, with the same
 * type and the same meaning. This duplication makes maintenance of the hierarchy more difficult,
 * and also makes it difficult to take advantage of built-in functionality like the `copy` method
 * that comes for free with all case classes.
 *
 * In this case, we can apply an "extract product" refactoring, which turns the enumeration into a
 * case class, and pushes the differences between the cases deeper, into a field of the case class
 * (modeled with a new enumeration).
 */
object extract_product {

  /**
   * EXERCISE 1
   *
   * The following cases of the `AdvertisingEvent` enum all share the `pageUrl` and `data` fields,
   * which have the same type and meaning in each case. Apply the _extract product_ refactoring so
   * that  `AdvertisingEvent` becomes a case class, which stores `pageUrl` and `data`, and
   * introduce a new field called `eventType`, which captures event-specific details for the
   * different event types.
   */
  final case class AdvertisingEvent(pageUrl: String, data: String, eventType: EventType)

  sealed trait EventType
  object EventType {
    final case class Impression()               extends EventType
    final case class Click(elementId: String)   extends EventType
    final case class Action(actionName: String) extends EventType
  }

  /**
   * EXERCISE 2
   *
   * The following cases of `Card` enum all share the `points` field, and this field has the same
   * type and meaning in each case. Apply the _extract product_ refactoring so that `Card` becomes a
   * case class, and introduce a new field called `cardType`, which captures card-specific details
   * for the different event types.
   */
  final case class Card(points: Int, cardType: CardType)
  sealed trait CardType
  object CardType {
    final object Clubs    extends CardType
    final object Diamonds extends CardType
    final object Spades   extends CardType
    final object Hearts   extends CardType
  }

  /**
   * EXERCISE 3
   *
   * Apply the _extract product_ refactoring to `Event`.
   */
  object extract {
    final case class Event(timestamp: java.time.Instant, eventType: EventType)
    sealed trait EventType
    object EventType {
      final case class User(userId: String, userEventType: UserEventType)         extends EventType
      final case class Device(deviceId: String, deviceEventType: DeviceEventType) extends EventType
    }
    sealed trait UserEventType
    object UserEventType {
      final case class Purchase(amount: Double) extends UserEventType
      final case class Return(itemId: String)   extends UserEventType
    }
    sealed trait DeviceEventType
    object DeviceEventType {
      final case class Refund(orderId: String, refundAmount: Double) extends DeviceEventType
    }
  }
}

/**
 * Another anti-pattern occurs when many fields of a case class are optional (or `null`). This
 * indicates there is a "missing enumeration" that can be extracted from the case class, which
 * bundles fields that always appear together.
 *
 * In this case, the "extract sum" refactoring involves identifying patterns of optional fields
 * that always appear together, and pulling them out into the cases of a new enumeration.
 */
object extract_sum {
  final case class Job(title: String, salary: Double)

  final case class Enrollment(university: String, credits: Int, year: java.time.YearMonth)

  /**
   * EXERCISE 1
   *
   * Extract out a missing enumeration from the following data type.
   */
  final case class Person(name: String, studentType: StudentType)
  sealed trait StudentType
  object StudentType {
    final case class FullTimeStudent(school: Enrollment)                      extends StudentType
    final case class PartTimeStudent(job: Job, employmentDate: LocalDateTime) extends StudentType
  }

  /**
   * EXERCISE 2
   *
   * Extract out a missing enumeration from the following data type.
   */
  final case class Event(timestamp: Instant, eventType: EventType)

  sealed trait EventType
  object EventType {
    final case class DeviceEvent private (deviceId: String, reading: Double)      extends EventType
    final case class UserEvent private (userId: String, eventType: UserEventType) extends EventType
  }

  sealed trait UserEventType
  object UserEventType {
    final case class UserClickEvent(click: String)       extends UserEventType
    final case class UserPurchaseEvent(purchase: String) extends UserEventType
  }

  /**
   * EXERCISE 3
   *
   * Extract out a missing enumeration from the following data type.
   */
  final case class CreditCard(
    digit16: Option[Digit16],      // For VISA
    digit15: Option[Digit15],      // For AMEX
    securityCode3: Option[Digit3], // For AMEX
    securityCode4: Option[Digit4]  // For VISA
  )

  final case class Digit16(group1: Digit4, group2: Digit4, group3: Digit4, group4: Digit4)

  final case class Digit15(group1: Digit4, group2: Digit4, group3: Digit4, group4: Digit3)

  final case class Digit4(v1: Digit, v2: Digit, v3: Digit, v4: Digit)
  final case class Digit3(v1: Digit, v2: Digit, v3: Digit)

  sealed trait Digit {
    case object _0 extends Digit
    case object _1 extends Digit
    case object _2 extends Digit
    case object _3 extends Digit
    case object _4 extends Digit
    case object _5 extends Digit
    case object _6 extends Digit
    case object _7 extends Digit
    case object _8 extends Digit
    case object _9 extends Digit
  }
}

/**
 * Wildcard pattern matches on enumerations that change frequently are an anti-pattern, because
 * when new cases are added, there is no way for the compiler to remind developers to update
 * business logic involving the enumeration, which leads to subtle bugs in application logic.
 *
 * In this case, we can apply a "eliminate wildcard" refactoring that involves explicitly matching
 * against all cases of an enumeration.
 */
object eliminate_wildcard {
  sealed trait EscalationPoint
  object EscalationPoint {
    final object Phone    extends EscalationPoint
    final object LogEvent extends EscalationPoint

    def shoutAt(point: EscalationPoint, message: String) = point match {
      case x: Phone => sendText(x, message)
      case LogEvent => logEvent(message)
    }
  }
  trait Phone extends EscalationPoint
  def sendText(phone: Phone, message: String): Unit = println(s"Sending a text to ${phone}: ${message}")
  trait Log extends EscalationPoint
  def logEvent(message: String): Unit = println(s"Event Logged: $message")

  /**
   * EXERCISE 1
   *
   * Eliminate the wild card pattern match, and instead move to matching each case individually.
   */
  import EscalationPoint._
  import Event._
  def pageDeveloper(event: Event, point: EscalationPoint): Unit =
    event match {
      case ServerDown => shoutAt(point, "The server is down, please look into it right away!")
      case ServiceRestarted =>
        shoutAt(point, "The server is down due to anonymous reason, please look into it right away!")
    }

  /**
   * EXERCISE 2
   *
   * Add another type of event, such as `BillingOverage`, which might require immediate attention
   * from a devops engineer. The Scala compiler will make you update existing code safely.
   */
  sealed trait Event
  object Event {
    case object ServerDown       extends Event
    case object ServiceRestarted extends Event
    case object BillingOverage   extends Event
  }
}

/**
 * Another anti-pattern is when code uses pattern matching to perform a so-called "type case".
 * Type cases are when the runtime type of a value is checked, rather than deconstructing the
 * value using one of the cases of the enumeration. Matching cases of an enumeration helps ensure
 * that no implementation details leak into pattern matches, discourages using traits in any other
 * way than for enumerations, and helps give developers a chance to update logic when new
 * fields are added to case classes.
 */
object eliminate_typecase {
  sealed trait Event
  sealed trait IdentifiedEvent extends Event {
    def id: String
  }
  final case class Click(href: String)                extends Event
  final case class Purchase(id: String, item: String) extends IdentifiedEvent

  /**
   * EXERCISE 1
   *
   * Refactor this code to eliminate the type case. You may have to refactor the data model too.
   */
  def logIdentifiedEvents(event: Event): Unit =
    event match {
      case identified: IdentifiedEvent => println("User event: " + identified.id)
      case _                           =>
    }

}

/**
 * Another anti-pattern is creating many variables of the same type in a given scope by pattern
 * matching on recursive data structures and giving values of the same type different names.
 *
 * In this case, we can apply the "shadow variable" refactoring and deliberately choose to
 * shadow variables of the same name and type in outer scopes, reducing the possibility that we
 * accidentally refer to a variable in an outer scope.
 */
object nested_shadowing {

  /**
   * EXERCISE 1
   *
   * Identify the bug caused by too many variables of the same type in the same scope, and fix it
   * by applying the shadow variable refactoring.
   */
  def count[A](list: List[A]): Int =
    list match {
      case Nil       => 0
      case _ :: tail => 1 + count(tail)
    }

  sealed trait UserBehavior
  object UserBehavior {
    case object Purchase                                                 extends UserBehavior
    case object Return                                                   extends UserBehavior
    case object Anything                                                 extends UserBehavior
    final case class Sequence(first: UserBehavior, second: UserBehavior) extends UserBehavior
    final case class Not(behavior: UserBehavior)                         extends UserBehavior
  }

  import UserBehavior._

  /**
   * EXERCISE 2
   *
   * Identify the bug caused by too many variables of the same type in the same scope, and fix it
   * by applying the shadow variable refactoring.
   */
  def analyzePattern(b: UserBehavior): Boolean =
    b match {
      case Purchase => true
      case Return   => true
      case Anything => false
      case Sequence(first, second) =>
        analyzePattern(first) || analyzePattern(second)
      case Not(value) =>
        analyzePattern(b)
    }
}

/**
 * Another (often but not always) anti-pattern is embedding an "empty" term into a sum type.
 * This leads to an inability to describe all the terms except that "empty" term, forcing all code
 * to have to deal with the "empty" case.
 *
 * In this case, the solution is to delete the empty, and use `Option[XYZ]` at higher levels, where
 * code requires an empty type.
 */
object delete_empty {

  /**
   * EXERCISE 1
   *
   * Apply the "delete empty" refactoring to allow more precision.
   */
  sealed trait Currency
  object Currency {
    case object None                               extends Currency
    final case class USD(dollars: Int, cents: Int) extends Currency
    final case class EUR(euros: Int, cents: Int)   extends Currency
  }

  /**
   * EXERCISE 2
   *
   * Apply the "delete empty" refactoring to allow more precision.
   */
  sealed trait DataSource
  object DataSource {
    case object NoSource                                                extends DataSource
    final case class S3(bucket: String)                                 extends DataSource
    final case class JDBC(url: String, properties: Map[String, String]) extends DataSource
  }
}
