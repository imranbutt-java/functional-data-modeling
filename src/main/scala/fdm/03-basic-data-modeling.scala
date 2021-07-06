package fdm

import org.w3c.dom.html.HTMLButtonElement

import java.sql.Timestamp
import scala.annotation.tailrec

/**
 * The following exercises test your ability to model various entities using case classes.
 */
object product_modeling {

  /**
   * EXERCISE 1
   *
   * Using a case class, create a model of a product, which has a name, description, and a price.
   *
   */
  final case class Product(name: String, description: String, price: Double)

  /**
   * EXERCISE 2
   *
   * Using a case class, create a model of a a user profile, which has a picture URL, and text-
   * based location (indicating the geographic area where the user is from).
   */
  final case class UserProfile(picture: String, location: String)

  /**
   * EXERCISE 3
   *
   * Using a case class, create a model of an item that can be posted on LinkedIn's feed. This
   * item contains a subject and some text.
   */
  final case class FeedItem(subject: String, text: String)

  /**
   * EXERCISE 4
   *
   * Using a case class, create a model of an event, which has an event id, a timestamp, and a
   * map of properties (String/String).
   */
  final case class Event(id: String, timestamp: Timestamp, properties: Map[String, String])
}

/**
 * The following exercises test your ability to model various entities using enums.
 */
object sum_modeling {

  /**
   * EXERCISE 1
   *
   * Using an enum, create a model of a color, which could be `Red`, `Green`, `Blue`, or `Custom`,
   * and if `Custom`, then it should store `red`, `green`, and `blue` components individually, as
   * an integer (`Int`) value.
   */
  sealed trait Color
  object Color {
    case object Red                                 extends Color
    case object Green                               extends Color
    case object Blue                                extends Color
    final case class Custom(r: Int, g: Int, b: Int) extends Color
  }

  /**
   * EXERCISE 2
   *
   * Using an enum, create a model of an web event, which could be either a page load for a certain
   * URL, a click on a particular button, or a click to a specific URL.
   */
  sealed trait WebEvent
  object WebEvent {
    final case class PageLoad(url: String)                    extends WebEvent
    final case class URLClick(url: String)                    extends WebEvent
    final case class ButtonClick(buttonId: HTMLButtonElement) extends WebEvent
  }

  /**
   * EXERCISE 3
   *
   * Using an enum, create a model of an age bracket, which could be baby, child, young adult,
   * teenager, adult, mature adult, or senior adult.
   */
  sealed trait AgeBracket
  object AgeBracket {
    case object Baby        extends AgeBracket
    case object Child       extends AgeBracket
    case object YoungAdult  extends AgeBracket
    case object Teenager    extends AgeBracket
    case object Adult       extends AgeBracket
    case object MatureAdult extends AgeBracket
    case object SeniorAdult extends AgeBracket
  }

  /**
   * EXERCISE 4
   *
   * Using an enum, create a model of a step in a JSON pipeline, which could be transform,
   * aggregate, or save to file.
   * aggregate.
   */
  type Json
  sealed trait JsonPipelineStep
  object JsonPipeline {
    final case class Transform(fn: Json => Json)             extends JsonPipelineStep
    final case class Aggregate[S1, S2](fn: (S1, Json) => S2) extends JsonPipelineStep
    final case class Save[A](fn: (A, String) => Json)        extends JsonPipelineStep
  }
}

/**
 * The following exercises test your ability to model various entities using both case classes and
 * enums.
 */
object mixed_modeling {

  /**
   * EXERCISE 1
   *
   * Using only case classes and enums, create a model of an order for an e-commerce platform, which
   * would consist of a number of items, each with a certain price, and an overall price, including
   * shipping and handling charges.
   */
  case class Item(name: String, desc: String, price: Double)
  case class Order(itemsOrdered: Map[Item, Double], overallPrice: Double)

  /**
   * EXERCISE 2
   *
   * Using only case classes and enums, create a model of an `Email`, which contains a subject,
   * a body, a recipient, and a from address.
   */
  final case class EmailMessage(subject: String, body: String, recipient: String, fromAddress: String)

  /**
   * EXERCISE 3
   *
   * Using only case classes and enums, create a model of a page layout for a content-management
   * system, which could consist of predefined elements, such as a news feed, a photo gallery,
   * and other elements, arranged in some well-defined way relative to each other.
   */
  sealed trait PageLayout
  object PageLayout {
    final case class NewsFeed(headline: String, content: String, imageUrl: String) extends PageLayout
    final case class PhotoGallery(location: String)                                extends PageLayout
    final case class Ad(imageUrl: String, text: String, productLink: String)       extends PageLayout
  }

  /**
   * EXERCISE 4
   *
   * Using only case classes and enums, create a model of a rule that describes the conditions for
   * triggering an email to be sent to a shopper on an e-commerce website.
   */
  sealed trait EmailTriggerRule
  object EmailTriggerRule {
    final case class Purchase(orderId: Long)
    final case class AddToCartAndLeave(cartItems: List[Item])
    final case class Notification(message: String)
  }
}

object basic_dm_graduation {
  sealed trait Command
  object Command {
    case object Look                      extends Command
    case object Quit                      extends Command
    final case class LookAt(what: String) extends Command
    final case class Go(where: String)    extends Command
    final case class Take(item: String)   extends Command
    final case class Drop(item: String)   extends Command
    final case class Fight(who: String)   extends Command

    def fromString(string: String): Option[Command] =
      string.trim.toLowerCase.split("\\s+").toList match {
        case "go" :: where :: Nil          => Some(Go(where))
        case "look" :: Nil                 => Some(Look)
        case "look" :: "at" :: what :: Nil => Some(LookAt(what))
        case "take" :: item :: Nil         => Some(Take(item))
        case "drop" :: item :: Nil         => Some(Drop(item))
        case "fight" :: who :: Nil         => Some(Fight(who))
        case ("quit" | "exit") :: Nil      => Some(Quit)
        case _                             => None
      }
  }

  /**
   * EXERCISE
   *
   * Using case classes and sealed traits (and whatever other data types you like), design a game
   * world that can be used to play a simple text-based role playing game.
   *
   * The data type should model the player, non-player characters, and items available to pick up
   * or drop in the game world.
   */
  final case class Coordinate(lat: Float, long: Float)
  final case class State(
    playerState: CharacterState,
    npcs: List[CharacterState],
    map: GameMap
  )
  final case class GameMap(network: Map[Scene, List[Scene]])
  final case class Scene(items: Map[Item, Int], name: String, description: String)
  final case class CharacterState(
    name: String,
    scene: Scene,
    inventory: Map[Item, Int],
    level: Int
  )
  final case class Item(name: String, description: String, itemType: ItemType)
  sealed trait ItemType
  object ItemType {
    final case class Weapon(durability: Int, damage: Int) extends ItemType
    final case class HealthPotion(points: Int)            extends ItemType
    final case class Magical()                            extends ItemType
  }

  def describe(state: State): String =
    "You are playing this game."

  def process(state: State, command: Command): (String, Option[State]) =
    if (command == Command.Quit) ("You quitted", None)
    else (s"You did: ${command}, which had no effect.", Some(state))

  def main(args: Array[String]): Unit = {
    @tailrec
    def loop(state: State): Unit = {
      println(describe(state))

      val line = scala.io.StdIn.readLine()

      Command.fromString(line) match {
        case None =>
          println("Unrecognized command")
          loop(state)

        case Some(command) =>
          process(state, command) match {
            case (output, next) =>
              println(output)
              next match {
                case Some(value) => loop(value)
                case None        => println("Goodbye!")
              }
          }
      }
    }

    //loop()
  }
}
