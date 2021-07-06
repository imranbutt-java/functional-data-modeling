package fdm

object phantom_types {

  /**
   * EXERCISE 1
   *
   * Add a phantom type parameter to `Socket`, which can keep track of the state of the socket:
   * either `Created` or `Connected`. Use this type parameter in the methods below to improve their
   * type safety.
   */
  type Created
  type Connected
  trait Socket[State]

  def createSocket(): Socket[Created]                                            = ???
  def connectSocket(address: String, socket: Socket[Created]): Socket[Connected] = ???
  def readSocket(socket: Socket[Connected]): Array[Byte]                         = ???

  /**
   * EXERCISE 2
   *
   * Introduce a type parameter to this data type to model whether a `Path` is a file, a directory,
   * or either a file or a directory. Use this to improve the type safety of the `readFile` and
   * `listDirectory` methods.
   *
   * Note: In order to ensure safety, you will have to make the constructors of `Path` private, so
   * that outside code cannot call those constructors with just any type parameter. This is a
   * requirement of using phantom types properly.
   */
  type File
  type Directory
  sealed trait Path
  object Path {
    case object Root                                   extends Path
    final case class ChildOf(path: Path, name: String) extends Path
  }

  def readFile(path: Path): String          = ???
  def listDirectory(path: Path): List[Path] = ???

  /**
   * EXERCISE 3
   *
   * Phantom types work well with intersection types (`with` in Scala 2.x). They have many
   * wide-ranging applications, including making builder patterns safer.
   *
   * Introduce a phantom type parameter for `PersonBuilder`, and arrange such that the setters
   * add a new type into a type intersection, and that the build method requires both age and name
   * to be set in order to build the person.
   *
   * Note: As before, you must make the constructors of the data type with a phantom type parameter
   * private, so they cannot be called from outside code.
   */
  type Age
  type Name
  type Addresses
  final case class PersonBuilder[+Set] private (
    age: Option[Int],
    name: Option[String],
    addresses: Option[List[String]]
  ) {
    def addresses(list: List[String]): PersonBuilder[Set with Addresses] = copy(addresses = Some(list))

    def age(v: Int): PersonBuilder[Set with Age] = copy(age = Some(v))

    def name(s: String): PersonBuilder[Set with Name] = copy(name = Some(s))

    def build(implicit ev: Set <:< Name with Age): Person = Person(name.get, age.get, addresses.getOrElse(Nil))
  }
  object PersonBuilder {
    val builder: PersonBuilder[Any] = PersonBuilder(None, None, None)
  }
  import PersonBuilder._

  final case class Person(name: String, age: Int, addresses: List[String])

  builder.age(32).name("John Smith").build
}
