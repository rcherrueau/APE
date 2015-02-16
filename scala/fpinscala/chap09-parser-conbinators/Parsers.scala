package fpinscala

// Designe of an interface
trait Parsers[Parser[+_]] {
  self => // Introduces the name `self` to refer to this
          // `Parsers`instance; it's used later in `ParserOps`.

  /** Parser that recognize a specific char.
    *
    * `char('a')` will produce a parser will succeed only if the input
    * is exactly the character 'a'.
    */
  def char(c: Char): Parser[Char] =
    map(string(c.toString))(_.charAt(0))

  /** Running a parser. */
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  /** Parser that recognize a specific string */
  def string(s: String): Parser[String]

  /** Parser that recognize either one or the other string */
  def orString(s1: String, s2: String): Parser[String]

  /** Choose between two parsers
    *
    * First attempting `p1`, and then `p2` if `p1` fails.
    *
    * `or` is the generalization of `orString`. `or` is non strict on
    * the second argument obviously!
    */
  def or[A](a1: Parser[A], a2: => Parser[A]): Parser[A]

  /** Handle repetition */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(Nil)
    else map2(p, listOfN(n-1, p))(_ :: _)

  /** Parser that recognizes zero or more repetitions.
    *
    * Many tries running p, followed by many(p) again and again, and
    * so on until the attempt to parse p fails.
    */
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) | succeed(List())

  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    p flatMap { a => succeed(f(a)) }

  /** Parser that always succeed with the value `a`. */
  // Succeed in terms of map doesn't work with the current
  // implementation of map since map uses succeed and thus result in a
  // circular definition ;)
  def succeed_1[A](a: A): Parser[A] =
    string("") map ( _ => a)

  def succeed[A](a: A): Parser[A]

  /** Parser that recognizes zero or more repitions. */
  def numberOf[A](p: Parser[A]): Parser[Int] =
    // many(p) map (_.size) // Inefficient since `many(p)` construct a
    //                      // `List[A]` only to discard its value and
    //                      // extract its length. See `slice`.
    slice(many(p)) map (_.size) // `size` referencing the `size`
                                // method on string that takes
                                // constant time.

  /** Returns portion of the input inspected by `p` if successful.
    *
    * We ignore the list accumulated by `many` and simply return the
    * portion of the input string matched by the parser.
    * {{{
    *  ?=(run(slice(many(string("a") | string("b"))))("ababab"),
    *     Right("ababab")) check
    * }}}
    */
  def slice[A](p: Parser[A]): Parser[String]

  /** Sequences two parsers.
    *
    * Running `p1` and the `p2` and returns the pair of their results
    * if both succeed.
    */
  def product[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    p1 flatMap { a => p2 map ((a,_)) }

  // Here we implement `map2` in terms of product. We also could do
  // the other choice of implementing `product` in terms of `map2`.
  // See `product` in chap08.
  def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    // (p1 ** p2) map { case (a,b) => f(a,b) }
    // More idiomatic
    (p1 ** p2) map (f.tupled)

  /** Recognizes one or more repetition */
  def many1[A](p: Parser[A]): Parser[List[A]] =
    // Really, `many1(p)` is just _`p` followed by `many(p)`_.
    p.map2(many(p)) (_ :: _)

  // `flatMap` is good for context-sensitive grammar.
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  import scala.util.matching.Regex
  def regex(r: Regex): Parser[String]

  // Using flatMap
  def map2_2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    p1 flatMap { a => p2 map { f(a,_) } }


  /** Adding infix syntax */
  implicit def operators[A](p: Parser[A]): ParserOps[A] =
    ParserOps(p)

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def map2[B,C](p2: Parser[B])(f: (A,B) => C): Parser[C] = self.map2(p,p2)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
    import org.scalacheck.Gen
    import org.scalacheck.Prop._
    import org.scalacheck.Prop

    val unicodeChar: Gen[Char] = Gen.choose(Char.MinValue, Char.MaxValue).
      filter(Character.isDefined)

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll (in) { s => run(p1)(s) == run(p2)(s) }

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(map(p)(identity), p)(in)

    def succeedLaw[A](a: A)(in: Gen[String]): Prop =
      forAll (in) { s => run(succeed(a))(s) == Right(a) }

    // In programming language, a product of types is another,
    // compounded, type in a structure. The values of a product type
    // typically contain several values, called field. The "operands"
    // of the product are types, and the structure of a product type
    // is determined by the fixed order of the operands in the
    // product. Thus, operands of `**` are parsers.
    def productLaw[A,B,C](p1: Parser[A],
                          p2: Parser[B],
                          p3: Parser[C])(in: Gen[String]): Prop =
      // Product is associative.
      equal((p1 ** p2) ** p3, p1 ** (p2 ** p3))(in) &&
      // From theorems for free [Wad89] map is commutative
      equal(p1.map(identity) ** p2.map(identity), (p1 ** p2) map (identity))(in)


    forAll (unicodeChar) {
      c => run(char(c))(c.toString) == Right(c)
    } check

    forAll (Gen.alphaStr) {
      s => run(string(s))(s) == Right(s)
    } check

    forAll (Gen.alphaStr flatMap { s1 => Gen.alphaStr map ((s1, _)) }) {
      case (s1, s2) => run(orString(s1, s2))(s1) == Right(s1)
    } && forAll (Gen.alphaStr flatMap { s1 => Gen.alphaStr map ((s1, _)) }) {
      case (s1, s2) => run(orString(s1, s2))(s2) == Right(s2)
    } check

    forAll (for {
              s1 <- Gen.alphaStr
              s2 <- Gen.alphaStr
            } yield (s1, string(s1), string(s2))) {
      case (s1, ps1, ps2) => run(or(ps1, ps2))(s1) == Right(s1)
    } && forAll (for {
                   s1 <- Gen.alphaStr
                   s2 <- Gen.alphaStr
                 } yield (s2, string(s1), string(s2))) {
      case (s2, ps1, ps2) => run(or(ps1, ps2))(s2) == Right(s2)
    } && forAll (for {
                   c1 <- unicodeChar
                   c2 <- unicodeChar
                 } yield (c1, char(c1), char(c2))) {
      case (c1, pc1, pc2) => run(or(pc1, pc2))(c1.toString) == Right(c1)
    } && forAll (for {
                   c1 <- unicodeChar
                   c2 <- unicodeChar
                 } yield (c2, char(c1), char(c2))) {
      case (c2, pc1, pc2) => run(or(pc1, pc2))(c2.toString) == Right(c2)
    } && forAll (for {
                   s1 <- Gen.alphaStr
                   s2 <- Gen.alphaStr
                 } yield (s1, string(s1), string(s2))) {
      case (s1, ps1, ps2) => run(ps1 | ps2)(s1) == Right(s1)
    } && forAll (for {
                   s1 <- Gen.alphaStr
                   s2 <- Gen.alphaStr
                 } yield (s2, string(s1), string(s2))) {
      case (s2, ps1, ps2) => run(ps1 | ps2)(s2) == Right(s2)
    } && forAll (for {
                   c1 <- unicodeChar
                   c2 <- unicodeChar
                 } yield (c1, char(c1), char(c2))) {
      case (c1, pc1, pc2) => run(pc1 | pc2)(c1.toString) == Right(c1)
    } && forAll (for {
                   c1 <- unicodeChar
                   c2 <- unicodeChar
                 } yield (c2, char(c1), char(c2))) {
      case (c2, pc1, pc2) => run(pc1 | pc2)(c2.toString) == Right(c2)
    } check

    ?=(run(listOfN(3, string("ab") |  string("cad")))("ababcas"),
       Right("ababcas")) &&
    ?=(run(listOfN(3, string("ab") |  string("cad")))("cadabab"),
       Right("cadabab")) &&
    ?=(run(listOfN(3, string("ab") |  string("cad")))("ababab"),
       Right("ababab")) check

    // We ignore the list accumulated by `many` and simply return the
    // portion of the input string matched by the parser.
    ?=(run(slice(many(string("a") | string("b"))))("ababab"),
       Right("ababab")) check

    // A Parser[Int] that recognizes zero or more 'a' characters, and
    // whose result value is the number of 'a' characters it has seen.
    ?=(run(numberOf(char('a')))("aa"),
       Right(2)) &&
    ?=(run(numberOf(char('a')))(""),
       Right(0)) &&
    ?=(run(numberOf(char('a')))("b123"),
       Right(0)) check

    // A Parser[Int] that recognizes *one* or more 'a' characters, and
    // whose result value is the number of 'a' characters it has seen.
    val ptest1 = slice(many1(char('a'))) map (_.size)
    ?=(run(ptest1)("aa"),
       Right(2)) &&
    ?=(run(ptest1)(""),
       Left(ParseError)) &&
    ?=(run(ptest1)("b123"),
       Left(ParseError)) check

    // A Perser that recognizes *zero or more 'a'*, followed by *one
    // or more 'b'* and which results in the pair of counts of
    // character seen.
    val ptest2 =
      (slice(many(char('a'))) map (_.size)) **
        (slice(many1(char('b'))) map (_.size))
    ?=(run(ptest2)("bbb"),
       Right((0,4))) &&
    ?=(run(ptest2)("aaaab"),
       Right((4,1))) &&
    ?=(run(ptest2)("aaaa"),
       Right((0,0))) &&
    ?=(run(ptest2)(""),
       Right((0,0))) check

    // A context sensitive parser
    val pCtxSensi =
      regex("""\d""".r) flatMap { s => listOfN(s.toInt, char('a')) }
    ?=(run(pCtxSensi)("0"),
       Right("0")) &&
    ?=(run(pCtxSensi)("1a"),
       Right("1a")) &&
    ?=(run(pCtxSensi)("4aaaa"),
       Right("4aaaa")) &&
    ?=(run(pCtxSensi)(""),
       Left(ParseError)) &&
    ?=(run(pCtxSensi)("0a"),
       Left(ParseError)) &&
    ?=(run(pCtxSensi)("4a"),
       Left(ParseError)) check


  }
}

// We need an instance of ParseError.
case class ParseError()
