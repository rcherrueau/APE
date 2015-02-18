package fpinscala

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._ // Gives access to all the combinators

    // Tokens
    val lbrace = token(char('{'))
    val rbrace = token(char('}'))
    val lsquare = token(char('['))
    val rsquare = token(char(']'))
    val comma = token(char(','))
    val colon = token(char(':'))
    val doubleQuote = char('"')

    // An empty value, using the word `null`.
    def jNull: Parser[JNull.type] =
      string("null") map { _ => JNull }

    // A signed decimal number that may contain a fractional part and
    // may use exponential E notation. JSON does not allow non-numbers
    // like NaN, nor does it make any distinction between integer and
    // floating-point. (Even though JavaScript uses a double-precision
    // floating-point format for all its numeric values, other
    // languages implementing JSON may encode numbers differently)
    def jNumber: Parser[JNumber] = {
      // Regexp from
      // http://hermes.readthedocs.org/en/latest/cookbook/json.html
      val number =
        regex("""-?([1-9][0-9]+|0)(\.[0-9]+)?([eE][+-]?[0-9]+)?""".r)

      number map { d => JNumber(d.toDouble) }
    }

    // A sequence of zero or more Unicode characters. Strings are
    // delimited with double-quotation marks and support a backslash
    // escaping syntax.
    def jString: Parser[JString] = {
      // Regexp from
      // http://hermes.readthedocs.org/en/latest/cookbook/json.html
      val string = regex("""([^"\\]|\\[\\"/bfnrt]|\\u\d{4})*""".r)

      (doubleQuote *> string <* doubleQuote).slice map {
        s => JString(s) }
    }

    // Either of the values `true` or `false`.
    def jBool: Parser[JBool] = {
      val bool = string("true") | string("false")

      bool map (_ match {
                  case "true" => JBool(true)
                  case _ => JBool(false)
                })
    }

    // An ordered list of zero or more values, each of which may be of
    // any type. Arrays use square bracket notation with elements
    // being comma-separated.
    def jArray: Parser[JArray] = {
      val value = jNull | jNumber | jString | jBool | jArray | jObject

      lsquare *> mkList(value)(comma) <* rsquare map {
        l => JArray(l.toIndexedSeq)
      }
    }

    // An unordered collection of name/value pairs where the names
    // (also called keys) are strings. Since objects are intended to
    // represent associative arrays, it is recommended, though not
    // required, that each key is unique within an object. Objects are
    // delimited with curly brackets and use commas to separate each
    // pair, while within each pair the colon ':' character separates
    // the key or name from its value.
    def jObject: Parser[JObject] = {
      val key =
        doubleQuote *> regex("""\w+""".r) <* doubleQuote <* colon
      val value =
        jNull | jNumber | jString | jBool | jArray | jObject

      lbrace *> mkList(key ** value)(colon) <* rbrace map {
        l: List[(String, JSON)] => JObject(l.toMap)
      }
    }

    jObject
  }
}
