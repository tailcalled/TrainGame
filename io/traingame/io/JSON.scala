package traingame.io

sealed trait JSON {
  override def toString = show(0)
  def show(indentation: Int): String = toString
  def bool = this.asInstanceOf[JSON.Bool]
  def text = this.asInstanceOf[JSON.Text]
  def number = this.asInstanceOf[JSON.Number]
  def array = this.asInstanceOf[JSON.Array]
  def dict = this.asInstanceOf[JSON.Dict]
}
object JSON {
  case class Bool(value: Boolean) extends JSON {
    override def toString = value.toString
  }
  case class Text(value: String) extends JSON {
    override def toString = "\"" + value.flatMap {
      case x => x.toString
    } + "\""
  }
  case class Number(value: Double) extends JSON {
    def intValue = if (value.toInt.toDouble == value) value.toInt else throw new Exception
    override def toString = value.toString
  }
  case class Array(values: Vector[JSON]) extends JSON {
    override def show(indentation: Int) =
      "[" + values.map(("\n" + "\t" * (indentation + 1)) + _.show(indentation + 1)).mkString(",") + "]"
  }
  case class Dict(values: Map[String, JSON]) extends JSON {
    def apply(key: String) = values(key)
    def get(key: String) = values.get(key)
    override def show(indentation: Int) =
      "{" + values.map {
        case (k, v) => ("\n" + "\t" * (indentation + 1)) + Text(k).show(indentation + 1) + ": " + v.show(indentation+1)
      }.mkString(",") + "}"
  }
  case object Nil extends JSON {
    override def toString = "null"
  }
  
  object parser {
    import parsers._
    private object tokens {
      sealed trait Token
      case class BooleanToken(value: Boolean) extends Token
      case class StringToken(value: String) extends Token
      case class NumberToken(value: Double) extends Token
      case object ArrayStartToken extends Token
      case object ArrayEndToken extends Token
      case object DictStartToken extends Token
      case object DictEndToken extends Token
      case object DictAssocToken extends Token
      case object SepToken extends Token
      case object NilToken extends Token
      lazy val keychar =
        for {
          ch <- char
          if (ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z')
        } yield ch
      lazy val keyword =
        keychar.star.map(_.mkString)
      lazy val text = {
        val validStringChar = for {
          ch <- char
          if (ch != '\\')
        } yield ch.toString
        val escape = lit("\\") *> char.flatMap {
          case 'n' => done("\n")
          case 'r' => done("\r")
          case '\"' => done("\"")
          case '\\' => done("\\")
        }
        val stringChar = validStringChar ++ escape
        lit("\"") *> stringChar.star.map(_.mkString) *< lit("\"") map StringToken
      }
      lazy val number = {
        val numChar = for {
          ch <- char
          if (ch >= '0' && ch <= '9')
        } yield ch
        val numSeq = numChar.plus.map(_.mkString)
        val num = lit("-").opt ** numSeq ** (lit(".") *> numSeq).opt map {
          case sign ** int ** frac => (sign.getOrElse("") + int + frac.map("." + _).getOrElse("")).toDouble
        }
        num map NumberToken
      }
      lazy val _token =
        keyword.flatMap {
          case "true" => done(BooleanToken(true))
          case "false" => done(BooleanToken(false))
          case "null" => done(NilToken)
          case "NaN" => done(NumberToken(Double.NaN))
          case x => fail(s"invalid keyword $x")
        } ++
        text ++
        number ++
        lit("[").replace(ArrayStartToken) ++
        lit("]").replace(ArrayEndToken) ++
        lit("{").replace(DictStartToken) ++
        lit("}").replace(DictStartToken) ++
        lit(":").replace(DictAssocToken) ++
        lit(",").replace(SepToken)
      lazy val whitespace = char.withFilter(_.isWhitespace)
      lazy val token = whitespace.star *> _token *< whitespace.star
    }
    
    private def tok(token: tokens.Token) = tokens.token.withFilter(_ == token)
    
    lazy val json: Parser[JSON] = bool ++ text ++ number ++ array ++ dict ++ nil
    lazy val bool = for {
      tokens.BooleanToken(value) <- tokens.token
    } yield Bool(value)
    lazy val text = for {
      tokens.StringToken(value) <- tokens.token
    } yield Text(value)
    lazy val number = for {
      tokens.NumberToken(value) <- tokens.token
    } yield Number(value)
    lazy val array = tok(tokens.ArrayStartToken) *> json.sepSeq(tok(tokens.SepToken)) *< tok(tokens.ArrayEndToken) map Array
    private lazy val dictMapping = text *< tok(tokens.DictAssocToken) ** json map {
      case Text(key) ** value => key -> value
    }
    lazy val dict = tok(tokens.DictStartToken) *> dictMapping.sepSeq(tok(tokens.SepToken)) *< tok(tokens.DictEndToken) map {
      mappings => Dict(Map(mappings: _*))
    }
    lazy val nil = tok(tokens.NilToken) replace Nil
  }
}