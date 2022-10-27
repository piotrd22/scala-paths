package paths
import scala.annotation.tailrec

object Components {
  
  val data = io.Source
    .fromResource("example.txt")
    .getLines
    .toList
    .mkString(" ")

  val data_ww = data
    .filter(!_.isWhitespace)


  def unwrap(element: Extraction): Option[(String, String)] = {
    element match {
      case Found(x: String, y: String) => Some((x, y))
      case _ => None
    }
  }

  //  println(unwrap(Extractors.quoted("\"hahaha\"")).get(1) == "")

  def if_key_head(element: String): String = {
    if (unwrap(Extractors.key(element)).isDefined) {
      unwrap(Extractors.key(element)).get(0)
    }
    else ""
  }

  def if_key_tail(element: String): String = {
    if (unwrap(Extractors.key(element)).isDefined) {
      unwrap(Extractors.key(element)).get(1)
    }
    else ""
  }

  def if_symbol_head(symbol: Char)(element: String): String = {
    if (unwrap(Extractors.symbol(symbol)(element)).isDefined) {
      unwrap(Extractors.symbol(symbol)(element)).get(0)
    }
    else ""
  }

  def if_symbol_tail(symbol: Char)(element: String): String = {
    if (unwrap(Extractors.symbol(symbol)(element)).isDefined) {
      unwrap(Extractors.symbol(symbol)(element)).get(1)
    }
    else ""
  }

  def if_quoted_head(element: String): String = {
    if (unwrap(Extractors.quoted(element)).isDefined) {
      unwrap(Extractors.quoted(element)).get(0)
    }
    else ""
  }

  def if_quoted_tail(element: String): String = {
    if (unwrap(Extractors.quoted(element)).isDefined) {
      unwrap(Extractors.quoted(element)).get(1)
    }
    else ""
  }

  def to_componentes(data: String): List[List[String]] = {
    @tailrec def helper(data: String, acc: List[String] = List(), acc2: List[List[String]] = List()): List[List[String]] = {
      data match {
        case "" => acc2.reverse
        case x if (if_key_head(x) != "") => helper(if_key_tail(x), if_key_head(x) :: acc, acc2)
        case x if (if_symbol_head('=')(x) != "") => helper(if_symbol_tail('=')(x), if_symbol_head('=')(x) :: acc, acc2)
        case x if (if_symbol_head('[')(x) != "") => helper(if_symbol_tail('[')(x), if_symbol_head('[')(x) :: acc, acc2)
        case x if (if_quoted_head(x) != "") => helper(if_quoted_tail(x), if_quoted_head(x) :: acc, acc2)
        case x if (if_symbol_head(']')(x) != "" && acc.count(_ == "[") != (acc.count(_ == "]") + 1)) =>
          helper(if_symbol_tail(']')(x), if_symbol_head(']')(x) :: acc, acc2)
        case x if (if_symbol_head(']')(x) != "" && acc.count(_ == "[") == (acc.count(_ == "]") + 1)) =>
          helper(if_symbol_tail(']')(x), List(), (if_symbol_head(']')(x) :: acc).reverse :: acc2)
      }
    }
    helper(data)
  }
}