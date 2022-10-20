package paths
import scala.annotation.tailrec


@main def main = {
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

  def if_key_head(element: String): String = {
    unwrap(Extractors.key(element)).get(0)
  }

  def if_key_tail(element: String): String = {
    unwrap(Extractors.key(element)).get(1)
  }

  def if_symbol_head(symbol: Char)(element: String): String = {
    unwrap(Extractors.symbol(symbol)(element)).get(0)
  }

  def if_symbol_tail(symbol: Char)(element: String): String = {
    unwrap(Extractors.symbol(symbol)(element)).get(1)
  }

  def if_quoted_head(element: String): String = {
    unwrap(Extractors.quoted(element)).get(0)
  }

  def if_quoted_tail(element: String): String = {
    unwrap(Extractors.quoted(element)).get(1)
  }

  println(if_key_tail(data_ww))

}