package com.yukinagae

import scala.util.parsing.combinator._
import scala.util.matching.Regex

sealed abstract trait RouteParser extends JavaTokenParsers {
  def route: Parser[Any]
  var map: Map[String, String] = Map.empty
}

class BeforeRouteParser extends RouteParser {

  def route: Parser[Any] = (scheme | part) ~ part.*
  def scheme: Parser[String] = """(https?:)?//""".r
  def part: Parser[Any] = literal | escaped | param
  def literal: Parser[String] = """(:[^\p{L}_*{}\\]|[^:*{}\\])+""".r
  def escaped: Parser[String] = """\\.""".r
  def param: Parser[Any] = key ^^ {
    case b =>
      val v = b.toString.replaceAll("[(~)]", "")
      map += v -> v
      b
  }
  def key: Parser[Any] = ":" ~ """([\p{L}_][\p{L}_0-9-]*)""".r
}

class AfterRouteParser(path: String, keys: List[String]) extends RouteParser {

  val literal = """(:[^\p{L}_*{}\\]|[^:*{}\\])+"""

  def route: Parser[Any] = part ~ param.?
  def part: Parser[Any] = {
    var p = path
    keys.foreach { key =>
      p = p.replace(key, literal)
    }
    p.r ^^ {
      case accessed => {
	val in = accessed.split("""\?""").head
	val paths = path.split("/")
	val ins = in.split("/")
	val zipped = paths.zip(ins)
	zipped.foreach { z =>
	  if(keys.contains(z._1)) {
	    map += z._1 -> z._2
	  }
	}
	in
      }
    }
  }
  def param: Parser[Any] = """?""" ~ """.*""".r

}

case class CompiledRoute(path: String, keys: List[String], isAbsolute: Boolean) {

  def routeMatches(request: Map[String, String]): (Boolean, Option[Map[String, String]]) = {
    val pathInfo = if(isAbsolute) requestUrl(request) else request.get("URI").get
    val pathRule = if(isAbsolute && !path.startsWith("http")) request.get("scheme").get + ":" + path else path
    val parser = new AfterRouteParser(pathRule, keys)
    val result = parser.parseAll(parser.route, pathInfo)
    result match {
      case parser.Success(result, _) => (true, if(parser.map.isEmpty) None else Some(parser.map))
      case _ => (false, None)
    }
  }

  def requestUrl(request: Map[String, String]): String = request.get("scheme").get + """://""" + request.get("host").get + request.get("URI").get
}

object Scalout {

  def routeCompile(path: String): CompiledRoute = {
    val parser = new BeforeRouteParser
    parser.parseAll(parser.route, path)
    val ast = parser.map
    val keys = ast.values.toList
    val isAbsolute = absoluteUrl(path)
    CompiledRoute(path, keys, isAbsolute)
  }

  def absoluteUrl(path: String): Boolean = {
    val r = """(https?:)?//.*""".r
    path match {
      case r(_*) => true
      case _ => false
    }
  }

  def routeMatches(path: String, request: Request): (Boolean, Option[Map[String, String]]) = {
    val requestMap = Map("scheme" -> request.scheme, "host" -> request.serverName, "URI" -> request.URI)
    routeMatches(path, requestMap)
  }

  def routeMatches(path: String, request: Map[String, String]): (Boolean, Option[Map[String, String]]) = {
    val compiled = routeCompile(path)
    compiled.routeMatches(request)
  }
}
