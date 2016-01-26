package com.yukinagae

import org.scalatest.FlatSpec

import com.yukinagae.Scalout._

class ScaloutSpec extends FlatSpec {

  "Scalout" should "fixed path" in {
   assert(routeMatches("/", Map("URI" -> "/"))._1)
   assert(routeMatches("/foo", Map("URI" -> "/foo"))._1)
   assert(routeMatches("/foo/bar", Map("URI" -> "/foo/bar"))._1)
   assert(routeMatches("/foo/bar.html", Map("URI" -> "/foo/bar.html"))._1)
  }

  "Scalout" should "keyword path" in {
   assert(routeMatches("/:x", Map("URI" -> "/foo")) == (true, Some(Map(":x" -> "foo"))))
   assert(routeMatches("/foo/:x", Map("URI" -> "/foo/bar")) == (true, Some(Map(":x" -> "bar"))))
    assert(routeMatches("/a/b/:c", Map("URI" -> "/a/b/c")) == (true, Some(Map(":c" -> "c"))))
    assert(routeMatches("/:a/b/:c", Map("URI" -> "/a/b/c")) == (true, Some(Map(":a" -> "a", ":c" -> "c"))))
  }

  "Scalout" should "urlencoded keywords" in {
    assert(routeMatches("/:x", Map("URI" -> "/foo%20bar")) == (true, Some(Map(":x" -> "foo%20bar"))))
    assert(routeMatches("/:x", Map("URI" -> "/foo+bar")) == (true, Some(Map(":x" -> "foo+bar"))))
    assert(routeMatches("/:x", Map("URI" -> "/foo%5Cbar")) == (true, Some(Map(":x" -> "foo%5Cbar"))))
  }

  "Scalout" should "non-ascii keywords" in {
    assert(routeMatches("/:äñßOÔ", Map("URI" -> "/abc")) == (true, Some(Map(":äñßOÔ" -> "abc"))))
    assert(routeMatches("/:ÁäñßOÔ", Map("URI" -> "/abc")) == (true, Some(Map(":ÁäñßOÔ" -> "abc"))))
    assert(routeMatches("/:ä/:ش", Map("URI" -> "/foo/bar")) == (true, Some(Map(":ä" -> "foo", ":ش" -> "bar"))))
    assert(routeMatches("/:Ä-ü", Map("URI" -> "/baz")) == (true, Some(Map(":Ä-ü" -> "baz"))))
  }

  "Scalout" should "escaped chars" in {
    assert(routeMatches("/\\:foo", Map("URI" -> "/foo")) == (false, None))
    assert(routeMatches("/\\:foo", Map("URI" -> "/:foo")) == (true, None))
    assert(routeMatches("/foo", Map("URI" -> "/foo?x=1")) == (true, None))
    assert(routeMatches("/foo", Map("URI" -> "/foo?x=1&y=2")) == (true, None))
    assert(routeMatches("/foo/:x", Map("URI" -> "/foo/bar?z=1")) == (true, Some(Map(":x" -> "bar"))))
  }

  "Scalout" should "url paths" in {
    assert(routeMatches("http://localhost/", Map("URI" -> "/", "scheme" -> "http", "host" -> "localhost"))._1)
    assert(routeMatches("//localhost/", Map("URI" -> "/", "scheme" -> "http", "host" -> "localhost"))._1)
    assert(routeMatches("//localhost/", Map("URI" -> "/", "scheme" -> "https", "host" -> "localhost"))._1)
  }

  "Scalout" should "url port paths" in {
    assert(routeMatches("http://localhost:8080/", Map("URI" -> "/", "scheme" -> "http", "host" -> "localhost:8080"))._1 == true)
    assert(routeMatches("http://localhost:8080/", Map("URI" -> "/", "scheme" -> "http", "host" -> "localhost:7070"))._1 == false)
  }

}
