package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("wordOccurrences: MiMamaMeMima") {
    assert(wordOccurrences("MiMamaMeMima") === List(('a', 3), ('e', 1), ('i', 2), ('m', 6)))
  }

  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: Mi mama me mima, Mi mama me ama") {
    assert(sentenceOccurrences(List("Mi", "mama", "me", "mima", "mi", "mama", "me", "ama"))
      === List(('a', 7), ('e', 2), ('i', 3), ('m', 11)))
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }

  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: ab") {
    val ab = List(('a', 1), ('b', 1))
    val abcomb = List(
      List(),
      List(('a', 1)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)))
    assert(combinations(ab).toSet === abcomb.toSet)
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2)))
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("combinations: aaa") {
    val aaa = List(('a', 3))
    val aaacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('a', 3)))
    assert(combinations(aaa).toSet === aaacomb.toSet)
  }

  test("combinations: cba") {
    val abc = List(('c', 1), ('b', 1), ('a', 1))
    val abccomb = List(
      List(),
      List(('a', 1)),
      List(('b', 1)),
      List(('c', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 1), ('c', 1)),
      List(('b', 1), ('c', 1)),
      List(('a', 1), ('b', 1), ('c', 1)))
    assert(combinations(abc).toSet === abccomb.toSet)
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract Nil - Nil") {
    assert(subtract(Nil, Nil) === Nil)
  }

  test("subtract a - Nil") {
    val a = List(('a', 1))
    assert(subtract(a, Nil) === a)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez"))
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

  test("sentence anagrams: Yes, man") {
    val sentence = List("Yes", "man")
    val anas = List(
      List("en", "as", "my"),
      List("en", "my", "as"),
      List("man", "yes"),
      List("men", "say"),
      List("as", "en", "my"),
      List("as", "my", "en"),
      List("sane", "my"),
      List("Sean", "my"),
      List("my", "en", "as"),
      List("my", "as", "en"),
      List("my", "sane"),
      List("my", "Sean"),
      List("say", "men"),
      List("yes", "man"))
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

}
