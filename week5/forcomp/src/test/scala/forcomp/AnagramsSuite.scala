package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite  {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }


  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: abcd e a e") {
    assert(sentenceOccurrences(List("abcd", "e", "a", "e")) === List(('a', 2), ('b', 1), ('c', 1), ('d', 1), ('e', 2)))
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

  test("pre combinations") {
    println(charCombinations(List(('a', 2), ('b', 2))))
  }

  test("sort combinations print") {
    println("sort combinations print 1")

    val list1 = List(List(('a', 2)), List(('a', 1)), List())
    val list2 = sortOccurrencesList(list1)
    list2.foreach(println)
    assert(list2 === List(List(), List(('a', 1)), List(('a', 2))))

    println("sort combinations print 2")

    val list3 = List(List(('a', 2), ('b', 3)), List(('a', 1)), List(), List(('a', 1), ('b', 5)), List(), List(('a', 2)), List(('a', 2), ('b', 4)), List(('a', 2), ('b', 3)), List(('b', 1)))
    val list4 = sortOccurrencesList(list3)
    list4.foreach(println)
    assert(list4 === List(List(), List(), List(('a', 1)), List(('a', 1), ('b', 5)), List(('a', 2)), List(('a', 2), ('b', 3)), List(('a', 2), ('b', 3)), List(('a', 2), ('b', 4)), List(('b', 1))))
  }


  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: lard - r 2") {
    val lard = List(('a', 3), ('d', 1), ('l', 1), ('r', 2))
    val r = List(('a', 1), ('r', 1))
    val lad = List(('a', 2), ('d', 1), ('l', 1), ('r', 1))
    assert(subtract(lard, r) === lad)
  }


  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
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
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

/*  test("yes man") {
    println("yes man")
    sentenceAnagrams(List("yes", "man")).foreach(println)
  }*/


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
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

  test("sentence anagrams: zzz") {
    val sentence = List("en", "en", "as")
    println
    println("zzz")
    println
    sentenceAnagrams(sentence).foreach(println)
  }

}
