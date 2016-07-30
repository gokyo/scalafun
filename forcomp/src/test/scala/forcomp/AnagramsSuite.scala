package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
    assert(wordOccurrences("baafazba") === List(('a',4), ('b',2), ('f',1), ('z',1)))
  }

  test("sentenceOccurrences") {
    assert(sentenceOccurrences(Nil) === List())
    assert(sentenceOccurrences(List("abcd","e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
    assert(sentenceOccurrences(List("bac","ear","ca")) === List(('a',3),('b',1),('c',2),('e',1),('r',1)))
  }

  test("dictionaryByOccurrences") {
    val words = List(('a',1),('e',1),('t',1))
    assert(dictionaryByOccurrences.get(words).map(_.toSet) === Some(Set("ate","eat","tea")))
  }

  test("word anagrams") {
    assert(wordAnagrams("tae").toSet === Set("ate","eat","tea"))
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("combinations") {
    
    assert(combinations(Nil) === List(Nil))
    
    val set1 = List(List(), List(('c',1))).toSet
    assert(combinations(List(('c',1))).toSet === set1)
    
    val set2 = List(List(), List(('c',1)), List(('d',1)), List(('c',1), ('d',1))).toSet
    assert(combinations(List(('c',1),('d',1))).toSet === set2)

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

  test("subtract") {
    assert(subtract(List(), Nil) === Nil)
    
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)

    val abcde = List(('a',3),('d',1),('b',1),('c', 1),('e',1))
    val abc = List(('a',2),('b',1),('c',1))
    val ade = List(('a',1),('d',1),('e',1))
    assert(subtract(abcde, abc) === ade)
  }
  
  test("sentence anagrams") {
    assert(sentenceAnagrams(List()) === List(Nil))

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

}
