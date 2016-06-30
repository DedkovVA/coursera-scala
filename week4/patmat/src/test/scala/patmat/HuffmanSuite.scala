package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))

    val leaflist2 = List(Leaf('e', 1), Leaf('x', 4), Leaf('t', 2))
    assert(combine(leaflist2) === List(Leaf('t',2), Fork(Leaf('e',1),Leaf('x',4),List('e', 'x'),5)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
//      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("times test 1") {
    assert(times(List('a', 'b', 'a')).sorted equals List(('a', 2), ('b', 1)).sorted)
    assert(times(List('a', 'b', 'a')).sorted equals List(('b', 1), ('a', 2)).sorted)
    assert(times(List()) equals List())
    assert(times(List('a', 'b', 'a')).sorted equals times(List('a', 'a', 'b')).sorted)
  }

  test("makeOrderedLeafList test") {
    assert(makeOrderedLeafList(List(('a', 1))) == List(new Leaf('a', 1)))
    assert(makeOrderedLeafList(List(('a', 1), ('c', 10), ('b', 2), ('z', 3))) == List(new Leaf('a', 1),new Leaf('b', 2),new Leaf('z', 3),new Leaf('c', 10)))
    assert(makeOrderedLeafList(List(('a', 1), ('c', 10), ('b', 2), ('z', 3))) != List(new Leaf('a', 1),new Leaf('b', 2),new Leaf('z', 3)))
    assert(makeOrderedLeafList(List()) == List())
  }

  test("unit test") {
    val i = 0
    val inner = {
      println("MARK")
      { i => i+1 }
    }
  }

/*  test("until") {
    val trees = makeOrderedLeafList(List(('a', 8), ('b', 3), ('c', 1), ('d', 1), ('e', 1), ('f', 1), ('g', 1), ('h', 1)))
    val t1 = until(Huffman.singleton, Huffman.combine)(trees).head
    val t2 =
    makeCodeTree(
      new Leaf('a', 8),
      makeCodeTree(
        makeCodeTree(
          new Leaf('b', 3),
          makeCodeTree(new Leaf('c', 1), new Leaf('d', 1))
        ),
        makeCodeTree(
          makeCodeTree(new Leaf('e', 1), new Leaf('f', 1)),
          makeCodeTree(new Leaf('g', 1), new Leaf('h', 1))
        )
      )
    )
    assert(List(t1) == List(t2))
  }*/
}
