package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("makeOrderedLeafList of Nil") {
    assert(makeOrderedLeafList(Nil).isEmpty)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("custom combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4), Leaf('f', 3))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), Leaf('f', 3)))
  }

  test("custom combine of some leaf list 2") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 2))
    assert(combine(leaflist) === List(Leaf('x', 2), Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3)))
  }

  test("custom combine") {
    val leaflist = List(Leaf('x', 2), Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3))
    assert(combine(leaflist) === List(Fork(Leaf('x', 2), Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), List('x', 'e', 't'), 5)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("times") {
    val list = 'a' :: 'a' :: 'b' :: 'a' :: Nil
    val contado = times(list)
    assert(contado.head._1 === 'a')
    assert(contado.head._2 === 3)
    assert(contado.tail.head._1 === 'b')
    assert(contado.tail.head._2 === 1)
  }

  test("times of Nil") {
    assert(times(Nil).isEmpty)
  }

  test("times abrakadabra") {
    val freq = times("abrakadabra".toList)
    val posta = List(('a', 5), ('b', 2), ('d', 1), ('k', 1), ('r', 2))
    assert(freq === posta)
  }

  test("orderedLeafList abrakadabra") {
    val freq = times("abrakadabra".toList)
    val leafList = makeOrderedLeafList(freq)
    assert(leafList === List(Leaf('d', 1), Leaf('k', 1), Leaf('b', 2), Leaf('r', 2), Leaf('a', 5)))
  }

  test("custom makeOrderedLeafList") {
    assert(makeOrderedLeafList(List(('a', 0), ('t', 2), ('e', 1), ('x', 3))) === List(Leaf('a', 0), Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("custom decode") {
    val decoder = Fork(Leaf('a', 4), Fork(Leaf('b', 1), Leaf('c', 3), "bc".toList, 4), "abc".toList, 8)
    val encoded = List(0, 1, 0, 1, 1, 0)
    assert(decode(decoder, encoded) === List('a', 'b', 'c', 'a'))
  }

  test("decoded secret") {
    assert(decodedSecret.mkString == "huffmanestcool")
  }

  test("codeBits") {
    val table = List[(Char, List[Bit])](('a', List(1, 0)), ('b', List(0, 0)), ('c', List(1, 1)), ('d', List(0, 1)))
    val func = codeBits(table)(_)
    assert(func('a') === List(1, 0))
    assert(func('b') === List(0, 0))
    assert(func('c') === List(1, 1))
    assert(func('d') === List(0, 1))
    intercept[Error] {
      func('w')
    }
  }

  test("convert") {
    // val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    // val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    new TestTrees {
      val table1 = convert(t1)
      assert(table1.head === ('a', List(0)))
      assert(table1.tail.head === ('b', List(1)))
      assert(table1.tail.tail.isEmpty)

      val table2 = convert(t2)
      assert(table2.head === ('a', List(0, 0)))
      assert(table2.tail.head === ('b', List(0, 1)))
      assert(table2.tail.tail.head === ('d', List(1)))
      assert(table2.tail.tail.tail.isEmpty)
    }
  }

  test("merge CodeTables") {
    val t1 = List(('a', List(0)))
    val t2 = List(('c', List(0, 0)), ('d', List(0, 1)), ('e', List(1)))
    val table = mergeCodeTables(t1, t2)
    val func = codeBits(table)(_)
    assert(func('a') === List(0))
    assert(func('c') === List(1, 0, 0))
    assert(func('d') === List(1, 0, 1))
    assert(func('e') === List(1, 1))
    intercept[Error] {
      func('w')
    }
  }

  test("quick secret encode") {
    val codificado = quickEncode(frenchCode)("huffmanestcool".toList)
    assert(codificado === secret)
  }

  test("createCodeTree") {
    val codeTree = createCodeTree(
      "Lisa en este momento estoy muy triste pues nuestro presidente ha sido derrocado...Y SUSTITUIDO POR EL BENEVOLO Y AMADO GENERAL, TODOS AMAMOS A KROLL Y A SU GLORIOS REGIMEN,CON AMOR NIÑITA".toLowerCase.toList)
    assert(!isLeaf(codeTree))
  }


  test("encode size") {
    val string = "mi mama me mima, mi mama me ama, mi mama me mima, me mima mi mama".toList
    val code = encode(createCodeTree(string))(string)
    assert(code.size === 151)
  }
  
  test("times long") {
    val string = """For example, if you use letters as symbols and have details of the frequency of occurrence of those letters in typical strings, then you could just encode each letter with a fixed number of bits, such as in ASCII codes. You can do better than this by encoding more frequently occurring letters such as e and a, with smaller bit strings; and less frequently occurring letters such as q and x with longer bit strings. """.toList
    assert(times(string) === List((' ',75), (',',4), ('.',2), (';',1), ('A',1), ('C',1), ('F',1), ('I',2), ('S',1), ('Y',1), ('a',18), ('b',7), ('c',18), ('d',11), ('e',40), ('f',9), ('g',7), ('h',13), ('i',19), ('j',1), ('l',16), ('m',5), ('n',23), ('o',20), ('p',2), ('q',4), ('r',23), ('s',29), ('t',31), ('u',16), ('v',1), ('w',3), ('x',3), ('y',8)))
  }
  
  test("makeOrderedLeafList long"){
    val string = """For example, if you use letters as symbols and have details of the frequency of occurrence of those letters in typical strings, then you could just encode each letter with a fixed number of bits, such as in ASCII codes. You can do better than this by encoding more frequently occurring letters such as e and a, with smaller bit strings; and less frequently occurring letters such as q and x with longer bit strings. """.toList
    val leafList = makeOrderedLeafList(times(string))
  }
  
  test("encode long") {
    val string = """For example, if you use letters as symbols and have details of the frequency of occurrence of those letters in typical strings, then you could just encode each letter with a fixed number of bits, such as in ASCII codes. You can do better than this by encoding more frequently occurring letters such as e and a, with smaller bit strings; and less frequently occurring letters such as q and x with longer bit strings. """.toList
    val code = encode(createCodeTree(string))(string)
    assert (code.size === 1790)
  }
}
