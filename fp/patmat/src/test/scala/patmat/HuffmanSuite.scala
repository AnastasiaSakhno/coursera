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
  }

  test("decode sample") {
    val leafA = Leaf('A', 8)
    val leafB = Leaf('B', 3)
    val leafC = Leaf('C', 1)
    val leafD = Leaf('D', 1)
    val leafE = Leaf('E', 1)
    val leafF = Leaf('F', 1)
    val leafG = Leaf('G', 1)
    val leafH = Leaf('H', 1)
    val nodeCD = Fork(leafC, leafD, "CD".toList, 2)
    val nodeEF = Fork(leafE, leafF, "EF".toList, 2)
    val nodeGH = Fork(leafG, leafH, "GH".toList, 2)
    val nodeEFGH = Fork(nodeEF, nodeGH, "EFGH".toList, 4)
    val nodeBCD = Fork(leafB, nodeCD, "BCD".toList, 5)
    val nodeBCDEFGH = Fork(nodeBCD, nodeEFGH, "BCDEFGH".toList, 9)
    val nodeABCDEFGH = Fork(leafA, nodeBCDEFGH, "ABCDEFGH".toList, 17)

    val decodedValue = decode(nodeABCDEFGH, List(1, 0, 0, 0, 1, 0, 1, 0))
    assert(decodedValue.mkString == "BAC")
  }


  test(" decoded secret should return huffmanestcool") {
    assert(decodedSecret.mkString === "huffmanestcool")
  }

  test("encode sample") {
    val leafA = Leaf('A', 8)
    val leafB = Leaf('B', 3)
    val leafC = Leaf('C', 1)
    val leafD = Leaf('D', 1)
    val leafE = Leaf('E', 1)
    val leafF = Leaf('F', 1)
    val leafG = Leaf('G', 1)
    val leafH = Leaf('H', 1)
    val nodeCD = Fork(leafC, leafD, "CD".toList, 2)
    val nodeEF = Fork(leafE, leafF, "EF".toList, 2)
    val nodeGH = Fork(leafG, leafH, "GH".toList, 2)
    val nodeEFGH = Fork(nodeEF, nodeGH, "EFGH".toList, 4)
    val nodeBCD = Fork(leafB, nodeCD, "BCD".toList, 5)
    val nodeBCDEFGH = Fork(nodeBCD, nodeEFGH, "BCDEFGH".toList, 9)
    val nodeABCDEFGH = Fork(leafA, nodeBCDEFGH, "ABCDEFGH".toList, 17)

    val encodedValue = encode(nodeABCDEFGH)(List('D'))
    assert(encodedValue == List(1, 0, 1, 1))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits should return bits associated to char") {
    val A = ('A', List(1, 1, 1))
    val B = ('B', List(1, 1, 0))
    val C = ('C', List(1, 0, 0))
    val table = List(A, B, C)

    assert(codeBits(table)('A') == List(1, 1, 1))
    assert(codeBits(table)('B') == List(1, 1, 0))
    assert(codeBits(table)('C') == List(1, 0, 0))
  }

  test("merge two code table") {
    val A = ('A', List(1, 1, 1))
    val B = ('B', List(1, 1, 0))
    val C = ('C', List(1, 0, 0))
    val D = ('D', List(0, 0, 0))

    val table1: CodeTable = List(A, B)
    val table2: CodeTable = List(C, D)

    val mergedTable = mergeCodeTables(table1, table2)
    assert(mergedTable.size == 4)
    assert(mergedTable.contains(A))
    assert(mergedTable.contains(B))
    assert(mergedTable.contains(C))
    assert(mergedTable.contains(D))
  }

  test("convert") {
    val leafA = Leaf('A', 8)
    val leafB = Leaf('B', 3)
    val leafC = Leaf('C', 1)
    val leafD = Leaf('D', 1)
    val leafE = Leaf('E', 1)
    val leafF = Leaf('F', 1)
    val leafG = Leaf('G', 1)
    val leafH = Leaf('H', 1)
    val nodeCD = Fork(leafC, leafD, "CD".toList, 2)
    val nodeEF = Fork(leafE, leafF, "EF".toList, 2)
    val nodeGH = Fork(leafG, leafH, "GH".toList, 2)
    val nodeEFGH = Fork(nodeEF, nodeGH, "EFGH".toList, 4)
    val nodeBCD = Fork(leafB, nodeCD, "BCD".toList, 5)
    val nodeBCDEFGH = Fork(nodeBCD, nodeEFGH, "BCDEFGH".toList, 9)
    val nodeABCDEFGH = Fork(leafA, nodeBCDEFGH, "ABCDEFGH".toList, 17)

    val table: CodeTable = convert(nodeABCDEFGH)
    val D = ('D', List(1, 0, 1, 1))
    assert(table.size == 8)
    assert(table contains D)
  }

  test("quick encode") {
    val leafA = Leaf('A', 8)
    val leafB = Leaf('B', 3)
    val leafC = Leaf('C', 1)
    val leafD = Leaf('D', 1)
    val leafE = Leaf('E', 1)
    val leafF = Leaf('F', 1)
    val leafG = Leaf('G', 1)
    val leafH = Leaf('H', 1)
    val nodeCD = Fork(leafC, leafD, "CD".toList, 2)
    val nodeEF = Fork(leafE, leafF, "EF".toList, 2)
    val nodeGH = Fork(leafG, leafH, "GH".toList, 2)
    val nodeEFGH = Fork(nodeEF, nodeGH, "EFGH".toList, 4)
    val nodeBCD = Fork(leafB, nodeCD, "BCD".toList, 5)
    val nodeBCDEFGH = Fork(nodeBCD, nodeEFGH, "BCDEFGH".toList, 9)
    val nodeABCDEFGH = Fork(leafA, nodeBCDEFGH, "ABCDEFGH".toList, 17)

    val encodedValue = quickEncode(nodeABCDEFGH)(List('D'))
    assert(encodedValue == List(1, 0, 1, 1))
  }


}
