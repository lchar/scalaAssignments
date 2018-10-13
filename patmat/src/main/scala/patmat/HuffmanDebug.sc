object bobo {
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree


  // Part 1: Basics
  def weight(tree: CodeTree): Int =  tree match {
    case Fork(_, _, _, weight) => weight
    case Leaf(_, weight) => weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_, _, chars, _) => chars
    case Leaf(char, _) => List(char)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {
    def isUnique(char: Char, acc: List[(Char, Int)]): Boolean =
      if (acc.isEmpty) true
      else if (char == acc.head._1) false
      else isUnique(char, acc.tail)

    def timesAcc(chars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = {
      def timesSingleAcc(char: Char, chars: List[Char], count: Int): Int =
        if (chars.isEmpty) count
        else if (char == chars.head) timesSingleAcc(char, chars.tail, count + 1)
        else timesSingleAcc(char, chars.tail, count)

      if (chars.isEmpty) acc
      else if (isUnique(chars.head, acc)) timesAcc(chars.tail, (chars.head, timesSingleAcc(chars.head, chars.tail, 1)) :: acc)
      else timesAcc(chars.tail, acc)
    }

    timesAcc(chars, List())

  }
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    val freqsi = freqs

    def isLargest(leaf: (Char, Int), freqs: List[(Char, Int)], acc: List[Leaf]): Boolean = {
      def isUnique(leaf: (Char, Int), acc: List[Leaf]): Boolean =
        if (acc.isEmpty) true
        else if (leaf._1 == chars(acc.head).head) false
        else isUnique(leaf, acc.tail)

      if (isUnique(leaf, acc))
        if (freqs.isEmpty) true
        else if (leaf._2 < freqs.head._2 && isUnique(freqs.head, acc)) false
        else isLargest(leaf, freqs.tail, acc)
      else false
    }

    def makeOrderedLeafListAcc(freqs: List[(Char, Int)], acc: List[Leaf]): List[Leaf] =
      if (freqs.isEmpty) acc
      else if (isLargest(freqs.head, freqs.tail, acc)) makeOrderedLeafListAcc(freqsi, Leaf(freqs.head._1, freqs.head._2) :: acc)
      else makeOrderedLeafListAcc(freqs.tail, acc)

    makeOrderedLeafListAcc(freqs, List())
  }

  def singleton(trees: List[CodeTree]): Boolean =
    if (trees.isEmpty) false
    else trees.tail.isEmpty

  def combine(trees: List[CodeTree]): List[CodeTree] = {
    def sortOrdered(tree: CodeTree, left: List[CodeTree], right: List[CodeTree]): List[CodeTree] =
      if (right.isEmpty) left ::: List(tree)
      else if (weight(tree) <= weight(right.head)) left ::: (tree :: right)
      else sortOrdered(tree, left ::: List(right.head), right.tail)

    if (trees.isEmpty) trees
    else if (trees.tail.isEmpty) trees
    else sortOrdered(makeCodeTree(trees.head, trees.tail.head), List(), trees.tail.tail)
  }

  def until(sing: List[CodeTree] => Boolean, comb: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): CodeTree =
    if (sing(trees)) trees.head
    else until(sing, comb)(comb(trees))

  def createCodeTree(chars: List[Char]): CodeTree =
    until(singleton, combine)(makeOrderedLeafList(times(chars)))

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    val treei = tree
    def decodeAcc(tree: CodeTree, bits: List[Bit], acc: List[Char]): List[Char] =
      if (bits.isEmpty) tree match{
        case Fork(left, right, chars, _) => acc
        case Leaf(char, _) => acc ::: List(char)
      }
      else tree match{
      case Fork(left, right, chars, _) =>
        if (bits.head == 0) decodeAcc(left, bits.tail, acc)
        else decodeAcc(right, bits.tail, acc)
      case Leaf(char, _) => decodeAcc(treei, bits, acc ::: List(char))
    }
    decodeAcc(tree, bits, List())

  }

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val treei = tree
    def encodeAcc(tree: CodeTree)(text: List[Char], acc: List[Bit]): List[Bit] = {
      def charPresent(char: Char, chars: List[Char]): Boolean =
        if (chars.isEmpty) false
        else if (char == chars.head) true
        else charPresent(char, chars.tail)

      if (text.isEmpty) acc
      else tree match {
        case Fork(left, right, _, _) =>
          if (charPresent(text.head, chars(left))) encodeAcc(left)(text, acc ::: List(0))
          else encodeAcc(right)(text, acc ::: List(1))
        case Leaf(char, _) => encodeAcc(treei)(text.tail, acc)
      }
    }
    encodeAcc(tree)(text, List())
  }

  type CodeTable = List[(Char, List[Bit])]

  /**
    * This function returns the bit sequence that represents the character `char` in
    * the code table `table`.
    */
  def codeBits(table: CodeTable)(char: Char): List[Bit] =
    if (table.isEmpty) throw new Error("Could not find character")
    else if (char == table.head._1) table.head._2
    else codeBits(table.tail)(char)

  /**
    * Given a code tree, create a code table which contains, for every character in the
    * code tree, the sequence of bits representing that character.
    *
    * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
    * a valid code tree that can be represented as a code table. Using the code tables of the
    * sub-trees, think of how to build the code table for the entire tree.
    */
  def convert(tree: CodeTree): CodeTable = tree match {
    case Fork(left, right, chars, _) => mergeCodeTables2(convert(left), convert(right))
    case Leaf(char, _) => List((char, List()))
  }

  /**
    * This function takes two code tables and merges them into one. Depending on how you
    * use it in the `convert` method above, this merge method might also do some transformations
    * on the two parameter code tables.
    */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable =
    if (a.isEmpty && b.isEmpty) List()
    else if (a.isEmpty && b.nonEmpty) List((b.head._1, List(1) ::: b.head._2)) ::: mergeCodeTables(a, b.tail)
    else if (a.nonEmpty && b.isEmpty) List((a.head._1, List(0) ::: a.head._2)) ::: mergeCodeTables(a.tail, b)
    else List((a.head._1, List(0) ::: a.head._2), (b.head._1, List(1) ::: b.head._2)) ::: mergeCodeTables(a.tail, b.tail)

  def mergeCodeTables2(a: CodeTable, b: CodeTable): CodeTable = List(a, b) match {
    case List(List(), List()) => List()
    case List(List(), q1 :: qs) => List((q1._1, List(1) ::: q1._2) ) ::: mergeCodeTables(List(), qs)
    case List(p1 :: ps, List()) => List((p1._1, List(0) ::: p1._2) ) ::: mergeCodeTables(ps, List())
    case List(p1 :: ps, q1 :: qs) => List((p1._1, List(0) ::: p1._2), (q1._1, List (1) ::: q1._2) ) ::: mergeCodeTables(ps, qs)
  }


  /**
    * This function encodes `text` according to the code tree `tree`.
    *
    * To speed up the encoding process, it first converts the code tree to a code table
    * and then uses it to perform the actual encoding.
    */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] ={
    val table = convert(tree)
    if (text.isEmpty) List()
    else codeBits(table)(text.head) ::: quickEncode(tree)(text.tail)
  }

  val t1 = List('a', 'b', 'a', 'r', 'b', 'a', 'a', 'd', 'r', 'a', 'b', 'd', 'a', 'b', 'd')
  val times1 = times(t1)
  val leaves1 =  makeOrderedLeafList(times1)
  singleton(leaves1)
  singleton(List())
  singleton(List(Leaf('a', 30)))

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)
  decode(frenchCode, List(0, 0, 0))
  decode(frenchCode, secret)

  encode(frenchCode)(List('h'))
  val secret2 = encode(frenchCode)(List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  val secret3 = quickEncode(frenchCode)(List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))

  t1.tail
  val t2 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
  decode(t2, encode(t2)("ab".toList))
}