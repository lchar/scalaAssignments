import forcomp._

/** A word is simply a `String`. */
type Word = String

/** A sentence is a `List` of words. */
type Sentence = List[Word]

/** `Occurrences` is a `List` of pairs of characters and positive integers saying
  *  how often the character appears.
  *  This list is sorted alphabetically w.r.t. to the character in each pair.
  *  All characters in the occurrence list are lowercase.
  *
  *  Any list of pairs of lowercase characters and their frequency which is not sorted
  *  is **not** an occurrence list.
  *
  *  Note: If the frequency of some character is zero, then that character should not be
  *  in the list.
  */
type Occurrences = List[(Char, Int)]

/** The dictionary is simply a sequence of words.
  *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
  */
val dictionary: List[Word] = loadDictionary

/** Converts the word into its character occurrence list.
  *
  *  Note: the uppercase and lowercase version of the character are treated as the
  *  same character, and are represented as a lowercase character in the occurrence list.
  *
  *  Note: you must use `groupBy` to implement this method!
  */
def wordOccurrences(w: Word): Occurrences =
  (w.toLowerCase.toList.groupBy((c: Char) => c).toList
    map (x => (x._1, x._2.length))).sorted

/** Converts a sentence into its character occurrence list. */
def sentenceOccurrences(s: Sentence): Occurrences = {
  def wordOccConc(o1: Map[Char, Int], o2: Map[Char, Int]): Map[Char, Int] =
    (o2 foldLeft o1) (addOcc)

  def addOcc(o: Map[Char, Int], cp: (Char, Int)): Map[Char, Int] = {
    val (c, n) = cp
    o ++ Map(c -> (n + (o withDefaultValue 0)(c)))
  }

  if (s.isEmpty) List()
  else wordOccConc(wordOccurrences(s.head).toMap, sentenceOccurrences(s.tail).toMap).toList.sorted
}

lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
  dictionary.groupBy(w => wordOccurrences(w))

def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

def combinations(occurrences: Occurrences): List[Occurrences] = {
  def combHead(occurrences: Occurrences): List[Occurrences] =
    if (occurrences.isEmpty) List(List())
    else List() :: (for {
      n <- 1 to occurrences.head._2
    } yield List((occurrences.head._1, n))).toList

  if (occurrences.isEmpty) List(List())
  else for{
    ch1 <- combHead(occurrences)
    ch2 <- combinations(occurrences.tail)
  } yield  (ch1 ++ ch2).sorted
}

def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  def subSing(x1: Map[Char, Int], cp: (Char, Int)): Map[Char, Int] =
    if (x1.apply(cp._1) - cp._2 > 0)
      x1 + (cp._1 -> (x1.apply(cp._1) - cp._2))
    else x1 - cp._1

  (y.toMap foldLeft x.toMap) (subSing).toList
}

def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  val senLen = (sentence foldLeft 0) ((n, s) => s.length + n)
  val occ =  sentenceOccurrences(sentence)
  def sentenceAnagramsHelper(o: Occurrences): List[Sentence] =
    if (o.isEmpty) List(Nil)
    else if ((combinations(o) flatMap (dictionaryByOccurrences withDefaultValue Nil)) == Nil) List(Nil)
    else for {
      occ2 <- combinations(o)
      words <- (dictionaryByOccurrences withDefaultValue Nil)(occ2)
      rest <- sentenceAnagramsHelper(subtract(o, wordOccurrences(words)))

    } yield words :: rest

  val anagramspure = sentenceAnagramsHelper(occ)

  anagramspure filter (p => (p foldLeft 0) ((n, s) => s.length + n) == senLen)
}


val lrocc = sentenceOccurrences(List("Linux", "Rulez"))
val sub = wordOccurrences("uzi")
subtract(lrocc, sub)
wordAnagrams("center")
dictionaryByOccurrences(wordOccurrences("Center"))

sentenceAnagrams(List("scramble"))
sentenceAnagrams(List("scramble")).length
sentenceOccurrences(List("cen", "ter"))

(List("linux", "rulez") foldLeft 0) ((n, s) => s.length + n)

sentenceAnagrams(List("nil"))

subtract (wordOccurrences("linuulz"), wordOccurrences("zulu"))

def sentenceAnagramsHelper(o: Occurrences): List[Sentence] =
  if (o.isEmpty) List(Nil)
  else if ((combinations(o) flatMap (dictionaryByOccurrences withDefaultValue Nil)) == Nil) List(Nil)
  else for {
    occ2 <- combinations(o)
    words <- (dictionaryByOccurrences withDefaultValue Nil)(occ2)
    rest <- sentenceAnagramsHelper(subtract(o, wordOccurrences(words)))
    if (words.length + (rest foldLeft 0) (_ + _.length)) == (o foldLeft 0) (_ + _._2)

  } yield words :: rest

val o1 = List(('e', 1), ('i', 1), ('l', 2), ('n', 1), ('r', 1), ('u', 2), ('x', 1), ('z', 1))

combinations(o1) filter (x => x == List(('r', 1), ('e', 1), ('x', 1)).sorted)
(dictionaryByOccurrences withDefaultValue Nil)(combinations(o1)(15))

sentenceAnagramsHelper(o1)

