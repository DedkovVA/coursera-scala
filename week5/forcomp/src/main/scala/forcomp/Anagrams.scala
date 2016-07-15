package forcomp

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object Anagrams {

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
    List(w.toLowerCase.toCharArray.groupBy((ch: Char) => ch).map(entry => (entry._1, entry._2.length)).toSeq.sortBy(_._1):_*)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences(s.mkString.toLowerCase)

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy(word => wordOccurrences(word))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.get(wordOccurrences(word)).get

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    val numToCharCombs = charCombinations(occurrences)

    val occurrencesMap = occurrences.toMap
    var result = List[List[(Char, Int)]]()

    numToCharCombs.foreach(entry => entry._2.foreach(charList => {
      //if occurrences = List((a, 2), (b, 2)) and charList = List(a) then charListCombinations = List((a, 1), (a, 2))
      var charListCombinations = List[List[(Char, Int)]](List[(Char, Int)]())
      charList.foreach(ch => {
        var swapCharListCombinations = List[List[(Char, Int)]]()
        for (j <- 1 to occurrencesMap.get(ch).get)
          charListCombinations.foreach(chFreqList =>
            swapCharListCombinations :+= (chFreqList :+ (ch, j))
          )
        charListCombinations = swapCharListCombinations
      })
      result ++= charListCombinations
    }))

    result
  }

  def sortWithOcc(occ1: Occurrences, occ2: Occurrences): Boolean = {
    if (occ1.isEmpty && occ2.nonEmpty) true
    else if (occ1.nonEmpty && occ2.isEmpty) false
    else if (occ1.isEmpty && occ2.isEmpty) true
    else {
      val len = Math.min(occ1.length, occ2.length)
      for (i <- 0 until len) {
        if (occ1(i)._1 < occ2(i)._1) return true
        else if (occ1(i)._1 > occ2(i)._1) return false
        else {
          if (occ1(i)._2 < occ2(i)._2) return true
          else if (occ1(i)._2 > occ2(i)._2) return false
        }
      }
      if (occ1.length <= occ2.length) true
      else false
    }
  }

  def sortOccurrencesList(occurrencesList: List[Occurrences]): List[Occurrences] = {
    occurrencesList.sortWith((occ1, occ2) => sortWithOcc(occ1, occ2))
  }

  /**List((a, na), (b, nb), (c, nc)) --> Map(
    * 0 --> Set(List()),
    * 1 --> Set(List(a), List(b), List(c)),
    * 2 --> Set(List(a, b), List(b, c), List(a, c))
    * 3 --> Set(List(a, b, c))
    * )*/
  def charCombinations(occurrences: Occurrences): mutable.Map[Int, mutable.Set[List[Char]]] = {
    val chars = occurrences.map(entry => entry._1)
    val numToCharCombs = mutable.Map[Int, mutable.Set[List[Char]]]((0, mutable.Set(List())))

    for (i <- 1 to chars.length) {
      numToCharCombs(i) = mutable.Set()

      val prevCombs = numToCharCombs(i - 1)
      prevCombs.foreach(charList => {
        chars.foreach(ch => {
          if (charList.isEmpty || charList.last < ch) {
            val charListCandidate = charList :+ ch
            if (!numToCharCombs(i).contains(charListCandidate))
              numToCharCombs(i).add(charListCandidate)
          }
        })
      })
    }

    numToCharCombs
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    var xMap = x.toMap

    val xCharList = x.map(entry => entry._1)
    y.foreach(chFreq =>
      if (xCharList.contains(chFreq._1))
//        if (xMap(chFreq._1) >= chFreq._2)
          xMap = xMap.updated(chFreq._1, xMap(chFreq._1) - chFreq._2)
    )

    xMap.filter(chFreq => chFreq._2 != 0).toList
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */

  //todo шаг 1 -
  //получить все возможные упорядоченные(?) комбинации List[Occurrences] из словаря:
  //Set[List[Occurrences]]
  //List[Occurrences] - упорядочивать лексикографически, по длине Occurrences

    //todo шаг 1.1 - посчитать combinations (упорядочивать их лексикографически, по длине Occurrences)
    //todo шаг 1.2 - среди combinations найти те, которые есть в словаре
    //обновить словарь этими значениями (в конце процедуры)
    //todo шаг 1.3 - subtract эти комбинации, повторить процедуру для каждой ветви
    //на каждой итерации проверять, что данное решение еще не содержится в дереве решиений
    //например, (a,b) == (b,a) для Occurrences a,b, поэтому нет смысла рассматривать (b,a),
    //если (a,b) уже есть в дереве решений

  //todo шаг 2 -
  // считать комбинации слов (в т.ч. перестановки слов в прелложении;
  // учесть, что в предложении одно слово может встречаться больше одного раза)

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    if (sentence.isEmpty) return List(Nil)

    val sntncOccurrences = sentenceOccurrences(sentence)

    val occurrencesCombinations = ListBuffer[List[Occurrences]]()
    val input = mutable.Queue[(List[Occurrences], List[Occurrences], Occurrences)]((List[Occurrences](), dictionaryByOccurrences.toList.map(p => p._1), sntncOccurrences))

    while (input.nonEmpty) {
      calcStep(input, occurrencesCombinations)
    }

    val result = ListBuffer[Sentence]()

    occurrencesCombinations.foreach(occurrencesList => result ++= occurrencesListToSentenceList(occurrencesList))

    result.toList
  }

  //todo сделать перестановки слов в предложении. учесть, что могут быть два одинаковых слова
  def occurrencesListToSentenceList(occurrencesList: List[Occurrences]): List[Sentence] = {
    var result1 = ListBuffer[Sentence](List())
    var result2 = ListBuffer[Sentence]()

    for (i <- occurrencesList.indices) {
      val wordList = dictionaryByOccurrences(occurrencesList(i))
      result2 = ListBuffer[Sentence]()
      for (j <- wordList.indices) {
        result1.foreach(tempWordList => result2 += (tempWordList :+ wordList(j)))
      }
      result1 = result2
    }

    val result = ListBuffer[Sentence]()
    result2.foreach(sentence => result ++= permutateWordsInSentence(sentence))
    result.toList
  }

  def permutateWordsInSentence(sentence: Sentence): List[Sentence] = {
    val result = mutable.Set[Sentence](sentence)
    sentence.permutations.foreach(s => result += s)
    result.toList
  }

  def combinationsWithTails(occurrences: Occurrences, dictionary: List[Occurrences]): List[(Occurrences, Occurrences)] = {
    var result = List[(Occurrences, Occurrences)]()

    combinations(occurrences).foreach(occ => if (dictionary.contains(occ)) result :+= (occ, subtract(occurrences, occ)))

    result
  }

  def calcStep(input: mutable.Queue[(List[Occurrences], List[Occurrences], Occurrences)], output: ListBuffer[List[Occurrences]]): Unit = {
    if (input.isEmpty) return

    val first = input.dequeue

    val preResult = first._1
    val dictionary = first._2
    val tail = first._3

    val combinationsPlusTails = combinationsWithTails(tail, dictionary)

    //в словаре не нашлось слов для данного остатка tail
    if (combinationsPlusTails.isEmpty) {
//      recur(input, output)
      return
    }

    val newDictionary = combinationsPlusTails.map(e => e._1)

    combinationsPlusTails.foreach(e => {
      val sortedList = sortOccurrencesList(preResult :+ e._1)

      if (e._2.isEmpty && !output.contains(sortedList)) {
        output += sortedList
      }
      else if (e._2.nonEmpty && !input.map(e => e._1).contains(sortedList)) {
        input.enqueue((sortedList, newDictionary, e._2))
      }
    })

//    recur(input, output)
  }
}
