import scala.io.Source

object LexicalTagComparison extends App:
  val expectedTags = readTags("src/main/scala/taggedUPOS.txt")
  val obtainedTags = readTags("src/main/scala/stanzaOutputUPOS.txt")
  val (errorsMap, matches) = compare(expectedTags, obtainedTags)
  println(s"\nAciertos: $matches de ${expectedTags.length}")
  println(s"Precisión: ${(matches.toDouble / expectedTags.length) * 100}")
  println(errorsMap.toSeq.sortBy(_._1).mkString("\n"))

  type tagList = Seq[(String, String)]

  def compare(expected:tagList, obtained:tagList):(Map[String,Int], Int) =
    val errorList = expected
      .zip(obtained)
      .map((tag1, tag2) =>
        if tag1._2 == tag2._2 then None
        else Some(s"Expected: ${tag1._2} \tObtained: ${tag2._2}")
      )
    val errorOcurrencesByKind = errorList
      .filter(_.isDefined)
      .map(_.get)
      .groupMapReduce(s => s)(_ => 1)(_ + _)
    (errorOcurrencesByKind, errorList.count(_.isEmpty))

  def readTags(filename: String): tagList =
    val textFile = Source.fromFile(filename)
    val tags = textFile
      .getLines()
      .map(line => {
        val elements = line.split(" ")
        (elements(0).toLowerCase(), elements(1).toLowerCase())
      })
      .toSeq
    textFile.close()
    tags
