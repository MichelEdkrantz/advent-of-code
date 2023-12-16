package tools

import scala.util.Using

object InstructionsReader {
  def read(file: String): List[String] = {
    Using(scala.io.Source.fromFile(file)) { source =>
      source.getLines.toList
    }.get
  }
  def readInstructions(year: Int, day: Int): List[String] = readInstructions(year, s"day$day.txt")
  def readTestInstructions(year: Int, day: Int): List[String] = readInstructions(year, s"day$day.test.txt")
  def readTest2Instructions(year: Int, day: Int): List[String] = readInstructions(year, s"day$day.test2.txt")
  def readTest3Instructions(year: Int, day: Int): List[String] = readInstructions(year, s"day$day.test3.txt")
  def readInstructions(year: Int, filename: String): List[String] = read(s"data/$year/$filename")
}

trait AocDay {
  def year: Int
  def day: Int
  lazy val instructions = InstructionsReader.readInstructions(year, day)
  lazy val testInstructions = InstructionsReader.readTestInstructions(year, day)
  lazy val test2Instructions = InstructionsReader.readTest2Instructions(year, day)
  lazy val test3Instructions = InstructionsReader.readTest3Instructions(year, day)
}
