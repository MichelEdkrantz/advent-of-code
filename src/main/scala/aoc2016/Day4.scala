package aoc2016

/**
  * Created by Michel Edkrantz on 2016-12-04.
  */
object Day4 extends App {
  case class Sector(letters: String, sectorId: Int, checksum: String)
  val pattern = "([a-z-]+)-(\\d+)\\[([a-z]+)\\]".r
  def toSector(string: String) = string match {
    case pattern(letters, sectorId, checkSum) => Sector(letters, sectorId.toInt, checkSum)
  }

  def isValid(sector: Sector): Boolean = {
    sector.letters.toCharArray
      .filterNot(_ == '-')
      .groupBy(identity)
      .mapValues(_.size).toSeq.sortWith {
        case ((letter1,size1),(letter2, size2)) =>
          if(size1 == size2) letter1 < letter2 else size1 > size2
      }.take(5).map(_._1).mkString == sector.checksum
  }

  def rotate(char: Char, n: Int): Char =
    ('a' + Math.floorMod((char - 'a') + Math.floorMod(n,26), 26)).toChar

  def decipher(sector: Sector): String = sector.letters.toCharArray.map {
    case '-' => ' '
    case char => rotate(char,sector.sectorId)
  }.mkString

  //tests
  println(isValid(toSector("aaaaa-bbb-z-y-x-123[abxyz]")))
  println(isValid(toSector("a-b-c-d-e-f-g-h-987[abcde]")))
  println(isValid(toSector("not-a-real-room-404[oarel]")))
  println(isValid(toSector("totally-real-room-200[decoy]")))
  println(decipher(toSector("qzmt-zixmtkozy-ivhz-343[xxxxx]")))

  val sectors = io.Source.fromFile("data/2016/day4.txt").getLines().toList.map(toSector)
  val validSectors = sectors.filter(isValid)
  println(validSectors.map(_.sectorId).sum)
  validSectors.map(s => s -> decipher(s)).filter(_._2.contains("northpole")).foreach(println)
}
