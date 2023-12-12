package aoc2023
import scala.collection.parallel.CollectionConverters._

object Day5 extends App with tools.AocDay {
  val (year, day)  = (2023, 5)
  case class SeedInstructions(seeds: Vector[Long], markerMaps: List[MarkerMap])
  case class RangeDef(destinationStart: Long, sourceStart: Long, rangeLen: Long)
  case class MarkerMap(from: String, to: String, mapping: List[RangeDef]) {
    val fullname = from + "-to-" + to
  }

  //parsed lines
  val MapLine = "(.*)-to-(.*) map:".r
  def parseInstructions(lines: List[String]): SeedInstructions = {
    val seeds = lines.head.dropWhile(_ != ':').drop(2).split("\\s+").map(_.toLong).toVector
    val markerMaps = lines.tail.filter(_.nonEmpty).foldLeft(List[MarkerMap]()) { case (acc, line) =>
      line match {
        case MapLine(from, to) => acc :+ MarkerMap(from, to, Nil)
        case str =>
          val init = if(acc.length > 1) acc.init else Nil
          val rangeDef = str.split("\\s+").map(_.toLong) match {
            case Array(dest, src, len) => RangeDef(dest, src, len)
          }
          init :+ acc.last.copy(mapping = acc.last.mapping :+ rangeDef)
      }
    }
    SeedInstructions(seeds, markerMaps)
  }

  def convert(seed: Long, map: MarkerMap): Long = {
    val destinations = for {
      rangeDef <- map.mapping
      if rangeDef.sourceStart <= seed && seed < rangeDef.sourceStart + rangeDef.rangeLen
    } yield {
      rangeDef.destinationStart + (seed - rangeDef.sourceStart)
    }
    destinations.headOption.getOrElse(seed)
  }

  def solveProblemA(inst: SeedInstructions): Long = {
    inst.seeds.map(inst.markerMaps.foldLeft(_)(convert)).min
  }

  def getSeedRanges(seeds: Seq[Long]) = for {
    (seed, si) <- seeds.zipWithIndex
    if si % 2 == 0
  } yield {
    seed to seed + seeds(si + 1) - 1
  }

  def solveProblemB(inst: SeedInstructions): Long = {
    val seedRanges = getSeedRanges(inst.seeds)
    val nSeeds = seedRanges.map(_.length).sum
    println(s"nSeeds=$nSeeds")
    val seeds = seedRanges.iterator.flatMap(_.iterator)
    // this is too naive and too slow
    // we should map map do operations on ranges instead and check overlaps on the converters
    seeds.foldLeft(Long.MaxValue) { case (min, seed) =>
      val pos = inst.markerMaps.foldLeft(seed)(convert)
      Math.min(min, pos)
    }
  }

  val testParsed = parseInstructions(testInstructions)
  val parsed = parseInstructions(instructions)
  val seedToSoil = testParsed.markerMaps.find(_.fullname == "seed-to-soil").get

  val testConvert = Seq((79, 81), (14, 14), (55, 57), (13, 13), (99, 51))
  testConvert.foreach { case (seed, expected) =>
    assert(convert(seed, seedToSoil) == expected)
  }

  println(solveProblemA(testParsed))
  println(solveProblemA(parsed))
  println(solveProblemB(testParsed))
  println(solveProblemB(parsed))

}
