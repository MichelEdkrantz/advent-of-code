package aoc2023

import scala.collection.immutable.NumericRange

object Day5 extends App with tools.AocDay {
  val (year, day) = (2023, 5)
  type NumRange = NumericRange.Inclusive[Long]

  case class SeedInstructions(seeds: Vector[Long], markerMaps: List[MarkerMap])

  case class RangeDef(destinationStart: Long, sourceStart: Long, rangeLen: Long) {
    val destinationEnd: Long = destinationStart + rangeLen - 1
    val sourceEnd: Long = sourceStart + rangeLen - 1
  }

  case class MarkerMap(from: String, to: String, mappings: List[RangeDef]) {
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
          val init = if (acc.length > 1) acc.init else Nil
          val rangeDef = str.split("\\s+").map(_.toLong) match {
            case Array(dest, src, len) => RangeDef(dest, src, len)
          }
          init :+ acc.last.copy(mappings = acc.last.mappings :+ rangeDef)
      }
    }.map(m => m.copy(mappings = m.mappings.sortBy(_.sourceStart)))
    SeedInstructions(seeds, markerMaps)
  }

  def convert(seed: Long, map: MarkerMap): Long = {
    val destinations = for {
      rangeDef <- map.mappings
      if rangeDef.sourceStart <= seed && seed < rangeDef.sourceStart + rangeDef.rangeLen
    } yield {
      rangeDef.destinationStart + (seed - rangeDef.sourceStart)
    }
    destinations.headOption.getOrElse(seed)
  }

  def convertRange(range: NumRange, markerMap: MarkerMap): List[NumRange] = {
    var mappedRanges = List[NumRange]()
    var pos = range.start
    for {
      m <- markerMap.mappings
      if m.sourceStart <= range.end && m.sourceEnd >= range.start
    } {
      if (pos < m.sourceStart) {
        mappedRanges :+= pos to m.sourceStart - 1
      }
      mappedRanges :+= convertMarkerMapRange(range, m)
      pos = m.sourceEnd + 1
    }
    if (pos < range.end)
      mappedRanges :+= pos to range.end

    mappedRanges
  }

  def convertMarkerMapRange(range: NumRange, m: RangeDef): NumRange = {
    // range is partially overlapping
    val start = Math.max(m.sourceStart, range.start)
    val end = Math.min(m.sourceEnd, range.end)
    val diff = end - start
    val destStart = m.destinationStart + (start - m.sourceStart)
    destStart to destStart + diff
  }

  def convertRangeWithMaps(range: NumRange,
                           markerMaps: List[MarkerMap]
                          ): List[NumRange] = {
    //propagate a range through all the maps
    markerMaps.foldLeft(List(range)) { case (acc, map) =>
      acc.flatMap(convertRange(_, map))
    }
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

  def solveProblemBSlow(inst: SeedInstructions): Long = {
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

  def solveProblemB(inst: SeedInstructions): Long = {
    val seedRanges = getSeedRanges(inst.seeds)
    seedRanges.map(convertRangeWithMaps(_, inst.markerMaps)).map {
      _.map(_.min).min
    }.min
  }

  val testParsed = parseInstructions(testInstructions)
  val parsed = parseInstructions(instructions)
  val seedToSoil = testParsed.markerMaps.find(_.fullname == "seed-to-soil").get

  val testConvert = Seq((79, 81), (14, 14), (55, 57), (13, 13), (99, 51))
  testConvert.foreach { case (seed, expected) =>
    assert(convert(seed, seedToSoil) == expected)
  }

  private val testRange = 0L to 99L
  //println("seed-to-soil", seedToSoil)
  val r = convertRange(testRange, seedToSoil)
  println(solveProblemA(testParsed))
  println(solveProblemA(parsed))
  println(solveProblemB(testParsed))
  println(solveProblemB(parsed))


}
