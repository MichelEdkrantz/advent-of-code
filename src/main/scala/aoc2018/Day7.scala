package aoc2018

object Day7 extends App {
  private val file = "data/2018/day7.txt"
  val isTest = file.contains("test")
  val instructions = io.Source.fromFile(file).getLines().toList

  val pattern = "Step ([A-Z]) must be finished before step ([A-Z]) can begin.".r
  val letters = instructions.map {
    case pattern(a, b) => (a.charAt(0), b.charAt(0))
  }

  val allLetters = letters.flatMap(l => Seq(l._1, l._2)).toSet
  val dependencies = letters.groupBy(_._2).mapValues(_.map(_._1).toList)

  def partA(): Unit = {
    def resolveOrder(done: List[Char], left: Set[Char]): List[Char] = {
      //for each lap, try to take out each letter that is not done
      if(left.isEmpty)
        done
      else {
        val d = left.toList.filter(l => dependencies.getOrElse(l, Nil).forall(done.contains)).min
        resolveOrder(d :: done, left - d)
      }
    }
    val answer1 = resolveOrder(Nil, allLetters).reverse.mkString
    println(answer1)
  }

  def partB(): Unit = {
    val workers = if(isTest) 2 else 5
    val basetime = if(isTest) 0 else 60
    def taskTime(l: Char) = l + basetime - 64

    //recursive function, step forward in time until the next task completes
    def process(done: List[Char], current: List[(Char, Int)], jobs: Set[Char], time: Int): Int = {
      val (stillProcessing, d) = current.partition(_._2 > time)
      val allDone = done ::: d.map(_._1)
      print(s"At time=$time have stillProcessing=$stillProcessing. ")
      if(jobs.isEmpty) {
        if(stillProcessing.nonEmpty) {
          println("No more jobs")
          process(allDone, stillProcessing, Set.empty, stillProcessing.minBy(_._2)._2)
        } else {
          println("Done")
          time
        }
      } else {
        //take the next available job
        val nextJobOpt = jobs.filter(j => dependencies.getOrElse(j, Nil).forall(allDone.contains)).toList.sorted.headOption
        val (processing, remaining, blocked) = nextJobOpt match {
          case Some(nextJob) =>
            val nextItem = (nextJob, taskTime(nextJob) + time)
            val processing = nextItem :: stillProcessing
            val blocked = processing.size == workers
            println(s"Processing is blocked=$blocked, adding job $nextJob")
            (processing, jobs - nextJob, blocked)
          case None =>
            (stillProcessing, jobs, true)
        }
        val nextTime = if(blocked) processing.minBy(_._2)._2 else time
        process(allDone, processing, remaining, nextTime)
      }
    }

    val answer = process(Nil, Nil, allLetters, 0)
    println(answer)
  }

  partA()
  partB()

}
