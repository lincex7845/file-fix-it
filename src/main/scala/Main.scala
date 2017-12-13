import java.nio.file.{Files, Paths}
import collection.JavaConverters._
import scala.collection.mutable
import scala.io.StdIn
import scala.util.control.Breaks._

object Main extends App {

  print("Path of the input file: ")
  val inputFile = StdIn.readLine()

  val lines: List[String] = Files.readAllLines(Paths.get(inputFile)).asScala.toList
  println("==============================")
  println("Input")
  println()
  lines.foreach(println)
  println("==============================")
  val content = mutable.ListBuffer.empty[String]
  val tests = lines.head.toInt
  var currentTest = 0
  var i = 1
  breakable{
    while(true){
      currentTest += 1
      val result = resolveTest(currentTest, i, lines)
      val (numberOfTest, numberOfMkdir, index) = result
      content.append(s"Case #$numberOfTest: $numberOfMkdir")
      i = index
      if(index >= lines.size)
        break
    }
  }
  println("==============================")
  println("Output")
  println()
  content.foreach(println)
  println("==============================")


  def resolveTest(numberOfTest: Int, index: Int, lines: List[String]): (Int, Int, Int) = {
    val _m_n: Array[Int] = lines(index).split(" ").map(_.toInt)
    val m: Int = _m_n(0)
    val n: Int = _m_n(1)
    val nextIndex = index + m + n + 1
    if(m == 0){
      val limit:Int = index + 1 + n
      val _new1: List[String] = lines.slice(index+1, limit)
      val n1: List[String] = _new1.flatMap(_.split("/").toList).distinct.filter(_.nonEmpty)
      (numberOfTest, n1.size, nextIndex)
    }
    else if(n == 0) {
      (numberOfTest, 0, nextIndex)
    }
    else {
      val test: List[String] = lines.slice(index, index + 1 + m + n)
      val _old: List[String] = test.slice(1, 1 + m)
      val _new: List[String] = test.slice(m +1, 1 + m + n)
      val d: List[String] = _new diff _old
      if(d.isEmpty)
        (numberOfTest, 0, nextIndex)
      else{
        val o: List[String] = _old.flatMap(_.split("/").toList).filter(_.nonEmpty)
        val nn: List[String] = _new.flatMap(_.split("/").toList).filter(_.nonEmpty)
        (numberOfTest, (nn diff o).size, nextIndex)
      }
    }
  }

}