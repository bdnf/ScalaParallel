package scalashop

import org.scalameter._
import common._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
  // TODO implement this method using the `boxBlurKernel` method
    var blur_pixel = 0;
    for(y <- from until end)
      for(x <- 0 until src.width){
        blur_pixel = boxBlurKernel(src, x, y, radius)
        dst.update(x,y, blur_pixel)
    }

  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
  // TODO implement using the `task` construct and the `blur` method

    if (numTasks <= 1) blur(src, dst, 0, src.height, radius)
    else {
      val range = 0 to src.height by (src.height/ (Math.min(numTasks, src.height)))
      val range_tuples:IndexedSeq[(Int, Int)] = range.zip(range.tail) //Vector((0,10), (10,20), (20,30), (30,40), (40,50), (50,60), (60,70), (70,80), (80,90), (90,100))
      //arr(0)._1 , arr(0)._2
//      val tasks = for(i <- (0 until numTasks).par){
//        task( blur(src, dst, arr(i)._1, arr(i)._2, radius) ).join
//      }
      val tasks = range_tuples.map( { case (from, to) => task(blur(src, dst, from, to, radius)) } )
      tasks foreach (_.join)

//        range.zip(range.tail).toArray.map { case (from, end) =>
//        task (
//          blur(src, dst, from, end, radius)
//        )
//      } .foreach(_.join())
    }



  }

}
