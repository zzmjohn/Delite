package epfl.mdarrays.tests.gol

import epfl.mdarrays.library.scala.Conversions._
import epfl.mdarrays.library.scala._
import epfl.mdarrays.library.scala.Operations._

import epfl.mdarrays.staged._
import scala.util.Random


object GameOfLifeStagedRunner extends StagedSACApplicationRunner with GameOfLifeStaged
trait GameOfLifeStaged extends StagedSACApplication with GameOfLifeStagedFakeReadMDArray {

  def main() = {
    /*
      Need to do the following:
      1. read a multidimensional array from an argument
      2. run testGameOfLife(1000, <array>)
      3. print the result? -- we don't have any effects, is there another way to do this?
     */
    val matrix = With(step=2::1::Nil, function = iv => 1).GenArray(300::300::Nil) // simulating some matrix :|
    val start = startTimer(matrix::Nil)
    val result = testGameOfLife(10, matrix)
    val stop = stopTimer(result::Nil)
  }

  def testGameOfLife(iter: Rep[MDArray[Int]], start: Rep[MDArray[Int]]): Rep[MDArray[Int]] = {
    var alive = start
    var i = 0: Rep[MDArray[Int]]

    while (i < iter) {
      alive = gameOfLife(alive)
      i = i + 1
    }
    alive
  }

  def computeIfDead(neigh: Rep[Int], alive: Rep[Int]): Rep[Int] = {
    timed(if (alive === 1) {
      val intermed = neigh - alive
      if (intermed < 2)
        int(1) // Rule1: Any live cell with fewer than two live neighbours dies, as if caused by under-population.
      else if (intermed < 4)
        int(0) // Rule2: Any live cell with two or three live neighbours lives on to the next generation.
      else
        int(1) // Rule3: Any live cell with more than three live neighbours dies, as if by overcrowding.
    }
    else
      int(0), "compute if dead")
  }

  def computeIfReborn(neigh: Rep[Int], alive: Rep[Int]): Rep[Int] = {
    timed(if (alive === 0) {
      if (neigh === 3)
        int(1) // Rule 4: Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
      else
        int(0)
    }
    else
      int(0),"compute if reborn")
  }

  def gameOfLife(alive: Rep[MDArray[Int]]) = {

    val dead = timed(With(lbStrict = true, ubStrict = true, function =
      iv => computeIfDead(timed(sum(tile(values(dim(alive), 3), iv-1, alive)), "dead-sum"), alive(iv))).GenArray(shape(alive)), "dead")

    val reborn = timed(With(lbStrict = true, ubStrict = true, function =
      iv => computeIfReborn(timed(sum(tile(values(dim(alive), 3), iv-1, alive)), "reborn-sum"), alive(iv))).GenArray(shape(alive)), "reborn")

    val result = timed(alive - dead + reborn, "result")

//    System.out.println("dead: " + dead)
//    System.out.println("reborn: " + reborn)
//    System.out.println("result: " + result)

    result
  }
}

trait GameOfLifeStagedFakeReadMDArray {
  def fakeReadMDArray(): MDArray[Int] = {
    // We want deterministic random
    Random.setSeed(0);

    import epfl.mdarrays.library.scala.Operations._
    import epfl.mdarrays.library.scala.Conversions._
    import epfl.mdarrays.library.scala._

    // Create the matrix
    val size: Int = 300
    val arr2: Array[Int] = new Array[Int](size * size)
    val rnd2: Random = new Random(1) // We need to have a fixed seed
    for (i <- arr2.indices)
      arr2(i) = rnd2.nextInt(2)

    val lwss = Array(
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,1,1,1,1,0,0,0,0,
      0,1,0,0,0,1,0,0,0,0,
      0,0,0,0,0,1,0,0,0,0,
      0,1,0,0,1,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0)

    val osc4 = Array(
      0,0,0,0,0,0,0,0,0,0,
      0,0,1,0,0,0,0,0,0,0,
      0,0,1,0,0,0,1,1,1,0,
      0,0,1,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,1,0,0,
      0,1,1,1,0,0,0,1,0,0,
      0,0,0,0,0,0,0,1,0,0,
      0,0,0,0,0,0,0,0,0,0)

    val osc = Array(
      0,0,0,0,0,
      0,0,0,0,0,
      0,1,1,1,0,
      0,0,0,0,0,
      0,0,0,0,0
    )

    val gameOfLifeMatrix: MDArray[Int] = reshape(size :: size :: Nil, arr2)
    //val gameOfLifeMatrix: MDArray[Int] = reshape(5::5::Nil, osc)

    gameOfLifeMatrix
  }
}
