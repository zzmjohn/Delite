package ppl.apps.ml.rbm

import ppl.dsl.optiml._
import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix}
import ppl.delite.framework.DeliteApplication

object RBMRunner extends OptiMLApplicationRunner with RBM

trait RBM extends OptiMLApplication {

  def print_usage = {
    println("Usage: RBM <MNIST data file> <numHiddenUnits> <numcases>")
    exit(-1)
  }

  def main() = {
    reseed

    if (args.length < 3) print_usage

    val maxEpoch = 10 // maximum number of epochs
    val numHiddenUnits = Integer.parseInt(args(1))

    val epsilonw = 0.1f // Learning rate for weights
    val epsilonvb = 0.1f // Learning rate for biases of visible units
    val epsilonhb = 0.1f //Learning rate for biases of hidden units
    val weightcost = 0.0002f
    val initialmomentum = 0.5f
    val finalmomentum = 0.9f

    println("Using " + numHiddenUnits + " hidden units.")
    
    println("Reading MNIST dataset")
    val numcases = Integer.parseInt(args(2)) // batchsize
    //val numcases = 100 // batchsize
    val trainingdata = MLInputReader.read(args(0)).toFloat(e => e.floatValue())
    val numdims = trainingdata.numCols
    val numbatches = trainingdata.numRows / numcases

    // Initialize symmetric weights and biases
    val vishid = Matrix.randnf(numdims, numHiddenUnits).mutable //* 0.1f
    vishid mmap { _ * 0.1f }
    val hidbiases = Vector.zerosf(numHiddenUnits).mutable
    val visbiases = Vector.zerosf(numdims).mutable

    val vishidinc = Matrix.zerosf(numdims, numHiddenUnits).mutable
    val hidbiasinc = Vector.zerosf(numHiddenUnits).mutable
    val visbiasinc = Vector.zerosf(numdims).mutable

    tic()
    var epoch = 0
    while (epoch < maxEpoch) {
      var errsum = 0f
      var batch = 0
      while (batch < numbatches) {
        //println("Epoch: " + epoch + ", Batch: " + batch)

        // Positive phase
        //PerformanceTimer.start("RBM-posphase", false)
        val data = trainingdata.sliceRows(batch * numcases, (batch + 1) * numcases) // data: numcases x numdims
        val poshidprobs = (data * vishid + hidbiases.replicate(numcases, 1)).sigmoidf
        val posprods = data.t * poshidprobs
        val poshidact = poshidprobs.sumCol
        val posvisact = data.sumCol
        val poshidstates = (poshidprobs :> Matrix.randf(numcases, numHiddenUnits))
        //val poshidstates = (poshidprobs zip Matrix.randf(numcases, numHiddenUnits)){ (a,b) => if (a < b) 0f else 1f }
        //PerformanceTimer.stop("RBM-posphase", false)

        // Negative phase
        //PerformanceTimer.start("RBM-negphase", false)
        val negdata = (poshidstates * vishid.t + visbiases.replicate(numcases, 1)).sigmoidf
        val neghidprobs = (negdata * vishid + hidbiases.replicate(numcases, 1)).sigmoidf
        val negprods = negdata.t * neghidprobs
        val neghidact = neghidprobs.sumCol
        val negvisact = negdata.sumCol
        val diff = data - negdata
        errsum += (diff *:* diff).sum
        //PerformanceTimer.stop("RBM-negphase", false)

        // Update weights and biases
        //PerformanceTimer.start("RBM-biasupdates", false)
        val momentum = if (epoch > 5) finalmomentum else initialmomentum
        vishidinc mmap { _ * momentum }
        vishidinc += ((posprods - negprods) / numcases  - (vishid * weightcost))*epsilonw
        visbiasinc mmap { _ * momentum }
        visbiasinc += (posvisact - negvisact) * (epsilonvb / numcases)
        hidbiasinc mmap { _ * momentum }
        hidbiasinc += (poshidact - neghidact) * (epsilonhb / numcases)
        //vishidinc = vishidinc * momentum + ((posprods - negprods) / numcases  - (vishid * weightcost))*epsilonw
        //visbiasinc = visbiasinc * momentum + (posvisact - negvisact) * (epsilonvb / numcases)
        //hidbiasinc = hidbiasinc * momentum + (poshidact - neghidact) * (epsilonhb / numcases)

        vishid += vishidinc
        visbiases += visbiasinc
        hidbiases += hidbiasinc
        //PerformanceTimer.stop("RBM-biasupdates", false)
        batch += 1
      }
      println("--> Epoch " + epoch)
      println(" error = " + errsum)
      epoch += 1
    }
    toc()

    //PerformanceTimer.print("RBM-posphase")
    //PerformanceTimer.save("RBM-posphase")
    //PerformanceTimer.print("RBM-negphase")
    //PerformanceTimer.save("RBM-negphase")
    //PerformanceTimer.print("RBM-biasupdates")
    //PerformanceTimer.save("RBM-biasupdates")
  }
}
