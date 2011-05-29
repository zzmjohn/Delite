package ppl.dsl.optiml.datastruct.scala

import collection.mutable.{HashMap, ArrayBuffer}

/**
 * Author: Bo Wang
 * Date: May 28, 2011
 * Time: 12:04:18 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */


object LinearRegression {

  def fit(data: HashMap[String, ArrayBuffer[(Int,Double)]]) = {
    val model = new HashMap[String, (Double,Double,Double)]
    for((kernelName,listOfData) <- data){
      val n = listOfData.length
      var sumX = 0.0
      var sumY = 0.0
      for((x,y) <- listOfData){
        sumX += x
        sumY += y
      }
      val avgX = sumX / n
      val avgY = sumY / n
      val div = (avgX+avgY)/2
      var sigma1 = 0.0
      var sigma2 = 0.0
      var sigma3 = 0.0
      var sigma4 = 0.0
      var sigma5 = 0.0
      for((x,y) <- listOfData){
        val x1 = x/div
        val y1 = y/div
        sigma1 += x1*y1
        sigma2 += x1*x1
        sigma3 += (x1-avgX/div)*(y1-avgY/div)
        sigma4 += (x1-avgX/div)*(x1-avgX/div)
        sigma5 += (y1-avgY/div)*(y1-avgY/div)
      }
      val b = (sigma1-n*avgX/div*avgY/div) / (sigma2-n*avgX/div*avgX/div)
      val a = avgY - b*avgX
      val r = sigma3/(math.sqrt(sigma4)*math.sqrt(sigma5))
      model += kernelName -> (a, b, r)
    }
    model
  }

}

