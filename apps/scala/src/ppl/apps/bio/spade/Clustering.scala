
package ppl.apps.bio.spade

import ppl.dsl.optiml.OptiMLApplication
import ppl.dsl.optiml.datastruct.scala.{ACluster, Vector, Matrix}

trait Clustering {
  this: OptiMLApplication =>
  
  def cluster(d: Rep[Matrix[Double]], k: Rep[Int]): Rep[Vector[Int]] = {
    val obs     = d.numRows
    val dim     = d.numCols
    val data    = Vector[Double](obs*dim, true)
    val centers = Vector[Double](obs*dim, true)
    println("obs = " + obs + ", dim = " + dim + ", k = " + k)
/*
    val ac = ACluster(3)
    val pr = APair(ac, 5)
    val pq = PQ(4)
    println("pr = APair(" + pr._1 + ", " + pr._2 + ")")

    val members = Vector[Int](1, true)
    ac.init_RM(data, centers, members, 6)
    println("ACluster.dim = " + ac.dim)
    println("ACluster.index = " + ac.index)
    println("ACluster.valid = " + ac.valid)
    ac.valid_=(false)
    println("ACluster.valid = " + ac.valid)
    println("ACluster.merged = " + ac.merged)
    println("ACluster.offset = " + ac.offset)
    println("ACluster.num_members = " + ac.num_members)
    println("ACluster.members(0) = " + ac.members(0))

    println("PQ.MMD = " + pq.MMD)
    println("PQ.empty = " + pq.empty)
    pq.push(ac, 20)
    pq.push(pr)
    println("PQ.empty = " + pq.empty)
    pq.pop
    println("PQ.MMD = " + pq.MMD)

    println("normalize")
    pq.normalize
    println("merge_in_pq")
    val ac1 = ACluster(3)
    val members1 = Vector[Int](1, true)
    ac1.init_RM(data, centers, members1, 7)
    ac1.merge_in_pq(pq)
    println("PQ.empty = " + pq.empty)
    println("ACluster.num_members = " + ac1.num_members)
*/

    var idx = 0
    var i = 0
    while(i < obs){
      var j = 0
      while(j < dim){
        data(idx) = d(i)(j)
        centers(idx) = data(idx);
        idx += 1
        j += 1
      }
      i += 1
    }

    val c_ap = Vector[ACluster](obs, true)
    i = 0
    while(i < obs){
      c_ap(i) = ACluster(dim)
      val members = Vector[Int](1, true)
      c_ap(i).init_RM(data, centers, members, i)
      i += 1
    }

    var ac_valid = c_ap
    var flag = true
    var round = 0
    while(flag){
      //ac_valid = ac_valid.partition(_.valid)._1
      //ac_valid mfilter {a:Rep[ACluster]=> a.valid}

      println("round = " + round + ", num_valid = " + ac_valid.length)
/*
      if(round==5){
        /*
        val ac_valid_val = ac_valid
        for(i <- (0::ac_valid_val.length-1)){
          if(ac_valid_val(i).num_members==1)
            ac_valid_val(i).valid_=(false)
        }
        ac_valid = ac_valid_val.partition(_.valid)._1
        */
        //ac_valid = ac_valid map {e=> if(e.num_members==1) e.valid_=(false); e}
        //ac_valid = ac_valid.partition(_.valid)._1
        //ac_valid mfilter {_.valid}
      }
*/
/*
      val num_valid = ac_valid.length
      // TODO: anyway to write the line below more elegant?
      if(num_valid.asInstanceOfL[Double] < 1.5*k.asInstanceOfL[Double]){
        flag = false
      }
      else{
        if(round!=0){
          // TODO: sort is non-mutable
          //ac_valid = ac_valid.sort
          //ac_valid.foreach{ _.reset_RM }
          var i = 0
          while(i < ac_valid.length){
            ac_valid(i).reset_RM
            i += 1
          }
        }
        val limit = max(1, num_valid/5000).asInstanceOfL[Int]

        // TODO: Concise
/*
        for(ac_into <- ac_valid if !ac_into.merged){
          ac_into.merged_=(true)
          val candidates = (0::num_valid-1) map {from=> ac_into.getCandidates(ac_valid(from))}
          ac_into.mergeCandidates(candidates, limit)
        }
*/

        // TODO: Imperative
        var into = 0
        while(into < num_valid){
          if(!ac_valid(into).merged){
            ac_valid(into).merged_=(true)
            val pq = PQ(limit)
            val offset_center = ac_valid(into).offset
            val offset_end = offset_center + dim
            var from = 0
            while(from < num_valid){
              if(into != from){
                // note: ac_valid(into).push_on_pq(ac_valid(from), pq)
                val from_cluster = ac_valid(from)
                var i = 0
                val num_members = from_cluster.num_members
                while(i < num_members){
                  // note: val d = absdist(into_center, from_cluster.members(i))
                  var offset0 = offset_center
                  var offset1 = from_cluster.members(i)
                  var d:Double = 0
                  while(offset0 < offset_end){
                    val tmp = centers(offset0) - data(offset1)
                    d += abs(tmp)
                    offset0 += 1
                    offset1 += 1
                  }
                  pq.push(from_cluster, d)
                  i += 1
                }
              }
              from += 1
            }
            ac_valid(into).merge_in_pq(pq)
          }
          into += 1
        }
        // System.exit(-1)
        round += 1

      }
*/
    }

    // Assignment and clean up
    val assgn = Vector[Int](obs, true)
    //val ac_valid = ac_valid.partition(_.valid)._1
    //ac_valid mfilter {a:Rep[ACluster]=> a.valid}
    var cur_Id = 0
    while(cur_Id < ac_valid.length){
      var b = 0
      val num_members = ac_valid(cur_Id).num_members
      val members = ac_valid(cur_Id).members
      while(b < num_members){
        assgn(members(b)/dim) = cur_Id+1
        b += 1
      }
      cur_Id += 1
    }

    assgn
  }

}

