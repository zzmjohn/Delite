package ppl.delite.benchmarking.reverseweb

object Config {
  
  val num_procs = Integer.parseInt(System.getProperty("threads", "1"))
  
}
