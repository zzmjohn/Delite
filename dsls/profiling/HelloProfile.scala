

import example.profiling._


object HelloProfileRunner extends ProfileApplicationRunner with HelloProfile


trait HelloProfile extends ProfileApplication {
  
  def main() {
    var acc = true
    val time = profile (100) times {
      acc = false
    } report average
    println("average loop time " )
  }
  
}
