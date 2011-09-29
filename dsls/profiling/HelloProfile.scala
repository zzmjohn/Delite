

import example.profiling._


object HelloProfileRunner extends ProfileApplicationRunner with HelloProfile


trait HelloProfile extends ProfileApplication {
  
  def main() {
    println("average loop time")
  }
  
}
