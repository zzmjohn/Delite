version := "0.2"

organization := "Stanford_PPL"

retrieveManaged := true
scalacOptions in (Compile,doc) := Seq("-diagrams", "-diagrams-debug", "-diagrams-max-classes 25")
//resolvers := Seq(mavenLocal)
