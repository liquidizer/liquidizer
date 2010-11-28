import org.liquidizer.lib.VoteVector

object VoteVectorTest {
    val v1= new VoteVector(1)
    val v2= new VoteVector(2)
    val v3= new VoteVector(3)
    val v4= new VoteVector(4)

    for (i <- 1 to 5) {
      println(v1 + " -> "+ v1.getInflow)
      println(v2 + " -> "+ v2.getInflow)
      println(v3 + " -> "+ v3.getInflow)
      println(v4 + " -> "+ v4.getInflow)
      println()

      val w= 1.0
      v1.clear(1.0)
      v1.addSupporter(w/2.0, v2)

      v2.clear(1.0)
      v2.addSupporter(w, v3)
      v2.addSupporter(w, v4)
      
      v3.clear(1.0)
      v3.addSupporter(w/2.0, v2)

    }
    exit(0)
}
