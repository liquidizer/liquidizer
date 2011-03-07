import org.liquidizer.lib.VoteVector

object VoteVectorTest {
    val v1= new VoteVector(1)
    val v2= new VoteVector(2)
    val v3= new VoteVector(3)
    val v4= new VoteVector(4)

    for (i <- 1 to 5) {
      println(v1)
      println(v2)
      println(v3)
      println(v4)
      println()

      val w= 1.0
      v1.clear()
      v1.addDelegate(w/2.0, v2)

      v2.clear()
      v2.addDelegate(w, v3)
      v2.addDelegate(w, v4)
      
      v3.clear()
      v3.addDelegate(w/2.0, v2)

    }
    exit(0)
}
