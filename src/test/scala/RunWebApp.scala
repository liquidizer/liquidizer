import _root_.org.mortbay.jetty.Connector
import _root_.org.mortbay.jetty.Server
import _root_.org.mortbay.jetty.webapp.WebAppContext
import org.mortbay.jetty.nio._
import java.util.Properties
import java.io.FileInputStream

object RunWebApp extends Application {

  override def main(args : Array[String]) = {
    val port= if (args.size>0) args(0).toInt else 8080
    println(port)

    val server = new Server
    val scc = new SelectChannelConnector
    scc.setPort(port)
    server.setConnectors(Array(scc))

    val context = new WebAppContext()
    context.setServer(server)
    context.setContextPath("/")
    context.setWar("src/main/webapp")

    server.addHandler(context)

    try {
      println(">>> STARTING EMBEDDED JETTY SERVER, PRESS ANY KEY TO STOP")
      server.start()
      while (System.in.available() == 0) {
	Thread.sleep(5000)
      }
      server.stop()
      server.join()
    } catch {
      case exc : Exception => {
	exc.printStackTrace()
	System.exit(100)
      }
    }
  }
}
