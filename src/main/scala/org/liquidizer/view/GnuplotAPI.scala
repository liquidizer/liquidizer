package org.liquidizer.view

import scala.xml._

import org.liquidizer.lib.VoteMap.DECAY
import org.liquidizer.model.Tick
import org.liquidizer.model.Quote

object LiquidColors {
  val pro= "#FF9933"
  val contra= "#CC0000"
  val pro_light= "#ffefac"
  val contra_light= "#ffd6d6"
}

class GnuplotAPI extends CommandAPI("gnuplot") {

  lazy val tzOffset= java.util.TimeZone.getDefault.getOffset(now)
  val now= Tick.now
  var minX= 0L
  var islog= false
  var hasgrid= false

  def setBaseOptions(options : Map[String, String]) = {
    val width= options.get("width").getOrElse("640").toInt
    val height= options.get("height").getOrElse("480").toInt
    run("set term svg size "+width+","+height)
    
    options.get("grid") match {
      case Some("on") => {
	hasgrid= true
	run("set grid")
      } 
      case _ => {
	hasgrid= false
	run("set tmargin at screen 0.01")
	run("set rmargin at screen 0.99")
	run("set bmargin at screen 0.99")
	run("set lmargin at screen 0")
	run("unset border")
	run("set xzeroaxis")
	run("unset xtics")
	run("unset ytics")
	run("unset grid")
      }
    }
  }

  def setHistOptions(options : Map[String, String]) = {
    setBaseOptions(options)
    run("set style histogram rowstacked")
    run("set style fill solid")
    run("set yrange [0:]")
    run("set xrange [-1:1]")
    run("set boxwidth "+options.get("dx").map{ _.toDouble*0.9 }.getOrElse(0.5))
  }

  def setOptions(options : Map[String,String]) = {
    setBaseOptions(options)

    val h= 3600000L
    def setTimeAxis(t : Int) = {
      islog= false
      minX=now- t*h
      run("set timefmt '%s'")
      run("set xdata time")
      run("set xrange['"+formatX(minX)+"':'"+formatX(now)+"']")
    }
    options.get("axis") match {
      case Some("log") => {
	islog= true
	minX= 0L
	run("unset xdata")
	if (hasgrid)
	  run("set xtics ('' 0, " +
	      (1 to 7).map(t=> ("'"+t+"' "+formatX(now-t*24*h))).mkString(", ") + ", " +
	      "'    2 weeks' "+formatX(now - 24*14*h)+", "+
	      (3 to 7).map(t=> ("'"+t+"' "+formatX(now-t*7*24*h))).mkString(", ") + ", " +
	      "'days' 1)")
	run ("set xrange[0:1]")
      }
      case Some("h") => 
	setTimeAxis(1)
      run("set format x \"%R\"")
      case Some("d") => 
	setTimeAxis(24)
      run("set format x \"%R\"")
      case Some("w") => 
	setTimeAxis(7*24)
      run("set format x \"%a\"")
      case Some("m") => 
	setTimeAxis(30*24)
      run("set format x \"%b-%d\"")
      case Some("y") => 
	setTimeAxis(365*24)
      run("set format x \"%b\"")
      case _ => {
	islog= false
	minX= 0L
	run("set timefmt '%s'")
	run("set xdata time")
	run ("set xrange[:'"+formatX(now)+"']")
      }
    }
  }

  def run(cmd : String) = {
    //println("gnuplot> "+cmd)
    super.out(cmd)
  }

  override def getSVG() : NodeSeq = {
    run("unset output")
    run("exit")
    super.getSVG()
  }

  def formatX(x:Long) : String = {
    if (islog) {
      Math.exp(1e-9*(x-now).toDouble).toString
    } else {
      ((x+tzOffset)/1000).toString
    }
  }

  def formatData(data : List[Tick], f:(Quote => Double)) = {
    var lastX= now
    run(formatX(lastX)+" 0")
    if (!data.isEmpty) {
      var oldY= f(data.first.quote)
      for (tick <- data) {
	val newX= tick.time.is
	if (lastX>minX) {
	  val y= f(tick.quote)
	  val dy= y * Math.exp(DECAY * (newX - lastX))
	  run(formatX(lastX)+" "+dy)
	  run(formatX(Math.max(newX,minX))+" "+y)
	}
 	lastX= newX
     }
      run(formatX(Math.max(minX,data.last.time))+" "+0);
    }
    run("e")
  }

  /** Plot a time series */
  def plotTS(data : List[Tick], options : Map[String, String]) : Node = {
    setOptions(options)
    run("plot "+
	  "'-' using 1:2 with filledcurve title '' lt rgb '"+LiquidColors.pro_light+"', "+
	  "'-' using 1:2 with filledcurve title '' lt rgb '"+LiquidColors.contra_light+"', " +
	  "'-' using 1:2 with filledcurve title '' lt rgb '"+LiquidColors.contra+"', " +
	  "'-' using 1:2 with filledcurve title '' lt rgb '"+LiquidColors.pro+"' ")
    formatData(data, quote => quote.pro);
    formatData(data, quote => -quote.contra);
    formatData(data, quote => Math.min(quote.pro-quote.contra,0))
    formatData(data, quote => Math.max(quote.pro-quote.contra,0));
    
    getSVG.first
  }

  def hist(data : List[(Double,Double)], 
	   options : Map[String,String]) : Node = {

    setHistOptions(options)
    run ("plot '-' with boxes title '' lt rgb '"+LiquidColors.contra+"', " +
	 "     '-' with boxes title '' lt rgb '"+LiquidColors.pro+"'")
    
    data.filter { _._1 < 0 }.foreach { row => run(row._1+" "+row._2) }
    run("e")
    
    data.filter { _._1 > 0 }.foreach { row => run(row._1+" "+row._2) }
    run("e")
    
    getSVG.first
  }
}
