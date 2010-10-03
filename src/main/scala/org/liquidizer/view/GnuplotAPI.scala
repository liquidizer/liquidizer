package org.liquidizer.view

import java.io._
import scala.xml._

import org.liquidizer.lib._

object LiquidColors {
//  val contra_light= "#ffc8a2"
//  val pro_light= "#c7fbb3"
//  val contra= "#ff6400"
//  val pro= "#06a500"
  val pro= "#FF9933"
  val contra= "#CC0000"
  val pro_light= "#ffefac"
  val contra_light= "#ffd6d6"  //"cc6667"
}

class GnuplotAPI {
  // run gnuplot through a process         
  val p = Runtime.getRuntime().exec("gnuplot");  
  val stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
  val stdOutput = new BufferedWriter(new OutputStreamWriter(p.getOutputStream()));
  val stdError = new BufferedReader(new InputStreamReader(p.getErrorStream()));

  lazy val tzOffset= java.util.TimeZone.getDefault.getOffset(now)
  val now= Tick.now
  var minX= 0L
  var islog= false
  var hasgrid= false

  def close() = {
    stdInput.close()
    stdError.close()
    stdOutput.close()
  }

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
    run("set boxwidth "+options.get("dx").getOrElse("0.5"))
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
    stdOutput.write(cmd + "\n");   
  }

  def getSVG() : NodeSeq = {
    run("unset output")
    run("exit")
    stdOutput.flush
    stdOutput.close()

    val buf= new java.lang.StringBuilder
    var aws = stdInput.readLine
    while(aws!=null) {
      buf.append(aws+"\n")
      aws= stdInput.readLine
    }
    
    val src=scala.io.Source.fromString(buf.toString)
    val doc=scala.xml.parsing.XhtmlParser.apply(src)
    doc
  }

  def formatX(x:Long) : String = {
    if (islog) {
//      Math.exp(1e-9*(x-now).toDouble).toString
      Math.exp(1e-8*(x-now).toDouble).toString
    } else {
      ((x+tzOffset)/1000).toString
    }
  }

  def formatData[A](data : List[Tick[A]], f:(A => Double)) = {
    var oldX= now
    run(formatX(oldX)+" 0")
    if (!data.isEmpty) {
      var oldY= f(data.first.value)
      data.foreach {
	case Tick(newX,quote) => {
	  if (oldX>minX) {
	    var y= f(quote)
	    run(formatX(oldX)+" "+y)
	    run(formatX(Math.max(newX,minX))+" "+y)
	    oldX= newX
	  }
	}}
      run(formatX(Math.max(minX,data.last.time))+" "+0);
    }
    run("e")
  }

  /** Plot a time series */
  def plotTS(data : List[Tick[Quote]], options : Map[String, String],
	   shade:Boolean, pro:Boolean, contra:Boolean) : Node = {
    setOptions(options)
    try {
      run("plot "+
	  (if (shade) {
	    "'-' using 1:2 with filledcurve title '' lt rgb '"+LiquidColors.pro_light+"', "+
	    "'-' using 1:2 with filledcurve title '' lt rgb '"+LiquidColors.contra_light+"', " } else "")+
	  (if (contra) {
	    "'-' using 1:2 with filledcurve title '' lt rgb '"+LiquidColors.contra+"', " } else "")+
	  (if (pro) {
	    "'-' using 1:2 with filledcurve title '' lt rgb '"+LiquidColors.pro+"' " } else ""))
      
      if (shade) {
	formatData[Quote](data, quote => quote.pro);
	formatData[Quote](data, quote => -quote.contra);
      }
      if (contra)         formatData[Quote](data, quote => Math.min(quote.pro-quote.contra,0));
      if (pro && contra)  formatData[Quote](data, quote => Math.max(quote.pro-quote.contra,0));
      if (pro && !contra) formatData[Quote](data, quote => quote.pro);
      
      getSVG.first
    } finally {
      close
    }
  }

  def hist(data : List[List[(Double,Double)]], 
	   options : Map[String,String]) : Node = {
    try {
      setHistOptions(options)
      run ("plot '-' with boxes title '' lt rgb '#999999', " +
	   "'-' with boxes title '' lt rgb '"+LiquidColors.contra_light+"', " +
	   "'-' with boxes title '' lt rgb '"+LiquidColors.pro_light+"', " +
	   "'-' with boxes title '' lt rgb '"+LiquidColors.contra+"', " +
	   "'-' with boxes title '' lt rgb '"+LiquidColors.pro+"'")
      data.foreach {
	set => 
	  set.foreach {
	    row => run(row._1+" "+row._2)
	  }
	run("e")
      }
      getSVG.first
    } finally {
      close
    }
  }
}
