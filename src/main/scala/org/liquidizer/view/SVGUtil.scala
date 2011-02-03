package org.liquidizer.view

import scala.util.matching.Regex
import scala.xml._
import java.io._
import java.text._
import java.util.Locale

object SVGUtil {

  val df= new DecimalFormat("0.##", new DecimalFormatSymbols(Locale.US))
  def format(value : Double) = df.format(value)

  def hexParser(value : CharSequence):Int = {
    def hexDigitParser(value : Char):Int = {
      if (value>='0' && value<='9') value-'0'
      else
	if (value>='A' && value<='F') value-'A'+10
	else
	  if (value>='a' && value<='f') value-'a'+10
	  else 
	    0
    }
    (1 to value.length).foldLeft(0) { (a,b) => a*16 + hexDigitParser(value.charAt(b-1)) }
  }
  def parseColor(color:String) = 
    (hexParser(color.substring(1,3)), hexParser(color.substring(3,5)), hexParser(color.substring(5,7)))
  
  val DIGITS= ('0' to '9')++('a' to 'f')
  def hexFormat(value: Int, len:Int):String =
    (0 to len-1).reverse.map { digit => DIGITS((value >> (4*digit))&15) }.mkString

  def formatColor(r: Int, g:Int, b:Int) : String = 
    "#"+(List(r,g,b).map { channel => hexFormat(Math.min(255,Math.max(0,channel)),2) }.mkString)


  def resize(svg:Node, width:Int, height:Int, scale:Double = 1.0) : Node = {
    val oldWidth=svg.attribute("width").get.text.replace("pt","").toInt
    val oldHeight=svg.attribute("height").get.text.replace("pt","").toInt
    val viewBox= List(
      oldWidth*(1-1/scale)/2,
      oldHeight*(1-1/scale)/2,
      oldWidth/scale,
      oldWidth/scale)
    svg match {
      case Elem(prefix, label, attribs, scope, children @ _*) => {
	val newAttr= attribs
	.remove("width").remove("height")
	.append(new UnprefixedAttribute(
	  "width",width.toString, new UnprefixedAttribute(
	    "height", height.toString, new UnprefixedAttribute(
	      "viewBox", viewBox.mkString(" "), Null))))
	  Elem(prefix, label, newAttr, scope, children :_*)
      }
      case _ => svg
    }
  }

  def normalizePath(path: String) : String = {
	  val NUMBER= "[+-]?[0-9]+(\\.[0-9]+)?".r
	  val PATHSEG= ("[a-zA-Z]|("+NUMBER+"),("+NUMBER+")").r
	  var mode= "  "
	  var index= 0
	  var argnum= 1
	  var x=  0.0
	  var x0= 0.0
	  var y=  0.0
	  var y0= 0.0
	  PATHSEG.replaceAllIn(path, seg => {
		  val text= seg.matched
		  if (text.length==1) {
			  x0= x
			  y0= y
			  argnum= text.toUpperCase match {
				  case "C" => 3
				  case "Q" => 2
				  case _ => 1
			  }
			  x0=x
			  y0=y
			  index=0
			  if (text.toUpperCase==mode.toUpperCase) {
				  mode= text
				  ""
			  } else {
				  mode= text
				  text.toUpperCase
			  }

	 	  } else {
	 	 	  if (index == argnum) { x0=x; y0=y; index=0 }
	 	 	  index += 1
	 	 	  if (mode == mode.toUpperCase) {
	 	 	 	  x= seg.group(1).toDouble
	 	 	 	  y= seg.group(3).toDouble
	 	 	 	  text
	 	 	  } else {
	 	 	 	  val dx= seg.group(1).toDouble
	 	 	 	  val dy= seg.group(3).toDouble
	 	 	 	  x= x0+dx
	 	 	 	  y= y0+dy
	 	 	 	  format(x)+","+format(y)
	 	 	  }
	 	  }
	  }).replaceAll("  "," ")
  }
}

class CommandAPI(command : String) {

  val p = Runtime.getRuntime().exec(command);
  val stdOutput = new BufferedWriter(new OutputStreamWriter(p.getOutputStream()));
  val stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
  val stdError = new BufferedReader(new InputStreamReader(p.getErrorStream()));

  def out(line:String) = { 
    stdOutput.write(line+"\n") 
  }
  
  def closeAll() = {
    stdInput.close()
    stdError.close()
    stdOutput.close()
  }

  def getSVG() : NodeSeq = {
    val buf= new java.lang.StringBuilder
    stdOutput.flush()
    stdOutput.close()

    try {
      var aws = stdInput.readLine
      while(aws!=null) {
	buf.append(aws+"\n")
	aws= stdInput.readLine
      }
    } finally {
      closeAll
    }

    val src=scala.io.Source.fromString(buf.toString)
    val doc=scala.xml.parsing.XhtmlParser.apply(src)
    (doc)
  }
}
