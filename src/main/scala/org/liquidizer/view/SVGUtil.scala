package org.liquidizer.view

import scala.util.matching.Regex
import scala.xml._

object SVGUtil {
  
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

  def formatColor(r: Int, g:Int, b:Int) = 
    "#"+(List(r,g,b).map { channel => hexFormat(Math.min(255,Math.max(0,channel)),2) }.mkString)


  def resize(svg:Node, width:Int, height:Int) : Node = {
    val oldWidth=svg.attribute("width").get.text.toInt
    val oldHeight=svg.attribute("height").get.text.toInt

    svg match {
      case Elem(prefix, label, attribs, scope, children @ _*) => {
	val newAttr= attribs
	.remove("width").remove("height")
	.append(new UnprefixedAttribute(
	  "width",width.toString, new UnprefixedAttribute(
	    "height", height.toString, new UnprefixedAttribute(
	      "viewBox", "0 0 "+oldWidth+" "+oldHeight, Null))))
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
	 	 	 	  ("%1.2f" format x)+","+("%1.2f" format y)
	 	 	  }
	 	  }
	  }).replaceAll("  "," ")
  }
}
