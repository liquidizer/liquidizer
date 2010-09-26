package org.liquidizer.view

import java.io.File
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

class Mesmerizer {

  abstract class Param
  case class NumberParam(val value:Double) extends Param
  case class ColorParam(val r:Double, g:Double, b:Double) extends Param

  case class BaseShape (
    val children: Map[String, BaseShape], 
    val attributes: Map[String, (Regex, String)]) {
	  
	def isEmpty() = children.isEmpty && attributes.isEmpty

	def format(): Iterable[String] =
		attributes.map { case (key, (pattern,format)) => "@"+key+" : "+format } ++
		children.flatMap { case (key, child) => key :: (child.format.map { "  "+_ }.toList) }
	override def toString() : String = format.mkString("\n")
  }

  case class ParametricShape (
    val children: Map[String, ParametricShape],
    val values: Map[String, List[Param]]) {
    
    def format(): Iterable[String] =
		values.map { case (key, value) => "@"+key+" : "+value } ++
		children.flatMap { case (key, child) => key :: (child.format.map { "  "+_ }.toList) }
	override def toString() : String = format.mkString("\n")
  }
  
  class MatchException(msg : String) extends Exception(msg)

  val PARAM= "[+-]?[0-9]+(\\.[0-9]+)?|#[0-9a-fA-F]{6}".r
  def id(node : Node) = node.attribute("id").map { _.text }.getOrElse("("+node.label+")")
  
  /** Extract a tree of regex patterns which match variations of this base shape */
  def extractBaseShape(node : Node) : BaseShape = {
    val children= node.child
    .filter { !_.label.startsWith("#") }
    .map { child => (id(child) -> extractBaseShape(child)) }
    .filter { case (key,value) => ! value.isEmpty}
    val attributes= node
    	.attributes.elements.map {
    	attr => attr.key match {
    		case "transform" => ("transform" -> getPattern(attr.value.toString))
    		case "style" => ("style" -> getPattern(attr.value.toString))
    		case "d" => ("d" -> getPattern(SVGUtil.normalizePath(attr.value.toString)))
    		case _ => ("" -> ("".r,""))
     }}.filter { _._1 != "" }.toList
     BaseShape(Map(children:_*), Map(attributes:_*))
   }

   /** Use the regex pattern in the base shape to extract variable parameters in the shape */
   def extractParameters(node: Node, base: BaseShape): ParametricShape = {
	 try {
	     val children= base.children.map {
	    	 case (key, child) =>
	    	 	val matchedChild= node.child.filter { id(_)==key }
	    	 	if (matchedChild.isEmpty)
	    	 		throw new MatchException(id(node)+" does not have a child "+key)
	    	 	(key -> extractParameters(matchedChild.first, child))
		 }
	     val values= base.attributes.map {
	    	 case (key, (pattern, format)) =>
	    	 	val value= node.attribute(key).getOrElse(
	    	 			{throw new MatchException(" Missing attribute "+key)}).text
	    	 	(key -> getParameters( if (key=="d") SVGUtil.normalizePath(value) else value, pattern))
	     }
	     ParametricShape(children, values)
	 } catch {
	 case e:MatchException => throw new MatchException( id(node)+"/"+e.getMessage() )
	 }
   }

   def getPattern(text: String) : (Regex, String) = {
	 (PARAM.replaceAllIn(text
			 .replaceAll("\\(","\\\\(")
			 .replaceAll("\\)","\\\\)"), m => "([^,; ]+)").r,
	  PARAM.replaceAllIn(text, "*"))
   }

   def getParameters(text: String, pattern: Regex): List[Param] = {
	   pattern.findPrefixMatchOf(text) match {
	  	   case Some(m) =>
	  	     m.subgroups.map { param =>
		       if (param.startsWith("#")) {
			 val c= SVGUtil.parseColor(param)
			 ColorParam(c._1, c._2, c._3)
		       } else 
			 NumberParam(param.toDouble) }
	  	   case _ =>
	  	     throw new MatchException(" pattern not matched: "+text+" does not match "+pattern)
	   }
   }

   def formatParam(value : Param):String = value match {
     case NumberParam(value) => 
     if (value == value.toInt)
       value.toInt.toString
     else
       "%1.2f" format value
     case ColorParam(r,g,b) => SVGUtil.formatColor(r.toInt,g.toInt,b.toInt)
   }
   
   def format(shape: Node, base: BaseShape, param: ParametricShape): Node = shape match {
	   case Elem(prefix, label, attribs, scope, children @ _*) => {
	     val newAttribs= base.attributes.foldLeft (attribs) { 
	    	 case (a, (key, (pattern, format))) =>
	    	 	val value= param.values.get(key).get.elements
	    	 	val newValue= "\\*".r.replaceAllIn(format, _ => formatParam(value.next))
	    	 	a.remove(key).append(new UnprefixedAttribute(key,newValue.replaceAll("\\\\",""), Null))
	     }
	     val newChildren= children.map {
	    	 child => base.children .get(id(child)) match {
	    		 case Some(childBase) => format(child, childBase, param.children.get(id(child)).get)
	    		 case _ => child
	    	 }
	     }
	     Elem(prefix, label, newAttribs, scope, newChildren :_*)
	   }
	   case _ => shape
   }

   def morph(left: Double, right: Double, shape1:List[Param], shape2:List[Param]):List[Param] = {
     val it= shape2.elements
     shape1.map { v => (v,it.next) match {
       case (NumberParam(a), NumberParam(b)) => 
	 NumberParam(left * a + right * b)
       case (ColorParam(ar,ag,ab), ColorParam(br,bg,bb)) => 
	 ColorParam(left * ar + right * br,
		    left * ag + right * bg,
		    left * ab + right * bb)
       case _ => throw new Exception("Parameter types don't match")
     }}
   }
     
   def morph(left: Double, right:Double, shape1: ParametricShape, shape2: ParametricShape): ParametricShape = {
     ParametricShape(
       shape1.children.map { 
       case (key,value) => key -> morph(left, right, value, shape2.children.get(key).get) 
       },
       shape1.values.map { 
       case (key,value) => key -> morph(left, right, value, shape2.values.get(key).get) 
       }
     )
   }

   def morph(factor:Double, shape1: ParametricShape, shape2: ParametricShape) : ParametricShape =
       morph(factor, 1-factor, shape1, shape2)

  def load(file:String) : Node = {
    val root="src/main/resources/"
    val src= scala.io.Source.fromFile(new File(root+file+".svg"))
    val doc= scala.xml.parsing.XhtmlParser.apply(src)
    doc.first
  }

  def extractParameters(file: String, base: BaseShape): ParametricShape = {
    try {
      extractParameters(load(file), base)
    } catch {
      case e:MatchException =>
        println("ERROR in file : "+file)
	println(e.getMessage())
	throw new MatchException(file+": "+e.getMessage)
    }
   }

   val master= load("front-master")
   val base = extractBaseShape(master)
   val shape1= extractParameters("front-v0-a1-p1", base)
   val shape2= extractParameters("front-v1-a1-p1", base)
   val shape3= extractParameters("front-v0-a0-p1", base)
   val shape4= extractParameters("front-v1-a0-p1", base)
   val shape5= extractParameters("front-v0-a1-p0", base)
   val shape6= extractParameters("front-v1-a1-p0", base)
   val shape7= extractParameters("front-v0-a0-p0", base)
   val shape8= extractParameters("front-v1-a0-p0", base)

   def emoticon(v: Double, a: Double, p:Double):Node = {
    val param= morph(p,
		  morph(a, 
    		    morph(v, shape2, shape1), 
    		    morph(v, shape4, shape3)),
		  morph(a, 
    		    morph(v, shape6, shape5), 
    		    morph(v, shape8, shape7)))
    format(master, base, param)
  }
}
