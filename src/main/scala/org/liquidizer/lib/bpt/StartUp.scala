package org.liquidizer.lib.bpt

import java.io._
import java.text._
import scala.xml._

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.mapper._

import org.liquidizer.model._
import org.liquidizer.lib._

object StartUp {

  def load(file:File) : Node = {
    val src= scala.io.Source.fromFile(file)
    val doc= scala.xml.parsing.XhtmlParser.apply(src)
    doc.first
  }

  def load(file:String) : Node = load(new File(file))

  val TITLE_R= "class=\"firstHeading\"[^>]*>([^<]*)<".r
  val HEAD_R= "class=\"mw-headline\"[^>]*>([^<]*)<".r
  val CONTENT_R= "^(<p>|<ul><li>)(.*)".r
  val DATE_FORMAT= new SimpleDateFormat("dd.mm.yyyy")

  def dehtml(str:String) = str.replaceAll("&amp;","&")

  def process(dir : File) : Unit = {
    for (file <- dir.listFiles) {
      if (file.isDirectory) {
	process(file)
      } else {
	println("ANTRAG: "+file.getName)
	var state : Option[String] = None
	var values= Map[String,String]()

	val input= new BufferedReader(new FileReader(file))
	try {
	  var aws = input.readLine
	  while(aws!=null) {
	    state match {
	      case None => {
		val m= HEAD_R.findFirstMatchIn(aws)
		if (!m.isEmpty) {
		  state= Some(m.get.group(1))
		}
		val m2= TITLE_R.findFirstMatchIn(aws)
		if (!m2.isEmpty)
		  values += "title" -> dehtml(m2.get.group(1).trim)
	      }
	      case Some(name) => {
		val m= CONTENT_R.findFirstMatchIn(aws)
		if (!m.isEmpty) {
		  values += name.trim -> dehtml(m.get.group(2).trim)
		  state = None
		}}
	    }
	    aws= input.readLine
	  }
	} finally {
	  input.close
	}
	
	val title= values.get("Antragstitel")
	if (title.isEmpty || title.get.contains("<strike>")) {
	  println("Ignored")
	} else {
	  val no= values.get("Antragsnummer").get
	  val url= "http://wiki.piratenpartei.de/"+values.get("title").get
	  val text= no+": "+title.get+" "+url.replaceAll(" ","_")
	  val query= Query.find(By(Query.what, text)).getOrElse {
	    val date= values.get("Einreichungsdatum").map {
	      DATE_FORMAT.parse(_).getTime() }.getOrElse(0L)
	    val q= Query.create
	    .what(text)
	    .creation(date)
	    q.save
	    Votable.create.query(q).save
	    q
	  }
	  val typ= values.get("Antragstyp").getOrElse("").replaceAll(" ","")
	  val group= if (typ.contains("Satzung")) "" else
	    values.get("Antragsgruppe").getOrElse("")
	  .replaceAll("Keine der Gruppen","")
	  .replaceAll(" ","")
	  query.keys(typ+" "+group)
	  query.save
	}
      }
    }
  }

  def run() : Unit = {
    val dir= new File("wiki.piratenpartei.de/Antragsportal/")
    process(dir)

    val codes= load("invitations.xml")
    for (code <- codes \ "code") {
      val text= code.child.text.trim
      if (InviteCode.get(text).isEmpty) {
	println("Adding invitation code: "+text)
	InviteCode.create.code(text).save
      }
    }
  }
}
