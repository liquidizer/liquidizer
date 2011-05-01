package org.liquidizer.lib.bpt

import java.io._
import java.text._

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.mapper._

import org.liquidizer.model._
import org.liquidizer.lib._

object StartUp {

  val TITLE_R= "class=\"firstHeading\"[^>]*>([^<]*)<".r
  val HEAD_R= "class=\"mw-headline\"[^>]*>([^<]*)<".r
  val CONTENT_R= "^(<p>|<ul><li>)(.*)".r
  val CONC_R= ">(\\p{L}+[0-9]+) ".r
  val DATE_FORMAT= new SimpleDateFormat("dd.mm.yyyy")

  var concMap= Map[String, List[String]]()

  def dehtml(str:String) = str
  .replaceAll("&amp;","&")
  .replaceAll("</?b>","")

  def process(dir : File) : Unit = {
    for (file <- dir.listFiles) {
      if (file.isDirectory) {
	process(file)
      } else {
	var state : Option[String] = None
	var values= Map[String,String]()

	val reader = new InputStreamReader(new FileInputStream(file), "UTF-8")
	val input= new BufferedReader(reader)
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
	  println("Ignored: "+file.getName)
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
	  .replaceAll(" und ","&")
	  .replaceAll("^.$","")
	  .replaceAll(" ","")
	  query.keys(typ+" "+group)
	  query.save
	  
	  val conc= values.get("Konkurrenzanträge")
	  if (!conc.isEmpty) {
	    val m= CONC_R.findFirstMatchIn(conc.get)
	    if (!m.isEmpty) {
	      val a2= m.get.group(1)
	      concMap += no -> (a2 :: concMap.get(no).getOrElse(Nil))
	    }
	  }
	}
      }
    }
  }

  def readInviteCodes(file : File) {
    val input= new BufferedReader(new FileReader(file))
    try {
      var aws = input.readLine
      while(aws!=null) {
	val code= aws.trim
	if (code.length>0 && InviteCode.get(code).isEmpty) {
	  InviteCode.create.code(code).save
	}
	aws= input.readLine
      }
    } finally {
      input.close
    }
  }

  def processConcurrency() {
    var blocks= concMap.toList.flatMap { e => e._1 :: e._2 }.removeDuplicates
    val queries= Query.findAll
    var bno= 1
    while (!blocks.isEmpty) {
      val h= blocks.head
      var block= h :: concMap.get(h).get
      for (i <- 1 to 10) {
	for (b <- block) {
	  block ++= concMap.filter {_._2.contains(b)}.map { _._1 }.toList
	  block ++= concMap.get(b).getOrElse(Nil)
	}
	block= block.removeDuplicates
      }
      println(block)
      for (b <- block) {
      	for (q <- queries.filter { _.what.is.startsWith(b) }) {
          q.keys(q.keys.is+" #block"+bno).save
        }
      }
      bno += 1
      blocks = blocks -- block
    }
  }

  def run() : Unit = {
    val dir= new File("wiki.piratenpartei.de/Antragsportal/")
    process(dir)
    processConcurrency()

    val inv= new File("invitations.txt")
    if (inv.exists)
      readInviteCodes(inv)
  }
}
