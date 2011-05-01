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
  val CONTENT_R= "(<p>|<li>)(.*)".r
  val CONC_R= "((X|PA|SÄA)[0-9]{3})".r
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
		var old= values.get(name.trim).getOrElse("")
		if (m.isEmpty) {
		  if (old.length>0) state=None
		} else {
		  if (old.length>0) old=old+" ";
		  values += name.trim -> dehtml(old+m.get.group(2).trim)
		}
	      }
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
	    println()
	    println(no+": "+conc)
	    val ms= CONC_R.findAllIn(conc.get)
	    for (a2 <- ms) {
	      concMap += no -> (a2 :: concMap.get(no).getOrElse(Nil))
	    }
	    println(concMap.get(no))
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

  def processPreselected(file : File) {
    val queries= Query.findAll
    val input= new BufferedReader(new FileReader(file))
    try {
      var aws = input.readLine
      while(aws!=null) {
	val no= aws.trim
	queries.filter { _.what.is.startsWith(no) }
	.foreach { q => q.keys(q.keys.is+" #vorselektiert").save }
	aws= input.readLine
      }
    } finally {
      input.close
    }
  }

  def processConcurrency() {
    concMap= concMap.map { case (a,b) => (a, b.map{ _.replaceAll(" ","")}.removeDuplicates ) }
    var blocks= concMap.toList.flatMap { e => e._1 :: e._2 }.removeDuplicates
    val queries= Query.findAll
    var bno= 1
    concMap.foreach {case (a,b) => println(a + ":" +b) }
    while (!blocks.isEmpty) {
      val h= blocks.head
      var block= h :: concMap.get(h).getOrElse(Nil)
      for (i <- 1 to 10) {
	for (b <- block) {
          block ++= concMap.filter {_._2.contains(b)}.map { _._1 }.toList
          block ++= concMap.get(b).getOrElse(Nil)
	}
	block= block.removeDuplicates
      }
      println("#block"+bno+" = "+block)
      for (b <- block) {
	println("'"+b+"'")
      	for (q <- queries.filter { _.what.is.startsWith(b) }) {
          q.keys(q.keys.is+" #block"+("%02d".format(bno))).save
        }
      }
      bno += 1
      blocks = blocks -- block
    }
  }

  def run() : Unit = {
    Query.findAll.filter { _.what.is.contains("<b>") }
    .foreach { q => q.what(dehtml(q.what.is)).save }
    val dir= new File("wiki.piratenpartei.de/Antragsportal/")
    process(dir)
    processConcurrency()

    val presel= new File("preselect.txt")
    if (presel.exists)
      processPreselected(presel)

    val inv= new File("invitations.txt")
    if (inv.exists)
      readInviteCodes(inv)
  }
}
