package bootstrap.liftweb

import scala.xml._

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.mapper._

import org.liquidizer.view._
import org.liquidizer.model._
import org.liquidizer.snippet._
import org.liquidizer.lib._

object LadeAntragsDaten {

  def ladeDaten() {
    val room= Room.find(By(Room.name,"BPT11.2")).get
    val me= User.find(By(User.nick,"Dadim")).get
    val data= XML.loadFile("bpt.xml")
    for (antrag <- data \\ "antrag") {
      val titel= antrag.attribute("titel").get.text
      val nummer= antrag.attribute("nummer").get.text
      val gruppe= antrag.attribute("gruppe").get.text
      val lqfb= antrag.attribute("lqfb")
      val wiki= antrag.attribute("wiki")
      val pad= antrag.attribute("pad")
      
      var name= nummer+": "+titel
      name += " [http://wiki.piratenpartei.de/Bundesparteitag_2011.2/Antragsportal/"+nummer+" Portal]"
      if (!lqfb.isEmpty)
	name+= "["+lqfb.get.text+" LQFB]"
      if (!wiki.isEmpty)
	name+= "[http://wiki.piratenpartei.de"+wiki.get.text+" Wiki]"
      if (!pad.isEmpty)
	name+= "["+pad.get.text+" Pad]"

      val query= Query.create
      .what(name)
      .creator(me)
      .creation(Tick.now)
      query.save
      val nominee=Votable.create
      .query(query)
      .room(room)
      nominee.save
      if (gruppe.length>0)
	PollingBooth.comment(me,nominee,"#"+gruppe.replaceAll(" ","_"));
    }
  }

}
