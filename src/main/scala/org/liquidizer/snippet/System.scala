package org.liquidizer.snippet

import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper._

import _root_.org.liquidizer.model._
import _root_.org.liquidizer.lib._

/** Render generic information about the Liquidizer system */
class System extends InRoom {

  /** List of new users to be shown in the sidebar */
  def newUsers(in : NodeSeq) : NodeSeq = {
    val data= Votable.findAll(By(Votable.room, room),
			      By_>(Votable.user, 0),
			      OrderBy(Votable.id, Descending), MaxRows(5))
    val helper= new VotingHelper
    data.flatMap { helper.bind(in, _) }
  }

  /** List of new queries to be shown in the sidebar */
  def newQueries(in : NodeSeq) : NodeSeq = {
    val data= Votable.findAll(By(Votable.room, room),
			      By_>(Votable.query, 0),
			      OrderBy(Votable.id, Descending), MaxRows(4))
    val helper= new VotingHelper
    data.flatMap { helper.bind(in, _) }
  }
}
