package org.liquidizer.snippet

import scala.xml._
import scala.collection.mutable

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper._
import Helpers._

import _root_.org.liquidizer.lib._
import _root_.org.liquidizer.model._

class Invitation {
 
  val code= S.param("code").getOrElse("")

  def validAndFree(in : NodeSeq) : NodeSeq = 
    if (InviteCode.get(code).exists(!_.user.defined_?)) in else NodeSeq.Empty

  def invalid(in : NodeSeq) : NodeSeq = 
    if (InviteCode.get(code).isEmpty) in else NodeSeq.Empty

  def code(in : NodeSeq) : NodeSeq = Text(code)
}
