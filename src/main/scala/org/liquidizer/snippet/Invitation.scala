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

  def isValid() = 
      InviteCode.get(code).exists {
      ic => !ic.user.defined_? || !ic.user.obj.get.validated }

  def validAndFree(in : NodeSeq) : NodeSeq = 
    if (isValid) in else NodeSeq.Empty

  def invalid(in : NodeSeq) : NodeSeq = 
    if (!code.isEmpty && !isValid) in else NodeSeq.Empty

  def isEmpty(in : NodeSeq) : NodeSeq =
    if (code.isEmpty) in else NodeSeq.Empty

  def code(in : NodeSeq) : NodeSeq = Text(code)

  def cheatLink(in : NodeSeq) : NodeSeq = {
    <div id="cheatLink">{
     SHtml.a(() => {
	 val uuid = java.util.UUID.randomUUID();
	 InviteCode.create.code(uuid.toString).save
	 val link= "/index.html?code="+uuid.toString
	 SetHtml("cheatLink", <a href={link}>{link}</a>)
       }, in)
    }</div>
  }
}
