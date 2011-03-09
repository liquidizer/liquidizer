package org.liquidizer.snippet

import scala.xml._
import scala.collection.mutable

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper._

import _root_.org.liquidizer.lib._
import _root_.org.liquidizer.model._

/** Controls the state of a preference vote with two buttons for + and - */
class VoteControl(var value : Int, val id : Int, min : Int, max : Int) {

  /** determine style for option pref */
  def getStyle(pref : Int) = 
    if (pref!=value) "inactive" else "active "+
    (if (pref<0) "contra" else if (pref>0) "pro" else "pass")

  /** determine style for option pref */
  def format(pref : Int) = (if (pref>0) "+" else "") + pref

  /** render the current value */
  def renderValue() : NodeSeq =
    for (no <- min to max) yield { 
      val id2="vote"+id+"_"+no
      SHtml.a(<div id={id2} class={ getStyle(no) }>{ format(no) }</div>,
	      SetHtml(id2, <img src="/images/vote.gif" class="vote_anim"/>)
	      & SHtml.ajaxInvoke(() => updateValue(no))._2) 
    }
      
  /** function to count votes and updates visuals */
  def updateValue(newValue : Int) : JsCmd = {
    val oldValue= value
    value = newValue
    SetHtml("vote"+id, renderValue())
  }

  /** render the voting control */
  def render(in : NodeSeq) : NodeSeq = {
    <div id={"vote"+id} class="vote">{ renderValue() }</div>
  }
}
