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
class VoteControl(var value : Int, val id : Int, nonNegative : Boolean) {
  
  /** render the current value */
  def renderValue() : Node =
    <span class={
      if (value==0) "pass" else if (value>0) "pro" else "contra" } >{
	(if (value>0) "+" else "") + value } </span>
      
  /** function to count votes and updates visuals */
  def updateValue(diff : Int) : JsCmd = {
    val oldValue= value
    value = oldValue + diff
    (
      if (nonNegative && (value==0 || oldValue==0))
	SetHtml("nay_switch_"+id, nayButton())
      else
	JsCmds.Noop
    ) & SetHtml("value"+id, renderValue())
  }

  /** render the +/- voting control buttons */
  def render(in : NodeSeq) : NodeSeq = {
    <table class="vote">
    <tr>
    <td>{ splash("yay_"+id, "yay", +1) }</td>
    <td><span id={"value"+id}>{ renderValue() }</span></td>
    <td><span id={"nay_switch_" + id}>{ nayButton() }</span></td>
    </tr>
    </table>
  }

  /** render a minus button, that grays out to avoid negative input */
  def nayButton() = 
    if (nonNegative && value <=0) 
      img("nay_gray") 
    else
      splash("nay_"+id, "nay", -1)

  /** render an image html element */
  def img(base : String) =
    <img src={"/images/"+base+".png"}/>

  /** render a vote button that splashes, while vote recounting takes place */
  def splash(id : String, base : String, diff : Int) : NodeSeq= {
    SHtml.a(<span id={id}>{img(base)}</span>, 
	    SetHtml(id, img(base+"_splash")) &
	    SHtml.ajaxInvoke(() => updateValue(diff) & SetHtml(id, img(base)) )
	    ._2)
  }
}
