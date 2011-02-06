package org.liquidizer.snippet

import scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.common._

case class ToggleButtonData(
  val index : Int,
  val getText : ()=> String,
  val getHtml : ()=> NodeSeq,
  val setText : String => Unit,
  val h:Int,
  val w:Int) {

  val editNode= <img src="/images/edit.png" class="button" alt="edit"/>
  val saveNode= <img src="/images/save.png" class="button" alt="save"/>

  def textId = "toggleText"+index
  def buttonId = "toggleButton"+index
}

class EditButtonToggler() {
  var index= 0;
  var current : ToggleButtonData = null

  def newRecord(data : ToggleButtonData) {
    index += 1
    current = data
  }

  def newCommentRecord(text : ()=> String, update : String => Unit, maxlen : Int = 0) =
    newRecord(ToggleButtonData(index, text, () => trimmedComment(text(), maxlen, index), update, 5, 50))

  def trimmedComment(in : String, maxlen : Int, id : Int) : NodeSeq = {
    if (in==null || maxlen==0 || in.length < maxlen+50) {
      Markup.renderComment(in)
    } else {
      <span id={"togglemore"+id}>{Markup.renderComment(in.substring(0,maxlen))}
      <div style="text-align:right">
      { SHtml.a(Text("... mehr ..."), SetHtml("togglemore"+id, Markup.renderComment(in))) }
      </div></span>
    }
  }

  def newLineRecord(text : ()=> String, update : String => Unit) =
    newRecord(ToggleButtonData(index, text, () => Markup.renderComment(text()), update, 1, 50))

  def newKeyListRecord(keys : ()=> List[String], update : String => Unit) =
    newRecord(ToggleButtonData(index, 
			       () => keys().mkString("\n"), 
			       () => Markup.renderTagList(keys()),
			       text => update(text), 1, 50))

  def toggleText() : NodeSeq = {
    <span id={current.textId}>{current.getHtml()}</span>
  }

  def toggleButton() : NodeSeq = {
    val formCurrent= current
    <span id={current.buttonId}>{
      editButton(formCurrent)
    }</span>
  }

  def editButton(data : ToggleButtonData) : NodeSeq = {
    SHtml.a(() => { dynamicForm(data) }, data.editNode)
  }

  def dynamicForm(data : ToggleButtonData) : JsCmd = {
    SetHtml(data.textId, 
	    if (data.h==1) {
	      SHtml.text(data.getText(), 
			 { text => {data.setText(text)} }, 
			 "size"->data.w.toString)
	    } else {
	      SHtml.textarea(data.getText(), 
			     { text => data.setText(text) }, 
			     "rows"->data.h.toString, "cols"->data.w.toString)
	    }) &
    SetHtml(data.buttonId, SHtml.ajaxSubmit(S ? "user.data.save",() => {
      SetHtml(data.textId, data.getHtml()) &
      SetHtml(data.buttonId, editButton(data))
    }))
  }
}
