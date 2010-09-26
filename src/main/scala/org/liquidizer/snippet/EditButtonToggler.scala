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

  def newCommentRecord(text : ()=> String, update : String => Unit) =
    newRecord(ToggleButtonData(index, text, () => Markup.renderComment(text()), update, 5, 60))

  def newLineRecord(text : ()=> String, update : String => Unit) =
    newRecord(ToggleButtonData(index, text, () => Markup.renderComment(text()), update, 1, 48))

  def newKeyListRecord(keys : ()=> List[String], update : String => Unit) =
    newRecord(ToggleButtonData(index, 
			       () => keys().mkString("\n"), 
			       () => Markup.renderTagList(keys()),
			       text => update(text), 4, 25))

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
    SHtml.a(() => { dynamicForm(data) }, Text("Edit"))
  }

  def dynamicForm(data : ToggleButtonData) : JsCmd = {
    SetHtml(data.textId, 
	    if (data.h==1) {
	      SHtml.text(data.getText(), 
			 { text => data.setText(text) }, 
			 "cols"->data.w.toString)
	    } else {
	      SHtml.textarea(data.getText(), 
			     { text => data.setText(text) }, 
			     "rows"->data.h.toString, "cols"->data.w.toString)
	    }) &
    SetHtml(data.buttonId, SHtml.ajaxSubmit("save", () => {
      SetHtml(data.textId, data.getHtml()) &
      SetHtml(data.buttonId, editButton(data))
    }))
  }
}

object Markup {

  val url= "(http:/[^\\s]*[^\\s!?&.<>])".r
  val nl= "(\\s*(\n|\r)+\\s*)+".r
 
  def renderComment(in:String) : NodeSeq = {
    if (in.length==0) return Nil
    val node:Node= Text(in)
    var text= node.toString

    text= url.replaceAllIn(text, "<a href=\"$1\">$1</a>")
    text= nl.replaceAllIn(text, "<br/>")

    try {
      val src=scala.io.Source.fromString("<div>"+text+"</div>")
      scala.xml.parsing.XhtmlParser.apply(src)
    } catch {
      case _ => Text(in)
    }
  }

  def renderTag(tag : String) : Node = {
    <a href={"/queries.html?search="+tag}><div>{tag}</div></a>
  }

  def renderTagList(in:List[String]) : NodeSeq = {
    renderTagList(in, in.size+1, "tagList")
  }

  def renderTagList(in : List[String], len : Int, id : String) : NodeSeq = {
    <span>{in.slice(0, len).map { tag => renderTag(tag) }}</span>
    <span id={id}>{
      if (len < in.size)
	SHtml.a(() => SetHtml(id, 
			      renderTagList(in.slice(0,len).toList, len, id+"x")),
		<div>...</div>)
		   
      else 
	NodeSeq.Empty
    }</span>
  }
}

