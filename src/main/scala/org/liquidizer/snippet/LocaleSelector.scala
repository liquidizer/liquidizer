package org.liquidizer.snippet

import scala.xml._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._

import java.util.Locale

class LocaleSelector {
  def render(in : NodeSeq) : NodeSeq = {
    val uri= S.uri
    for (loc <- LocaleSelector.locales) yield {
      SHtml.a(() => {
	LocaleSelector.setLocale(loc)
        RedirectTo(uri) }, Text(" ["+loc.toUpperCase+"] "))
    }
  }
}

object LocaleSelector {
  val locales= List("de","en","ru")
  def suggestLocale(loc : Locale) : Unit = {
    if (myLocale.is.isEmpty) setLocale(loc)
  }
  def setLocale(loc : Locale) : Unit = {
    if (locales.contains(loc.getLanguage))
      myLocale.set(Some(loc))
  }
  def setLocale(loc : String): Unit = setLocale(new Locale(loc))
  def getLocale() = myLocale.is.getOrElse(Locale.getDefault)

  object myLocale extends SessionVar[Option[Locale]](Empty)
}	       

