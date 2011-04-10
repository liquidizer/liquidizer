package bootstrap.liftweb

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.provider._
import net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import Helpers._

import java.sql._
import java.util.Locale

import org.liquidizer.view._
import org.liquidizer.model._
import org.liquidizer.snippet._
import org.liquidizer.lib._
import org.liquidizer.lib.ssl._


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {

    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = 
	new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
			     Props.get("db.url") openOr "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
			     Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }
    Schemifier.schemify(true, Schemifier.infoF _, Votable, User, 
			Query, Vote, Comment, Certificate, Emotion, Tick,
		        InviteCode)

    println("Starting LIQUIDIZER")
    bpt.StartUp.run()
    VoteMap.refresh(false)

    // where to search snippet
    LiftRules.addToPackages("org.liquidizer")

    // Internationalization
    LiftRules.early.append(makeUtf8)
    LiftRules.localeCalculator = req => localeCalculator(req)
    LiftRules.resourceNames = 
      "instance" :: 
      "liquidizer" ::  LiftRules.resourceNames

    LiftSession.onBeginServicing =
      certificateLogin _ :: LiftSession.onBeginServicing
    
    // dynamic pages
    LiftRules.dispatch.append {
      case Req(List("queries",query,"chart.svg"),_,_) => () => TimeseriesView.queryChart(query)
      case Req(List("queries",query,"delegation.svg"),_,_) => () => DelegationGraphView.queryGraph(query)
      case Req(List("queries",query,"histogram.svg"),_,_) => () => HistogramView.hist(query)
      case Req(List("users",user,"delegation.svg"),_,_) => () => DelegationGraphView.userGraph(user)
      case Req(List("users",user,"chart.svg"),_,_) => () => TimeseriesView.userChart(user)
      case Req(List("emoticons","face.svg"),_,_) => () => EmotionView.face()
      case Req(List("user_mgt","reset_password", id),_,_) => () => UserInfo.passwordReset(id)
    }

    LiftRules.statelessRewrite.append {
      case RewriteRequest(
        ParsePath(List("queries",query,"index"),_,_,_),_,_) =>
        RewriteResponse("query_details" :: Nil, Map("query" -> query))
      case RewriteRequest(
        ParsePath(List("queries",query,"graph"),_,_,_),_,_) =>
        RewriteResponse("query_graph" :: Nil, Map("query" -> query))
      case RewriteRequest(
        ParsePath(List("queries",query,"histogram"),_,_,_),_,_) =>
        RewriteResponse("query_hist" :: Nil, Map("query" -> query))
      case RewriteRequest(
        ParsePath(List("queries",query,"analyzer"),_,_,_),_,_) =>
        RewriteResponse("query_analyzer" :: Nil, Map("query" -> query))
      case RewriteRequest(
        ParsePath(List("queries",query,"info"),_,_,_),_,_) =>
        RewriteResponse("query_info" :: Nil, Map("query" -> query))
      case RewriteRequest(
        ParsePath(List("users",user,"index"),_,_,_),_,_) =>
        RewriteResponse("user_details" :: Nil, Map("user" -> user))
      case RewriteRequest(
        ParsePath(List("users",user,"analyzer"),_,_,_),_,_) =>
        RewriteResponse("user_analyzer" :: Nil, Map("user" -> user))
      case RewriteRequest(
        ParsePath(List("users",user,"support"),_,_,_),_,_) =>
        RewriteResponse("user_support" :: Nil, Map("user" -> user))
      case RewriteRequest(
        ParsePath(List("users",user,"delegates"),_,_,_),_,_) =>
        RewriteResponse("user_delegates" :: Nil, Map("user" -> user))
      case RewriteRequest(
        ParsePath(List("users",user,"graph"),_,_,_),_,_) =>
        RewriteResponse("user_graph" :: Nil, Map("user" -> user))
      case RewriteRequest(
        ParsePath(List("users",user,"vote","queries",query,"analyzer"),_,_,_),_,_) =>
        RewriteResponse("vote_analyzer" :: Nil, Map("user" -> user, "query" -> query))
      case RewriteRequest(
    	ParsePath(List("users",user,"vote","users",user2,"analyzer"),_,_,_),_,_) =>
        RewriteResponse("vote_analyzer" :: Nil, Map("user" -> user, "user2" -> user2))
    }

    //  make all DB updates atomic
    //  S.addAround(DB.buildLoanWrapper)
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }

  def localeCalculator(request : Box[HTTPRequest]): Locale = {
    if (LocaleSelector.myLocale.is.isEmpty) {
      val locale= LiftRules.defaultLocaleCalculator(request)
      LocaleSelector.suggestLocale(locale)
    }
    LocaleSelector.getLocale
  }

  private def certificateLogin(session: LiftSession, req: Req) {
    import net.liftweb.mapper.{By, ByList}
    if (!User.loggedIn_?) {
      for {
        certs <- SSLClient.valid_certificates
        known_cert <- Certificate.find(ByList(Certificate.id, certs.map(SSLClient.certificate_id(_))))
        owner <- User.find(By(User.id, known_cert.owner))
      } User.logUserIn(owner)
    }
  }
}

