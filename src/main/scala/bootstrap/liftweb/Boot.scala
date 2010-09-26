package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.java.sql.{Connection, DriverManager}

import _root_.org.liquidizer.view._
import _root_.org.liquidizer.model._
import _root_.org.liquidizer.lib._


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
    Schemifier.schemify(true, Schemifier.infoF _, User, Query, Vote, Comment)
    VoteCounter.init

    // where to search snippet
    LiftRules.addToPackages("org.liquidizer")

    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)

    LiftRules.dispatch.append {
    case Req(List("queries",query,"chart.svg"),_,_) => () => TimeseriesView.queryChart(query)
      case Req(List("users",user,"chart.svg"),_,_) => () => TimeseriesView.userChart(user)
      case Req(List("users",user,"vote","queries",query,"chart.svg"),_,_) => () => TimeseriesView.voteQueryChart(user, query)
      case Req(List("users",user,"vote","users",user2,"chart.svg"),_,_) => () => TimeseriesView.voteUserChart(user, user2)
      case Req(List("queries",query,"delegation.svg"),_,_) => () => DelegationGraphView.queryGraph(query)
      case Req(List("users",user,"delegation.svg"),_,_) => () => DelegationGraphView.userGraph(user)
      case Req(List("queries",query,"histogram.svg"),_,_) => () => HistogramView.hist(query)
      case Req(List("graph.svg"),_,_) => () => DelegationGraphView.superGraph()
      case Req(List("emotions","front.svg"),_,_) => () => EmotionView.front()
    }

    LiftRules.statelessRewrite.append {
      case RewriteRequest(
        ParsePath(List("queries",query),_,_,_),_,_) =>
        RewriteResponse("query_details" :: Nil, Map("query" -> query))
      case RewriteRequest(
        ParsePath(List("queries",query,"delegation"),_,_,_),_,_) =>
        RewriteResponse("query_graph" :: Nil, Map("query" -> query))
      case RewriteRequest(
        ParsePath(List("queries",query,"histogram"),_,_,_),_,_) =>
        RewriteResponse("query_hist" :: Nil, Map("query" -> query))
      case RewriteRequest(
        ParsePath(List("users",user),_,_,_),_,_) =>
        RewriteResponse("user_details" :: Nil, Map("user" -> user))
      case RewriteRequest(
        ParsePath(List("queries",query,"analyzer"),_,_,_),_,_) =>
        RewriteResponse("query_analyzer" :: Nil, Map("query" -> query))
      case RewriteRequest(
        ParsePath(List("users",user,"analyzer"),_,_,_),_,_) =>
        RewriteResponse("user_analyzer" :: Nil, Map("user" -> user))
      case RewriteRequest(
        ParsePath(List("users",user,"support"),_,_,_),_,_) =>
        RewriteResponse("user_support" :: Nil, Map("user" -> user))
      case RewriteRequest(
        ParsePath(List("users",user,"delegation"),_,_,_),_,_) =>
        RewriteResponse("user_graph" :: Nil, Map("user" -> user))
      case RewriteRequest(
        ParsePath(List("users",user,"vote","queries",query,"analyzer"),_,_,_),_,_) =>
        RewriteResponse("vote_analyzer" :: Nil, Map("user" -> user, "query" -> query))
      case RewriteRequest(
    	ParsePath(List("users",user,"vote","users",user2,"analyzer"),_,_,_),_,_) =>
        RewriteResponse("vote_analyzer" :: Nil, Map("user" -> user, "user2" -> user2))
    }

//  make all DB updates atomic
//    S.addAround(DB.buildLoanWrapper)
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }
}
