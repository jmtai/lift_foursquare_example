package code {

import net.liftweb.common.{ Box, Full, Empty }
import net.liftweb.http._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import scala.xml.NodeSeq
import org.scalafoursquare.call.{UserlessApp, HttpCaller, AuthApp}
import org.scalafoursquare.response.{VenueSearchResponse, UserCompact, Response, VenueExploreResponse, VenueCompact, VenueLocation, CheckinForFriend}
import net.liftweb.json.Serialization.{read, write}

object SearchManager {
  val CLIENT_ID = "" // Enter your client_id here
  val CLIENT_SECRET = "" // Enter your client_secret here

  implicit val formats = Serialization.formats(NoTypeHints)

  def processSearchInput: LiftRules.DispatchPF = { 
    case Req("search" :: Nil, _, GetRequest) =>
      () => {
        val term = S.param("term")
        val city = S.param("loc") match {
          case Full(loc) => loc
          case _ => "Chicago, IL"
        }

        var resp: List[JObject] = Nil

        term match {
          case Full(text) =>
            val userlessApp = new UserlessApp(HttpCaller(CLIENT_ID, CLIENT_SECRET, readTimeout = 10000))
            val results = userlessApp.venueSearch(near = Some(city), query = Some(text), limit = Some(6)).get

            results.response.map(res => {
              val venues = res.venues

              venues.foreach(v => {
                var imageUrl = ""
                var sizes: List[Int] = Nil 
                var fileType = ""
                val categories = v.categories

                if (!categories.isEmpty) {
                  val category = categories.head

                  imageUrl = category.icon.prefix.getOrElse("")
                  sizes = category.icon.sizes.getOrElse(Nil)
                  fileType = category.icon.name.getOrElse("")
                }

                val location = v.location.address.getOrElse("")

                var size = {
                  if (!sizes.isEmpty) {
                    if (sizes.contains(32)) {
                      32
                    } else {
                      sizes.head
                    }
                  }
                }

                var image = NodeSeq.Empty

                if (imageUrl != "") {
                  imageUrl += size + fileType
                  image = <img src={imageUrl} style="width: 32px; height: 32px" />
                }

                val html = <div style="text-align: left; width: 250px; margin: 3px 0px">
                             <div style="float: left; width: 32px; height: 32px; margin-right: 5px">
                               {image}
                             </div>
                             <div style="height: 16px; line-height: 16px; font-weight: bold; overflow: hidden; text-overflow: ellipsis; white-space: nowrap">{v.name}</div>
                             <div style="height: 16px; line-height: 16px; color: gray; overflow: hidden; text-overflow: ellipsis; white-space: nowrap">{location}</div>
                           </div>

                 var displayName = v.name
                 if (location != "") {
                   displayName += " (" + location + ")"
                 }  

                 resp :+= ("label" -> html.toString) ~ ("value" -> displayName) ~ ("json" -> write(v))
              })
            })

          case _ =>
        }

        Full(new PlainTextResponse(compact(render(resp)),
                                   S.getHeaders(Nil),
                                   200))
      }
  }
}

}
