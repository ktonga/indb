package com.github.ktonga.indb

import org.http4s._
import org.http4s.client._
import org.http4s.client.blaze._
import org.http4s.util.{ CaseInsensitiveString => CIS }
import scalaz.concurrent.Task
import scalaz.std.option._
import scalaz.syntax.applicative._


object HTTP {

  val client = PooledHttp1Client()

  def shutdown(): Unit = client.shutdownNow()

  def getNetflixMyListHtml(cookie: String): Task[String] = {
    println("Fetching Netflix 'My List'...")
    val req = Request(
      uri = Uri.uri("https://www.netflix.com/browse/my-list"),
      headers = Headers(Header.Raw(CIS("Cookie"), cookie))
    )
    client.expect[String](req).mapFailure { t => new RuntimeException("Error fetching Netflix 'My List'", t) }
  }

  def getOmdbByTitle(title: String): Task[Result[OmdbLookup]] = {
    import io.circe.generic.auto._
    import org.http4s.circe._
    case class OmdbResponse(Response: String, Title: Option[String], imdbID: Option[String], Error: Option[String])
    val resp = client.expect(Uri.uri("http://www.omdbapi.com").+?("t", title))(jsonOf[OmdbResponse])
    resp.map { r =>
      if(r.Response == "True")
        (r.Title |@| r.imdbID)(OmdbLookup(_, _))
          .fold(failureMsg[OmdbLookup]("Empty?"))(success)
      else
        failure(MissingTitleError(title))
    }
  }

  def addTitleToImdbList(cookie: String, listId: String)(titleId: String): Task[Result[ImdbListItem]] = {
    import io.circe.generic.auto._
    import org.http4s.circe._
    case class ImdbAddResponse(status: Int, position: Option[String], list_item_id: Option[String])
    val req = Method.POST(
      Uri.uri("http://www.imdb.com/list/_ajax/edit"),
      UrlForm("const" -> titleId, "list_id" -> listId, "ref_tag" -> "title")
    ).map(_.putHeaders(Header.Raw(CIS("Cookie"), cookie)))
    val resp = client.expect(req)(jsonOf[ImdbAddResponse])
    resp.map { r =>
      if(r.status == 200)
        (r.position |@| r.list_item_id)(ImdbListItem(_, _))
          .fold(failureMsg[ImdbListItem]("Empty?"))(success)
      else
        failureMsg[ImdbListItem]("Cannot add title to list")
    }.handle { case t => failureMsg[ImdbListItem](t.toString) }
  }

}
