package com.github.ktonga.indb

import scalaz.concurrent.Task
import fs2.{ Task => _, _ }
import fs2.interop.scalaz._


object INDB extends App {

  val netflixCookie = "NetflixId=ct%3DBQAOA..."
  val imdbListId = "ls063634480"
  val imdbCookie = "session-id=221-5043443-1..."

  def addOrSkip(doAdd: String => Task[Result[ImdbListItem]])(res: Result[OmdbLookup]): Task[Result[ImdbListItem]] =
    res.fold(e => Task.now(failure(e)), r => doAdd(r.imdbID))

  def debug[A]: Pipe[Task, A, A] = { in => in.evalMap { a => Task.delay { println(a); a } } }

  val summary = Stream
    .eval(Netflix.fetchMyList(HTTP.getNetflixMyListHtml(netflixCookie)))
    .flatMap(Stream.emits)
    .evalMap(OMDB.lookupTitle(HTTP.getOmdbByTitle) _)
    .evalMap(addOrSkip(IMDB.addToList(HTTP.addTitleToImdbList(imdbCookie, imdbListId))))
    .through(debug)
    .zipWithScan(Summary.empty) { (s, r) => s.append(r) }
    .runLast

  val res = summary.unsafePerformSyncAttempt
  HTTP.shutdown()
  println(res)
  res.fold({ t => t.printStackTrace(); sys.exit(1) }, s => println(s))
}
