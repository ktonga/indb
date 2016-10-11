package com.github.ktonga.indb

import scalaz.concurrent.Task


object Netflix {
  val TitleR = """.+ aria-label="(.+?)" role.+""".r

  def fetchMyList(getHtml: Task[String]): Task[Seq[String]] = {
    def decodeHtml(str: String) = str.replaceAll("&#x27;", "'").replaceAll("&amp;", "&")
    def extractTitle(card: String) = TitleR.unapplySeq(card).flatMap(_.headOption.map(decodeHtml))
    def parseMyList(htmlText: String) = htmlText.split("smallTitleCard").toSeq.drop(1).map(extractTitle)
    getHtml.map(parseMyList(_).flatten)
  }

}
