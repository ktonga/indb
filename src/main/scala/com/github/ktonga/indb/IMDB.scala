package com.github.ktonga.indb

import scalaz.concurrent.Task


object IMDB {

  def addToList(http: String => Task[Result[ImdbListItem]])(titleId: String): Task[Result[ImdbListItem]] =
    http(titleId)

}
