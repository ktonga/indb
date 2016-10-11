package com.github.ktonga.indb

import scalaz.concurrent.Task


object OMDB {

  def lookupTitle(http: String => Task[Result[OmdbLookup]])
                 (title: String): Task[Result[OmdbLookup]] = http(title)

}
