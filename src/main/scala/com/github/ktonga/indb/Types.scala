package com.github.ktonga.indb

import scalaz.{ \/, -\/, \/- }

trait Types {

  sealed trait Error { def message: String }
  case class MessageError(message: String) extends Error
  case class MissingTitleError(title: String) extends Error {
    def message = s"Missing title: $title"
  }

  type Result[A] = Error \/ A

  case class OmdbLookup(title: String, imdbID: String)
  case class ImdbListItem(position: String, id: String)

  case class Summary(added: Int, missing: Vector[String], errors: Vector[String]) {
    def append(res: Result[ImdbListItem]): Summary = res match {
      case \/-(_)                     => this.copy(added = this.added + 1)
      case -\/(MissingTitleError(t)) => this.copy(missing = this.missing :+ t)
      case -\/(error)                => this.copy(errors = this.errors :+ error.message)
    }
  }
  object Summary {
    val empty = Summary(0, Vector(), Vector())
  }
}
