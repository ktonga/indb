package com.github.ktonga

import scalaz.syntax.either._
import scalaz.concurrent.Task

package object indb extends Types {
  def failure[A](error: Error): Result[A] = error.left[A]
  def failureMsg[A](msg: String): Result[A] = failure(MessageError(msg))
  def success[A](a: A): Result[A] = a.right

  implicit class TaskOps[A](val self: Task[A]) extends AnyVal {
    def mapFailure(f: Throwable => Throwable): Task[A] =
      self.attempt.flatMap{
        d => Task.fromDisjunction(d.leftMap(f))
      }
  }

}
