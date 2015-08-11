package utils

import scala.concurrent._

import scalaz.{Validation, Success, Failure}

import play.api.Logger
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import utils.DatabaseOps.DeserializationErr

object ValidationOps {

  trait Err {
    val message: String
    override def toString = message
  }

  case class ListOfErrors (xs: List[Err]) extends Err {
    val message: String = xs.foldLeft("")(_ + " " + _.toString)
  }

  def ifSucceeds[A, B] (
    a: Validation[Err, A]
  )(f: A => Future[Validation[Err, B]]): Future[Validation[Err, B]] = a match {
    case Success(x) => f(x)
    case e: Failure[Err] => Future(e)
  }

  def ifSucceeds[A, B, C] (
    a: Validation[Err, A],
    b: Validation[Err, B]
  )(f: (A, B) => Future[Validation[Err, C]]): Future[Validation[Err, C]] = (a, b) match {
    case (Success(x), Success(y)) => f(x, y)
    case (Failure(x), Failure(y)) => Future(Failure(ListOfErrors(x :: y :: Nil)))
    case (_, y: Failure[Err]) => Future(y)
    case (x: Failure[Err], _) => Future(x)
  }

  def ifSucceeds[A, B] (validation: JsResult[A])(f: A => B): Future[Validation[Err, B]] =
    validation match {
      case s: JsSuccess[A] =>
        Future(Success(f(s.get)))
      case e: JsError =>
        val error = DeserializationErr("JsError when trying to deserialize a db result: " + JsError.toJson(e).toString())
        Logger.error(error.toString)
        Future(Failure(error))
    }
}
