/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

//import scala.concurrent._
//import scala.concurrent.duration._
//
//import org.specs2.mutable._
//import org.specs2.execute.AsResult
//import org.specs2.execute.Result
//import org.specs2.specification.Scope
//
//import play.api.test._
//import play.api.test.Helpers._
//import play.api.libs.json._
//
//import play.api.libs.ws._
//
//import arckane.db.transaction._
//import arckane.db.Node
//
//class SchedulesAPISpecs extends Specification {
//
//  trait TestEntities extends Around with Scope {
//
//    //val skill1Payload = Json.obj("title" -> "test", "web" -> "www.something.com")
//    //lazy val skill1 = await(SyntaxlingNode create skill1Payload)
//    //lazy val deleteSkill1 = await(skill1.delete)
//
//    abstract override def around[T: AsResult](t: => T): Result = {
//      super.around {
//        try t // Execute test
//        finally {
//          //deleteSkill1
//        }
//      }
//    }
//
//    def await[A] (ft: Future[A]): A = Await.result(ft, 2 seconds)
//  }
//
//  "Session API endpoint" should {
//
///*
//POST    /api/schedules/session             arckane.schedules.SchedulesApi.postSession
//GET     /api/schedules/session/mentors     arckane.schedules.SchedulesApi.getSessionMentors
//GET     /api/schedules/session/:uri        arckane.schedules.SchedulesApi.getSession(uri: String)
//PUT     /api/schedules/session/:uri        arckane.schedules.SchedulesApi.putSession(uri: String)
//DELETE  /api/schedules/session/:uri        arckane.schedules.SchedulesApi.deleteSession(uri: String)
//*/
//
//    "POST /api/schedules/session : 401 when requesting without an active session"
//    "Execute and respond one map for one query" in new WithApplication with TestEntities {
//      import play.api.libs.concurrent.Execution.Implicits._
//
//      val uri = await(Node.create(Json.obj(
//        "name" -> "Franco"
//      ))("Person"))
//      await(Node.set(uri, Json.obj(
//        "name" -> "Francisco",
//        "lastname" -> "Aramburo"
//      )))
//      await(Node.set(uri, Json.obj(
//        "lastname" -> "Ara"
//      )))
//      println(await(Node.get(uri)))
//      await(Node.delete(uri))
//      true must beEqualTo(true)
//    }
//  }
//}
//
