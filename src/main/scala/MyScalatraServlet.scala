import org.scalatra._
import scalate.ScalateSupport
import com.mongodb.casbah.Imports._
import org.scala_tools.time.Imports._
import java.security.MessageDigest
import javax.servlet.http.HttpSession
import scala.util.matching.Regex

class MyScalatraServlet extends ScalatraServlet with FlashMapSupport with ScalateSupport {

  get("/") {
    contentType = "text/html"
    val map = Map("logged_in" -> auth(session))
    templateEngine.layout("/WEB-INF/layouts/default.scaml", map)
  }

  get("/signout") {
    contentType = "text/html"
    session.invalidate
  }

  def auth(session: HttpSession): Boolean = {
    session.contains("number") && session.contains("pw") && exists(
      mongoDB("user").findOne(
        MongoDBObject("number" -> session("number"),
                      "pw" -> session("pw"))))
  }

  def exists(doc: Option[DBObject]): Boolean = doc match {
    case Some(x) => true
    case None => false
  }

  post("/signup") {
    verifyInput(params)

    val pw =  hash(params("password"))
    val newObj = MongoDBObject("fname" -> params("fname"),
                               "lname" -> params("lname"),
                               "number" -> params("number"),
                               "pw" -> pw)
    mongoDB("user").insert(newObj)
    session("number") = params("number")
    session("pw") = pw
    redirect("/selectbar")
  }

  post("/finish") {
    auth(session)
    if (inGame(session)) {
      finishGame(session)
    }
  }

  def inGame(session: HttpSession): Boolean = {
    mongoDB("fight").findOne(MongoDBObject("member" -> session("number"))).isDefined
  }

  def finishGame(session: HttpSession) {
    mongoDB("fight").remove(MongoDBObject("member" -> session("number")))
    newFight(session("bar").asInstanceOf[String])
  }

  def newFight(bar: String) {
    val iter = mongoDB("queue").find(MongoDBObject("bar" -> bar)).sort(MongoDBObject("time" -> 1)).limit(2)
    if (iter.hasNext){
      val first = iter.next()
      if (iter.hasNext) {
        val second = iter.next()
        engagePlayers(first, second)
      }
    }
  }

  def engagePlayers(first: DBObject, second: DBObject) {
    mongoDB("queue").remove(first)
    mongoDB("queue").remove(second)
    val newObj = MongoDBObject("members" -> (first.get("members").asInstanceOf[Array[Number]] ++ second.get("members").asInstanceOf[Array[Number]]),
                               "team1" -> first.get("members"),
                               "team2" -> second.get("members"),
                               "bar" -> first.get("bar"))
    mongoDB("fight").insert(newObj)
  }

  def verifyInput(map: Map[String, String]) {
    if (map("fname").length == 0 || map("lname").length == 0) {
      flash += ("error" -> "Trivial name.")
      redirect("/signup")
    }
    else if (isInvalidNumber(map("number"))) {
      flash += ("error" -> "Bad phone number.")
      redirect("/signup")
    }
    else if (!(map("password") equals map("password2"))) {
      flash += ("error" -> "Different passwords.")
      redirect("/signup")
    }
  }

  def isInvalidNumber(str: String): Boolean = {
    val len = stripNumber(str).length
    len != 10 && len != 11
  }

  def stripNumber(str: String): String = {
    var tmp: String = ""
    for (x <- new Regex("""\d""").findAllIn(str)) {
      tmp += x
    }
    tmp
  }

  def hash(str: String): String = {
    val md = MessageDigest.getInstance("SHA")
    val salt = "g8:6U&dS(c"
    md.update(str.getBytes ++ salt.getBytes)
    new String(md.digest(), "ASCII")
  }

  post("/login") {
    val number = params("number")
    val pw = hash(params("password"))
    val result = mongoDB("user").findOne(MongoDBObject("number" -> number,
                                                 "pw" -> pw))
    validate(result)
    session("number") = number
    session("pw") = pw
    redirect("/selectbar")
  }

  def validate(result: Option[DBObject]): Unit = {
    result match {
      case Some(s) => 2
      case None => {
        flash += ("error" -> "Wrong password.")
        redirect("/login")
      }
    }

  }

  val mongoConn = MongoConnection()
  val mongoDB = mongoConn("casbah_test")

  get("/signup") {
    contentType = "text/html"
    val map = Map("error" -> flash.contains("error"))
    val myType = flash.getOrElse("error", "")
    val rest = Map("myType" -> myType)
    templateEngine.layout("/WEB-INF/layouts/signup.scaml", map ++ rest)
  }

  get("/login") {
    contentType = "text/html"
    val map = Map("error" -> (flash.contains("error")))
    templateEngine.layout("/WEB-INF/layouts/login.scaml", map)
  }

  post("/selectbar") {
      session("bar") = params("barstring").asInstanceOf[String]
      redirect("/jointeam")
  }

  get("/selectbar") {
    if (auth(session)) {
      contentType = "text/html"
      templateEngine.layout("/WEB-INF/layouts/selectbar.scaml",
                            Map("bars" -> mongoDB("bar").find()))
    }
    else {
      redirect("/")
    }
  }

  get("/queue") {
    contentType = "text/html"
    val coll = mongoDB("team")
    val opt = coll.findOne(MongoDBObject("size" -> 2, "members" -> session("number")))
    if (busyBar(session)) {
      opt match {
        case Some(x) => {
          x.removeKey("_id")
          x.put("time", DateTime.now)
          mongoDB("queue").insert(x)
        }
        case None => redirect("/createteam")
      }
    }
    else {
      opt match {
        case Some(x) => {
          x.removeKey("_id")
          val otherUser = getNextUser(session("bar").asInstanceOf[String])
          if (otherUser.isDefined) {
            engagePlayers(x, otherUser.get)
          }
        }
        case None => redirect("/createteam")
      }
    }
    templateEngine.layout("/WEB-INF/layouts/queue.scaml")
  }

  def getNextUser(bar: String): Option[DBObject] = {
    null
  }

  def busyBar(session: HttpSession): Boolean = {
    val barOccupancy = mongoDB("bar").findOne(MongoDBObject("bar" -> session("bar"))).get.get("size")
    mongoDB("fight").find(MongoDBObject("bar" -> session("bar"))).length == barOccupancy
  }

  post("/jointeam") {
    val coll = mongoDB("team")
    val pw = params("password")
    if (pw equals "") {
      coll.remove(MongoDBObject("members" -> session("number")))
      coll.update(MongoDBObject("number" -> params("number")),
                  MongoDBObject("$inc" -> MongoDBObject("size" -> 1), "$push" ->
                                MongoDBObject("members" -> session("number"))))
    }
    else {
      if (coll.findOne(MongoDBObject("pw" -> hash(pw))).isDefined) {
        coll.remove(MongoDBObject("members" -> session("number")))
        coll.update(MongoDBObject("number" -> params("number")),
                    MongoDBObject("$inc" -> MongoDBObject("size" -> 1),
                                  "$push" -> MongoDBObject("members" -> session("number"))))
      }
      else {
        flash += ("error" -> "Wrong password.")
        redirect("/jointeam")
      }
    }
    //add to queue
    redirect("/queue")
  }

  get("/jointeam"){
    if (auth(session)) {
      contentType = "text/html"
      val tmp = mongoDB("team").find(MongoDBObject("bar" -> session("bar"), "size" -> MongoDBObject("$lt" -> 2),
                                                 "members" -> MongoDBObject("$ne" -> session("number"))))
      templateEngine.layout("/WEB-INF/layouts/jointeam.scaml",
                            Map("teams" -> tmp, "error" -> flash.getOrElse("error", "")))
    }
    else {
      redirect("/")
    }
  }

  post("/createteam") {
    val pw = params("pw")
    val mongoColl = mongoDB("team")

    if (pw equals "") {
      mongoColl.insert(MongoDBObject("size" -> 1,
                                     "members" -> Array(session("number")),
                                     "bar" -> session("bar")))
    }
    else {
      mongoColl.insert(MongoDBObject("size" -> 1,
                                     "members" -> Array(session("number")),
                                     "pw" -> pw,
                                     "bar" -> session("bar")))
    }
    redirect("/jointeam")
  }

  get("/createteam"){
    if (auth(session)) {
      contentType = "text/html"
      templateEngine.layout("/WEB-INF/layouts/createteam.scaml")
    }
    else {
      redirect("/")
    }
  }

  notFound {
    // Try to render a ScalateTemplate if no route matched
    findTemplate(requestPath) map { path =>
      contentType = "text/html"
      println(path)
      templateEngine.layout(path)
    } orElse serveStaticResource() getOrElse resourceNotFound()
  }
}
