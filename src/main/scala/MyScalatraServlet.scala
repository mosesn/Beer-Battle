import org.scalatra._
import scalate.ScalateSupport
import com.mongodb.casbah.Imports._
import java.util.Date
import java.security.MessageDigest
import javax.servlet.http.HttpSession
import scala.util.matching.Regex
import com.twilio.sdk.TwilioRestClient
import scala.collection.JavaConversions._

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
                               "number" -> stripNumber(params("number")),
                               "pw" -> pw)
    mongoDB("user").insert(newObj)
    session("number") = stripNumber(params("number"))
    session("pw") = pw
    redirect("/selectbar")
  }

/*  before() {
    if (inGame(session)) {
      redirect("/ingame")
    }
  }*/

  post("/finish") {
    auth(session)
    if (inGame(session)) {
      finishGame(session, params("winner").asInstanceOf[String])
    }
  }

  get("/ingame") {
    auth(session)
    if (inGame(session)) {
      contentType = "text/html"
      templateEngine.layout("/WEB-INF/layouts/ingame.scaml")
    }
    else {
      redirect("/")
    }
  }

  def inGame(session: HttpSession): Boolean = {
    if (session.contains("number")) {
      mongoDB("fight").findOne(MongoDBObject("members" -> session("number"))).isDefined
    }
    else {
      false
    }
  }

  def finishGame(session: HttpSession, winner: String) {
    val tmp = mongoDB("fight").findOne(MongoDBObject("member" -> session("number"))).get
    mongoDB("relax").insert(MongoDBObject("bar" -> tmp.get("bar"),
                                          "members" -> tmp.get(winner)))
    mongoDB("fight").remove(tmp)
    println("NEW GAME")
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

  def sendMessage(to: String, from: String, body: String) {
    val smsFactory = account.getSmsFactory();
    smsFactory.create(Map("To" -> to,
                          "From" -> from, // Replace with a valid phone number in your account
                          "Body" -> body))
  }

  def engagePlayers(first: DBObject, second: DBObject) {
    mongoDB("queue").remove(first)
    mongoDB("queue").remove(second)
    val tmp = munge(first.get("members").asInstanceOf[BasicDBList], second.get("members").asInstanceOf[BasicDBList])
    val newObj = MongoDBObject("members" -> tmp,
                               "home" -> first.get("members"),
                               "away" -> second.get("members"),
                               "bar" -> first.get("bar"))
    for (number <- tmp) {
      sendMessage(number, "17814377887", "GO TO PLAY BEER BATTLE NOOOOOOOOOOOOW")
    }
    mongoDB("fight").insert(newObj)
  }

  def munge(first: BasicDBList, second: BasicDBList): Array[String] ={
    val tmp = new Array[String](first.length + second.length)
    for ((elem, index) <- first.zipWithIndex) {
      tmp(index) = elem.asInstanceOf[String]
    }
    for ((elem, index) <- second.zipWithIndex) {
      tmp(index + first.length) = elem.asInstanceOf[String]
    }
    tmp
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
    str.filter(_.isDigit)
  }

  def hash(str: String): String = {
    val md = MessageDigest.getInstance("SHA")
    val salt = "g8:6U&dS(c"
    md.update(str.getBytes ++ salt.getBytes)
    new String(md.digest(), "ASCII")
  }

  post("/login") {
    val number = stripNumber(params("number"))
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
  val account = new TwilioRestClient("AC420cb3581df14275a7fd6bfd8f1207ff", "c2990d1fa7bba4bfa96f95caefdd20d0").getAccount()

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
      session("bar") = params("bar").asInstanceOf[String]
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
    if (auth(session)) {
      contentType = "text/html"
      val coll = mongoDB("team")
      val opt = coll.findOne(MongoDBObject("size" -> 2, "members" -> session("number")))
      if (busyBar(session)) {
        opt match {
          case Some(x) => {
            coll.remove(MongoDBObject("_id" -> x.get("_id")))
            x.removeKey("_id")
            x.put("time", new Date())
            mongoDB("queue").insert(x)
          }
          case None => redirect("/jointeam")
        }
      }
      else {
        opt match {
          case Some(x) => {
            coll.remove(x)
            x.removeKey("_id")
            val otherUser = getNextUser(session("bar").asInstanceOf[String], session("number").asInstanceOf[String])
            if (otherUser.isDefined) {
              engagePlayers(x, otherUser.get)
            }
            else {
              mongoDB("queue").insert(x)
            }
          }
          case None => redirect("/jointeam")
        }
      }
      templateEngine.layout("/WEB-INF/layouts/queue.scaml")
    }
    else {
      redirect("/")
    }
  }

  def getNextUser(bar: String, number: String): Option[DBObject] = {
    val x = mongoDB("queue").find(MongoDBObject("bar" -> bar, "number" -> MongoDBObject("$ne" -> number))).sort(MongoDBObject("time" -> 1)).limit(1)
    if (x.hasNext) {
      Some(x.next)
    }
    else {
      None
    }
  }

  def busyBar(session: HttpSession): Boolean = {
    val barOccupancy = mongoDB("bar").findOne(MongoDBObject("_id" -> new ObjectId(session("bar").asInstanceOf[String]))).get.get("size")
    mongoDB("fight").find(MongoDBObject("bar" -> session("bar"))).length == barOccupancy
  }

  post("/jointeam") {
    val coll = mongoDB("team")
    val pw = params.getOrElse("password", "")
    if (pw equals "") {
      coll.remove(MongoDBObject("members" -> session("number")))
      coll.update(MongoDBObject("number" -> params("team")),
                  MongoDBObject("$inc" -> MongoDBObject("size" -> 1), "$push" ->
                                MongoDBObject("members" -> session("number"))))
    }
    else {
      if (coll.findOne(MongoDBObject("pw" -> hash(pw))).isDefined) {
        coll.remove(MongoDBObject("members" -> session("number")))
        coll.update(MongoDBObject("number" -> params("team")),
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
                                     "bar" -> session("bar"),
                                     "number" -> session("number")))
    }
    else {
      mongoColl.insert(MongoDBObject("size" -> 1,
                                     "members" -> Array(session("number")),
                                     "pw" -> pw,
                                     "bar" -> session("bar"),
                                     "number" -> session("number")))
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
