import org.scalatra._
import scalate.ScalateSupport
import com.mongodb.casbah.Imports._
import java.security.MessageDigest
import javax.servlet.http.HttpSession
import scala.util.matching.Regex

class MyScalatraServlet extends ScalatraServlet with FlashMapSupport with ScalateSupport {

  get("/") {
    contentType= "text/html"
    val map = Map("logged_in" -> auth(session))
    templateEngine.layout("/WEB-INF/layouts/default.scaml", map)
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

  get("/selectbar") {
    contentType = "text/html"
    templateEngine.layout("/WEB-INF/layouts/selectbar.scaml",
                          Map("bars" -> mongoDB("bar").find()))
  }

  get("/jointeam"){
    contentType = "text/html"
    val tmp = mongoDB("team").find(
      MongoDBObject("bar" -> session("bar").asInstanceOf[ObjectId]))
    templateEngine.layout("/WEB-INF/layouts/jointeam.scaml",
                          Map("teams" -> tmp))
  }

  get("/createteam"){
    contentType = "text/html"
    templateEngine.layout("/WEB-INF/layouts/createteam.scaml")
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

case class User(phone: Int, fname: String, lname: String)
