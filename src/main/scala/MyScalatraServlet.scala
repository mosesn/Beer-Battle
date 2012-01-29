import org.scalatra._
import scalate.ScalateSupport
import com.mongodb.casbah.Imports._
import java.security.MessageDigest
import javax.servlet.http.HttpSession
import scala.util.matching.Regex

class MyScalatraServlet extends ScalatraServlet with FlashMapSupport with ScalateSupport {

  get("/") {
    contentType= "text/html"
    templateEngine.layout("/WEB-INF/layouts/default.scaml")
  }

  get("/play") {
    contentType = "text/html"
    if (auth(session)) {
      "logged in"
    } else {
      "not logged in."
    }
  }

  def auth(session: HttpSession): Boolean = {
    session.contains("number") && session.contains("pw") && exists(
      mongoDB("tmp").findOne(
        MongoDBObject("number" -> session("number"),
                      "pw" -> session("pw"))))
  }

  def exists(doc: Option[DBObject]): Boolean = doc match {
    case Some(x) => true
    case None => false
  }

  post("/signup") {
    val fname = params("fname")
    val lname = params("lname")
    if (fname.length == 0 || lname.length == 0) {
      flash += ("error" -> "Trivial name.")
      redirect("/signup")
    }
    val number = params("number")
    if (isInvalidNumber(number)) {
      flash += ("error" -> "Bad phone number.")
      redirect("/signup")
    }
    val pass1 = params("password")
    val pass2 = params("password2")
    if (!(pass1 equals pass2)) {
      flash += ("error" -> "Different passwords.")
      redirect("/signup")
    }
    else {
      val mongoColl = mongoDB("tmp")
      val pw =  hash(pass1)
      val newObj = MongoDBObject("fname" -> fname,
                               "lname" -> lname,
                               "number" -> number,
                               "pw" -> pw)
      mongoColl.insert(newObj)
      session("number") = number
      session("pw") = pw
      //TODO: Redirect where?
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

    val mongoColl = mongoDB("tmp")
    val number = params("number")
    val pw = hash(params("password"))
    val result = mongoColl.findOne(MongoDBObject("number" -> number,
                                                 "pw" -> pw))
    result match {
      case Some(s) => {
          //TODO: Redirect where?
          session("number") = number
          session("pw") = pw
          2
      }
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

  get("/checkin"){
    contentType = "text/html"
    templateEngine.layout("/WEB-INF/layouts/checkin.scaml")
  }
  
  get("/jointeam"){
    contentType = "text/html"
    templateEngine.layout("/WEB-INF/layouts/jointeam.scaml")
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
