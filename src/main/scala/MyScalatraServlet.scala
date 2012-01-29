import org.scalatra._
import scalate.ScalateSupport
import com.mongodb.casbah.Imports._
import java.security.MessageDigest
import javax.servlet.http.HttpSession

class MyScalatraServlet extends ScalatraServlet with FlashMapSupport with ScalateSupport {

  get("/") {
    contentType= "text/html"
    templateEngine.layout("/WEB-INF/layouts/default.scaml")
  }

  get("/play") {
    contentType = "text/html"
    if (auth(session)) {
      templateEngine.layout("/WEB-INF/bleh.scaml", Map("sweetness" -> "fuck"))
    } else {
      templateEngine.layout("/WEB-INF/bleh.scaml", Map("sweetness" -> "fuck"))
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
    val number = params("number")
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

  def hash(str: String): String = {
    val md = MessageDigest.getInstance("SHA")
    val salt = "g8:6U&dS(c"
    md.update(str.getBytes ++ salt.getBytes)
    new String(md.digest(), "ASCII")
  }

  post("/login") {

    val mongoColl = mongoDB("tmp")
    val number = params("number")
    val pass = params("password")

    val result = mongoColl.findOne(MongoDBObject("number" -> number,
                                                 "pw" -> pass))
    result match {
      case Some(s) => {
        val pw = s("pw")
        if (pw equals hash(pass)) {
          //TODO: Redirect where?
          //TODO: log into session
          session("number") = number
          session("pw") = pw
          2
        } else {
          flash += ("error" -> "Wrong password.")
          redirect("/login")
        }
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
    val rest = if (map("error")) {
      Map("type" -> flash("error"))
    }
    else {
      Map()
    }
    templateEngine.layout("/WEB-INF/layouts/signup.scaml", map ++ rest)
  }

  get("/login") {
    contentType = "text/html"
    if (flash.contains("error")) {
      templateEngine.layout("/WEB-INF/layouts/login.scaml")
    } else {
      templateEngine.layout("/WEB-INF/layouts/login.scaml")
    }
  }

  get("/checkin"){
    contentType = "text/html"
    templateEngine.layout("/WEB-INF/layouts/checkin.scaml")
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
