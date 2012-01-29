import org.scalatra._
import scalate.ScalateSupport
import com.mongodb.casbah.Imports._

class MyScalatraServlet extends ScalatraServlet with ScalateSupport {
  get("/") {
    contentType= "text/html"
    templateEngine.layout("/WEB-INF/layouts/default.scaml")
  }

  get("/play") {
    val mongoColl = mongoDB("tmp")
    val newObj = MongoDBObject("bleh" -> "fuck")
    mongoColl.insert(newObj)
    contentType = "text/html"
    templateEngine.layout("/WEB-INF/bleh.scaml", Map("sweetness" -> "fuck"))
  }

  val mongoConn = MongoConnection()
  val mongoDB = mongoConn("casbah_test")

  post("/signup") {
    println(params("fname"))
    println(params("lname"))
    println(params("number"))
    println(params("password"))
  }

  get("/signup") {
    contentType = "text/html"
    templateEngine.layout("/WEB-INF/layouts/signup.scaml")
  }
  
  get("/login"){
    contentType = "text/html"
    templateEngine.layout("/WEB-INF/layouts/login.scaml")
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
