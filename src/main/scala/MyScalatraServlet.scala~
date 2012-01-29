import org.scalatra._
import scalate.ScalateSupport

class MyScalatraServlet extends ScalatraServlet with ScalateSupport {

  get("/") {
    contentType= "text/html"
    templateEngine.layout("/WEB-INF/layouts/default.scaml")
  }

  get("/play") {
    contentType = "text/html"
    templateEngine.layout("/WEB-INF/bleh.scaml", Map("sweetness" -> "fuck"))
  }

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
