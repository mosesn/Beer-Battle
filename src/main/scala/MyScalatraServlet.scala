import org.scalatra._
import scalate.ScalateSupport

class MyScalatraServlet extends ScalatraServlet with ScalateSupport {

  get("/") {
    <html>
      <body>
        <a href="play">PLAY</a>
        Say <a href="hello-scalate">hello no Scalate</a>.
      </body>
    </html>
//      templateEngine.layout(path, Map("content" -> "content"))
  }

  get("/play") {
    contentType = "text/html"
    templateEngine.layout("/WEB-INF/bleh.scaml", Map("sweetness" -> "fuck"))
  }

  post("/signup") {
    println(request("fname"))
    println(request("lname"))
    println(request("number"))
    println(request("password"))
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
