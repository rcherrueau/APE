package fr.rol3x.websocket

import org.eclipse.jetty.server._
import handler.ContextHandlerCollection
import org.eclipse.jetty.webapp.WebAppContext
import org.eclipse.jetty.servlet.{ServletHolder, ServletContextHandler}
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import org.eclipse.jetty.util.component.LifeCycle.Listener

object JettyLauncher {
  def main(args: Array[String]) = {
    val server = new Server(8080)
    val context = new WebAppContext

    context.setDescriptor("/WEB-INF/web.xml")
    context.setResourceBase("/Users/rol3x/rol3x_place/scala/clock-stream/src/main/webapp");
    context.setContextPath("/clock-webapp")

    server.setHandler(context)

    server.start
    server.join
  }
}
