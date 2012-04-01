package fr.rol3x.websocket

import org.eclipse.jetty.websocket.WebSocketServlet
import javax.servlet.http.HttpServletRequest

/**
 * Websocket servlet offers service that stream current time.
 *
 * @author Ronan-Alexandre Cherrueau
 */

class ClockStreamServlet extends WebSocketServlet {
  override def doWebSocketConnect(request: HttpServletRequest,
      protocol: Nothing) = return ClockStreamSocket
}
