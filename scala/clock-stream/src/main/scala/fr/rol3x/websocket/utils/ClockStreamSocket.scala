package fr.rol3x.websocket.utils

import org.eclipse.jetty.websocket.WebSocket
import org.eclipse.jetty.websocket.WebSocket.Connection

/**
 * Websocket streams current time.
 *
 * @author Ronan-Alexandre Cherrueau
 */
object ClockStreamSocket extends WebSocket {
  private var connections = List[Connection]

  override def onOpen(connection: Connection) = {
    println("CONNECT [" + connection + "]")
    connections = connection :: connections
  }

  override def onClose(closeCode: Int, message: String) = {
    println("DISCONNECT [" + closeCode + ":" + message + "]")
  }
}
