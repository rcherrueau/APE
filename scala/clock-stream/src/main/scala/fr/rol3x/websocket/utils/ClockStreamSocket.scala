package fr.rol3x.websocket.utils

import org.eclipse.jetty.websocket.WebSocket
import org.eclipse.jetty.websocket.WebSocket.Connection
import actors.Actor

/**
 * Websocket streams current time.
 *
 * @author Ronan-Alexandre Cherrueau
 */
object ClockStreamSocket extends WebSocket {
  private var connections = List[Connection]()
  private val timer = new TimeActor(1000, new Actor {
      def act {
        loop {
          react {
            case WakeUp =>
              val timeJSon = """{"hour":"%s", "minute":"%s", "second":"%s"}"""
                  .format(CurrentTime.getHour, CurrentTime.getMinute,
                       CurrentTime.getSecond);
              connections.foreach{
                c: Connection => if (c.isOpen) c sendMessage timeJSon
              }
          }
        }
      }
    })

  timer.start

  override def onOpen(connection: Connection) = {
    println("CONNECT [" + connection + "]")
    connections = connection :: connections
  }

  override def onClose(closeCode: Int, message: String) = {
    println("DISCONNECT [" + closeCode + ":" + message + "]")
  }
}

case object WakeUp
case object Stop

class TimeActor(val timeout: Long, val timerTask: Actor)
    extends Actor {
  timerTask.start

  def act {
    loop {
      reactWithin(timeout) {
        case x => 
          timerTask ! WakeUp
      }
    }
  }
}
