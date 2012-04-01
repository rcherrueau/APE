package fr.rol3x.websocket;

import javax.servlet.http.HttpServletRequest;

import org.eclipse.jetty.websocket.WebSocket;
import org.eclipse.jetty.websocket.WebSocketServlet;

import fr.rol3x.websocket.utils.ClockStreamSocket;

/**
 * Websocket servlet offers service that stream current time.
 * 
 * @author Ronan-Alexandre Cherrueau
 */
public class ClockStreamServlet extends WebSocketServlet {

  private static final long serialVersionUID = -4273251867183521552L;

  @Override
  public WebSocket doWebSocketConnect(HttpServletRequest arg0, String arg1) {
    return ClockStreamSocket.getInstance();
  }
}
