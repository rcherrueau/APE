package fr.rol3x.websocket;

import java.io.IOException;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

import javax.servlet.http.HttpServletRequest;

import org.eclipse.jetty.websocket.WebSocket;
import org.eclipse.jetty.websocket.WebSocketServlet;

import fr.rol3x.websocket.utils.CurrentTime;

public class ClockStreamServlet extends WebSocketServlet {

  private static final long serialVersionUID = -4273251867183521552L;

  @Override
  public WebSocket doWebSocketConnect(HttpServletRequest arg0, String arg1) {
    return new ClockStreamSocket();
  }

  public class ClockStreamSocket implements WebSocket {
    private Timer timer;
    private List<Connection> connections;

    public ClockStreamSocket() {
      super();

      connections = new LinkedList<WebSocket.Connection>();
      timer = new Timer();

      timer.scheduleAtFixedRate(new TimerTask() {
        @Override
        public void run() {
          String time = CurrentTime.get();
          for (Connection connection : connections) {
            try {
              connection.sendMessage(time);
            } catch (IOException e) {
              e.printStackTrace();
            }
          }
        }
      }, new Date(), 1000);
    }

    @Override
    public void onOpen(Connection connection) {
      System.out.println("CONNECT [" + connection + "]");
      connections.add(connection);
    }

    @Override
    public void onClose(int closeCode, String message) {
      System.out.println("DISCONNECT ");
    }
  }
}
