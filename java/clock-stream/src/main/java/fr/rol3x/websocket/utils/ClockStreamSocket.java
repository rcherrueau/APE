package fr.rol3x.websocket.utils;

import java.io.IOException;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.jetty.websocket.WebSocket;

/**
 * Websocket streams current time.
 * 
 * @author Ronan-Alexandre Cherrueau
 */
public class ClockStreamSocket implements WebSocket {
  private static ClockStreamSocket instance = null;
  
  private Timer timer;
  private List<Connection> connections;

  /**
   * Clock Stream socket constructor.
   */
  private ClockStreamSocket() {
    super();
    connections = new LinkedList<Connection>();
    timer = new Timer();

    // Stream current time each second
    timer.scheduleAtFixedRate(new TimerTask() {
      @Override
      public void run() {
        String timeJSon = "{" + "\"hour\":\"" + CurrentTime.getHour() + "\", "
            + "\"minute\":\"" + CurrentTime.getMinute() + "\", "
            + "\"second\":\"" + CurrentTime.getSecond() + "\"}";

        // Use tmp list in order to delete close connection in origin
        // TODO: Use better structure like hashmap for deletion in O(1)
        List<Connection> connTmp = new LinkedList<Connection>(connections);
        for (Connection connection : connTmp) {
          try {
            if (connection.isOpen()) {
              connection.sendMessage(timeJSon);
            } else {
              connections.remove(connTmp);
            }
          } catch (IOException e) {
            e.printStackTrace();
          }
        }
      }
    }, new Date(), 1000);
  }
  
  /**
   * Returns {@link ClockStreamSocket} instance.
   * 
   * @return {@link ClockStreamSocket} instance.
   */
  public static ClockStreamSocket getInstance() {
    if (instance == null) {
      instance = new ClockStreamSocket();
    }
    
    return instance;
  }

  @Override
  public void onOpen(Connection connection) {
    System.out.println("CONNECT [" + connection + "]");
    connections.add(connection);
  }

  @Override
  public void onClose(int closeCode, String message) {
    System.out.println("DISCONNECT [" + closeCode + ":" + message + "]");
  }
}