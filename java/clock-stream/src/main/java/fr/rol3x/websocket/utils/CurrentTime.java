package fr.rol3x.websocket.utils;

import java.util.GregorianCalendar;

/**
 * Returns current time.
 * 
 * @author Ronan-Alexandre Cherrueau
 */
public class CurrentTime {
  
  /**
   * Returns current time hour.
   * 
   * @return Current time hour.
   */
  public static String getHour() {
    String hour = String.valueOf(GregorianCalendar.getInstance()
        .get(GregorianCalendar.HOUR));
    
    return padUnit(hour);
  }
  
  /**
   * Returns current time minute.
   * 
   * @return Current time minute.
   */
  public static String getMinute() {
    String min = String.valueOf(GregorianCalendar.getInstance()
        .get(GregorianCalendar.MINUTE));
    
    return padUnit(min);
  }
  
  /**
   * Returns current time second.
   * 
   * @return Current time second.
   */
  public static String getSecond() {
    String sec = String.valueOf(GregorianCalendar.getInstance()
        .get(GregorianCalendar.SECOND));
    
    return padUnit(sec);
  }
  
  /**
   * Pad string with 0 if length is less than 2.
   * 
   * @return Padded string (if any).
   */
  private static String padUnit(String unit) {
    return (unit.length() < 2) ? "0" + unit : unit;
  }
}
