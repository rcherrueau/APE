package fr.rol3x.websocket.utils;

import java.util.Calendar;
import java.util.GregorianCalendar;

public class CurrentTime {
  public static String get() {
    Calendar calendar = new GregorianCalendar();
    int hour = calendar.get(Calendar.HOUR);
    int minute = calendar.get(Calendar.MINUTE);
    int second = calendar.get(Calendar.SECOND);

    return hour + ":" + minute + ":" + second;
  }
}
