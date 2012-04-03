package fr.rol3x.websocket.utils

import java.util.{Calendar, GregorianCalendar}


object CurrentTime {
  def getHour = padUnit(Calendar.getInstance().get(Calendar.HOUR).toString)
  def getMinute = padUnit(Calendar.getInstance().get(Calendar.MINUTE).toString)
  def getSecond = padUnit(Calendar.getInstance().get(Calendar.SECOND).toString)
  def padUnit(unit: String) = if (unit.length() < 2) "0" + unit else unit
}
