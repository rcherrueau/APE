package org.freedesktop

/* Desktop Notification with D-Bus
 *
 * Links:
 * - https://people.gnome.org/~mccann/docs/notification-spec/notification-spec-latest.html
 * - https://dbus.pages.freedesktop.org/zbus/concepts.html
 * - https://dbus.pages.freedesktop.org/zbus/client.html
 *
 * Services on D-Bus are identified by a name. The name for the
 * notifications service is `org.freedesktop.Notifications`. Services
 * offer an interface which is described by an XML description.  In
 * case of notifications, the name of the interface is the same as
 * the service (`org.freedesktop.Notifications`). Services expose an
 * object on the bus and clients interact with the service through
 * this object. An object is identified by a path. The path for the
 * notification object is `/org/freedesktop/Notifications`.
 *
 * We can play with the notification service from the shell using the
 * `busctl` command.
 *
 * busctl --user call               \
 *   org.freedesktop.Notifications  \
 *   /org/freedesktop/Notifications \
 *   org.freedesktop.Notifications  \
 *   Notify                         \
 *   susssasa\{sv\}i                \
 *   "my-app" 0 "dialog-information" "A summary" "Some body" 0 0 5000
 *
 * The command arguments are
 * - `--user`: Connect to and use the session (user) bus (i.e., not the system one)
 * - `call`: Send a method call message
 * - destination: The name of the service `org.freedesktop.Notifications`
 * - object path: `/org/freedesktop/Notifications`
 * - interface: The name of the interface `org.freedesktop.Notifications`
 * - method: The name of the method to call `Notify`
 * - signature: `susssasa{sv}i` means the method takes 8 arguments of
 *   various types. `s` is for string, `as` is for array of strings.
 *
 * We can fetch the XML interface of the notification service using the
 * `--xml-interface` option of the `busctl` command.
 *
 * busctl --user --xml-interface introspect \
 *   org.freedesktop.Notifications \
 *   /org/freedesktop/Notifications
 *
 * In the output, the XML has an `Notify` method that shows the signature
 * of the method and the name of each arguments
 *
 * <method name="Notify">
 *   <arg type="s" name="app_name" direction="in"> </arg>
 *   <arg type="u" name="replaces_id" direction="in"> </arg>
 *   <arg type="s" name="app_icon" direction="in"> </arg>
 *   <arg type="s" name="summary" direction="in"> </arg>
 *   <arg type="s" name="body" direction="in"> </arg>
 *   <arg type="as" name="actions" direction="in"> </arg>
 *   <arg type="a{sv}" name="hints" direction="in"> </arg>
 *   <arg type="i" name="expire_timeout" direction="in"> </arg>
 *   <arg type="u" name="id" direction="out"> </arg>
 * </method>
 */

import org.freedesktop.dbus.connections.impl.DBusConnection
import org.freedesktop.dbus.connections.impl.DBusConnection.DBusBusType
import org.freedesktop.dbus.types.UInt32
import org.freedesktop.dbus.interfaces.DBusInterface

import scala.util.{Try,Success,Failure}
import scala.jdk.CollectionConverters._
import org.freedesktop.dbus.types.Variant

import java.util.{List => JavaList}
import java.util.{Map => JavaMap}
import org.freedesktop.dbus.exceptions.DBusException
import org.freedesktop.dbus.interfaces.DBusInterface
import org.freedesktop.dbus.messages.DBusSignal
import org.freedesktop.dbus.types.UInt32
import org.freedesktop.dbus.types.Variant

/** Desktop Notification */
trait Notifications extends DBusInterface {

  /** Send a notification
   *
   * @param appName Optional name of the application sending the notification
   * @param replacesId Optional ID of an existing notification (0 lets the Bus define an ID for us)
   * @param appIcon Optional icon for the application
   * @param summary Single line overview of the notification
   * @param body Multi-line body of text (may contain simple Markup)
   * @param action TODO
   * @param hints Provide extra data such as setting the urgency level
   * @param timeout Time in milliseconds the notification stays on screen (0 to never disappears)
   * @return The ID to identify that notification
   *
   * See https://people.gnome.org/~mccann/docs/notification-spec/notification-spec-latest.html#basic-design
   */
  def Notify(appName: String,
             replacesId: UInt32,
             appIcon: String,
             summary: String,
             body: String,
             actions: JavaList[String],
             hints: JavaMap[String, Variant[_]],
             timeout: Int): UInt32

}

/** Urgency levels
  *
  * Notifications have an urgency level associated with them. Urgency
  * level are defined by a byte:
  *
  * 0 for Low
  * 1 for Normal
  * 2 for Critical
  *
  * See https://people.gnome.org/~mccann/docs/notification-spec/notification-spec-latest.html#urgency-levels
  */
private enum Urgency(val theByte: Byte):
  case Low extends Urgency((0).toByte)
  case Normal extends Urgency((1).toByte)
  case Critical extends Urgency((2).toByte)


object Notifications {

  /** Connect to the DBus and release the connection after the computation */
  private def withDBusConnection[A](f: DBusConnection => A): Try[A] = {
    val conn = DBusConnection.getConnection(DBusBusType.SESSION)
    val res = Try {f(conn)}
    conn.close()

    res
  }

  private def show(appName: String,
                   summary: String,
                   body: String,
                   urgency: Urgency,
                   timeout: Int): Try[Unit] = withDBusConnection { conn =>
    val notifications = conn.getRemoteObject(
      "org.freedesktop.Notifications",
      "/org/freedesktop/Notifications",
      classOf[org.freedesktop.Notifications])

    val actions = List()
    val hints = Map(("urgency", new Variant(urgency.theByte)))
    notifications.Notify(appName, new UInt32(0), "", summary, body, actions.asJava, hints.asJava, timeout)
  }

  def low(appName: String, summary: String, body: String, timeout: Int): Try[Unit] =
    show(appName, summary, body, Urgency.Low, timeout)
  def normal(appName: String, summary: String, body: String, timeout: Int): Try[Unit] =
    show(appName, summary, body, Urgency.Normal, timeout)
  def critical(appName: String, summary: String, body: String, timeout: Int): Try[Unit] =
    show(appName, summary, body, Urgency.Critical, timeout)
}
