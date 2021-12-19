package mua

import scala.sys.env
import java.nio.file.{Path}

import io.circe._, io.circe.generic.semiauto._

/** Directory to store emails */
val MAILDIR = Path.of(env.getOrElse[String]("MAILDIR",
                        Path.of(System.getProperty("user.home"), ".mail").toString))

/** Path */
case class BoxPath(_path: String) {
  /** Path of that box */
  val path: Path = MAILDIR.resolve(Path.of(_path, "cur"))
  /** Path Query ob that box */
  val pathQ = s"'path:\"${_path}/**\"'"
  /** Folder Query of that box */
  val folderQ = s"'folder:\"${_path}\"'"
  override def toString = _path
}

/** Path to drafts, send and trash inbox */
case class Boxes(inbox: List[String], drafts: String, sent: String, trash: String)

/** Email account information */
case class Account(name: String, email: String, boxes: Boxes, sync: Boolean) {
  /** mbsync store where emails are saved */
  val store = name
  /** Returns the folder path of the drafts box */
  val draft = BoxPath(s"$store/${boxes.drafts}")
  /** Returns the path path of the sent box */
  val sent = BoxPath(s"$store/${boxes.sent}")
  /** Returns the path path of the trash box */
  val trash = BoxPath(s"$store/${boxes.trash}")
}

object Account {
  // JSON automatic decoders
  implicit val boxesDecoder: Decoder[Boxes] = deriveDecoder[Boxes]
  implicit val accountDecoder: Decoder[Account] = deriveDecoder[Account]
}
