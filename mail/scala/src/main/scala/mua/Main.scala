package mua

import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration
import concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import scala.sys.env

import java.io.IOException
import java.nio.file.{Path, Files}

import org.freedesktop.Notifications

import cats.implicits._
import io.circe.parser.decode

val MBSYNC_BIN = env.getOrElse("MBSYNC_BIN", "mbsync")
val NOTMUCH_BIN = env.getOrElse("NOTMUCH_BIN", "notmuch")


// Tasks

/** Synchronize one email store
 *
 * > mbsync --verbos <account-store>-inbox
 *
 */
def syncEmails(account: Account): Try[Unit] = if (account.sync) {
  val store = s"${account.store}-inbox"

  Command.output(MBSYNC_BIN, "--verbose", store).flatMap(
    (output: Output)  =>
    if (output.isSuccess) {   // Sync runs smoothly...
      // Get the stdout and log it
      println(s"Sync of ${store}: ${output.stdout}")

      // TODO: parse stdout to get Inbox changes and notify the use
      // ...

      Success(())
    } else {                  // Sync runs with errors...
      // Get the stderr and log it
      println(s"Sync error for ${store}: ${output.stderr}")

      // Notify user and Fail
      Notifications
        .critical(
          "email",
          s"📪 Sync error for ${store}",
          s"${output.stderr}\n\nTry 'mbsync -V ${store}' to debug.",
          0)
        .flatMap(_ => Failure(new IOException(output.stderr)))
    })
} else { Success(()) /* Do nothing if the account should not be synchronized */ }

/** Index emails
 *
 * > notmuch new
 */
def indexEmails(): Try[Unit] = Command.output(NOTMUCH_BIN, "new").flatMap(
  (output: Output)  =>
  if (output.isSuccess) {   // Indexing runs smoothly...
    // Get the stdout and log it
    println(s"notmuch indexing: ${output.stdout}")
    Success(())
  } else {                  // Indexing runs with errors...
    // Get the stderr and log it
    println(s"notmuch indexing error: ${output.stderr}")
    Failure(new IOException(output.stderr))
  })

/** Tag emails
 *
 * > notmuch tag --batch --input=<taggingScript>
 *
 */
def tagEmails(accounts: List[Account]): Try[Boolean] = {
  // Notmuch tagging script
  val taggingScript = s"""
    # Tag new mails according to their folder path
    # > notmuch tag +Inria -- path:Inria/**
    ${accounts.map(acc =>
       s"+${acc.store} -- path:${acc.store}/**").mkString("\n")}

    +ISP -- from:freetelecom.fr or from:free-mobile.fr or from:assistance.free.fr
    +Banque -- from:ca-atlantique-vendee.fr or from:ing.com
    +Shopping -- to:"/.*\\+shopping@.*/"

    # Mailing Lists
    +List +G5k -- to:lists.grid5000.fr or from:grid5000.fr
    +List +Types -- to:types-list@lists.seas.upenn.edu
    # Requires to set  `header.List=List-Id` in the notmuch config file,
    # and then `notmuch reindex '*'`
    +List +Nix -- List:24d1741146b951f90adf436fdmc
    +List +Racket -- List:2d4bcd7724e2a351c8e594233mc
    +List +Scala -- List:ba834c562d82d9aba5eaf90bamc

    # Inria redirection to Gmail
    # Requires to set  `header.DeliveredTo=Delivered-To` in the notmuch config file,
    # and then `notmuch reindex '*'`
    -Gmail +Inria -- DeliveredTo:RonanCherrueau+Inria@gmail.com

    # SPAM
    +SPAM -- subject:spam

    # Mark emails in the sent box as `sent`
    -inbox -unread +sent -- ${accounts.map(_.sent.folderQ).mkString(" OR ")}

    # Mark emails in the trash box as `deleted`
    -inbox +deleted -- ${accounts.map(_.trash.folderQ).mkString(" OR ")}
  """

  for {
    // Create a temporary file with the tagging script
    file <- mkTmpFile(taggingScript)

    // Call notmuch ...
    output <- Command.output(NOTMUCH_BIN, "tag", "--batch", s"--input=${file}")
  } yield {
    output.isSuccess match
      // ... tagging runs smoothly:
      // Log successful tagging and delete the script
      case true => {
        println("notmuch tagging OK")
        Files.deleteIfExists(file)
      }
      // ... tagging runs with errors:
      // Get the stderr, log it and abort the computation
      case false => {
        println(s"notmuch tagging error: ${output.stderr}")
        throw new IOException(output.stderr)
      }
  }
}

/** Get email paths of the `query`
 *
 * > notmuch search --output=files <query>
 */
def searchEmails(query: String): Try[List[Path]] =
  Command.output(NOTMUCH_BIN, "search", "--output=files", query).flatMap(
    (output: Output) =>
    if (output.isSuccess) {   // Searching runs smoothly...
      // Get the stdout, log it and parse it
      println(s"notmuch search ${query}")
      Try(output.stdout
            // One email path per line, split on new line
            .split("\\r?\\n")
            // filter Blank lines
            .filterNot(_.isBlank)
            // Transform each string into a Path
            .map(Path.of(_)).toList)
    } else {                  // Indexing runs with errors...
      // Get the stderr and log it
      println(s"notmuch indexing error: ${output.stderr}")
      Failure(new IOException(output.stderr))
    })

/** Locally move the `email` to `box`
 *
 * From `man mbsync`:
 * > When using the more efficient default UID mapping scheme, it
 * > is important that the MUA renames files when moving them
 * > between Maildir folders.  Mutt always does that, while mu4e
 * > needs to be configured to do it: (setq
 * > mu4e-change-filenames-when-moving t)
 */
def moveToLocalBox(email: Path, box: BoxPath): Try[Path] = Try {
  // Strip UID from email name
  //
  // mbsync adds a unique identifier to file names (e.g.,
  // `/path/to/mail,U=<UID>:2,SR` -- with `2` stands for the version
  // of UID generation if I am right).  Moving files causes UID
  // conflicts and prevent mbsync from syncing with "Maildir error:
  // UID 9610 is beyond highest assigned UID 86."  The sed command in
  // the following removes the UID to force mbsync to regenerate one
  // and avoid UID conflicts.
  val emailBasename = email.getFileName().toString
  val emailNoUID = emailBasename.replaceFirst("U=[0-9]+:2", "U=:2")
  val emailInBoxPath = box.path.resolve(emailNoUID)

  // Move email to the new box
  println(s"Move $email -> $emailInBoxPath")
  Files.move(email, emailInBoxPath)
  emailInBoxPath
}


// Apps

/** Pull emails concurrently and tag them */
def pull(accounts: List[Account]): Future[String] = for {
  // First synchronize email stores concurrently
  _ <- accounts.map(acc => liftTry(syncEmails(acc))).sequence

  // Then index emails
  _ <- liftTry(indexEmails())

  // Finally tag emails
  _ <- liftTry(tagEmails(accounts))
} yield ("Pulling finished")

/** Delete emails */
def delete(accounts: List[Account]): Future[String] = {
  val trashBoxesQ = accounts.map(_.trash.folderQ).mkString(" OR ")

  /** Delete emails of a specific account */
  def _deleteEmails(account: Account): Try[List[(Path, Path)]] = for {
    // Get the path of all emails mark as deleted
    emailPaths <- searchEmails(
      s"tag:${account.store} AND tag:deleted AND NOT ($trashBoxesQ)")

    // Move emails into the trash box locally
    emailTrashPaths <- emailPaths.map(moveToLocalBox(_, account.trash))
                                 .sequence

    // Propagate changes to the remote mailbox (if need be)
    _ <- if !emailPaths.isEmpty then { syncEmails(account) } else { Success(()) }
  } yield (emailPaths.zip(emailTrashPaths))


  for {
    // First, delete emails for each account concurrently
    _ <- accounts.map(acc => liftTry(_deleteEmails(acc))).sequence

    // Then re-Index emails so notmuch knows the new email paths
    _ <- liftTry(indexEmails())
  } yield ("Deleting finished")
}

@main def app(action: String): Unit = {
  // Parse the `accounts` and execute the `action` in a future.
  val appF = liftTry(parseAccounts(ACCOUNTS)).flatMap(
    accounts => action match {
      case "pull"   => pull(accounts.values.toList)
      case "delete" => delete(accounts.values.toList)
      case _ => Future.failed(
        new IOException(s"Unknown action ${action}.  Expected either `pull` or `delete`."))
    })

  // `Await` for the future to complete and ensure that the main
  // thread does not terminate prematurely
  Await.ready(appF, Duration.Inf).onComplete {
    case Success(x) =>
      println(s"Success: ${x}")
    case Failure(e) =>
      e.printStackTrace
      Notifications.critical("email", s"📪 General ERROR", s"${e}", 0)
      println("*I SHOULD BE PRINTED IN CASE OF ERROR*")
  }
}


// Utils

/** Execute the `t` into a `Future` and make the `Future` to fail if
  * the `Try` fails */
def liftTry[A](t: =>Try[A]): Future[A] = Future { t }.flatMap(Future.fromTry)

/** Creates a temporary file */
def mkTmpFile(content: String): Try[Path] = Try {
  // Create and write to the temporary file
  val tmpFile = Files.createTempFile(null, null)
  Files.write(tmpFile, content.getBytes)

  // Log tmp file path
  println(s"Write temporary file to ${tmpFile}")
  tmpFile
}

/** Parses a JSON string input into an Account list */
def parseAccounts(json: String): Try[Map[String, Account]] =
  decode[Map[String, Account]](json) match {
    case Right(decoded) => Success(decoded)
    case Left(err) => Failure(new IOException(err.getMessage))
  }


// Debug
val ACCOUNTS = """
{
  "Gmail": {
    "store": "Gmail",
    "sync": true,
    "boxes": { "drafts": "[Gmail]/Drafts", "inbox": ["*", "![Gmail]*"], "sent": "[Gmail]/Sent Mail", "trash": "[Gmail]/Bin" },
    "default": true,
    "email": "RonanCherrueau@gmail.com",
    "imap": { "host": "imap.gmail.com" },
    "smtp": { "host": "smtp.gmail.com" }
  },
  "IMT": {
    "store": "IMT",
    "sync": false,
    "boxes": { "drafts": "Drafts", "inbox": ["*", "!Junk"], "sent": "Sent", "trash": "Trash" },
    "email": "Ronan-Alexandre.Cherrueau@imt-atlantique.fr",
    "imap": { "host": "z.imt.fr" },
    "smtp": { "host": "z.imt.fr" }
  },
  "Inria": {
    "store": "Inria",
    "sync": false,
    "boxes": { "drafts": "Drafts", "inbox": ["*", "!Junk"], "sent": "Sent", "trash": "Trash" },
    "email": "Ronan-Alexandre.Cherrueau@inria.fr",
    "imap": { "host": "zimbra.inria.fr" },
    "smtp": { "host": "smtp.inria.fr", "user": "rcherrue" }
  }
}
"""
