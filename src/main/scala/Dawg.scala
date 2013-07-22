package com.conbere.speakandspell

import com.typesafe.scalalogging.log4j.Logging
import scala.sys.process._
import org.conbere.irc._
import akka.actor._
import com.typesafe.config._
import java.io.{ File, InputStream, OutputStream }
import scala.io.Source
import scala.collection.JavaConversions._

class Worker (
  actor:ActorRef,
  executable:File,
  from:String,
  to:String,
  msg:String,
  sendTo:String
) extends Runnable {
  import Messages._
  def run() {
    // response is filled in when we finish reading stdout
    var response = ""
    var error = ""

    val cmd = List(from, to, to, "login", msg).mkString(" ")

    val proc = Seq(executable.getPath, cmd).run(new ProcessIO(
      { stdin:OutputStream => stdin.close() },
      { stdout:InputStream =>
        response = Source.fromInputStream(stdout).mkString
        stdout.close()
      },
      { stderr:InputStream => stderr.close() }
    ))

    if (proc.exitValue == 0) {
      actor ! PrivMsg(sendTo, response)
    }
  }
}

class Dawg ( val serverName:String
           , val nickName:String
           , val userName:String
           , val password:String
           , val realName:String
           , val rooms:List[Room]
           , val executable:File
           )
extends ClassicBot with Logging {
  import Tokens._
  import Messages._

  object Patterns {
    val _toNick = "^(" + nickName + ")[ :]+"
    val toMe = (_toNick + "(.*)").r
    val join = (_toNick + "join (.*)").r
    val part = (_toNick + "part").r
  }

  def receive = onConnect orElse defaultHandler orElse {
    case PrivMsg(from, `nickName`, msg) =>
      new Thread(new Worker(sender, executable, from, nickName, msg, from)).start
    case PrivMsg(from, to, Patterns.join(_leader, channel)) =>
      sender ! Join(List(Room(channel, None)))
    case PrivMsg(from, to, Patterns.part(_leader)) =>
      sender ! Part(List(to))
    case PrivMsg(from, to, Patterns.toMe(_nick, msg)) =>
      new Thread(new Worker(sender, executable, from, nickName, msg, to)).start
  }
}

object Main {
  def main(args:Array[String]) {
    if (args.length < 1) {
      println("No Command file given")
      System.exit(1)
    }

    val file = args(0)

    val conf = ConfigFactory.parseFile(new File(file))

    val server = conf.getString("irc.server")
    val port = conf.getInt("irc.port")
    val rooms = conf.getStringList("irc.rooms").toList.flatMap {
      r:String =>
        r.split(":").toList match {
          case List(room, p) =>
            Some(Room(room, Some(p)))
          case List(room) =>
            Some(Room(room, None))
          case _ =>
            None
        }
    }

    val nickName = conf.getString("bot.nickname") 
    val userName = conf.getString("bot.username") 
    val password = conf.getString("bot.password") 
    val realName = conf.getString("bot.realname") 
    val command = conf.getString("bot.command")
    val executable = new File(command).getAbsoluteFile()

    if (!executable.exists()) {
      println("Command file " + command + " does not exist")
      System.exit(2);
    } else if (!executable.isFile()) {
      println("Command file " + command + " is not a file")
      System.exit(3);
    } else if (!executable.canExecute()) {
      println("Command file " + command + " can not be executed")
      System.exit(4);
    }

    val system = ActorSystem("dawg")

    val bot = system.actorOf(
      Props(classOf[Dawg], server, nickName, userName, password, realName, rooms, executable)
    )

    val client = system.actorOf(
      Props(classOf[Client], server, port, bot)
    )
  }
}
