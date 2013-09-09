package com.conbere.dawg

import scala.sys.process._
import org.conbere.irc.{ Messages, Tokens, ClassicBot, Room, Client }
import akka.actor.{ ActorRef, ActorSystem, Props }
import com.typesafe.config.{ Config, ConfigFactory }
import java.io.{ File, InputStream, OutputStream }
import scala.io.Source
import scala.collection.JavaConversions._

// a thread to run the command in
class Worker (
  val actor:ActorRef,
  val executable:File,
  val from:String,
  val to:String,
  val msg:String,
  val sendTo:String
) extends Runnable {
  import Messages._

  def run() {
    // response is filled in when we finish reading stdout
    var response = ""

    val cmd = List(from, to, to, "login", msg).mkString(" ")

    val proc = Seq(executable.getPath, cmd).run(
      new ProcessIO(_.close(), {
        stdout:InputStream =>
          response = Source.fromInputStream(stdout).mkString
          stdout.close()
      }, _.close()))

    if (proc.exitValue == 0) {
      actor ! PrivMsg(sendTo, response)
    }
  }
}

// Bot definition
class Dawg (
  val serverName:String,
  val nickName:String,
  val userName:String,
  val password:String,
  val realName:String,
  val rooms:List[Room],
  val executable:File
) extends ClassicBot {
  import Tokens._
  import Messages._

  object Patterns {
    val toNick = "^(" + nickName + ")[ :]+"
    val toMe = (toNick + "(.*)").r
    val join = (toNick + "join (.*)").r
    val part = (toNick + "part").r
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

// A container for the typed config properties
class DawgConfig (
  val server:String,
  val port:Int,
  val rooms: List[Room],
  val nickName: String,
  val realName: String,
  val userName: String,
  val password: String,
  val command:String,
  val executable: File
)

object DawgConfig {
  // process the typesafe.Conf into a simpler DawgConfig
  def fromConf(conf:Config) = {
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

    new DawgConfig(server, port, rooms, nickName, realName, userName, password, command, executable)
  }
}

object Main {
  def main(args:Array[String]) {
    if (args.length < 1) {
      println("No Command file given")
      System.exit(1)
    }

    val conf = DawgConfig.fromConf(ConfigFactory.parseFile(new File(args(0))))

    val system = ActorSystem("dawg")

    val bot = system.actorOf(
      Props(classOf[Dawg],
        conf.server,
        conf.nickName,
        conf.userName,
        conf.password,
        conf.realName,
        conf.rooms,
        conf.executable)
    )

    val client = system.actorOf(
      Props(classOf[Client],
        conf.server,
        conf.port,
        bot)
    )
  }
}
