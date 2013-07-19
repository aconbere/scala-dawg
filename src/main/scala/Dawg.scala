package com.conbere.speakandspell

import com.typesafe.scalalogging.log4j.Logging
import scala.sys.process._
import org.conbere.irc._
import akka.actor._
import com.typesafe.config._
import java.io.{ File, InputStream, OutputStream }
import scala.io.Source
import scala.collection.JavaConversions._

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

  def runCommand(from:String, to:String, msg:String):Option[String] = {

      // response is filled in when we finish reading stdout
      var response = ""
      var error = ""

      val proc = Seq(executable.getPath, List(from, to, to, msg).mkString(" ")).run(new ProcessIO(
        { stdin:OutputStream => stdin.close() },
        { stdout:InputStream =>
          response = Source.fromInputStream(stdout).mkString
          stdout.close()
        },
        { stderr:InputStream =>
          error = Source.fromInputStream(stderr).mkString
          stderr.close()
        }
      ))

      if (proc.exitValue == 0) {
        Some(response)
      } else {
        None
      }
  }

  val toMePattern = ("^(" + nickName + "[:| ](.*))").r

  val respondTo = defaultResponse.orElse[Message,Option[Response]] {
    case PrivMsg(from, `nickName`, msg) =>
      runCommand(from, nickName, msg).map(PrivMsg(from, _))
    case PrivMsg(from, to, toMePattern(leader, content)) =>
      runCommand(from, to, content).map(PrivMsg(to, _))
  }
}

object Main {
  def main(args:Array[String]) {
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
      System.exit(1);
    } else if (!executable.isFile()) {
      println("Command file " + command + " is not a file")
      System.exit(2);
    } else if (!executable.canExecute()) {
      println("Command file " + command + " can not be executed")
      System.exit(3);
    }

    val bot = new Dawg( server
                      , nickName
                      , userName
                      , password
                      , realName
                      , rooms
                      , new File(command)
                      )

    val actor = Client.start(server, port, bot)
  }
}
