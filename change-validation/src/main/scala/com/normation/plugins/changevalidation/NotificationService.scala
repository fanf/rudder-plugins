package com.normation.plugins.changevalidation

import java.io.File
import java.io.FileReader
import java.io.StringWriter
import java.util.Properties

import bootstrap.liftweb.FileSystemResource
import bootstrap.liftweb.RudderConfig
import com.github.mustachejava.DefaultMustacheFactory
import com.normation.NamedZioLogger
import com.normation.errors.IOResult
import com.normation.errors.Inconsistency
import com.normation.errors._
import com.normation.eventlog.EventLog
import com.normation.plugins.changevalidation.TwoValidationStepsWorkflowServiceImpl.Deployment
import com.normation.plugins.changevalidation.TwoValidationStepsWorkflowServiceImpl.Validation
import com.normation.rudder.domain.eventlog.AddChangeRequest
import com.normation.rudder.domain.eventlog.DeleteChangeRequest
import com.normation.rudder.domain.eventlog.ModifyChangeRequest
import com.normation.rudder.domain.workflows.ChangeRequest
import com.normation.rudder.domain.workflows.WorkflowNode
import com.normation.rudder.services.eventlog.ChangeRequestEventLogService
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import javax.mail.Session
import javax.mail._
import javax.mail.internet.InternetAddress
import javax.mail.internet.MimeMessage
import zio.syntax._

import scala.jdk.CollectionConverters._


case class Email(value: String)

case class Username(value: String)

case class SMTPConf(
    smtpHostServer : String
  , port           : Int
  , email          : Email
  , login          : Option[Username]
  , password       : Option[String]
)

case class EmailConf(
    state   : WorkflowNode
  , to      : Set[Email]
  , replyTo : Set[Email]
  , cc      : Set[Email]
  , bcc     : Set[Email]
  , subject : String
  , template: String
)


class NotificationService(
  changeRequestEventLogService: ChangeRequestEventLogService
) {
  val configMailPath = "/opt/rudder/etc/plugins/change-validation.conf"
  val logger         = NamedZioLogger("plugin.change-validation")

  def sendNotification(step: WorkflowNode, cr: ChangeRequest): IOResult[Unit] = {
    for {
      serverConfig <- getSMTPConf(configMailPath)
      emailConf    <- getStepMailConf(step, configMailPath)
      params       <- extractChangeRequestInfo(cr)
      emailBody    <- getContentFromTemplate(emailConf, params)
      _            <- sendEmail(serverConfig, emailBody, emailConf)
    } yield ()
  }

  private[this] def sendEmail(conf: SMTPConf, emailBody: String, mailParameter: EmailConf): IOResult[Unit] = {

    val prop = new Properties()
    prop.put("mail.smtp.host", conf.smtpHostServer)
    prop.put("mail.smtp.port", conf.port)
    prop.put("mail.smtp.starttls.enable", "true")

    IOResult.effect {
      val session = (conf.login, conf.password) match {
        case (Some(l), Some(p)) =>
          prop.put("mail.smtp.auth", "true");
          val auth = new Authenticator() {
            override protected def getPasswordAuthentication = new PasswordAuthentication(l.value, p)
          }
          Session.getInstance(prop, auth)
        case (_, None)          =>
          prop.put("mail.smtp.auth", "false");
          Session.getInstance(prop, null)
        case (None, _)          =>
          prop.put("mail.smtp.auth", "false");
          Session.getInstance(prop, null)
      }
      val message = new MimeMessage(session);
      message.setFrom(new InternetAddress(conf.email.value))
      message.setRecipients(
        Message.RecipientType.TO,
        mailParameter.to.map(_.value).mkString(",")
      )
      val reply: Array[Address] = mailParameter.replyTo.map(e => new InternetAddress(e.value)).toArray
      message.addRecipients(Message.RecipientType.BCC, mailParameter.bcc.map(_.value).mkString(","))
      message.addRecipients(Message.RecipientType.CC, mailParameter.cc.map(_.value).mkString(","))
      message.setReplyTo(reply)
      message.setSubject(mailParameter.subject);

      message.setContent(emailBody, "text/html; charset=utf-8");
      Transport.send(message)
    }
  }

  private[this] def getConfig(path: String): IOResult[Config] = {
    val file = new File(path)

    IOResult.effectM {
      for {
        configResource <- if (file.exists && file.canRead) {
          FileSystemResource(file).succeed
        } else {
          Inconsistency(s"Configuration file not found: ${file.getPath}").fail
        }
      } yield {
        ConfigFactory.load(ConfigFactory.parseFile(configResource.file))
      }
    }
  }

  private[this] def getSMTPConf(path: String): IOResult[SMTPConf] = {
    for {
      config <- getConfig(path)
      smtp   <- IOResult.effect(s"An error occurs while parsing SMTP conf in ${path}") {
        val hostServer = config.getString("smtp.hostServer")
        val port       = config.getInt("smtp.port")
        val email      = config.getString("smtp.email")
        val login      = {
          val l = config.getString("smtp.login")
          if (l.isEmpty) None else Some(Username(l))
        }
        val password   = {
          val p = config.getString("smtp.password")
          if (p.isEmpty) None else Some(p)
        }
        SMTPConf(
          hostServer
          , port
          , Email(email)
          , login
          , password
        )
      }
    } yield smtp
  }

  private[this] def getStepMailConf(step: WorkflowNode, path: String): IOResult[EmailConf] = {
    val s = step match {
      case Validation => "validation"
      case Deployment => "deployment"
      case _          => "unsupported"
    }
    for {
      config   <- getConfig(path)
      envelope <- IOResult.effect{
                    val to       = config.getString(s"${s}.to").split(",").map(Email).toSet
                    val replyTo  = config.getString(s"${s}.replyTo").split(",").map(Email).toSet
                    val cc       = config.getString(s"${s}.cc").split(",").map(Email).toSet
                    val bcc      = config.getString(s"${s}.bcc").split(",").map(Email).toSet
                    val subject  = config.getString(s"${s}.subject")
                    val template = config.getString(s"${s}.template")
                    EmailConf(
                      Validation
                      , to
                      , replyTo
                      , cc
                      , bcc
                      , subject
                      , template
                    )
                 }
    } yield envelope
  }

  private[this] def getContentFromTemplate(emailConf: EmailConf, param: Map[String, String]): IOResult[String] = {
    IOResult.effect(s"Error when getting `${emailConf.template}` template configuration"){
      val writer   = new StringWriter()
      val mf       = new DefaultMustacheFactory
      val mustache = mf.compile(new FileReader(emailConf.template), emailConf.template)
      mustache.execute(writer, param.asJava).toString
    }
  }

  private[this] def extractChangeRequestInfo(cr: ChangeRequest): IOResult[Map[String, String]] = {
    for {
      lastLog  <- changeRequestEventLogService.getLastLog(cr.id).toIO
      eventLog <- lastLog match {
                    case None              => Inconsistency("Error when retrieving the last action").fail
                    case Some(e: EventLog) => e.succeed
                  }
      actionName = eventLog match {
                     case _: ModifyChangeRequest => "Modified"
                     case _: AddChangeRequest    => "Created"
                     case _: DeleteChangeRequest => "Deleted"
                   }
    } yield {
      Map(
          "id"          -> cr.id.value.toString
        , "info"        -> cr.info.name
        , "description" -> cr.info.description
        , "actionName"  -> actionName
        , "author"      -> cr.owner
        , "date"        -> eventLog.creationDate.toDate.toString
        , "link"        -> RudderConfig.linkUtil.changeRequestLink(cr.id)
      )
    }
  }
}
