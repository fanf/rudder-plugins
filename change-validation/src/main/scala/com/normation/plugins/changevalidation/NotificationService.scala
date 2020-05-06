package com.normation.plugins.changevalidation

import java.io.File
import java.util.Properties

import bootstrap.liftweb.FileSystemResource
import bootstrap.liftweb.RudderConfig
import com.normation.NamedZioLogger
import com.normation.errors.IOResult
import com.normation.plugins.changevalidation.TwoValidationStepsWorkflowServiceImpl.Deployment
import com.normation.plugins.changevalidation.TwoValidationStepsWorkflowServiceImpl.Validation
import com.normation.rudder.domain.workflows.WorkflowNode
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import javax.mail.Session
import javax.mail._
import javax.mail.internet.InternetAddress
import javax.mail.internet.MimeMessage
import zio.ZIO
import zio.syntax._
import com.github.mustachejava
import com.github.mustachejava.DefaultMustacheFactory
import com.normation.eventlog.EventLog
import com.normation.rudder.domain.eventlog.AddChangeRequest
import com.normation.rudder.domain.eventlog.DeleteChangeRequest
import com.normation.rudder.domain.eventlog.ModifyChangeRequest
import com.normation.rudder.domain.workflows.ChangeRequest
import com.normation.rudder.domain.workflows.ChangeRequestId
import com.normation.rudder.services.eventlog.ChangeRequestEventLogService
import com.normation.rudder.web.components.DateFormaterService
import net.liftweb.common.EmptyBox
import net.liftweb.common.Full

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
  state     : WorkflowNode
  , to      : Set[Email]
  , replyTo : Set[Email]
  , cc      : Set[Email]
  , bcc     : Set[Email]
  , subject : String
  , template: String
)


class NotificationService(
  changeRequestEventLogService: ChangeRequestEventLogService
  , path: Option[String]
) {
  val logger          = NamedZioLogger("plugin.change-validation")

  def sendNotification(text: String, step: WorkflowNode, cr: ChangeRequest): ZIO[Any, Nothing, Object] = {
    path match {
      case Some(p) =>
        val notif = for {
          serverConfig <- getSMTPConf(p)
          emailConf <- getStepMailConf(step, p)
          emailBody <- getContentFromTemplate(emailConf)
          sendingMail <- sendEmail(serverConfig, emailBody, emailConf)
        } yield (sendingMail)

        notif.catchAll {
          err =>
            for {
              _ <- logger.error(s"Sending `${step.id.value}` notification have failed, cause by : ${err}")
            } yield {
              step
            }
        }
      case None =>
        logger.warn(s"No notification will be sent, cause by : missing email path in configuration.")
        step.succeed
    }
  }

  private[this] def sendEmail(conf: SMTPConf, content: String, mailParameter: EmailConf) = {

    val prop = new Properties()
    prop.put("mail.smtp.host", conf.smtpHostServer)
    prop.put("mail.smtp.port", conf.port)
    prop.put("mail.smtp.starttls.enable", "true")

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
    try {
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

      message.setText(content);
      Transport.send(message)
      mailParameter.succeed
    } catch {
      case e: MessagingException =>
        logger.error(s"Sending mail to notification have failed, cause by : ${e.getMessage}").fail
    }
  }

  private[this] def getConfig(path: String): Config = {
    val file           = new File(path)
    val configResource = if (file.exists && file.canRead) {
      Some(FileSystemResource(file))
    } else {
      logger.error("Can not find configuration file specified by JVM property %s: %s ; abort")
      throw new Exception("Configuration file not found: %s".format(file.getPath))
    }
    ConfigFactory.load(ConfigFactory.parseFile(configResource.value.file))
  }

  private[this] def getSMTPConf(path: String): IOResult[SMTPConf] = {
    IOResult.effect(s"An error occurs while parsing SMTP conf in ${path}") {
      val config     = getConfig(path)
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
  }

  private[this] def getStepMailConf(step: WorkflowNode, path: String): IOResult[EmailConf] = {
    IOResult.effect(s"Error when getting `${step.id}` email configuration") {
      val s = step match {
        case Validation => "validation"
        case Deployment => "deployment"
        case _ => "unsupported"
      }

      val config = getConfig(path)
      val to = config.getString(s"${s}.to").split(",").map(Email).toSet
      val replyTo = config.getString(s"${s}.replyTo").split(",").map(Email).toSet
      val cc = config.getString(s"${s}.cc").split(",").map(Email).toSet
      val bcc = config.getString(s"${s}.bcc").split(",").map(Email).toSet
      val subject = config.getString(s"${s}.subject")
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
  }

  private[this] def getContentFromTemplate(emailConf: EmailConf): IOResult[String] = {
    import com.github.mustachejava.DefaultMustacheFactory
    import com.github.mustachejava.Mustache
    import com.github.mustachejava.MustacheFactory
    import java.io.PrintWriter
    IOResult.effect(s"Error when getting `${emailConf.template}` template configuration"){
      import java.io.StringWriter
      val writer = new StringWriter
      val mf     = new DefaultMustacheFactory
      val mustache = mf.compile(emailConf.template)
      val m = Map(
        "toto" -> "tutu"
      )
      mustache.execute(writer, m).flush()
      writer.toString
    }
  }

  private[this] def extractChangeRequestInfo(cr: ChangeRequest) = {
    changeRequestEventLogService.getLastLog(cr.id) match {
      case eb:EmptyBox =>
        "Error when retrieving the last action".fail
      case Full(None)  => "Error, no action were recorded for that change request".fail //should not happen here !
      case Full(Some(e:EventLog)) =>
        val actionName = e match {
          case _: ModifyChangeRequest => "Modified"
          case _: AddChangeRequest    => "Created"
          case _: DeleteChangeRequest => "Deleted"
        }
        Map(
            "title" -> s"CR #${cr.id}: ${cr.info.name}"
          , "description" -> cr.info.description
          , "actionName" -> actionName
          , "author" -> cr.owner
          , "date" -> e.creationDate.toString
          , "link" -> "toto"
          , "diff" -> "titi"
        ).succeed
//        (s"${actionName} on ${DateFormaterService.getDisplayDate(e.creationDate)} by ${e.principal.name}",Some(e.creationDate))
    }
  }
}