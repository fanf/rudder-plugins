/*
*************************************************************************************
* Copyright 2014 Normation SAS
*************************************************************************************
*
* This file is part of Rudder.
*
* Rudder is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* In accordance with the terms of section 7 (7. Additional Terms.) of
* the GNU General Public License version 3, the copyright holders add
* the following Additional permissions:
* Notwithstanding to the terms of section 5 (5. Conveying Modified Source
* Versions) and 6 (6. Conveying Non-Source Forms.) of the GNU General
* Public License version 3, when you create a Related Module, this
* Related Module is not considered as a part of the work and may be
* distributed under the license agreement of your choice.
* A "Related Module" means a set of sources files including their
* documentation that, without modification of the Source Code, enables
* supplementary functions or services in addition to those offered by
* the Software.
*
* Rudder is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Rudder.  If not, see <http://www.gnu.org/licenses/>.
*
*************************************************************************************
*/

package com.normation.plugins.patrowl.extension

import java.nio.file.Path

import com.normation.plugins.PluginExtensionPoint
import com.normation.plugins.PluginStatus
import com.normation.plugins.patrowl.JsonPatrowConfig
import com.normation.plugins.patrowl.PatrowlLogger

import scala.xml.NodeSeq
import com.normation.rudder.web.components.ShowNodeDetailsFromNode
import net.liftweb.common._
import net.liftweb.util.Helpers._

import scala.reflect.ClassTag
import scala.xml.Elem

class CreateNodeDetailsExtension(configPath: Path, val status: PluginStatus)(implicit val ttag: ClassTag[ShowNodeDetailsFromNode]) extends PluginExtensionPoint[ShowNodeDetailsFromNode] with Loggable {

  def pluginCompose(snippet: ShowNodeDetailsFromNode) : Map[String, NodeSeq => NodeSeq] = Map(
      "popupDetails" -> addExternalReportTab(configPath, snippet) _
    , "mainDetails"  -> addExternalReportTab(configPath, snippet) _
  )

  /**
   * Add a tab:
   * - add an li in ul with id=ruleDetailsTabMenu
   * - add the actual tab after the div with id=ruleDetailsEditTab
   */
  def addExternalReportTab(configFile: Path, snippet: ShowNodeDetailsFromNode)(xml: NodeSeq) = {

    val content = status.isEnabled match {
      case false =>
        <p class="error">Patrowl security scan plugin is not enable.</p>
      case true =>
        import com.normation.plugins.patrowl.Serialisation._

        configFile.toConfig match {
          case Left(ex) =>
            PatrowlLogger.error(s"Error when trying to get security scan reports for node with id '${snippet.nodeId.value}'", ex)
            <p class="error">Error when loading Patrowl security scan plugin: {ex.getMessage}</p>
          case Right(config) =>
            config.nodeIdMapping.get(snippet.nodeId.value) match {
              case None =>
                <p class="info">Current node is not mapped to security scan. Check that the node
                  is configured in Patrowl and that the mapping between Rudder node ID and Patrowl node ID is correct.</p>
              case Some(id) =>
                requestAsset(config, id) match {
                  case eb: EmptyBox =>
                    val msg = (eb ?~! "Error when loading Patrowl report for that asset").messageChain
                    <p class="error">{msg}</p>
                  case Full(json) =>
                    <div>
                      <div id="patrowl-node-app"></div>
                      <script>
                        var assetReport = {json};
                        var node = document.getElementById("patrowl-node-app");
                        var app  = Elm.PatrowlNode.embed(node, {{
                            contextPath: contextPath
                          , assetId: {id}
                          , report: assetReport
                        }});
                      </script>
                    </div>
                }
          }
        }
    }

    head ++
    (   "#NodeDetailsTabMenu *" #> { (x:NodeSeq) => x ++ <li><a href="#securityScanReportTab">Security Scan</a></li> }
      & "#node_logs"            #> { (x:NodeSeq) => x ++ tabXml(content) }
    )(xml)
  }

  val head =
      <head_merge>
        <title>Plugin :: Patrowl Security Scan</title>
        <link rel="stylesheet" type="text/css" href="/toserve/patrowl/patrowl.css" media="screen" data-lift="with-cached-resource"></link>
        <link rel="stylesheet" type="text/css" href="/toserve/patrowl/toasty.css" media="screen" data-lift="with-cached-resource"></link>
        <script src="/toserve/patrowl/patrowl-node.js" data-lift="with-cached-resource"></script>
      </head_merge>


  def requestAsset(config: JsonPatrowConfig, id: Int): Box[String] = {
    // <input type='hidden' name='csrfmiddlewaretoken' value='qtNZwyNdDjgDJqIq1obKIp0mSxoZUpRTbNlgoFuFh6bkBNXOGOg0APTeiGzzT46F' />
    val csrInput = """.*<input\s+type='hidden'\s+name='csrfmiddlewaretoken'\s+value='(\w)+'.*""".r
    def parseCsr(html: String): Box[String] = {
      html.split("\n").find(_.contains("csrfmiddlewaretoken")) match {
        case None    => Failure("The response HTML does not contains a CSR token")
        case Some(l) => l match {
          case csrInput(token) => Full(token)
          case _ => Failure("We were not able to parse HTML to find CSR token")
        }
      }
    }

    import com.normation.plugins.patrowl.QueryHttp.{get, post}

    for {
      html    <- get(config.patrowlBaseUrl+"login", Map())
      csr     <- parseCsr(html.body)
      params  =  Map(
                     ("csrfmiddlewaretoken", csr)
                   , ("username", config.username)
                   , ("password", config.password)
                 )
      resp    <- post(config.patrowlBaseUrl+"login", headers = Map(), params = params)
      session <- resp.headers.get("Set-Cookie") match {
                   case None    => Failure("Missing session header")
                   case Some(x) => x.toList match {
                     case h :: Nil => Full(h)
                     case _        => Failure("Missing/bad session header")
                   }
                 }
      asset   <- get(config.patrowlBaseUrl+"assets/report/json/"+id.toString, Map("Cookie" -> session))
    } yield {
      asset.body
    }
  }

  private def tabXml(content: Elem): Elem =
    <div id="securityScanReportTab">
      <div class="inner-portlet">
        <div class="page-title">Patrowl security scan</div>
        <div class="portlet-content">
          <div class="col-xs-12 callout-fade callout-warning">
            <div class="marker">
              <span class="glyphicon glyphicon-info-sign"></span>
            </div>
            <div>
              <div class="col-xs-4">
                <img src="/toserve/patrowl/patrowl.png" alt="Patrowl logo" data-lift="with-cached-resource"></img>
              </div>
              <div class="col-xs-8">
                <b>This page shows the node's security scan reports from Patrowl.</b>
              </div>
            </div>
          </div>
          <div class="col-xs-12">
          {content}
          </div>
        </div>
        <p>&nbsp;</p>
      </div>
    </div>
}
