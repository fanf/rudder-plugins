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

package com.normation.plugins.packagescan.extension

import bootstrap.liftweb.RudderConfig
import com.normation.inventory.domain.NodeId
import com.normation.plugins.PluginExtensionPoint
import com.normation.plugins.PluginStatus
import com.normation.plugins.packagescan.JsonVulnerOs
import com.normation.rudder.web.components.ShowNodeDetailsFromNode
import net.liftweb.common._
import net.liftweb.util.Helpers._

import scala.reflect.ClassTag
import scala.xml.Elem
import scala.xml.NodeSeq

class CreateNodeDetailsExtension(val status: PluginStatus)(implicit val ttag: ClassTag[ShowNodeDetailsFromNode]) extends PluginExtensionPoint[ShowNodeDetailsFromNode] with Loggable {

  val serverAndMachineRepo = RudderConfig.fullInventoryRepository

  def pluginCompose(snippet: ShowNodeDetailsFromNode) : Map[String, NodeSeq => NodeSeq] = Map(
      "popupDetails" -> addExternalReportTab(snippet) _
    , "mainDetails"  -> addExternalReportTab(snippet) _
  )

  /**
   * Add a tab:
   * - add an li in ul with id=ruleDetailsTabMenu
   * - add the actual tab after the div with id=ruleDetailsEditTab
   */
  def addExternalReportTab(snippet: ShowNodeDetailsFromNode)(xml: NodeSeq) = {

    val content = status.isEnabled match {
      case false =>
        <p class="error">Patrowl security scan plugin is not enable.</p>
      case true =>
        requestVulner(snippet.nodeId) match {
          case eb: EmptyBox =>
            val msg = (eb ?~! "Error when loading Patrowl report for that asset").messageChain
            <p class="error">{msg}</p>
          case Full(json) =>
            <div>
              <div id="package-scan-node-app"></div>
              <script>
                var assetReport = {json};
                var node = document.getElementById("package-scan-node-app");
                var app  = Elm.PackageScanNode.embed(node, {{
                    contextPath: contextPath
                  , report: assetReport
                }});
              </script>
            </div>
        }
    }

    head ++
    (   "#NodeDetailsTabMenu *" #> { (x:NodeSeq) => x ++ <li><a href="#packageScanReportTab">Package Scan</a></li> }
      & "#node_logs"            #> { (x:NodeSeq) => x ++ tabXml(content) }
    )(xml)
  }

  val head =
      <head_merge>
        <title>Plugin :: Patrowl Security Scan</title>
        <link rel="stylesheet" type="text/css" href="/toserve/packagescan/package-scan.css" media="screen" data-lift="with-cached-resource"></link>
        <link rel="stylesheet" type="text/css" href="/toserve/packagescan/toasty.css" media="screen" data-lift="with-cached-resource"></link>
        <script src="/toserve/packagescan/package-scan-node.js" data-lift="with-cached-resource"></script>
      </head_merge>


  /*
   * Request vulner security scanner to get vulnerability on package list.
   */
  def requestVulner(nodeId: NodeId): Box[String] = {
    import com.normation.plugins.packagescan.QueryHttp.post
    import com.normation.plugins.packagescan.VulnerSerialization._

    val url = "https://vulners.com/api/v3/audit/audit/"

    import com.normation.plugins.packagescan.{JsonVulnerPackage => p}

    // actually look in node inventory
    // Debian version must be only major part (ie: 8, 9..)
    val testData = JsonVulnerOs("debian", "8", List(
         p("libarchive13" , "3.1.2-11+deb8u2", "all")
       , p("poppler"      , "0.26.5-2+deb8u5", "all")
    ))

    for {
      resp    <- post(url, headers = Map("Content-Type"->"application/json"), testData.toJsonString)
    } yield {
      println(resp)
      resp.body
    }
  }

  private def tabXml(content: Elem): Elem =
    <div id="packageScanReportTab">
      <div class="inner-portlet">
        <div class="page-title">Package scan</div>
        <div class="portlet-content">
          <div class="col-xs-12 callout-fade callout-warning">
            <div class="marker">
              <span class="glyphicon glyphicon-info-sign"></span>
            </div>
            <div>
              <b>This page shows the node's security scan reports about installed packages.</b>
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
