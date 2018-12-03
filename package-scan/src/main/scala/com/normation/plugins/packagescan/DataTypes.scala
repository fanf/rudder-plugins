/*
*************************************************************************************
* Copyright 2018 Normation SAS
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

package com.normation.plugins.packagescan

import net.liftweb.common.Box
import net.liftweb.common.Failure
import net.liftweb.common.Full
import net.liftweb.common.Logger
import org.slf4j.LoggerFactory
import scalaj.http.Http
import scalaj.http.HttpOptions
import scalaj.http.HttpResponse

import scala.concurrent.duration.Duration

/**
 * Applicative log of interest for Rudder ops.
 */
object PackageScanLogger extends Logger {
  override protected def _logger = LoggerFactory.getLogger("package-scan")
}


final case class JsonVulnerPackage(name: String, version: String, arch: String)
final case class JsonVulnerOs(name: String, version: String, packages: List[JsonVulnerPackage])

object VulnerSerialization {


  implicit class VulnerOsToJson(vulnerOs: JsonVulnerOs) {

    // prepare json for vunler GET
    def toJsonString: String = {
      import net.liftweb.json.JsonDSL._
      import net.liftweb.json.prettyRender

      prettyRender(
        ("os"      -> vulnerOs.name)
      ~ ("version" -> vulnerOs.version)
      ~ ("package" -> vulnerOs.packages.map(l => s"${l.name} ${l.version} ${l.arch}"))
      )
    }
  }
}



trait HttpMethod
object HttpMethod {
  final case object GET  extends HttpMethod
  final case object POST extends HttpMethod
}

object QueryHttp {

  /*
   * Simple synchronous http get/post, return the response
   * body as a string.
   */
  def QUERY(method: HttpMethod, url: String, headers: Map[String, String], params: Map[String, String], body: String, checkSsl: Boolean, connectionTimeout: Duration, readTimeOut: Duration): Box[HttpResponse[String]] = {
    val options = (
        HttpOptions.connTimeout(connectionTimeout.toMillis.toInt)
     :: HttpOptions.readTimeout(readTimeOut.toMillis.toInt)
     :: (if(checkSsl) {
          Nil
        } else {
          HttpOptions.allowUnsafeSSL :: Nil
        })
    )

    val client = {
      val c = Http(url).headers(headers).options(options).params(params)
      method match {
        case HttpMethod.GET  => c
        case HttpMethod.POST => c.postData(body)
      }
    }

    for {
      response <- net.liftweb.util.Helpers.tryo { client.asString }
      result   <- if(response.isNotError) {
                    Full(response)
                  } else {
                    Failure(s"Request failure URL '${url}': code ${response.code}: ${response.body}")
                  }
    } yield {
      result
    }
  }

  val timeout = Duration(5, "seconds")
  def post(url: String, headers: Map[String, String], body: String) = QUERY(HttpMethod.POST, url, headers, Map(), body, false, timeout, timeout)
}
