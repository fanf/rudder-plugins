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

package com.normation.plugins.patrowl

import java.nio.file.Files
import java.nio.file.Path

import net.liftweb.common.Box
import net.liftweb.common.Failure
import net.liftweb.common.Full
import net.liftweb.common.Logger
import net.liftweb.json.JValue
import org.slf4j.LoggerFactory

import scala.concurrent.duration.Duration
import scalaj.http.Http
import scalaj.http.HttpOptions
import scalaj.http.HttpResponse

/**
 * Applicative log of interest for Rudder ops.
 */
object PatrowlLogger extends Logger {
  override protected def _logger = LoggerFactory.getLogger("patrowl")
}

final case class JsonPatrowConfig(
    // Patrowl server base url, so that `patrowlBaseUrl`/assets/details/${node id mapping} leads to node asset content
    patrowlBaseUrl : String
  , username       : String
  , password       : String
    // mapping between a nodeId and the corresponding asset identifier in Patrowl
  , nodeIdMapping  : Map[String, Int]
)

object Serialisation {

  implicit class AuthConfigSer(config: JsonPatrowConfig) {
    def toJson: JValue = {
      import net.liftweb.json._
      implicit val formats = Serialization.formats(NoTypeHints)
      Extraction.decompose(config)
    }
  }

  implicit class ReadConfig(path: Path) {
    import net.liftweb.json._
    implicit val formats = Serialization.formats(NoTypeHints)

    def toJson: Either[Exception, JValue] = {

      try {
        Right(JsonParser.parse(Files.newBufferedReader(path), closeAutomatically = true))
      } catch {
        case ex: Exception =>
          Left(ex)
      }
    }

    def toConfig: Either[Exception, JsonPatrowConfig] = {
      toJson.fold(Left(_), json => try {
        Right(Extraction.extract[JsonPatrowConfig](json))
      } catch {
        case ex: Exception =>
          Left(ex)
      })
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
  def QUERY(method: HttpMethod, url: String, headers: Map[String, String], params: Map[String, String], checkSsl: Boolean, connectionTimeout: Duration, readTimeOut: Duration): Box[HttpResponse[String]] = {
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
        case HttpMethod.POST => c.postForm
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
  def get(url: String, headers: Map[String, String]) = QUERY(HttpMethod.GET, url, headers, Map(), false, timeout, timeout)
  def post(url: String, headers: Map[String, String], params: Map[String, String]) = QUERY(HttpMethod.POST, url, headers, params, false, timeout, timeout)
}
