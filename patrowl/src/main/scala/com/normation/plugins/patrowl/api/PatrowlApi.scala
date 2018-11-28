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

package com.normation.plugins.patrowl.api

import java.nio.file.Path

import com.normation.plugins.patrowl.PatrowlLogger
import com.normation.rudder.api.HttpAction.GET
import com.normation.rudder.rest.ApiModuleProvider
import com.normation.rudder.rest.ApiPath
import com.normation.rudder.rest.ApiVersion
import com.normation.rudder.rest.AuthzToken
import com.normation.rudder.rest.EndpointSchema
import com.normation.rudder.rest.EndpointSchema.syntax._
import com.normation.rudder.rest.GeneralApi
import com.normation.rudder.rest.RestExtractorService
import com.normation.rudder.rest.RestUtils
import com.normation.rudder.rest.SortIndex
import com.normation.rudder.rest.StartsAtVersion10
import com.normation.rudder.rest.ZeroParam
import com.normation.rudder.rest.lift.DefaultParams
import com.normation.rudder.rest.lift.LiftApiModule
import com.normation.rudder.rest.lift.LiftApiModule0
import com.normation.rudder.rest.lift.LiftApiModuleProvider
import net.liftweb.http.LiftResponse
import net.liftweb.http.Req
import net.liftweb.json.JsonAST.JString
import net.liftweb.json.NoTypeHints



/*
 * This file contains the internal API used to discuss with the JS application.
 *
 * It gives the list of currently configured authentication backends.
 */
sealed trait PatrowlApi extends EndpointSchema with GeneralApi with SortIndex
object PatrowlApi extends ApiModuleProvider[PatrowlApi] {

  final case object GetConfig extends PatrowlApi with ZeroParam with StartsAtVersion10 {
    val z = zz
    val description    = "Get information about registered users in Rudder"
    val (action, path) = GET / "patrowl" / "config"
  }

  def endpoints = ca.mrvisser.sealerate.values[PatrowlApi].toList.sortBy( _.z )
}


class PatrowlApiImpl(
    restExtractorService: RestExtractorService
  , configPath          : Path
) extends LiftApiModuleProvider[PatrowlApi] {
  api =>

  implicit val formats = net.liftweb.json.Serialization.formats(NoTypeHints)

  def schemas = PatrowlApi

  def getLiftEndpoints(): List[LiftApiModule] = {
    PatrowlApi.endpoints.map(e => e match {
        case PatrowlApi.GetConfig => GetConfig
    }).toList
  }

  /*
   */
  object GetConfig extends LiftApiModule0 {
    val schema = PatrowlApi.GetConfig
    val restExtractor = api.restExtractorService
    def process0(version: ApiVersion, path: ApiPath, req: Req, params: DefaultParams, authzToken: AuthzToken): LiftResponse = {

      import com.normation.plugins.patrowl.Serialisation._
      configPath.toJson match {
        case Right(json) =>
          RestUtils.toJsonResponse(None, json)(schema.name, params.prettify)
        case Left(ex) =>
          PatrowlLogger.error(ex.getMessage, ex)
          RestUtils.toJsonError(None, JString(s"Error when trying to read configuration: ${ex.getMessage}"))(schema.name, params.prettify)
      }
    }
  }
}



