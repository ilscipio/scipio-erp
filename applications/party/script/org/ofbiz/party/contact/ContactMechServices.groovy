/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */

import org.ofbiz.entity.GenericValue
import org.ofbiz.service.ModelService
import org.ofbiz.service.ServiceUtil

/**
 * Create FtpAddress contact Mech
 */
def createFtpAddress() {
    Map contactMech = run service: 'createContactMech', with: [contactMechTypeId: 'FTP_ADDRESS']
    String contactMechId = contactMech.contactMechId
    if (contactMechId) {
        GenericValue ftpAddress = makeValue('FtpAddress', parameters)
        ftpAddress.contactMechId = contactMechId
        ftpAddress.create()
    } else return error('Error creating contactMech')

    Map resultMap = success()
    resultMap.contactMechId = contactMechId
    return resultMap
}

/**
 * Update FtpAddress contact Mech
 */
def updateFtpAddressWithHistory() {
    Map resultMap = success()
    resultMap.oldContactMechId = parameters.contactMechId
    resultMap.contactMechId = parameters.contactMechId
    Map newContactMechResult
    if (resultMap.oldContactMechId) {
        newValue = makeValue('FtpAddress', parameters)
        if (newValue != from('FtpAddress').where(parameters).queryOne()) {  // if there is some modifications in FtpAddress data
            newContactMechResult = run service: 'createFtpAddress', with: parameters
        } else { //update only contactMech
            Map updateContactMechMap = dispatcher.getDispatchContext().makeValidContext('updateContactMech', ModelService.IN_PARAM, parameters)
            updateContactMechMap.contactMechTypeId = 'FTP_ADDRESS'
            newContactMechResult = run service: 'updateContactMech', with: updateContactMechMap
        }

        if (!resultMap.oldContactMechId.equals(newContactMechResult.contactMechId)) {
            resultMap.put('contactMechId', newContactMechResult.contactMechId)
        }
    }
    return resultMap
}

/**
 * Create FtpAddress contact Mech and link it with given partyId
 * @return
 */
def createPartyFtpAddress() {
    Map contactMech = run service: 'createFtpAddress', with: parameters
    if (ServiceUtil.isError(contactMech)) return contactMech
    String contactMechId = contactMech.contactMechId

    Map createPartyContactMechMap = parameters
    createPartyContactMechMap.put('contactMechId', contactMechId)
    Map serviceResult = run service: 'createPartyContactMech', with: createPartyContactMechMap
    if (ServiceUtil.isError(serviceResult)) return serviceResult

    //TODO: manage purpose

    Map resultMap = success()
    resultMap.contactMechId = contactMechId
    return resultMap
}

def updatePartyFtpAddress() {
    Map updateFtpResult = run service: 'updateFtpAddressWithHistory', with: parameters
    Map result = success()
    result.contactMechId = parameters.contactMechId
    if (parameters.contactMechId != updateFtpResult.contactMechId) {
        Map updatePartyContactMechMap = dispatcher.getDispatchContext().makeValidContext('updatePartyContactMech', ModelService.IN_PARAM, parameters)
        updatePartyContactMechMap.newContactMechId = updateFtpResult.contactMechId
        updatePartyContactMechMap.contactMechTypeId = 'FTP_ADDRESS'
        run service: 'updatePartyContactMech', with: updatePartyContactMechMap
        result.contactMechId = updateFtpResult.contactMechId
    }
    return result
}
