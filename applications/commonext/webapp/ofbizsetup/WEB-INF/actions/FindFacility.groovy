/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
import org.ofbiz.entity.util.EntityUtil;

findResult = delegator.findByAnd("Facility", [ownerPartyId: partyId], null, false);

listPartyPostalAddress = delegator.findByAnd("PartyAndPostalAddress", [partyId: partyId], null, false);
partyPostalAddress = EntityUtil.getFirst(EntityUtil.filterByDate(listPartyPostalAddress));
context.partyPostalAddress = partyPostalAddress;

if("productstore".equals(context.activeSubMenuItemTop)){
    if(findResult.size() == 0){
        request.setAttribute("_ERROR_MESSAGE_", "Facility not set!");
        context.showScreen = "message";
        return;
    }else{
        context.showScreen = "origin";
    }
}else if("facility".equals(context.activeSubMenuItemTop)){
    facilityId = parameters.facilityId;
    if (!facilityId && request.getAttribute("facilityId")) {
        facilityId = request.getAttribute("facilityId");
    }
    facility = delegator.findOne("Facility", [facilityId : facilityId], false);
    if(facility){
        facilityType = facility.getRelatedOne("FacilityType", false);
        context.facilityType = facilityType;
    }
    context.facility = facility;
    context.facilityId = facilityId;
}
