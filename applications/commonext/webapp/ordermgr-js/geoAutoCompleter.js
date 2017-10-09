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

function getCountryList() {
    countryTargetField = jQuery('#shipToCountryGeo');
    countryDivToPopulate = jQuery('#shipToCountries');
    countryHiddenTarget = jQuery('#shipToCountryGeoId');
    jQuery.ajax({
        url: "getCountryList",
        type: "POST",
        async: false,
        success: callCountryAutocompleter
    });
}

function callCountryAutocompleter(data) {
    countryList = data.countryList;
    countryTargetField.autocomplete({source: countryList, select: setKeyAsParameterAndGetStateList});
}

function setKeyAsParameterAndGetStateList(event, ui) {
    countryHiddenTarget.value = ui.item;
    getAssociatedStateListForAutoComplete();
}

// SCIPIO: SCIPIO: 2017-10-09: Now returns the jQuery.ajax object (instead of void).
function getAssociatedStateListForAutoComplete() {
    stateTargetField = jQuery('#shipToStateProvinceGeo');
    stateDivToPopulate = jQuery('#shipToStates');
    stateHiddenTarget = jQuery('#shipToStateProvinceGeoId');
    return jQuery.ajax({
        url: "getAssociatedStateList",
        type: "POST",
        data: jQuery('#shippingForm').serialize(),
        async: false,
        success: function(data) {callStateAutocompleter(data); }
    });
}

function callStateAutocompleter(data){
    stateList = data.stateList;
    if (stateList.size() <= 1) {
        jQuery('#shipToStateProvinceGeo').value = "No States/Provinces exists";
        jQuery('#shipToStateProvinceGeoId').value = "_NA_";
        jQuery("#shipStates").fadeOut("fast");
        jQuery("#advice-required-shipToStateProvinceGeo").fadeOut("fast");
        jQuery("#shipToStateProvinceGeo").unbind("blur");
    } else {
        jQuery('#shipToStateProvinceGeo').value = "";
        jQuery('#shipToStateProvinceGeoId').value = "";
        jQuery("#shipStates").fadeIn("fast");
        jQuery("#shipToStateProvinceGeo").bind("blur", function() {
            if (jQuery('#shipToStateProvinceGeo').val() == "") {
                jQuery("#advice-required-shipToStateProvinceGeo").fadeIn("fast");
            }
        });
    }
    stateTargetField.autocomplete({source: stateList, select: setKeyAsParameter});
}

function setKeyAsParameter(event, ui) {
    stateHiddenTarget.value = ui.item;
}

// SCIPIO: Generic function for fetching country's associated state list, with extended options.
// New parameters:
// async : boolean, whether to send sync or async (default: true)
// SCIPIO: 2017-10-09: Now returns the jQuery.ajax object (instead of void).
function getAssociatedStateListEx(options) {
    var countryId = options.countryId;
    var stateId = options.stateId;
    var errorId = options.errorId;
    var divId = options.divId;
    
    var countryGeoId = jQuery("#" + countryId).val();
    var requestToSend = "getAssociatedStateList";
    if (jQuery('#orderViewed')) {
        requestToSend = "/ordermgr/control/getAssociatedStateList"
    }
    // SCIPIO: check for async
    var async = true;
    if (options.async === false) {
        async = false;
    }
    return jQuery.ajax({
        url: requestToSend,
        async: async,
        type: "POST",
        data: {countryGeoId: countryGeoId},
        success: function(data) {
            if (data._ERROR_MESSAGE_ ) {
                // no data found/ error occurred
                return;
            }
            stateList = data.stateList;
            var stateSelect = jQuery("#" + stateId);
            // Scipio: Try to preserve the previous value if possible; allows using markup to specify the initial value
            var selected = stateSelect.val();
            stateSelect.find("option").remove();
            jQuery.each(stateList, function(state) {
                geoValues = this.split(': ');
                var selectedStr = "";
                if (selected && selected === geoValues[1]) {
                    selectedStr = ' selected="selected"';
                }
                stateSelect.append(jQuery('<option value="'+geoValues[1]+'"'+selectedStr+'>'+geoValues[0]+'</option>'));
            });
            // Scipio: FIXED for ID bugs
            if (stateList.length <= 1) {
                if (jQuery("#" + divId).is(':visible') || jQuery("#" + errorId).is(':visible')) {
                    jQuery("#" + divId).fadeOut("fast");
                    jQuery("#" + errorId).fadeOut("fast");
                    jQuery("#" + stateId).unbind("blur");
                }
            } else {
                jQuery("#" + divId).fadeIn("fast");
                jQuery("#" + stateId).bind("blur", function() {
                    if (jQuery("#" + stateId).val() == "") {
                        jQuery("#" + errorId).fadeIn("fast")
                    }
                });
            }
        }
    });
}

//Generic function for fetching country's associated state list.
// SCIPIO: NOTE: this is the original Ofbiz function overload.
// SCIPIO: 2017-10-09: Now returns the jQuery.ajax object (instead of void).
function getAssociatedStateList(countryId, stateId, errorId, divId) {
    return getAssociatedStateListEx({countryId: countryId, stateId: stateId,
        errorId: errorId, divId: divId});
}

// SCIPIO: Generic function for fetching country's associated state list - synchronous drop-in replacement.
// SCIPIO: 2017-10-09: Now returns the jQuery.ajax object (instead of void).
function getAssociatedStateListSync(countryId, stateId, errorId, divId) {
    return getAssociatedStateListEx({countryId: countryId, stateId:
        stateId, errorId: errorId, divId: divId, async: false});
}

