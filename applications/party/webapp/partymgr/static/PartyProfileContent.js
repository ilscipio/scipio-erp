/*
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
* */

/**
 *
 */
var ppcUiLabelJsonObjects = null;
jQuery(document).ready(function() {

    var labelObject = {
            "CommonUiLabels" : ["CommonUpload", "CommonSave", "CommonCompleted", "CommonError", "CommonServerCommunicationError"]
          };

    ppcUiLabelJsonObjects = getJSONuiLabels(labelObject);

    //jQuery("#upc_progress_bar").progressbar({value: 0});
    ppcResetUploadProgress();
});

function ppcSetUploadProgressValue(percent) {
	jQuery("#upc_progress_bar_meter").css({"width": percent + "%"});
	if (typeof jQuery("#upc_progress_bar_meter").attr("aria-valuenow") !== 'undefined') {
		jQuery("#upc_progress_bar_meter").attr("aria-valuenow", percent.toString());
	}
	jQuery('#upcProgressBarSavingMsg').html(ppcUiLabelJsonObjects.CommonUiLabels[0] + "... (" + percent + "%)");
}

function ppcSetUploadProgressState(classStr) {
	var stateStyles = [catoStyles.color_info, catoStyles.color_success, catoStyles.color_alert, catoStyles.color_warning].join(" ");
	jQuery("#upc_progress_bar").removeClass(stateStyles).addClass(classStr);
	jQuery('#upcProgressBarSavingMsg').removeClass(stateStyles).addClass(classStr);
}

function ppcSetUploadProgressMsg(msg) {
	jQuery('#upcProgressBarSavingMsg').html(msg);
}

function ppcResetUploadProgress() {
	ppcSetUploadProgressValue(0);
	ppcSetUploadProgressState(catoStyles.color_info)
}

function ppcUploadPartyContent(event){
    //jQuery("#upc_progress_bar").progressbar("option", "value", 0);
	ppcResetUploadProgress();
	jQuery("#upc_progress_bar").removeClass(catoStyles.hidden);
    var targetFrame = jQuery('#target_upload');
    var infodiv = jQuery('#content-messages');
    if(infodiv.length < 1){
        jQuery('<div class="' + catoStyles.grid_row + '"><div class="' + catoStyles.grid_large + '12 ' + catoStyles.grid_cell + '" id="content-messages"></div></div>').insertAfter(jQuery("#partyContentList"));
    }
    if (targetFrame.length < 1){
        jQuery('#partyContent').append("<iframe id='target_upload' name='target_upload' style='display: none' src=''> </iframe>");
    }
    jQuery('#uploadPartyContent').attr("target", "target_upload");

    var labelField = jQuery("#upcProgressBarSavingMsg");
    if (labelField.length) {
        labelField.remove();
    }
}

function ppcUploadCompleted(){
    var iframePartyContentList = jQuery("#target_upload").contents().find("#partyContentList").html();

    // update partyContentList - copy the Data from the iFrame partyContentList
    // to the page partyContentList
    jQuery("#partyContentList").html(iframePartyContentList);

    // reset progressbar
    //jQuery("#upc_progress_bar").progressbar("option", "value", 0);
    // Cato: why reset here?
    //ppcResetUploadProgress();
    ppcSetUploadProgressValue(100);
    ppcSetUploadProgressState(catoStyles.color_success);
    ppcSetUploadProgressMsg(ppcUiLabelJsonObjects.CommonUiLabels[2]);
    
    // remove iFrame
    jQuery("#target_upload").remove();
    return;
}

function ppcCheckIframeStatus() {
    var iframePartyContentList = null;
    // if the new partyContentList isn't created wait a few ms and call the
    // method again
    jQuery.fjTimer({
        interval: 500,
        repeat: true,
        tick: function(counter, timerId) {
            iframePartyContentList = jQuery("#target_upload").contents().find("#partyContentList");
            if (iframePartyContentList != null && iframePartyContentList.length > 0) {
                timerId.stop();
                ppcUploadCompleted();
            }
        }
    });
    return;
}

function ppcShowUploadError(errdata) {
	jQuery('#content-messages').html('<div data-alert class="' + catoStyles.alert_wrap + ' ' + catoStyles.alert_prefix_type + 'alert">' + errdata + "</div>");
	ppcSetUploadProgressState(catoStyles.color_alert);
	ppcSetUploadProgressMsg(ppcUiLabelJsonObjects.CommonUiLabels[3]);
}

function ppcGetUploadProgressStatus(event){
    jQuery('#upcProgressMsgBox').append("<span id='upcProgressBarSavingMsg' class='label'>" + ppcUiLabelJsonObjects.CommonUiLabels[0] + "...</span>");
    var i=0;
    jQuery.fjTimer({
        interval: 1000,
        repeat: true,
        tick: function(counter, timerId) {
            var timerId = timerId;
            jQuery.ajax({
                url: getOfbizUrl('getFileUploadProgressStatus'),
                dataType: 'json',
                success: function(data) {
                    if (data._ERROR_MESSAGE_LIST_ != undefined) {
                    	ppcShowUploadError.html(data._ERROR_MESSAGE_LIST_);
                        timerId.stop();
                     } else if (data._ERROR_MESSAGE_ != undefined) {
                    	 ppcShowUploadError(data._ERROR_MESSAGE_);
                        timerId.stop();
                     } else {
                        var readPercent = data.readPercent;
                        //jQuery("#upc_progress_bar").progressbar("option", "value", readPercent);
                        ppcSetUploadProgressValue(readPercent);
                        if(readPercent > 99){
                        	ppcSetUploadProgressMsg(ppcUiLabelJsonObjects.CommonUiLabels[1] + "...");
                            // stop the fjTimer
                            timerId.stop();
                            // call the upload complete method to do final stuff
                            ppcCheckIframeStatus();
                        }
                     }
                },
                error: function(data) {
                	ppcShowUploadError(ppcUiLabelJsonObjects.CommonUiLabels[4]);
                    timerId.stop();
                }
            });
        }
    });
}
