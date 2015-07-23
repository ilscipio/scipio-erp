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

var catoUploadProgLabels = null;
function catoLoadUploadProgressLabels() {
	if (catoUploadProgLabels == null) {
	    var labelObject = {
	            "CommonUiLabels" : ["CommonUpload", "CommonSave", "CommonCompleted", "CommonError", "CommonServerCommunicationError"]
	          };
	    catoUploadProgLabels = getJSONuiLabels(labelObject);
	}
}


function CatoUploadProgress(options) {
	if (!options) {
		options = {};
	}
	
    this.progBarId = options.progBarId; // required ("#upc_progress_bar"), should exist in doc
    this.progMsgBoxId = options.progMsgBoxId; // required ("#upcProgressBarSavingMsg"), should exit in doc
    this.iframeParentId = options.iframeParentId; // required ("#partyContent"), should exist in doc
    this.iframeId = options.iframeId; // required ("#target_upload"), should NOT exist in doc
    this.msgContainerId = options.msgContainerId; // required ("#content-messages"), if no sibling must exist, if sibling must NOT exist
    this.msgContainerSiblingId = options.msgContainerSiblingId; // optional ("#partyContentList"), if not specified, won't generate
    
    
    /* Public functions */
    
    this.reset = function() {
    	catoLoadUploadProgressLabels();
        this.resetProgress();
    };
    
    this.initUpload = function() {
    	catoLoadUploadProgressLabels();
        this.resetInitContainers();
        this.beginProgressStatus();
    };


    /* Private functions */
    
	this.setProgressValue = function(percent) {
		jQuery("#upc_progress_bar_meter").css({"width": percent + "%"});
		if (typeof jQuery("#upc_progress_bar_meter").attr("aria-valuenow") !== 'undefined') {
			jQuery("#upc_progress_bar_meter").attr("aria-valuenow", percent.toString());
		}
		jQuery('#upcProgressBarSavingMsg').html(catoUploadProgLabels.CommonUiLabels[0] + "... (" + percent + "%)");
	};
	
	this.setProgressState = function(classStr) {
		var stateStyles = [catoStyles.color_info, catoStyles.color_success, catoStyles.color_alert, catoStyles.color_warning].join(" ");
		jQuery("#upc_progress_bar").removeClass(stateStyles).addClass(classStr);
		jQuery('#upcProgressBarSavingMsg').removeClass(stateStyles).addClass(classStr);
	};
	
	this.setProgressMsg = function(msg) {
		jQuery('#upcProgressBarSavingMsg').html(msg);
	};
	
	this.resetProgress = function() {
		this.setProgressValue(0);
		this.setProgressState(catoStyles.color_info)
	};
	
	this.resetInitContainers = function(event){
		this.resetProgress();
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
	};
	
	this.processUploadComplete = function(){
	    var iframePartyContentList = jQuery("#target_upload").contents().find("#partyContentList").html();
	
	    // update partyContentList - copy the Data from the iFrame partyContentList
	    // to the page partyContentList
	    jQuery("#partyContentList").html(iframePartyContentList);
	
	    this.setProgressValue(100);
	    this.setProgressState(catoStyles.color_success);
	    this.setProgressMsg(catoUploadProgLabels.CommonUiLabels[2]);
	    
	    // remove iFrame
	    jQuery("#target_upload").remove();
	    return;
	};
	
	this.checkIframeStatus = function() {
	    var iframePartyContentList = null;
	    // if the new partyContentList isn't created wait a few ms and call the
	    // method again
	    var uploadProg = this;
	    jQuery.fjTimer({
	        interval: 500,
	        repeat: true,
	        tick: function(counter, timerId) {
	            iframePartyContentList = jQuery("#target_upload").contents().find("#partyContentList");
	            if (iframePartyContentList != null && iframePartyContentList.length > 0) {
	                timerId.stop();
	                uploadProg.processUploadComplete();
	            }
	        }
	    });
	    return;
	};
	
	this.showError = function(errdata) {
		jQuery('#content-messages').html('<div data-alert class="' + catoStyles.alert_wrap + ' ' + catoStyles.alert_prefix_type + 'alert">' + errdata + "</div>");
		this.setProgressState(catoStyles.color_alert);
		this.setProgressMsg(catoUploadProgLabels.CommonUiLabels[3]);
	};
	
	this.beginProgressStatus = function(event){
	    jQuery('#upcProgressMsgBox').append("<span id='upcProgressBarSavingMsg' class='label'>" + catoUploadProgLabels.CommonUiLabels[0] + "...</span>");
	    var i=0;
	    var uploadProg = this;
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
	                    	uploadProg.showError(data._ERROR_MESSAGE_LIST_);
	                        timerId.stop();
	                    } else if (data._ERROR_MESSAGE_ != undefined) {
	                    	uploadProg.showError(data._ERROR_MESSAGE_);
	                        timerId.stop();
	                    } else {
	                        var readPercent = data.readPercent;
	                        uploadProg.setProgressValue(readPercent);
	                        if(readPercent > 99){
	                        	uploadProg.setProgressMsg(catoUploadProgLabels.CommonUiLabels[1] + "...");
	                            // stop the fjTimer
	                            timerId.stop();
	                            // call the upload complete method to do final stuff
	                            uploadProg.checkIframeStatus();
	                        }
	                     }
	                },
	                error: function(data) {
	                	uploadProg.showError(catoUploadProgLabels.CommonUiLabels[4]);
	                    timerId.stop();
	                }
	            });
	        }
	    });
	};
}
