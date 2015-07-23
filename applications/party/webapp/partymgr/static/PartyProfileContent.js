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

function CatoUploadProgress(options) {
	if (!options) {
		options = {};
	}
	
	CatoUploadProgress.instCount = CatoUploadProgress.instCount + 1;
	this.instNum = CatoUploadProgress.instCount;
    
	this.formId = options.formId; // required, should exist in doc
    this.progBarId = options.progBarId; // required, should exist in doc
    this.progMeterId = options.progMeterId; // optional, has default, should not exist in doc
    this.progTextBoxId = options.progTextBoxId; // required, should exist in doc
    this.progTextElemId = options.progTextElemId; // optional, has default, should not exist in doc
    this.iframeParentId = options.iframeParentId; // required, should exist in doc
    this.iframeId = options.iframeId; // optional, has default, should not exist in doc
    this.msgContainerId = options.msgContainerId; // required, if no sibling must exist, if has sibling must not exist
    this.msgContainerSiblingId = options.msgContainerSiblingId; // optional, if not specified, won't generate
    
    this.contentContainerId = options.contentContainerId; // currently required, should be optional?
    this.targetContentContainerId = options.targetContentContainerId; // currently required, should be optional?
    
    if (!this.progMeterId) {
    	this.progMeterId = this.progBarId + "_meter";
    }
    if (!this.progTextElemId) {
    	this.progTextElemId = this.progTextBoxId + "_msg";
    }
    if (!this.iframeId) {
    	this.iframeId = "cato_target_upload_" + this.instNum;
    }
    
    this.uiLabelMap = null;
    
    /* Public functions */
    
    this.reset = function() {
    	this.delayedInit();
        this.resetProgress();
    };
    
    this.initUpload = function() {
    	this.delayedInit();
        this.resetInitContainers();
        this.beginProgressStatus();
    };


    /* Private functions */
    
    this.delayedInit = function() {
    	CatoUploadProgress.loadUiLabels();
    	this.uiLabelMap = CatoUploadProgress.uiLabelMap;
    };
    
	this.setProgressValue = function(percent) {
		jQuery("#"+this.progMeterId).css({"width": percent + "%"});
		if (typeof jQuery("#"+this.progMeterId).attr("aria-valuenow") !== 'undefined') {
			jQuery("#"+this.progMeterId).attr("aria-valuenow", percent.toString());
		}
		jQuery("#"+this.progTextElemId).html(this.uiLabelMap.CommonUpload + "... (" + percent + "%)");
	};
	
	this.setProgressState = function(classStr) {
		var stateStyles = [catoStyles.color_info, catoStyles.color_success, catoStyles.color_alert, catoStyles.color_warning].join(" ");
		jQuery("#"+this.progBarId).removeClass(stateStyles).addClass(classStr);
		jQuery("#"+this.progTextElemId).removeClass(stateStyles).addClass(classStr);
	};
	
	this.setProgressText = function(msg) {
		jQuery("#"+this.progTextElemId).html(msg);
	};
	
	this.resetProgress = function() {
		this.setProgressValue(0);
		this.setProgressState(catoStyles.color_info)
	};
	
	this.resetInitContainers = function() {
		this.resetProgress();
		jQuery("#"+this.progBarId).removeClass(catoStyles.hidden);
	    
	    var infodiv = jQuery("#"+this.msgContainerId);
	    if(infodiv.length < 1){
	        jQuery('<div class="' + catoStyles.grid_row + '"><div class="' + catoStyles.grid_large + '12 ' + catoStyles.grid_cell + '" id="' + this.msgContainerId + '"></div></div>').insertAfter(jQuery("#"+this.msgContainerSiblingId));
	    }
	    jQuery("#"+this.msgContainerId).html('');
	    
	    var targetFrame = jQuery("#"+this.iframeId);
	    if (targetFrame.length < 1) {
	        jQuery("#"+this.iframeParentId).append('<iframe id="' + this.iframeId + '" name="' + this.iframeId + '" style="display: none" src=""> </iframe>');
	    }
	    jQuery("#"+this.formId).attr("target", this.iframeId);
	
	    var labelField = jQuery("#"+this.progTextElemId);
	    if (labelField.length) {
	        labelField.remove();
	    }
	};
	
	this.processUploadComplete = function() {
	    var iframePartyContentList = jQuery("#"+this.iframeId).contents().find("#"+this.targetContentContainerId).html();
	
	    // update partyContentList - copy the Data from the iFrame partyContentList
	    // to the page partyContentList
	    jQuery("#"+this.contentContainerId).html(iframePartyContentList);
	
	    this.setProgressValue(100);
	    this.setProgressState(catoStyles.color_success);
	    this.setProgressText(this.uiLabelMap.CommonCompleted);
	    
	    // remove iFrame
	    jQuery("#"+this.iframeId).remove();
	    return;
	};
	
	this.checkIframeStatus = function() {
	    var iframePartyContentList = null;
	    // if the new partyContentList isn't created wait a few ms and call the
	    // method again
	    var prog = this;
	    jQuery.fjTimer({
	        interval: 500,
	        repeat: true,
	        tick: function(counter, timerId) {
	            iframePartyContentList = jQuery("#"+prog.iframeId).contents().find("#"+prog.targetContentContainerId);
	            if (iframePartyContentList != null && iframePartyContentList.length > 0) {
	                timerId.stop();
	                prog.processUploadComplete();
	            }
	        }
	    });
	    return;
	};
	
	this.showError = function(errdata) {
		jQuery("#"+this.msgContainerId).html('<div data-alert class="' + catoStyles.alert_wrap + ' ' + catoStyles.alert_prefix_type + 'alert">' + errdata + "</div>");
		this.setProgressState(catoStyles.color_alert);
		this.setProgressText(this.uiLabelMap.CommonError);
	};
	
	this.beginProgressStatus = function() {
	    jQuery("#"+this.progTextBoxId).append('<span id="' + this.progTextElemId + '" class="label">' + this.uiLabelMap.CommonUpload + '...</span>');
	    var i = 0;
	    var prog = this;
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
	                    	prog.showError(data._ERROR_MESSAGE_LIST_);
	                        timerId.stop();
	                    } else if (data._ERROR_MESSAGE_ != undefined) {
	                    	prog.showError(data._ERROR_MESSAGE_);
	                        timerId.stop();
	                    } else {
	                        var readPercent = data.readPercent;
	                        prog.setProgressValue(readPercent);
	                        if (readPercent > 99) {
	                        	prog.setProgressText(prog.uiLabelMap.CommonSave + "...");
	                            // stop the fjTimer
	                            timerId.stop();
	                            // call the upload complete method to do final stuff
	                            prog.checkIframeStatus();
	                        }
	                     }
	                },
	                error: function(data) {
	                	prog.showError(prog.uiLabelMap.CommonServerCommunicationError);
	                    timerId.stop();
	                }
	            });
	        }
	    });
	};
}

CatoUploadProgress.instCount = 0;
CatoUploadProgress.uiLabelMap = null;
CatoUploadProgress.loadUiLabels = function() {
	if (CatoUploadProgress.uiLabelMap == null) {
	    var labelObject = {
	            "CommonUiLabels" : ["CommonUpload", "CommonSave", "CommonCompleted", "CommonError", "CommonServerCommunicationError"]
	          };
	    CatoUploadProgress.uiLabelMap = getJSONuiLabelMap(labelObject);
	}	
};


