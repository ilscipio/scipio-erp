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
	
	this.uploading = false; // can check this from caller as well
	
	CatoUploadProgress.instCount = CatoUploadProgress.instCount + 1;
	this.instNum = CatoUploadProgress.instCount;
	this.uploadCount = 0;
    
	this.formId = options.formId; // required, should exist in doc
    this.progBarId = options.progBarId; // optional, but if used should exist in doc
    this.progMeterId = options.progMeterId; // optional, has default (based on progBarId and @progress ftl macro)
    this.progTextBoxId = options.progTextBoxId; // optional, but if used should exist in doc
    this.progTextElemId = options.progTextElemId; // optional, has default (based on progTextBoxId), should NOT already exist in doc
    
    this.msgContainerId = options.msgContainerId; // optional, only required if no msgContainerParentId; for error messages
    this.msgContainerParentId = options.msgContainerParentId; // optional, only required if no msgContainerId; if not specified, won't generate; used for generating a new error message holder
    this.msgContainerInsertMode = options.msgContainerInsertMode; // optional, default "prepend"; either "prepend" or "append" (to parent)
    
    this.iframeParentId = options.iframeParentId; // required, should exist in doc; will contain hidden iframe(s) to internally hold file upload html page result 
    this.expectedResultContainerId = options.expectedResultContainerId; // required; id of an elem to test existence in upload page result; was originally same as resultContentContainerId
    this.errorResultContainerId = options.errorResultContainerId; // required; if this elem in upload page result exists, treat it as error and use its content as error message (required to help against forever-uploading bug)
    this.errorResultAddWrapper = options.errorResultAddWrapper; // optional, default false; if true, errorResultContainerId contents will be wrapped like other errors; else it must supply its own; does not apply to ajax and other errors (always get wrapper, needed)
    
    this.resultContentReplace = options.resultContentReplace; // boolean, default false; if true replace some content in this page with content from upload page result (iframe)
    this.contentContainerId = options.contentContainerId; // required if resultContentReplace true, id of content on current page to be replaced
    this.resultContentContainerId = options.resultContentContainerId; // required if resultContentReplace true, id of content on upload page result
    
    this.successRedirectUrl = options.successRedirectUrl; // optional; if specified, will redirect to this URL on upload success
    
    this.preventDoubleUpload = options.preventDoubleUpload; // optional, default true; not sure why would turn this off
    
    this.initOnce = false;
    
    if (!this.progMeterId) {
    	if (this.progBarId) {
    		this.progMeterId = this.progBarId + "_meter";
    	}
    }
    if (!this.progTextElemId) {
    	if (this.progTextBoxId) {
    		this.progTextElemId = this.progTextBoxId + "_msg";
    	}
    }
    
    if (!this.msgContainerId) {
    	this.msgContainerId = "cato_progupl_content_messages_" + this.instNum;
    }
    if (!this.msgContainerInsertMode) {
    	this.msgContainerInsertMode = "prepend";
    }
    
    this.iframeBaseId = "cato_progupl_target_upload_" + this.instNum;
    if (typeof this.errorResultAddWrapper !== 'boolean') {
    	this.errorResultAddWrapper = false;
    }
    
    if (typeof this.resultContentReplace !== 'boolean') {
    	this.resultContentReplace = false;
    }
    
    if (typeof this.preventDoubleUpload !== 'boolean') {
    	this.preventDoubleUpload = true;
    }
    
    this.uiLabelMap = null;
    
    /* Public functions */
    
    this.reset = function() {
    	if (!this.preventDoubleUpload || !this.uploading) {
    		this.delayedInit();
    		this.resetProgress();
    	}
    };
    
    this.initUpload = function() {
    	if (!this.preventDoubleUpload || !this.uploading) {
    		this.delayedInit();
    		this.uploading = true;
	    	// upload status for a specific upload attempt
	    	var uploadInfo = {
	    		finished : false,
	    		iframeCreated : false,
	    		iframeLoaded : false,
	    		iframeId : this.iframeBaseId + "_" + (this.uploadCount+1)
	    	};
	        this.resetInitContainers(uploadInfo);
	        this.beginProgressStatus(uploadInfo);
	        this.uploadCount = this.uploadCount + 1;
    	}
    };


    /* Private functions */
    
    this.delayedInit = function() {
    	CatoUploadProgress.loadUiLabels();
    	if (this.uiLabelMap == null) {
    		this.uiLabelMap = CatoUploadProgress.uiLabelMap;
    	}
    };
    
	this.setProgressValue = function(percent) {
		if (this.progMeterId) {
			jQuery("#"+this.progMeterId).css({"width": percent + "%"});
		
			if (typeof jQuery("#"+this.progMeterId).attr("aria-valuenow") !== 'undefined') {
				jQuery("#"+this.progMeterId).attr("aria-valuenow", percent.toString());
			}
		}
		if (this.progTextElemId) {
			jQuery("#"+this.progTextElemId).html(this.uiLabelMap.CommonUpload + "... (" + percent + "%)");
		}
	};
	
	this.setProgressState = function(classStr) {
		var stateStyles = [catoStyles.color_info, catoStyles.color_success, catoStyles.color_alert, catoStyles.color_warning].join(" ");
		if (this.progBarId) {
			jQuery("#"+this.progBarId).removeClass(stateStyles).addClass(classStr);
		}
		if (this.progTextElemId) {
			jQuery("#"+this.progTextElemId).removeClass(stateStyles).addClass(classStr);
		}
	};
	
	this.setProgressText = function(msg) {
		if (this.progTextElemId) {
			jQuery("#"+this.progTextElemId).html(msg);
		}
	};
	
	this.resetProgress = function() {
		this.setProgressValue(0);
		this.setProgressState(catoStyles.color_info)
	};
	
	this.showError = function(errdata, errorWrapper) {
		if (typeof errorWrapper !== 'boolean') {
			errorWrapper = true;
		}
		if (this.msgContainerId) {
			if (errorWrapper) {
				jQuery("#"+this.msgContainerId).html('<div data-alert class="' + catoStyles.alert_wrap + ' ' + catoStyles.alert_prefix_type + 'alert">' + errdata + "</div>");
			}
			else {
				jQuery("#"+this.msgContainerId).html(errdata);
			}
		}
		this.setProgressState(catoStyles.color_alert);
		this.setProgressText(this.uiLabelMap.CommonError);
	};
	
	this.resetInitContainers = function(uploadInfo) {
		this.resetProgress();
		if (this.progBarId) {
			jQuery("#"+this.progBarId).removeClass(catoStyles.hidden);
		}
	    
	    var infodiv = jQuery("#"+this.msgContainerId);
	    if(infodiv.length < 1){
	        var indodivbox = jQuery('<div class="' + catoStyles.grid_row + '"><div class="' + catoStyles.grid_large + '12 ' + catoStyles.grid_cell + '" id="' + this.msgContainerId + '"></div></div>');
	        if (this.msgContainerInsertMode == "append") {
	        	indodivbox.appendTo(jQuery("#"+this.msgContainerParentId));
	        }
	        else {
	        	indodivbox.prependTo(jQuery("#"+this.msgContainerParentId));
	        }
	    }
	    jQuery("#"+this.msgContainerId).empty();
	    
	    // Cato: we always create a new iframe for safety, but leaving guard code in case change
	    var targetFrame = jQuery("#"+uploadInfo.iframeId);
	    if (targetFrame.length < 1) {
	        jQuery("#"+this.iframeParentId).append('<iframe id="' + uploadInfo.iframeId + '" name="' + uploadInfo.iframeId + '" style="display: none" src=""> </iframe>');
	        uploadInfo.iframeCreated = true;
	    }
 
	    jQuery("#"+uploadInfo.iframeId).off("load");
	    jQuery("#"+uploadInfo.iframeId).empty();
	    jQuery("#"+uploadInfo.iframeId).load(jQuery.proxy(this.checkIframeAsyncLoad, this, uploadInfo));
	    
	    jQuery("#"+this.formId).attr("target", uploadInfo.iframeId);
	
	    if (this.progTextElemId) {
		    var labelField = jQuery("#"+this.progTextElemId);
		    if (labelField.length) {
		        labelField.remove();
		    }
	    }
	    this.initOnce = true;
	};
	
	this.processUploadComplete = function(uploadInfo) {
		var error = false;
	    if (this.resultContentReplace) {
	    	var iframeContent = jQuery("#"+uploadInfo.iframeId).contents().find("#"+this.resultContentContainerId);
	    	
	    	if (iframeContent.length > 0) {
			    // update content - copy the Data from the iFrame content container
			    // to the page content container
	    		var contentContainer = jQuery("#"+this.contentContainerId);
	    		if (contentContainer.length > 0) {
	    			jQuery("#"+this.contentContainerId).html(iframeContent.html());
	    		}
	    		else {
	    			// don't show error; no chance it reflects on upload success
	    		}
	    	}
	    	else {
	    		// something's missing, probably dev error but can't be sure
	    		error = true;
	    		this.showError(this.uiLabelMap.CommonUnexpectedError);
	    	}
	    }
	    
	    if (!error) {
		    this.setProgressValue(100);
		    this.setProgressState(catoStyles.color_success);
		    this.setProgressText(this.uiLabelMap.CommonCompleted);
	    }
	    
	    this.cleanup(uploadInfo);
	    
	    if (!error) {
		    if (this.successRedirectUrl) {
		    	window.location.href = this.successRedirectUrl;
		    }
	    }
	    return;
	};
	
	this.processError = function(uploadInfo, errdata, errorWrapper) {
		this.showError(errdata, errorWrapper);
		this.cleanup(uploadInfo);
	};
	
	this.cleanup = function(uploadInfo) {
		if (uploadInfo.iframeCreated) {
		    // remove iFrame
		    jQuery("#"+uploadInfo.iframeId).remove();
	    }
		this.uploading = false;
	};
	
	this.checkIframeAsyncLoad = function(uploadInfo) {
		// this version called by jquery... for now, just flag and let timer code handle
		// this helps prevent "forever uploading" bug
		uploadInfo.iframeLoaded = true;
	};
	
	this.checkIframeStatus = function(uploadInfo) {
	    var iframeContent = null;
	    var iframeErrorContent = null;
	    // if the new content isn't created wait a few ms and call the
	    // method again
	    var prog = this;
	    jQuery.fjTimer({
	        interval: 500,
	        repeat: true,
	        tick: function(counter, timerId) {
	        	// note: errorResultContainerId and expectedResultContainerId must be chosen carefully
	        	// note: explicitly not checking uploadInfo.iframeLoaded for these two, for now...
	        	if (prog.errorResultContainerId && !uploadInfo.finished) {
	        		iframeErrorContent = jQuery("#"+uploadInfo.iframeId).contents().find("#"+prog.errorResultContainerId);
	        		if (iframeErrorContent.length > 0) {
	        			uploadInfo.finished = true;
	        			timerId.stop();
	        			prog.processError(uploadInfo, iframeErrorContent.html(), prog.errorResultAddWrapper);
	        		}
	        	}
	        	
	        	if (!uploadInfo.finished) {
		            iframeContent = jQuery("#"+uploadInfo.iframeId).contents().find("#"+prog.expectedResultContainerId);
		            if (iframeContent.length > 0) {
		            	uploadInfo.finished = true;
		                timerId.stop();
		                prog.processUploadComplete(uploadInfo);
		            }
	        	}
	        	
	        	if (!uploadInfo.finished && uploadInfo.iframeLoaded) {
	        		// problem: iframe loaded but we got nothing... usually a coding error but can't be sure
	        		uploadInfo.finished = true;
	                timerId.stop();
	        		prog.processError(uploadInfo, prog.uiLabelMap.CommonUnexpectedError);
	        	}
	        }
	    });
	    return;
	};

	this.beginProgressStatus = function(uploadInfo) {
		if (this.progTextBoxId && this.progTextElemId) {
			jQuery("#"+this.progTextBoxId).append('<span id="' + this.progTextElemId + '" class="label">' + this.uiLabelMap.CommonUpload + '...</span>');
		}
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
	                    	uploadInfo.finished = true;
	                    	timerId.stop();
	                    	prog.processError(uploadInfo, data._ERROR_MESSAGE_LIST_);
	                        
	                    } else if (data._ERROR_MESSAGE_ != undefined) {
	                    	uploadInfo.finished = true;
	                    	timerId.stop();
	                    	prog.processError(uploadInfo, data._ERROR_MESSAGE_);
	                    } else {
	                        var readPercent = data.readPercent;
	                        prog.setProgressValue(readPercent);
	                        if (readPercent > 99) {
	                            // stop the fjTimer
	                            timerId.stop();
	                            prog.setProgressText(prog.uiLabelMap.CommonSave + "...");
	                            // call the upload complete method to do final stuff
	                            prog.checkIframeStatus(uploadInfo);
	                        }
	                    }
	                },
	                error: function(data) {
	                	uploadInfo.finished = true;
	                	timerId.stop();
	                	prog.processError(uploadInfo, prog.uiLabelMap.CommonServerCommunicationError);
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
	            "CommonUiLabels" : ["CommonUpload", "CommonSave", "CommonCompleted", "CommonError", "CommonServerCommunicationError", "CommonUnexpectedError"]
	          };
	    CatoUploadProgress.uiLabelMap = getJSONuiLabelMap(labelObject);
	}	
};


