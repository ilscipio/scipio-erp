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
// Foundation JavaScript
// Documentation can be found at: http://foundation.zurb.com/docs


function collapseFieldset(){
    var parent = $(".toggleField");
    parent.each(function( index ) {
        $(this).find("fieldset > .row").wrapAll('<div class="collapsehide"/>');
        if(parent.hasClass("collapsed")){
            parent.find(".collapsehide").hide();
        }
    });
    

    $(".toggleField legend, .toggleField .legend").click(function(){
        $(this).children("i").toggleClass(" fa-arrow-right").toggleClass(" fa-arrow-down");
        $(this).nextAll("div.collapsehide").slideToggle(300);
    });
}


$(function(){
	if (typeof $(document).foundation() !== 'undefined'){
	    $(document).foundation();
	    scipioObjectFit();
	    collapseFieldset();
	    $('.ui-autocomplete').addClass('f-dropdown');
	    
	    if (typeof Pizza !== 'undefined') {
	        Pizza.init(); // Create charts
	    }
	}

    //jQuery validate specific options
    $.validator.setDefaults({
        errorElement: "span",
        errorClass: "error",
        debug: true
    });
    
});

/* no longer needed; macros can decide where since patch
jQuery(document).ready(function() {
    var asteriks = jQuery('.form-field-input-asterisk');
    asteriks.each(function() {
        var label = jQuery(this).parents('.form-field-entry').first().find('.form-field-label').first();
        label.append(' <span class="form-field-label-asterisk">*</span>');
    });
});
*/

/**
 * Scipio upload progress handler. Instance represents one upload form.
 */
function ScipioUploadProgress(options) {
    if (!options) {
        options = {};
    }
    
    this.uploading = false; // can check this from caller as well
    
    ScipioUploadProgress.instCount = ScipioUploadProgress.instCount + 1;
    this.instNum = ScipioUploadProgress.instCount;
    this.uploadCount = 0;
    
    // All options that end in -Id specify a unique html/css ID.
    // All options that end in -Sel specify a full jQuery element selector (e.g., "form[name=myform]")
    
    this.formSel = options.formSel; // required, should exist in doc
    this.progBarId = options.progBarId; // optional, but if used should exist in doc
    this.progMeterId = options.progMeterId; // optional, has default (based on progBarId and @progress ftl macro)
    this.progTextBoxId = options.progTextBoxId; // optional, but if used should exist in doc
    this.progTextElemId = options.progTextElemId; // optional, has default (based on progTextBoxId), should NOT already exist in doc
    
    this.msgContainerId = options.msgContainerId; // optional, only required if no parent specified; for error messages
    this.msgContainerParentSel = options.msgContainerParentSel; // optional, default the form's parent element; designates an element as parent for a message container; if msgContainerId elem already exists on page, won't use
    this.msgContainerInsertMode = options.msgContainerInsertMode; // optional, default "prepend"; for parent; either "prepend" or "append" (to parent)
    
    this.iframeParentSel = options.iframeParentSel; // optional, default is html body, if specified should exist in doc; will contain hidden iframe(s) to internally hold file upload html page result
    this.expectedResultContainerSel = options.expectedResultContainerSel; // required; id of an elem to test existence in upload page result; was originally same as resultContentContainerSel
    
    this.successResultContainerSel = options.successResultContainerSel; // required; success message within the iframe content to display.
    this.successResultAddWrapper = options.successResultAddWrapper; // optional, default false; if true, successResultContainerSel contents will be wrapped like other messages; else it must supply its own; does not apply to ajax and other notifications (always get wrapper, needed)
    
    this.errorResultContainerSel = options.errorResultContainerSel; // required; if this elem in upload page result exists, treat it as error and use its content as error message (required to help against forever-uploading bug)
                                                                    // 2016-11-02: if the elem has a "has-scipio-errormsg" html attribute (true/false string value) it is consulted to check if should consider errors present
    this.errorResultAddWrapper = options.errorResultAddWrapper; // optional, default false; if true, errorResultContainerSel contents will be wrapped like other errors; else it must supply its own; does not apply to ajax and other errors (always get wrapper, needed)
    
    this.resultContentReplace = options.resultContentReplace; // boolean, default false; if true replace some content in this page with content from upload page result (iframe)
    this.contentContainerSel = options.contentContainerSel; // required if resultContentReplace true, id of content on current page to be replaced
    this.resultContentContainerSel = options.resultContentContainerSel; // required if resultContentReplace true, id of content on upload page result
    
    this.successRedirectUrl = options.successRedirectUrl; // optional; if specified, will redirect to this URL on upload success
    this.successSubmitFormSel = options.successSubmitFormSel; // optional; same as successRedirectUrl but submits a form instead
    this.successReloadWindow = options.successReloadWindow; // optional; same as successRedirectUrl but reloads current page instead (WARN: should usually avoid in Ofbiz screens as causes navigation issues)
    //this.successReplaceWindow = options.successReplaceWindow; // not implemented/possible; optional, default false; if true, upon success will replace this window with contents of iframe
    
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
        this.msgContainerId = "scipio_progupl_content_messages_" + this.instNum;
    }
    if (!this.msgContainerInsertMode) {
        this.msgContainerInsertMode = "prepend";
    }
    
    this.iframeBaseId = "scipio_progupl_target_upload_" + this.instNum;
    if (typeof this.successResultAddWrapper !== 'boolean') {
        this.successResultAddWrapper = false;
    }
    
    if (typeof this.errorResultAddWrapper !== 'boolean') {
        this.errorResultAddWrapper = false;
    }
    
    if (typeof this.resultContentReplace !== 'boolean') {
        this.resultContentReplace = false;
    }
    
    if (typeof this.successReloadWindow !== 'boolean') {
        this.successReloadWindow = false;
    }
    if (typeof this.successReplaceWindow !== 'boolean') {
        this.successReplaceWindow = false;
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
            return true;
        }
        return false;
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
            return true;
        }
        return false;
    };


    /* Private functions */
    
    this.delayedInit = function() {
        ScipioUploadProgress.loadUiLabels();
        if (this.uiLabelMap == null) {
            this.uiLabelMap = ScipioUploadProgress.uiLabelMap;
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
        var stateStyles = [scipioStyles.progress_state_info, scipioStyles.progress_state_success, scipioStyles.progress_state_alert].join(" ");
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
        this.setProgressState(scipioStyles.color_info)
    };
    
    this.showError = function(errdata, errorWrapper) {
        if (typeof errorWrapper !== 'boolean') {
            errorWrapper = true;
        }
        if (this.msgContainerId) {
            if (errorWrapper) {
                jQuery("#"+this.msgContainerId).html('<div data-alert class="' + scipioStyles.alert_wrap + ' ' + scipioStyles.alert_prefix_type + 'alert">' + errdata + "</div>");
            }
            else {
                jQuery("#"+this.msgContainerId).html(errdata);
            }
        }
        this.setProgressState(scipioStyles.color_alert);
        this.setProgressText(this.uiLabelMap.CommonError);
    };
    
    this.resetInitContainers = function(uploadInfo) {
        this.resetProgress();
        if (this.progBarId) {
            jQuery("#"+this.progBarId).removeClass(scipioStyles.hidden);
        }
        
        var infodiv = jQuery("#"+this.msgContainerId);
        if(infodiv.length < 1){
            var infodivbox = jQuery('<div class="' + scipioStyles.grid_row + '"><div class="' + scipioStyles.grid_large + '12 ' + scipioStyles.grid_cell + '" id="' + this.msgContainerId + '"></div></div>');
            
            var infodivparent = null;
            if (this.msgContainerParentSel) {
                infodivparent = jQuery(this.msgContainerParentSel);
            }
            else {
                infodivparent = this.getFormElem().parent();
            }
            
            if (this.msgContainerInsertMode == "append") {
                infodivbox.appendTo(infodivparent);
            }
            else {
                infodivbox.prependTo(infodivparent);
            }
        }
        jQuery("#"+this.msgContainerId).empty();
        
        // Scipio: we always create a new iframe for safety, but leaving guard code in case change
        var targetFrame = jQuery("#"+uploadInfo.iframeId);
        if (targetFrame.length < 1) {
            var iframeParent;
            if (this.iframeParentSel) {
                iframeParent = jQuery(this.iframeParentSel);
            }
            else {
                iframeParent = jQuery("body").first();
            }
            iframeParent.append('<iframe id="' + uploadInfo.iframeId + '" name="' + uploadInfo.iframeId + '" style="display: none" src=""> </iframe>');
            uploadInfo.iframeCreated = true;
        }
 
        jQuery("#"+uploadInfo.iframeId).off("load");
        jQuery("#"+uploadInfo.iframeId).empty();
        jQuery("#"+uploadInfo.iframeId).load(jQuery.proxy(this.checkIframeAsyncLoad, this, uploadInfo));
        
        this.getFormElem().attr("target", uploadInfo.iframeId);
    
        if (this.progTextElemId) {
            var labelField = jQuery("#"+this.progTextElemId);
            if (labelField.length) {
                labelField.remove();
            }
        }
        this.initOnce = true;
    };
    
    this.getFormElem = function() {
        return jQuery(this.formSel);
    };
    
    this.processUploadComplete = function(uploadInfo) {
        var error = false;
        if (this.resultContentReplace) {
            var iframeContent = jQuery("#"+uploadInfo.iframeId).contents().find(this.resultContentContainerSel);
            
            if (iframeContent.length > 0) {
                // update content - copy the Data from the iFrame content container
                // to the page content container
                var contentContainer = jQuery(this.contentContainerSel);
                if (contentContainer.length > 0) {
                    contentContainer.html(iframeContent.html());
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
            this.setProgressState(scipioStyles.color_success);
            this.setProgressText(this.uiLabelMap.CommonCompleted);
        }
        
        var iframeDocHtml = null;
        if (!error) {
            if (this.successReplaceWindow) {
                iframeDocHtml = jQuery("#"+uploadInfo.iframeId).contents().find("html").html();
                //var iframe = jQuery("#"+uploadInfo.iframeId)[0];
                //var iframeDocument = iframe.contentDocument || iframe.contentWindow.document;
            }
        }
        
        this.cleanup(uploadInfo);
        
        if (!error) {
            if (this.successRedirectUrl) {
                window.location.href = this.successRedirectUrl;
            }
            else if (this.successSubmitFormSel) {
                jQuery(this.successSubmitFormSel).submit();
            }
            else if (this.successReplaceWindow) {
                var newDoc = document.open("text/html");
                newDoc.write(iframeDocHtml);
                newDoc.close();
            }
            else if (this.successReloadWindow) {
                window.location.reload(true);
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
                // note: errorResultContainerSel and expectedResultContainerSel must be chosen carefully
                // note: explicitly not checking uploadInfo.iframeLoaded for these two, for now...
                if (prog.errorResultContainerSel && !uploadInfo.finished) {
                    iframeErrorContent = jQuery("#"+uploadInfo.iframeId).contents().find(prog.errorResultContainerSel);
                    if (iframeErrorContent.length > 0) {
                        // 2016-11-02: check for custom has-scipio-errormsg attribute
                        var flagAttr = jQuery(iframeErrorContent).attr("has-scipio-errormsg");
                        if (!flagAttr || flagAttr == "true") {
                            uploadInfo.finished = true;
                            timerId.stop();
                            prog.processError(uploadInfo, iframeErrorContent.html(), prog.errorResultAddWrapper);
                        }
                    }
                }
                
                if (!uploadInfo.finished) {
                    iframeContent = jQuery("#"+uploadInfo.iframeId).contents().find(prog.expectedResultContainerSel);
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

ScipioUploadProgress.instCount = 0;
ScipioUploadProgress.uiLabelMap = null;
ScipioUploadProgress.loadUiLabels = function() {
    if (ScipioUploadProgress.uiLabelMap == null) {
        var labelObject = {
                "CommonUiLabels" : ["CommonUpload", "CommonSave", "CommonCompleted", "CommonError", "CommonServerCommunicationError", "CommonUnexpectedError"]
            };
        ScipioUploadProgress.uiLabelMap = getJSONuiLabelMap(labelObject);
    }
};

/**
 * Object-fit fallback for IE (used by <@img> macro)
 * Based on:
 * https://medium.com/@primozcigler/neat-trick-for-css-object-fit-fallback-on-edge-and-other-browsers-afbc53bbb2c3#.cwwk7mtx0
 *
 * */
function scipioObjectFit(){
    Modernizr.addTest('objectfit',
            !!Modernizr.prefixed('objectFit')
        );
    if ( !Modernizr.objectfit ) {
        $('.scipio-image-container').each(function () {
            var $container = $(this),
                imgUrl = $container.find('img').prop('src');
            if (imgUrl) {
                var type= $container.attr('scipioFit');
                $container
                    .css('backgroundImage', 'url(' + imgUrl + ')')
                    .css('background-size',type)
                    .css('background-repeat','no-repeat')
                    .css('background-position','center center')
                    .addClass('compat-object-fit');
                $container.find('img').css('opacity','0');
            }
        });
        }
    return false;
}


function checkboxToFormHiddenInput(checkboxElem, formSel) {
    jQuery(formSel + ' input[name="' + checkboxElem.name + '"]').remove();
    if (checkboxElem.checked) {
        jQuery(formSel).append('<input type="hidden" name="' + checkboxElem.name + '" value="' + checkboxElem.value + '" />')
    }
}
