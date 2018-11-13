<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<@script>
    <#-- SCIPIO: this breaks everything (?)
    jQuery.noConflict();-->

    var host = document.location.host;
    jQuery(document).ready(function() {
        var productId = jQuery('#ImageFrames_productId').val();
        var imageName = jQuery('#ImageFrames_imageName').val();
    });
    jQuery(window).on('load', function() {
        var width = jQuery('td.image-src img').width();
        var height = jQuery('td.image-src img').height();
        jQuery('td.image-src img').css("width", 200);
        var dimension = width + " x " + height + " pixels";
        jQuery('td.dimension').text(dimension);
        
        var widthFrame = jQuery('td.image-fr img').width();
        var heightFrame = jQuery('td.image-fr img').height();
        jQuery('td.image-fr img').css("width", 200);
        var dimensionFrame = widthFrame + " x " + heightFrame + " pixels";
        jQuery('td.frameDimension').text(dimensionFrame);
    });
    function setTargetWindows(target) {
        if ((target == "upload") || (target == "choose")) {
            jQuery('#ImageFrames').attr("target", "_self");
        } else {
            jQuery('#ImageFrames').attr("target", target);
        }

        if (target == "upload") {
            document.ImageFrames.action = "uploadFrame?productId=" + jQuery('#ImageFrames_productId').val() + "&contentId=" + jQuery('#ImageFrames_contentId').val() + "&dataResourceId="+ jQuery('#ImageFrames_dataResourceId').val() + "&frameExistDataResourceId="+ jQuery('#ImageFrames_frameDataResourceId').val() + "&frameExistContentId="+ jQuery('#ImageFrames_frameContentId').val();
        } else if (target == "choose") {
            document.ImageFrames.action = "chooseFrameImage?productId=" + jQuery('#ImageFrames_productId').val() + "&contentId=" + jQuery('#ImageFrames_contentId').val() + "&dataResourceId="+ jQuery('#ImageFrames_dataResourceId').val() + "&frameExistDataResourceId="+ jQuery('#ImageFrames_frameDataResourceId').val() + "&frameExistContentId="+ jQuery('#ImageFrames_frameContentId').val() + "&frameContentId="+jQuery('#0_lookupId_ImageFrames_imageFrameContentId').val();
        } else if (target == "new") {
            document.ImageFrames.action = 'previewFrameImage';
        } else {
            document.ImageFrames.action = 'createImageFrame';
        }

        if ((target != "upload") && (target != "choose")) {
            var width = jQuery('#ImageFrames_imageWidth').val();
            var hieght = jQuery('#ImageFrames_imageHeight').val();
            if ((width == "") || (hieght == "")) {
                jQuery('#ImageFrames').attr("target", "_self");
            }
        }
    }
    function setUploadTarget(target) {
        jQuery('#ImageFrames').attr("target", target);
    }
    function deletePreviewFrameImage() {
        jQuery.post("deleteFrameImage");
    }
</@script>