<#--
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
-->
<@script src=makeOfbizContentUrl("/images/imagemanagement/sizzle.min.js") />
<@script>
    <#-- SCIPIO: this breaks everything (?)
    jQuery.noConflict();-->

    var host = document.location.host;
    jQuery(document).ready(function() {
        var productId = jQuery('#ImageWatermarking_productId').val();
        var imageName = jQuery('#ImageWatermarking_imageName').val();

       jQuery('.img-src img').click(function(e) {
           var previewCount = jQuery('#ImageWatermarking_previewCount').val();
           var next = parseInt(previewCount) + 1;
           imgOffset = jQuery('.img-src img').offset();
           var pointX = parseInt(e.pageX - imgOffset.left);
           var pointY = parseInt(e.pageY - imgOffset.top);
           jQuery('#ImageWatermarking_positionX').attr('value', pointX);
           jQuery('#ImageWatermarking_positionY').attr('value', pointY);
           getPreviewImage(next);
       });
       jQuery('.preview a').click(function() {
           var previewCount = jQuery('#ImageWatermarking_previewCount').val();
           var next = parseInt(previewCount) + 1;
           getPreviewImage(next);
       });
       jQuery('.txt_color select').change(function() {
           var previewCount = jQuery('#ImageWatermarking_previewCount').val();
           var next = parseInt(previewCount) + 1;
           getPreviewImage(next);
       });
       jQuery('.txt_size select').change(function() {
           var previewCount = jQuery('#ImageWatermarking_previewCount').val();
           var next = parseInt(previewCount) + 1;
           getPreviewImage(next);
       });
       jQuery('.opacity select').change(function() {
           var previewCount = jQuery('#ImageWatermarking_previewCount').val();
           var next = parseInt(previewCount) + 1;
           getPreviewImage(next);
       });
       jQuery('.img-src img').ajaxStart(function() {
            jQuery(this).attr('style', 'opacity:0.6;filter:alpha(opacity=40)');
       });
       jQuery('.img-src img').load(function() {
            jQuery(this).attr('style', 'opacity:1.0;filter:alpha(opacity=100)');
       });
    });
    jQuery(window).load(function() {
       var width = jQuery('.img-src img').width();
       var height = jQuery('.img-src img').height();
       jQuery('#ImageWatermarking_width').attr('value', width);
       jQuery('#ImageWatermarking_height').attr('value', height);
    });
    function getPreviewImage(next) {
       var imageServerUrl = jQuery('#ImageWatermarking_imageServerUrl').val();
       var productId = jQuery('#ImageWatermarking_productId').val();
       var imageName = jQuery('#ImageWatermarking_imageName').val();
       var width = jQuery('#ImageWatermarking_width').val();
       var height = jQuery('#ImageWatermarking_height').val();
       var positionX = jQuery('#ImageWatermarking_positionX').val();
       var positionY = jQuery('#ImageWatermarking_positionY').val();
       var pointX = parseFloat(positionX / width);
       var pointY = parseFloat(positionY / height);
       var text = jQuery('.watermark_txt input').val();
       var opacity = jQuery('.opacity select').val();
       var fontColor = jQuery('.txt_color select').val();
       var fontSize = jQuery('.txt_size select').val();
       var previewCount = jQuery('#ImageWatermarking_previewCount').val();
       var imageData = {productId : productId, imageName : imageName, text : text, opacity : opacity, x : pointX, y : pointY, 
                           width : width, count : previewCount, fontColor : fontColor, fontSize : fontSize};
       jQuery.post("setPreviewWaterMark", imageData, function() {
           var path = imageServerUrl + "/preview/" + "/previewImage" + next + ".jpg";
           jQuery('.img-src img').attr('src', path);
           jQuery('#ImageWatermarking_previewCount').attr('value', next);
       });
    }
    function setImageDimension() {
       var productId = jQuery('#ImageWatermarking_productId').val();
       var imageName = jQuery('#ImageWatermarking_imageName').val();
       var positionX = jQuery('#ImageWatermarking_positionX').val();
       var positionY = jQuery('#ImageWatermarking_positionY').val();
       var width = jQuery('#ImageWatermarking_width').val();
       var height = jQuery('#ImageWatermarking_height').val();
       var pointX = parseFloat(positionX / width);
       var pointY = parseFloat(positionY / height);
       jQuery('#ImageWatermarking_pointX').attr('value', pointX);
       jQuery('#ImageWatermarking_pointY').attr('value', pointY);
    }
    function deletePreviewWatermarkImage() {
       var productId = jQuery('#ImageWatermarking_productId').val();
       var imageName = jQuery('#ImageWatermarking_imageName').val();
       var contentId = jQuery('#ImageFrames_contentId').val();
       var dataResourceId = jQuery('#ImageFrames_dataResourceId').val();
       var previewCount = jQuery('#ImageWatermarking_previewCount').val();
       jQuery.post("deletePreviewWatermarkImage", {productId : productId, imageName : imageName, contentId : contentId, dataResourceId : dataResourceId, count : previewCount});
    }
</@script>
