<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@script src=makeOfbizContentUrl("/images/imagemanagement/sizzle.min.js") />
<@script src=makeOfbizContentUrl("/images/imagemanagement/jquery.Jcrop.min.js") />
<#-- FIXME: probably don't want this css here (not sure how to manage js files above) -->
<link rel="stylesheet" href="<@ofbizContentUrl>/images/imagemanagement/jquery.Jcrop.css</@ofbizContentUrl>" type="text/css" />
<@script>
<#-- SCIPIO: this breaks everything (?)
jQuery.noConflict();-->

var imgWidth = null;
var imgHeight = null;
var imageUrl = null;
var imageName = null;
var productId = null;

jQuery(document).ready(function(){

    imgWidth = jQuery('.cropbox img').width();
    imgHeight = jQuery('.cropbox img').height();
    imageUrl = jQuery('#ImageCropping_imageURL').val();
    imageName = jQuery('#ImageCropping_imageName').val();
    productId = jQuery('#ImageCropping_productId').val();
    
    if (imageName) {
        <#assign jsHtmlString>
          '<@field type="generic" label=uiLabelMap.CommonPreview>
            <div> <#-- extra div required, do not remove unless having alternate fix -->
              <div style="width:100px; height:100px; overflow:hidden;">
                <img src="' + imageUrl + '" id="preview" />
              </div>
            </div>
          </@field>'
        </#assign>
        jQuery('#ImageCropping').append(${compressStringBlankspace(jsHtmlString)});
        
        <#assign jsHtmlString>
          '<@field type="submitarea">
            <@field type="submit" text=uiLabelMap.CommonSubmit name="submitButton"/> 
            <@field type="submit" submitType="link" class="+${styles.link_nav_cancel!}" title=" " href=makeOfbizInterWebappUrl("/catalog/control/ListImageManage?productId=' + productId + '") text=uiLabelMap.CommonCancel />
          </@field>'
        </#assign>
        jQuery('#ImageCropping').append(${compressStringBlankspace(jsHtmlString)});
    }

    jQuery('.cropbox img').Jcrop({
        onChange: showPreview,
        onSelect: showPreview
    });
    
});

function showPreview(coords){
    jQuery('#ImageCropping_imageX').val(coords.x);
    jQuery('#ImageCropping_imageY').val(coords.y);
    jQuery('#ImageCropping_imageW').val(coords.w);
    jQuery('#ImageCropping_imageH').val(coords.h);
                
    if (parseInt(coords.w) > 0){
        var rx = 100 / coords.w;
        var ry = 100 / coords.h;
        
        jQuery('#preview').css({
            width: Math.round(rx * imgWidth) + 'px',
            height: Math.round(ry * imgHeight) + 'px',
            marginLeft: '-' + Math.round(rx * coords.x) + 'px',
            marginTop: '-' + Math.round(ry * coords.y) + 'px'
        });
    }
}
</@script>

<#-- SCIPIO: custom form based on component://product/widget/catalog/ImageManagementForms.xml#ImageCropping
     re-enabled the form widget temporarily instead
<@form id="ImageCropping" name="ImageCropping" action=makeOfbizUrl("CropImage")
  method="post" onsubmit="javascript:submitFormDisableSubmits(this);"> 
  <input id="ImageCropping_productId" type="hidden" name="productId" value="${parameters.productId!}" />
  <input id="ImageCropping_imageName" type="hidden" name="imageName" value="${(contentDataResource.drDataResourceName)!}" />
  
  <input id="ImageCropping_imageURL" type="hidden" name="imageURL" value="${imageURL!}" />
  <input id="ImageCropping_imageX" type="hidden" name="imageX" value="" />
  <input id="ImageCropping_imageY" type="hidden" name="imageY" value="" />
  <input id="ImageCropping_imageW" type="hidden" name="imageW" value="" />
  <input id="ImageCropping_imageH" type="hidden" name="imageH" value="" />
  
  <@container class="crop-fields">

    <@field type="generic" name="imageCropp" label=uiLabelMap.CommonImage>
      <@container class="cropbox">
        <img src="<@ofbizContentUrl><#if imageURL?has_content>${imageURL!}<#else>/images/defaultImage.jpg</#if></@ofbizContentUrl>" 
          alt="${uiLabelMap.CommonImage}" title="${uiLabelMap.CommonImage}"<#if !imageURL?has_content> class="cssImgXLarge"</#if> />
      </@container>
    </@field>

  </@container>
</@form>
-->
