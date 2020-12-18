<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://product/webapp/catalog/common/common.ftl">
<#if product?has_content>
  <#assign productAdditionalImage1 = (Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(product, "ADDITIONAL_IMAGE_1", locale, dispatcher, "url"))! />
  <#assign productAdditionalImage2 = (Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(product, "ADDITIONAL_IMAGE_2", locale, dispatcher, "url"))! />
  <#assign productAdditionalImage3 = (Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(product, "ADDITIONAL_IMAGE_3", locale, dispatcher, "url"))! />
  <#assign productAdditionalImage4 = (Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(product, "ADDITIONAL_IMAGE_4", locale, dispatcher, "url"))! />

  <#-- SCIPIO -->
  <#assign productContentAI1 = delegator.from("ProductContentAndDataResource").where("productId", product.productId, "productContentTypeId", "ADDITIONAL_IMAGE_1").orderBy("-fromDate").filterByDate().queryFirst()!>
  <#assign productContentAI2 = delegator.from("ProductContentAndDataResource").where("productId", product.productId, "productContentTypeId", "ADDITIONAL_IMAGE_2").orderBy("-fromDate").filterByDate().queryFirst()!>
  <#assign productContentAI3 = delegator.from("ProductContentAndDataResource").where("productId", product.productId, "productContentTypeId", "ADDITIONAL_IMAGE_3").orderBy("-fromDate").filterByDate().queryFirst()!>
  <#assign productContentAI4 = delegator.from("ProductContentAndDataResource").where("productId", product.productId, "productContentTypeId", "ADDITIONAL_IMAGE_4").orderBy("-fromDate").filterByDate().queryFirst()!>
</#if>
<@row>
  <@cell>
<#-- FIXME?: should specify a form/fields type here that implies manual row/cell markup 
     instead of norows=true nocells=true-->
<form id="addAdditionalImagesForm" method="post" action="<@pageUrl>addAdditionalImagesForProduct</@pageUrl>" enctype="multipart/form-data">
  <@fields type="default-manual">
  <input id="additionalImageProductId" type="hidden" name="productId" value="${productId!}" />
    <#macro imageField name imageHtml id="">
      <#if imageHtml?trim?has_content>
      <@row>
        <@cell>
          ${imageHtml}
        </@cell>
      </@row>
      </#if>
      <@row>
        <@cell>
          <@field type="file" id=id size="20" name=name norows=true nocells=true />
        </@cell>
      </@row>
    </#macro>
     
      <#assign imageHtml>
        <#if productAdditionalImage1?has_content><a href="javascript:void(0);" swapDetail="<@contentUrl>${productAdditionalImage1}</@contentUrl>" class="${styles.link_type_image!} ${styles.action_run_local!} ${styles.action_view!}"><img src="<@contentUrl>${productAdditionalImage1}</@contentUrl>" class="cssImgSmall" alt="" /></a></#if>
      </#assign>
      <@fields type="default">
        <@cataloglib.imageProfileSelect fieldName="additionalImageOne_imageProfile" profileName=(parameters.additionalImageOne_imageProfile!(productContentAI1.mediaProfile)!"") defaultProfileName="IMAGE_PRODUCT-ADDITIONAL_IMAGE_1"/>
      </@fields>
      <@imageField id="additionalImageOne" name="additionalImageOne" imageHtml=imageHtml />
      
      <#assign imageHtml>
        <#if productAdditionalImage2?has_content><a href="javascript:void(0);" swapDetail="<@contentUrl>${productAdditionalImage2}</@contentUrl>" class="${styles.link_type_image!} ${styles.action_run_local!} ${styles.action_view!}"><img src="<@contentUrl>${productAdditionalImage2}</@contentUrl>" class="cssImgSmall" alt="" /></a></#if>
      </#assign>
      <@fields type="default">
        <@cataloglib.imageProfileSelect fieldName="additionalImageTwo_imageProfile" profileName=(parameters.additionalImageTwo_imageProfile!(productContentAI2.mediaProfile)!"") defaultProfileName="IMAGE_PRODUCT-ADDITIONAL_IMAGE_2"/>
      </@fields>
      <@imageField name="additionalImageTwo" imageHtml=imageHtml />
  
      <#assign imageHtml>
        <#if productAdditionalImage3?has_content><a href="javascript:void(0);" swapDetail="<@contentUrl>${productAdditionalImage3}</@contentUrl>" class="${styles.link_type_image!} ${styles.action_run_local!} ${styles.action_view!}"><img src="<@contentUrl>${productAdditionalImage3}</@contentUrl>" class="cssImgSmall" alt="" /></a></#if>
      </#assign>
      <@fields type="default">
        <@cataloglib.imageProfileSelect fieldName="additionalImageThree_imageProfile" profileName=(parameters.additionalImageThree_imageProfile!(productContentAI3.mediaProfile)!"") defaultProfileName="IMAGE_PRODUCT-ADDITIONAL_IMAGE_3"/>
      </@fields>
      <@imageField name="additionalImageThree" imageHtml=imageHtml />
      
      <#assign imageHtml>
        <#if productAdditionalImage4?has_content><a href="javascript:void(0);" swapDetail="<@contentUrl>${productAdditionalImage4}</@contentUrl>" class="${styles.link_type_image!} ${styles.action_run_local!} ${styles.action_view!}"><img src="<@contentUrl>${productAdditionalImage4}</@contentUrl>" class="cssImgSmall" alt="" /></a></#if>
      </#assign>
      <@fields type="default">
        <@cataloglib.imageProfileSelect fieldName="additionalImageFour_imageProfile" profileName=(parameters.additionalImageFour_imageProfile!(productContentAI4.mediaProfile)!"") defaultProfileName="IMAGE_PRODUCT-ADDITIONAL_IMAGE_4"/>
      </@fields>
      <@imageField name="additionalImageFour" imageHtml=imageHtml />
      
      <@field type="submit" text=uiLabelMap.CommonUpload class="+${styles.link_run_sys!} ${styles.action_import!}" />

  <div class="right" style="margin-top:-250px;">
    <a href="javascript:void(0);" class="${styles.link_type_image!} ${styles.action_run_sys!} ${styles.action_view!}"><img id="detailImage" name="mainImage" vspace="5" hspace="5" width="150" height="150" style="margin-left:50px" src="" alt="" /></a>
    <input type="hidden" id="originalImage" name="originalImage" />
  </div>
  </@fields>
</form>
  </@cell>
</@row>
