<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://product/webapp/catalog/common/common.ftl">
<#if productCategory?has_content>
  <#assign productCategoryAdditionalImage1 = (Static["org.ofbiz.product.category.CategoryContentWrapper"].getProductCategoryContentAsText(productCategory, "ADDITIONAL_IMAGE_1", locale, dispatcher, "url"))! />
  <#assign productCategoryAdditionalImage2 = (Static["org.ofbiz.product.category.CategoryContentWrapper"].getProductCategoryContentAsText(productCategory, "ADDITIONAL_IMAGE_2", locale, dispatcher, "url"))! />
  <#assign productCategoryAdditionalImage3 = (Static["org.ofbiz.product.category.CategoryContentWrapper"].getProductCategoryContentAsText(productCategory, "ADDITIONAL_IMAGE_3", locale, dispatcher, "url"))! />
  <#assign productCategoryAdditionalImage4 = (Static["org.ofbiz.product.category.CategoryContentWrapper"].getProductCategoryContentAsText(productCategory, "ADDITIONAL_IMAGE_4", locale, dispatcher, "url"))! />

  <#-- SCIPIO -->
  <#assign productContentAI1 = delegator.from("ProductCategoryContentAndInfo").where("productCategoryId", productCategory.productCategoryId, "prodCatContentTypeId", "ADDITIONAL_IMAGE_1").orderBy("-fromDate").filterByDate().queryFirst()!>
  <#assign productContentAI2 = delegator.from("ProductCategoryContentAndInfo").where("productCategoryId", productCategory.productCategoryId, "prodCatContentTypeId", "ADDITIONAL_IMAGE_2").orderBy("-fromDate").filterByDate().queryFirst()!>
  <#assign productContentAI3 = delegator.from("ProductCategoryContentAndInfo").where("productCategoryId", productCategory.productCategoryId, "prodCatContentTypeId", "ADDITIONAL_IMAGE_3").orderBy("-fromDate").filterByDate().queryFirst()!>
  <#assign productContentAI4 = delegator.from("ProductCategoryContentAndInfo").where("productCategoryId", productCategory.productCategoryId, "prodCatContentTypeId", "ADDITIONAL_IMAGE_4").orderBy("-fromDate").filterByDate().queryFirst()!>
</#if>
<@row>
  <@cell>
<#-- FIXME?: should specify a form/fields type here that implies manual row/cell markup 
     instead of norows=true nocells=true-->
<form id="addAdditionalImagesForm" method="post" action="<@pageUrl>addAdditionalImagesForProduct</@pageUrl>" enctype="multipart/form-data">
  <@fields type="default-manual">
  <input id="additionalImageproductCategoryId" type="hidden" name="productCategoryId" value="${productCategoryId!}" />
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
        <#if productCategoryAdditionalImage1?has_content><a href="javascript:void(0);" swapDetail="<@contentUrl>${productCategoryAdditionalImage1}</@contentUrl>" class="${styles.link_type_image!} ${styles.action_run_local!} ${styles.action_view!}"><img src="<@contentUrl>${productCategoryAdditionalImage1}</@contentUrl>" class="cssImgSmall" alt="" /></a></#if>
      </#assign>
      <@fields type="default">
        <@cataloglib.imageProfileSelect fieldName="additionalImageOne_imageProfile" profileName=(parameters.additionalImageOne_imageProfile!(productContentAI1.mediaProfile)!"") defaultProfileName="IMAGE_PRODUCT-ADDITIONAL_IMAGE_1" parentProfile="IMAGE_CATEGORY"/>
      </@fields>
      <@imageField id="additionalImageOne" name="additionalImageOne" imageHtml=imageHtml />
      
      <#assign imageHtml>
        <#if productCategoryAdditionalImage2?has_content><a href="javascript:void(0);" swapDetail="<@contentUrl>${productCategoryAdditionalImage2}</@contentUrl>" class="${styles.link_type_image!} ${styles.action_run_local!} ${styles.action_view!}"><img src="<@contentUrl>${productCategoryAdditionalImage2}</@contentUrl>" class="cssImgSmall" alt="" /></a></#if>
      </#assign>
      <@fields type="default">
        <@cataloglib.imageProfileSelect fieldName="additionalImageTwo_imageProfile" profileName=(parameters.additionalImageTwo_imageProfile!(productContentAI2.mediaProfile)!"") defaultProfileName="IMAGE_PRODUCT-ADDITIONAL_IMAGE_2" parentProfile="IMAGE_CATEGORY"/>
      </@fields>
      <@imageField name="additionalImageTwo" imageHtml=imageHtml />
  
      <#assign imageHtml>
        <#if productCategoryAdditionalImage3?has_content><a href="javascript:void(0);" swapDetail="<@contentUrl>${productCategoryAdditionalImage3}</@contentUrl>" class="${styles.link_type_image!} ${styles.action_run_local!} ${styles.action_view!}"><img src="<@contentUrl>${productCategoryAdditionalImage3}</@contentUrl>" class="cssImgSmall" alt="" /></a></#if>
      </#assign>
      <@fields type="default">
        <@cataloglib.imageProfileSelect fieldName="additionalImageThree_imageProfile" profileName=(parameters.additionalImageThree_imageProfile!(productContentAI3.mediaProfile)!"") defaultProfileName="IMAGE_PRODUCT-ADDITIONAL_IMAGE_3" parentProfile="IMAGE_CATEGORY"/>
      </@fields>
      <@imageField name="additionalImageThree" imageHtml=imageHtml />
      
      <#assign imageHtml>
        <#if productCategoryAdditionalImage4?has_content><a href="javascript:void(0);" swapDetail="<@contentUrl>${productCategoryAdditionalImage4}</@contentUrl>" class="${styles.link_type_image!} ${styles.action_run_local!} ${styles.action_view!}"><img src="<@contentUrl>${productCategoryAdditionalImage4}</@contentUrl>" class="cssImgSmall" alt="" /></a></#if>
      </#assign>
      <@fields type="default">
        <@cataloglib.imageProfileSelect fieldName="additionalImageFour_imageProfile" profileName=(parameters.additionalImageFour_imageProfile!(productContentAI4.mediaProfile)!"") defaultProfileName="IMAGE_PRODUCT-ADDITIONAL_IMAGE_4" parentProfile="IMAGE_CATEGORY"/>
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
