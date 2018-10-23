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
<#if product?has_content>
  <#assign productAdditionalImage1 = (Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(product, "ADDITIONAL_IMAGE_1", locale, dispatcher, "url"))! />
  <#assign productAdditionalImage2 = (Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(product, "ADDITIONAL_IMAGE_2", locale, dispatcher, "url"))! />
  <#assign productAdditionalImage3 = (Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(product, "ADDITIONAL_IMAGE_3", locale, dispatcher, "url"))! />
  <#assign productAdditionalImage4 = (Static["org.ofbiz.product.product.ProductContentWrapper"].getProductContentAsText(product, "ADDITIONAL_IMAGE_4", locale, dispatcher, "url"))! />
</#if>
<@row>
  <@cell>
<#-- FIXME?: should specify a form/fields type here that implies manual row/cell markup 
     instead of norows=true nocells=true-->
<form id="addAdditionalImagesForm" method="post" action="<@ofbizUrl>addAdditionalImagesForProduct</@ofbizUrl>" enctype="multipart/form-data">
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
        <#if productAdditionalImage1?has_content><a href="javascript:void(0);" swapDetail="<@ofbizContentUrl>${productAdditionalImage1}</@ofbizContentUrl>" class="${styles.link_type_image!} ${styles.action_run_local!} ${styles.action_view!}"><img src="<@ofbizContentUrl>${productAdditionalImage1}</@ofbizContentUrl>" class="cssImgSmall" alt="" /></a></#if>
      </#assign>
      <@imageField id="additionalImageOne" name="additionalImageOne" imageHtml=imageHtml />
      
      <#assign imageHtml>
        <#if productAdditionalImage2?has_content><a href="javascript:void(0);" swapDetail="<@ofbizContentUrl>${productAdditionalImage2}</@ofbizContentUrl>" class="${styles.link_type_image!} ${styles.action_run_local!} ${styles.action_view!}"><img src="<@ofbizContentUrl>${productAdditionalImage2}</@ofbizContentUrl>" class="cssImgSmall" alt="" /></a></#if>
      </#assign>
      <@imageField name="additionalImageTwo" imageHtml=imageHtml />
  
      <#assign imageHtml>
        <#if productAdditionalImage3?has_content><a href="javascript:void(0);" swapDetail="<@ofbizContentUrl>${productAdditionalImage3}</@ofbizContentUrl>" class="${styles.link_type_image!} ${styles.action_run_local!} ${styles.action_view!}"><img src="<@ofbizContentUrl>${productAdditionalImage3}</@ofbizContentUrl>" class="cssImgSmall" alt="" /></a></#if>
      </#assign>
      <@imageField name="additionalImageThree" imageHtml=imageHtml />
      
      <#assign imageHtml>
        <#if productAdditionalImage4?has_content><a href="javascript:void(0);" swapDetail="<@ofbizContentUrl>${productAdditionalImage4}</@ofbizContentUrl>" class="${styles.link_type_image!} ${styles.action_run_local!} ${styles.action_view!}"><img src="<@ofbizContentUrl>${productAdditionalImage4}</@ofbizContentUrl>" class="cssImgSmall" alt="" /></a></#if>
      </#assign>
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
