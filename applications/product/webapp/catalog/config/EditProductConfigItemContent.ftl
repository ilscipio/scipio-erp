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
<@script>
function insertNowTimestamp(field) {
  eval('document.productForm.' + field + '.value="${nowTimestamp?string}";');
}
function insertImageName(size,nameValue) {
  eval('document.productForm.' + size + 'ImageUrl.value=nameValue;');
}
</@script>

<#if fileType?has_content>
  <@section title="${uiLabelMap.ProductResultOfImageUpload}">
    <#if !(clientFileName?has_content)>
        <div>${uiLabelMap.ProductNoFileSpecifiedForUpload}.</div>
    <#else>
        <div>${uiLabelMap.ProductTheFileOnYourComputer}: <b>${clientFileName!}</b></div>
        <div>${uiLabelMap.ProductServerFileName}: <b>${fileNameToUse!}</b></div>
        <div>${uiLabelMap.ProductServerDirectory}: <b>${imageServerPath!}</b></div>
        <div>${uiLabelMap.ProductTheUrlOfYourUploadedFile}: <b><a href="<@ofbizContentUrl>${imageUrl!}</@ofbizContentUrl>" class="${styles.link_nav_uri!}">${imageUrl!}</a></b></div>
    </#if>
  </@section>
</#if>

<#if !(configItem??)>
    <@alert type="error">${uiLabelMap.ProductCouldNotFindProductConfigItem} "${configItemId}".</@alert>
<#else>
    <@table type="data-list" autoAltRows=true cellspacing="0"> <#-- orig: class="basic-table" -->
      <@thead>
        <@tr class="header-row">
            <@th>${uiLabelMap.ProductContent}</@th>
            <@th>${uiLabelMap.ProductType}</@th>
            <@th>${uiLabelMap.CommonFrom}</@th>
            <@th>${uiLabelMap.CommonThru}</@th>
            <@th>&nbsp;</@th>
            <@th>&nbsp;</@th>
        </@tr>
      </@thead>
      <@tbody>
        <#list productContentList as entry>
        <#assign productContent=entry.productContent/>
        <#assign productContentType=productContent.getRelatedOne("ProdConfItemContentType", true)/>
        <@tr valign="middle">
            <@td><a href="<@ofbizUrl>EditProductConfigItemContentContent?configItemId=${productContent.configItemId}&amp;contentId=${productContent.contentId}&amp;confItemContentTypeId=${productContent.confItemContentTypeId}&amp;fromDate=${productContent.fromDate}</@ofbizUrl>" class="${styles.link_nav_record_desc!}">${entry.content.description?default("[${uiLabelMap.ProductNoDescription}]")} [${entry.content.contentId}]</@td>
            <@td>${productContentType.description?default(productContent.confItemContentTypeId)}</@td>
            <@td>${productContent.fromDate?default("N/A")}</@td>
            <@td>${productContent.thruDate?default("N/A")}</@td>
            <@td><a href="<@ofbizUrl>removeContentFromProductConfigItem?configItemId=${productContent.configItemId}&amp;contentId=${productContent.contentId}&amp;confItemContentTypeId=${productContent.confItemContentTypeId}&amp;fromDate=${productContent.fromDate}</@ofbizUrl>" class="${styles.link_action_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a></@td>
            <@td><a href="/content/control/EditContent?contentId=${productContent.contentId}&amp;externalLoginKey=${requestAttributes.externalLoginKey!}" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.ProductEditContent} ${entry.content.contentId}</@td>
         </@tr>
         </#list>
       </@tbody>
    </@table>

    <#if configItemId?has_content && configItem?has_content>
        <@section title="${uiLabelMap.ProductCreateNewProductConfigItemContent}">
            ${prepareAddProductContentWrapper.renderFormString(context)}
        </@section>
        
        <@section title="${uiLabelMap.ProductAddContentProductConfigItem}">
            ${addProductContentWrapper.renderFormString(context)}
        </@section>
    </#if>
    <@section title="${uiLabelMap.ProductOverrideSimpleFields}">
            <form action="<@ofbizUrl>updateProductConfigItemContent</@ofbizUrl>" method="post" name="productForm">
                <input type="hidden" name="configItemId" value="${configItemId!}" />
                <@field type="generic" label="${uiLabelMap.CommonDescription}">
                    <textarea name="description" cols="60" rows="2">${(configItem.description)!}</textarea>
                </@field>
                <@field type="generic" label="${uiLabelMap.ProductLongDescription}">
                    <textarea name="longDescription" cols="60" rows="7">${(configItem.longDescription)!}</textarea>
                </@field>
                <#assign labelDetail>
                    <#if (configItem.imageUrl)??>
                        <a href="<@ofbizContentUrl>${configItem.imageUrl}</@ofbizContentUrl>" target="_blank"><img alt="Image" src="<@ofbizContentUrl>${configItem.imageUrl}</@ofbizContentUrl>" class="cssImgSmall" /></a>
                    </#if>
                </#assign>
                <@field type="generic" label="${uiLabelMap.ProductSmallImage}" labelDetail=labelDetail>
                    <input type="text" name="imageUrl" value="${(configItem.imageUrl)?default(imageNameSmall + '.jpg')}" size="60" maxlength="255" />
                    <#if configItemId?has_content>
                        <div>
                        <span>${uiLabelMap.ProductInsertDefaultImageUrl}: </span>
                        <a href="javascript:insertImageName('small','${imageNameSmall}.jpg');" class="${styles.link_action!}">.jpg</a>
                        <a href="javascript:insertImageName('small','${imageNameSmall}.gif');" class="${styles.link_action!}">.gif</a>
                        <a href="javascript:insertImageName('small','');" class="${styles.link_action!}">${uiLabelMap.CommonClear}</a>
                        </div>
                    </#if>
                </@field>
                <@field type="submitarea">
                    <input type="submit" name="Update" value="${uiLabelMap.CommonUpdate}" />
                </@field>
            </form>
    </@section>
    
    <@section title="${uiLabelMap.ProductUploadImage}">
            <form method="post" enctype="multipart/form-data" action="<@ofbizUrl>UploadProductConfigItemImage?configItemId=${configItemId}&amp;upload_file_type=small</@ofbizUrl>" name="imageUploadForm">
                <@field type="file" size="50" name="fname" />
                <@field type="submitarea">
                    <input type="submit" class="${styles.link_action_sys!} ${styles.action_import!}" value="${uiLabelMap.ProductUploadImage}" />
                </@field>
            </form>
    </@section>
</#if>
