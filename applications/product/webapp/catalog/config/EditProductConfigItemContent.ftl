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
<script language="JavaScript" type="text/javascript">
function insertNowTimestamp(field) {
  eval('document.productForm.' + field + '.value="${nowTimestamp?string}";');
}
function insertImageName(size,nameValue) {
  eval('document.productForm.' + size + 'ImageUrl.value=nameValue;');
}
</script>

<#if fileType?has_content>
<@section title="${uiLabelMap.ProductResultOfImageUpload}">
    <#if !(clientFileName?has_content)>
        <div>${uiLabelMap.ProductNoFileSpecifiedForUpload}.</div>
    <#else>
        <div>${uiLabelMap.ProductTheFileOnYourComputer}: <b>${clientFileName!}</b></div>
        <div>${uiLabelMap.ProductServerFileName}: <b>${fileNameToUse!}</b></div>
        <div>${uiLabelMap.ProductServerDirectory}: <b>${imageServerPath!}</b></div>
        <div>${uiLabelMap.ProductTheUrlOfYourUploadedFile}: <b><a href="<@ofbizContentUrl>${imageUrl!}</@ofbizContentUrl>" class="${styles.button_default!}">${imageUrl!}</a></b></div>
    </#if>
</@section>
</#if>

<#if !(configItem??)>
    <@alert type="error">${uiLabelMap.ProductCouldNotFindProductConfigItem} "${configItemId}".</@alert>
<#else>
    <@table type="data-list" autoAltRows=true cellspacing="0" class="basic-table">
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
            <@td><a href="<@ofbizUrl>EditProductConfigItemContentContent?configItemId=${productContent.configItemId}&amp;contentId=${productContent.contentId}&amp;confItemContentTypeId=${productContent.confItemContentTypeId}&amp;fromDate=${productContent.fromDate}</@ofbizUrl>" class="${styles.button_default!}">${entry.content.description?default("[${uiLabelMap.ProductNoDescription}]")} [${entry.content.contentId}]</@td>
            <@td>${productContentType.description?default(productContent.confItemContentTypeId)}</@td>
            <@td>${productContent.fromDate?default("N/A")}</@td>
            <@td>${productContent.thruDate?default("N/A")}</@td>
            <@td><a href="<@ofbizUrl>removeContentFromProductConfigItem?configItemId=${productContent.configItemId}&amp;contentId=${productContent.contentId}&amp;confItemContentTypeId=${productContent.confItemContentTypeId}&amp;fromDate=${productContent.fromDate}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.CommonDelete}</a></@td>
            <@td><a href="/content/control/EditContent?contentId=${productContent.contentId}&amp;externalLoginKey=${requestAttributes.externalLoginKey!}" class="${styles.button_default!}">${uiLabelMap.ProductEditContent} ${entry.content.contentId}</@td>
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
            <form action="<@ofbizUrl>updateProductConfigItemContent</@ofbizUrl>" method="post" style="margin: 0;" name="productForm">
                <input type="hidden" name="configItemId" value="${configItemId!}" />
                <@table type="fields" cellspacing="0" class="basic-table">
                <@tr>
                    <@td width="20%" align="right" valign="top">${uiLabelMap.CommonDescription}</@td>
                    <@td>&nbsp;</@td>
                    <@td width="80%" colspan="4" valign="top">
                        <textarea name="description" cols="60" rows="2">${(configItem.description)!}</textarea>
                    </@td>
                </@tr>
                <@tr>
                    <@td width="20%" align="right" valign="top">${uiLabelMap.ProductLongDescription}</@td>
                    <@td>&nbsp;</@td>
                    <@td width="80%" colspan="4" valign="top">
                        <textarea name="longDescription" cols="60" rows="7">${(configItem.longDescription)!}</textarea>
                    </@td>
                </@tr>
                <@tr>
                    <@td width="20%" align="right" valign="top">
                        ${uiLabelMap.ProductSmallImage}
                        <#if (configItem.imageUrl)??>
                            <a href="<@ofbizContentUrl>${configItem.imageUrl}</@ofbizContentUrl>" target="_blank" class="${styles.button_default!}"><img alt="Image" src="<@ofbizContentUrl>${configItem.imageUrl}</@ofbizContentUrl>" class="cssImgSmall" /></a>
                        </#if>
                    </@td>
                    <@td>&nbsp;</@td>
                    <@td width="80%" colspan="4" valign="top">
                    <input type="text" name="imageUrl" value="${(configItem.imageUrl)?default(imageNameSmall + '.jpg')}" size="60" maxlength="255" />
                    <#if configItemId?has_content>
                        <div>
                        <span>${uiLabelMap.ProductInsertDefaultImageUrl}: </span>
                        <a href="javascript:insertImageName('small','${imageNameSmall}.jpg');" class="${styles.button_default!}">.jpg</a>
                        <a href="javascript:insertImageName('small','${imageNameSmall}.gif');" class="${styles.button_default!}">.gif</a>
                        <a href="javascript:insertImageName('small','');" class="${styles.button_default!}">${uiLabelMap.CommonClear}</a>
                        </div>
                    </#if>
                    </@td>
                </@tr>
                <@tr>
                    <@td colspan="2">&nbsp;</@td>
                    <@td><input type="submit" name="Update" value="${uiLabelMap.CommonUpdate}" /></@td>
                    <@td colspan="3">&nbsp;</@td>
                </@tr>
                </@table>
            </form>
    </@section>
    
    <@section title="${uiLabelMap.ProductUploadImage}">
            <form method="post" enctype="multipart/form-data" action="<@ofbizUrl>UploadProductConfigItemImage?configItemId=${configItemId}&amp;upload_file_type=small</@ofbizUrl>" name="imageUploadForm">
                <input type="file" size="50" name="fname" />
                <input type="submit" class="smallSubmit" value="${uiLabelMap.ProductUploadImage}" />
            </form>
    </@section>
</#if>
