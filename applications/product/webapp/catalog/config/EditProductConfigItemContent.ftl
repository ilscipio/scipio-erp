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
  <@section title=uiLabelMap.ProductResultOfImageUpload>
    <#if !(clientFileName?has_content)>
        <div>${uiLabelMap.ProductNoFileSpecifiedForUpload}.</div>
    <#else>
        <div>${uiLabelMap.ProductTheFileOnYourComputer}: <b>${clientFileName!}</b></div>
        <div>${uiLabelMap.ProductServerFileName}: <b>${fileNameToUse!}</b></div>
        <div>${uiLabelMap.ProductServerDirectory}: <b>${imageServerPath!}</b></div>
        <div>${uiLabelMap.ProductTheUrlOfYourUploadedFile}: <b><a href="<@ofbizContentUrl>${imageUrl!}</@ofbizContentUrl>" class="${styles.link_nav_info_uri!}">${imageUrl!}</a></b></div>
    </#if>
  </@section>
</#if>

<#if !(configItem??)>
    <@commonMsg type="error">${uiLabelMap.ProductCouldNotFindProductConfigItem} "${configItemId}".</@commonMsg>
<#else>
    <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
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
            <@td><a href="<@ofbizUrl>EditProductConfigItemContentContent?configItemId=${productContent.configItemId}&amp;contentId=${productContent.contentId}&amp;confItemContentTypeId=${productContent.confItemContentTypeId}&amp;fromDate=${productContent.fromDate}</@ofbizUrl>" class="${styles.link_nav_info_desc!}">${entry.content.description?default("[${uiLabelMap.ProductNoDescription}]")} [${entry.content.contentId}]</@td>
            <@td>${productContentType.description!productContent.confItemContentTypeId}</@td>
            <@td>${productContent.fromDate!(uiLabelMap.CommonNA)}</@td>
            <@td>${productContent.thruDate!(uiLabelMap.CommonNA)}</@td>
            <@td>
              <form name="removeContentFromProductConfigItem_${productContent.contentId}_${entry_index}" method="post" action="<@ofbizUrl>removeContentFromProductConfigItem</@ofbizUrl>">
                <input name="configItemId" type="hidden" value="${productContent.configItemId}"/>
                <input name="contentId" type="hidden" value="${productContent.contentId}"/>
                <input name="confItemContentTypeId" type="hidden" value="${productContent.confItemContentTypeId}"/>
                <input name="fromDate" type="hidden" value="${productContent.fromDate}"/>
                <input type="submit" value="${uiLabelMap.CommonDelete}"/>
              </form>
            </@td>
            <@td><a href="<@ofbizInterWebappUrl>/content/control/EditContent?contentId=${productContent.contentId}&amp;externalLoginKey=${requestAttributes.externalLoginKey!}</@ofbizInterWebappUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.ProductEditContent} ${entry.content.contentId}</a></@td>
         </@tr>
         </#list>
       </@tbody>
    </@table>

    <#if configItemId?has_content && configItem?has_content>
        <@section title=uiLabelMap.ProductCreateNewProductConfigItemContent>
            ${prepareAddProductContentWrapper.renderFormString(context)}
        </@section>
        
        <@section title=uiLabelMap.ProductAddContentProductConfigItem>
            ${addProductContentWrapper.renderFormString(context)}
        </@section>
    </#if>
    <@section title=uiLabelMap.ProductOverrideSimpleFields>
            <form action="<@ofbizUrl>updateProductConfigItemContent</@ofbizUrl>" method="post" name="productForm">
                <input type="hidden" name="configItemId" value="${configItemId!}" />
                <@field type="textarea" label=uiLabelMap.CommonDescription name="description" cols="60" rows="2">${(configItem.description)!}</@field>
                <@field type="textarea" label=uiLabelMap.ProductLongDescription name="longDescription" cols="60" rows="7">${(configItem.longDescription)!}</@field>
                <#assign labelDetail>
                    <#if (configItem.imageUrl)??>
                        <a href="<@ofbizContentUrl>${configItem.imageUrl}</@ofbizContentUrl>" target="_blank"><img alt="Image" src="<@ofbizContentUrl>${configItem.imageUrl}</@ofbizContentUrl>" class="cssImgSmall" /></a>
                    </#if>
                </#assign>
                <@field type="generic" label=uiLabelMap.ProductSmallImage labelDetail=labelDetail>
                    <@field type="input" name="imageUrl" value=(configItem.imageUrl)?default(imageNameSmall + '.jpg') size="60" maxlength="255" />
                    <#if configItemId?has_content>
                        <div>
                        <span>${uiLabelMap.ProductInsertDefaultImageUrl}: </span>
                        <a href="javascript:insertImageName('small','${imageNameSmall}.jpg');" class="${styles.link_run_local!} ${styles.action_add!}">.jpg</a>
                        <a href="javascript:insertImageName('small','${imageNameSmall}.gif');" class="${styles.link_run_local!} ${styles.action_add!}">.gif</a>
                        <a href="javascript:insertImageName('small','');" class="${styles.link_run_local!} ${styles.action_clear!}">${uiLabelMap.CommonClear}</a>
                        </div>
                    </#if>
                </@field>
                <@field type="submit" name="Update" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}" />
            </form>
    </@section>
    
    <@section title=uiLabelMap.ProductUploadImage>
            <form method="post" enctype="multipart/form-data" action="<@ofbizUrl>UploadProductConfigItemImage?configItemId=${configItemId}&amp;upload_file_type=small</@ofbizUrl>" name="imageUploadForm">
                <@field type="file" size="50" name="fname" />
                <@field type="submit" class="+${styles.link_run_sys!} ${styles.action_import!}" text=uiLabelMap.ProductUploadImage />
            </form>
    </@section>
</#if>
