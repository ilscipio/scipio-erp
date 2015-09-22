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
function insertImageName(type,nameValue) {
  eval('document.productCategoryForm.' + type + 'ImageUrl.value=nameValue;');
};
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

<#if !productCategory?has_content>
    <#if productCategoryId?has_content>
        <#assign sectionTitle>${uiLabelMap.ProductCouldNotFindProductCategoryWithId} "${productCategoryId}".</#assign>
        <#assign formAction><@ofbizUrl>createProductCategory</@ofbizUrl></#assign>
    <#else>
        <#assign sectionTitle>${uiLabelMap.PageTitleCreateProductCategory}</#assign>
        <#assign formAction><@ofbizUrl>createProductCategory</@ofbizUrl></#assign>
    </#if>
<#else>
    <#assign sectionTitle>${uiLabelMap.PageTitleEditProductCategories}</#assign>
    <#assign formAction><@ofbizUrl>updateProductCategory</@ofbizUrl></#assign>    
</#if>

<@section title=sectionTitle>

        <form action="${formAction}" method="post" name="productCategoryForm">
          <#if productCategory?has_content>
            <input type="hidden" name="productCategoryId" value="${productCategoryId}"/>
          </#if>

              <#if !productCategory?has_content>
                  <#if productCategoryId?has_content>
                    <@field type="generic" label="${uiLabelMap.ProductProductCategoryId}">
                        <input type="text" name="productCategoryId" size="20" maxlength="40" value="${productCategoryId}"/>
                    </@field>
                  <#else>
                    <@field type="generic" label="${uiLabelMap.ProductProductCategoryId}">
                        <input type="text" name="productCategoryId" size="20" maxlength="40" value=""/>
                    </@field>
                  </#if>
              <#else>
                <@field type="generic" label="${uiLabelMap.ProductProductCategoryId}">
                    <b>${productCategoryId}</b> (${uiLabelMap.ProductNotModificationRecreationCategory}.)
                </@field>
              </#if>

                <@field type="generic" label="${uiLabelMap.ProductProductCategoryType}">
                    <select name="productCategoryTypeId" size="1">
                            <#assign selectedKey = "">
                            <#list productCategoryTypes as productCategoryTypeData>
                                <#if requestParameters.productCategoryTypeId?has_content>
                                    <#assign selectedKey = requestParameters.productCategoryTypeId>
                                <#elseif (productCategory?has_content && productCategory.productCategoryTypeId! == productCategoryTypeData.productCategoryTypeId)>
                                    <#assign selectedKey = productCategory.productCategoryTypeId>
                                </#if>
                                <option <#if selectedKey == productCategoryTypeData.productCategoryTypeId!>selected="selected"</#if> value="${productCategoryTypeData.productCategoryTypeId}">${productCategoryTypeData.get("description",locale)}</option>
                            </#list>
                        </select>
                </@field>
                <@field type="generic" label="${uiLabelMap.ProductProductCategoryName}">
                    <input type="text" value="${(productCategory.categoryName)!}" name="categoryName" size="60" maxlength="60"/>
                </@field>
                <@field type="generic" label="${uiLabelMap.ProductProductCategoryDescription}">
                    <textarea name="description" cols="60" rows="2"><#if productCategory?has_content>${(productCategory.description)!}</#if></textarea>
                </@field>
                <#assign labelDetail>
                    <#if (productCategory.categoryImageUrl)??>
                        <a href="<@ofbizContentUrl>${(productCategory.categoryImageUrl)!}</@ofbizContentUrl>" target="_blank"><img alt="Category Image" src="<@ofbizContentUrl>${(productCategory.categoryImageUrl)!}</@ofbizContentUrl>" class="cssImgSmall" /></a>
                    </#if>
                </#assign>
                <@field type="generic" label="${uiLabelMap.ProductCategoryImageUrl}" labelDetail=labelDetail>
                    <input type="text" name="categoryImageUrl" value="${(productCategory.categoryImageUrl)?default('')}" size="60" maxlength="255"/>
                        <#if productCategory?has_content>
                            <div>
                            ${uiLabelMap.ProductInsertDefaultImageUrl}:
                            <a href="javascript:insertImageName('category','${imageNameCategory}.jpg');" class="${styles.button_default!}">.jpg</a>
                            <a href="javascript:insertImageName('category','${imageNameCategory}.gif');" class="${styles.button_default!}">.gif</a>
                            <a href="javascript:insertImageName('category','');" class="${styles.button_default!}">${uiLabelMap.CommonClear}</a>
                            </div>
                        </#if>
                </@field>
                <#assign labelDetail>
                    <#if (productCategory.linkOneImageUrl)??>
                        <a href="<@ofbizContentUrl>${(productCategory.linkOneImageUrl)!}</@ofbizContentUrl>" target="_blank"><img alt="Link One Image" src="<@ofbizContentUrl>${(productCategory.linkOneImageUrl)!}</@ofbizContentUrl>" class="cssImgSmall" /></a>
                    </#if>
                </#assign>
                <@field type="generic" label="${uiLabelMap.ProductLinkOneImageUrl}" labelDetail=labelDetail>
                    <input type="text" name="linkOneImageUrl" value="${(productCategory.linkOneImageUrl)?default('')}" size="60" maxlength="255"/>
                        <#if productCategory?has_content>
                            <div>
                                ${uiLabelMap.ProductInsertDefaultImageUrl}:
                                <a href="javascript:insertImageName('linkOne','${imageNameLinkOne}.jpg');" class="${styles.button_default!}">.jpg</a>
                                <a href="javascript:insertImageName('linkOne','${imageNameLinkOne}.gif');" class="${styles.button_default!}">.gif</a>
                                <a href="javascript:insertImageName('linkOne','');" class="${styles.button_default!}">${uiLabelMap.CommonClear}</a>
                            </div>
                        </#if>
                </@field>
                <#assign labelDetail>
                    <#if (productCategory.linkTwoImageUrl)??>
                        <a href="<@ofbizContentUrl>${(productCategory.linkTwoImageUrl)!}</@ofbizContentUrl>" target="_blank"><img alt="Link One Image" src="<@ofbizContentUrl>${(productCategory.linkTwoImageUrl)!}</@ofbizContentUrl>" class="cssImgSmall" /></a>
                    </#if>
                </#assign>
                <@field type="generic" label="${uiLabelMap.ProductLinkTwoImageUrl}" labelDetail=labelDetail>
                    <input type="text" name="linkTwoImageUrl" value="${(productCategory.linkTwoImageUrl)?default('')}" size="60" maxlength="255"/>
                        <#if productCategory?has_content>
                            <div>
                                ${uiLabelMap.ProductInsertDefaultImageUrl}:
                                <a href="javascript:insertImageName('linkTwo','${imageNameLinkTwo}.jpg');" class="${styles.button_default!}">.jpg</a>
                                <a href="javascript:insertImageName('linkTwo','${imageNameLinkTwo}.gif');" class="${styles.button_default!}">.gif</a>
                                <a href="javascript:insertImageName('linkTwo','');" class="${styles.button_default!}">${uiLabelMap.CommonClear}</a>
                            </div>
                        </#if>
                </@field>
                <@field type="generic" label="${uiLabelMap.ProductDetailScreen}">
                    <input type="text" <#if productCategory?has_content>value="${productCategory.detailScreen!}"</#if> name="detailScreen" size="60" maxlength="250"/>
                        <br /><span class="tooltip">${uiLabelMap.ProductDefaultsTo} &quot;categorydetail&quot;, ${uiLabelMap.ProductDetailScreenMessage}: &quot;component://ecommerce/widget/CatalogScreens.xml#categorydetail&quot;</span>
                </@field>
                <@field type="generic" label="${uiLabelMap.ProductPrimaryParentCategory}">
                    <@htmlTemplate.lookupField value="${(productCategory.primaryParentCategoryId)?default('')}" formName="productCategoryForm" name="primaryParentCategoryId" id="primaryParentCategoryId" fieldFormName="LookupProductCategory"/>
                </@field>
                <@field type="submitarea">
                    <input type="submit" name="Update" value="${uiLabelMap.CommonUpdate}"/>
                </@field>

        </form>
</@section>

<#if productCategoryId?has_content>
    <script language="JavaScript" type="text/javascript">
        function setUploadUrl(newUrl) {
        var toExec = 'document.imageUploadForm.action="' + newUrl + '";';
        eval(toExec);
        };
    </script>
    <@section title="${uiLabelMap.ProductCategoryUploadImage}">
        <@form fieldsLabelArea=false method="post" enctype="multipart/form-data" action=makeOfbizUrl("UploadCategoryImage?productCategoryId=${productCategoryId!}&amp;upload_file_type=category") name="imageUploadForm">
            <@field type="file" name="fname" size="50" />
            <@field type="generic">
                <input type="radio" name="upload_file_type_bogus" value="category" checked="checked" onclick='setUploadUrl("<@ofbizUrl>UploadCategoryImage?productCategoryId=${productCategoryId}&amp;upload_file_type=category</@ofbizUrl>");'/>${uiLabelMap.ProductCategoryImageUrl}
                <input type="radio" name="upload_file_type_bogus" value="linkOne" onclick='setUploadUrl("<@ofbizUrl>UploadCategoryImage?productCategoryId=${productCategoryId}&amp;upload_file_type=linkOne</@ofbizUrl>");'/>${uiLabelMap.ProductLinkOneImageUrl}
                <input type="radio" name="upload_file_type_bogus" value="linkTwo"onclick='setUploadUrl("<@ofbizUrl>UploadCategoryImage?productCategoryId=${productCategoryId}&amp;upload_file_type=linkTwo</@ofbizUrl>");'/>${uiLabelMap.ProductLinkTwoImageUrl}
            </@field>
            <@field type="submitarea">
                <input type="submit" class="smallSubmit ${styles.button_default!}" value="${uiLabelMap.ProductUploadImage}"/>
            </@field>
        </@form>
    </@section>
    <@section title="${uiLabelMap.ProductDuplicateProductCategory}">
            <form action="<@ofbizUrl>DuplicateProductCategory</@ofbizUrl>" method="post">
                <input type="hidden" name="oldProductCategoryId" value="${productCategoryId}"/>
                <@field type="generic" label="${uiLabelMap.ProductDuplicateProductCategorySelected}">
                  <input type="text" size="20" maxlength="20" name="productCategoryId"/>
                </@field>
                         
                <@field type="generic" label="${uiLabelMap.CommonDuplicate}">
                    ${uiLabelMap.ProductCategoryContent}&nbsp;<input type="checkbox" name="duplicateContent" value="Y" checked="checked" />
                    ${uiLabelMap.ProductCategoryRollupParentCategories}&nbsp;<input type="checkbox" name="duplicateParentRollup" value="Y" checked="checked" />
                    ${uiLabelMap.ProductCategoryRollupChildCategories}&nbsp;<input type="checkbox" name="duplicateChildRollup" value="Y" />
                    ${uiLabelMap.ProductProducts}&nbsp;<input type="checkbox" name="duplicateMembers" value="Y" checked="checked" />
                    ${uiLabelMap.ProductCatalogs}&nbsp;<input type="checkbox" name="duplicateCatalogs" value="Y" checked="checked" />
                    ${uiLabelMap.ProductFeatures}&nbsp;<input type="checkbox" name="duplicateFeatures" value="Y" checked="checked" />
                    ${uiLabelMap.PartyParties}&nbsp;<input type="checkbox" name="duplicateRoles" value="Y" checked="checked" />
                    ${uiLabelMap.ProductAttributes}&nbsp;<input type="checkbox" name="duplicateAttributes" value="Y" checked="checked" />
                </@field> 
          
                <@field type="submitarea">
                  <input type="submit" class="smallSubmit ${styles.button_default!}" value="${uiLabelMap.CommonGo}"/>
                </@field>  
            </form>
    </@section>
</#if>
