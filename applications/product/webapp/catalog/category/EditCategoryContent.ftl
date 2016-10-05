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
<@section title=uiLabelMap.ProductOverrideSimpleFields>
        <form action="<@ofbizUrl>updateCategoryContent</@ofbizUrl>" method="post" name="categoryForm">
            <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
                <@field type="select" label=uiLabelMap.ProductProductCategoryType name="productCategoryTypeId" size="1">
                    <option value="">&nbsp;</option>
                    <#list productCategoryTypes as productCategoryTypeData>
                        <option<#if productCategory?has_content><#if productCategory.productCategoryTypeId==productCategoryTypeData.productCategoryTypeId> selected="selected"</#if></#if> value="${productCategoryTypeData.productCategoryTypeId}">${productCategoryTypeData.get("description",locale)}</option>
                    </#list>
                </@field>
                <@field type="input" label=uiLabelMap.ProductName value=((productCategory.categoryName)!) name="categoryName" size="60" maxlength="60"/>
                <@field type="textarea" label=uiLabelMap.ProductCategoryDescription name="description" cols="60" rows="2">${(productCategory.description)!}</@field>
                <@field type="textarea" label=uiLabelMap.ProductLongDescription name="longDescription" cols="60" rows="7">${(productCategory.longDescription)!}</@field>
                <#assign fieldValue><#if productCategory?has_content>${productCategory.detailScreen!}</#if></#assign>
                <#-- Scipio: Now points to shop -->
                <@field type="input" label=uiLabelMap.ProductDetailScreen name="detailScreen" size="60" maxlength="250" value=fieldValue tooltip="${rawString(uiLabelMap.ProductDefaultsTo)} \"categorydetail\", ${rawString(uiLabelMap.ProductDetailScreenMessage)}: \"component://shop/widget/CatalogScreens.xml#categorydetail\"" />
                <@field type="submit" name="Update" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}" />
        </form>
</@section>