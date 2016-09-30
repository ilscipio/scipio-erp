<#-- TODO: License -->

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

<#if !productCategory?has_content>
    <#if productCategoryId?has_content>        
        <#assign formAction><@ofbizUrl>createProductCategory</@ofbizUrl></#assign>
    <#else>
        <#assign formAction><@ofbizUrl>createProductCategory</@ofbizUrl></#assign>
    </#if>
<#else>
    <#assign formAction><@ofbizUrl>updateProductCategory</@ofbizUrl></#assign>
</#if>

<@section id="EditProductCategory">
    <form action="${formAction}" method="post" name="productCategoryForm">
    
      <#if !productCategory?has_content>
        <input type="hidden" name="isCreate" value="true" />
      </#if>

      <#if productCategory?has_content>
        <input type="hidden" name="productCategoryId" value="${productCategoryId}"/>
      </#if>
      <@fields type="default">
        <#--
         <@row>
            <@cell columns=12>
              <#if !productCategory?has_content>
                <#if productCategoryId?has_content>
                  <@field type="input" label=uiLabelMap.CommonId name="productCategoryId" size="20" maxlength="40" value=productCategoryId/>
                <#else>
                  <@field type="input" label=uiLabelMap.CommonId name="productCategoryId" size="20" maxlength="40" value=""/>
                </#if>
              <#else>
                <@field type="display" label=uiLabelMap.CommonId>
                  <b>${productCategoryId}</b>
                </@field>
              </#if>
            </@cell>
        </@row>-->
        <@row>       
            <@cell columns=12>
                <@field type="select" label=uiLabelMap.CommonType name="productCategoryTypeId" size="1">
                    <#assign selectedKey = "">
                    <#list productCategoryTypes as productCategoryTypeData>
                        <#if requestParameters.productCategoryTypeId?has_content>
                            <#assign selectedKey = requestParameters.productCategoryTypeId>
                        <#elseif (productCategory?has_content && (productCategory.productCategoryTypeId!) == productCategoryTypeData.productCategoryTypeId)>
                            <#assign selectedKey = productCategory.productCategoryTypeId>
                        </#if>
                        <option<#if selectedKey == (productCategoryTypeData.productCategoryTypeId!)> selected="selected"</#if> value="${productCategoryTypeData.productCategoryTypeId}">${productCategoryTypeData.get("description",locale)}</option>
                    </#list>
                </@field>
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
              <@field type="input" label=uiLabelMap.CommonName value=((productCategory.categoryName)!) name="categoryName" size="60" maxlength="60"/>
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
              <@field type="lookup" label=uiLabelMap.CommonParent value=(productCategory.primaryParentCategoryId)?default('') formName="productCategoryForm" name="primaryParentCategoryId" id="primaryParentCategoryId" fieldFormName="LookupProductCategory"/>
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
              <#assign fieldValue = "">
              <#if productCategory?has_content>
                <#assign fieldValue = productCategory.detailScreen!>
              </#if>
              <#-- Scipio: Now points to shop -->
              <@field type="input" label=uiLabelMap.ProductDetailScreen name="detailScreen" size="60" maxlength="250" value=fieldValue 
                tooltip="${uiLabelMap.ProductDefaultsTo} &quot;categorydetail&quot;, ${uiLabelMap.ProductDetailScreenMessage}: &quot;component://shop/widget/CatalogScreens.xml#categorydetail&quot;"/>
            </@cell>
        </@row>
        <@row>
            <@cell columns=12>
              <@field type="textarea" label=uiLabelMap.CommonDescription name="description" cols="60" rows="2"><#if productCategory?has_content>${(productCategory.description)!}</#if></@field>
            </@cell>
        </@row>

    <div id="ImageFields">
        <@row>
            <@cell columns=12>
              <@field type="generic" label=uiLabelMap.ProductCategoryImageUrl>
                <@field type="input" name="categoryImageUrl" value=((productCategory.categoryImageUrl)!) size="60" maxlength="255"/>               
              </@field>
            </@cell>
        </@row>
        <#if (productCategory.categoryImageUrl)??>
            <@row>
                <@cell columns=2>
                </@cell>
                <@cell columns=10>
                        <#assign imgUrl><@ofbizContentUrl>${(productCategory.categoryImageUrl)!}</@ofbizContentUrl></#assign>
                        <@img src=imgUrl target="_blank" width="400px"/>
                </@cell>
            </@row>
        </#if>

        <#-- SCIPIO: deprecated -->
        <#if (productCategory.linkOneImageUrl)??>
            <@row>
                <@cell columns=12>
                  <@field type="generic" label=uiLabelMap.ProductLinkOneImageUrl>
                    <@field type="input" name="linkOneImageUrl" value=((productCategory.linkOneImageUrl)!) size="60" maxlength="255"/>
                  </@field>
                </@cell>
            </@row>

            <#if (productCategory.linkOneImageUrl)??>
                <@row>
                    <@cell columns=2>
                    </@cell>
                    <@cell columns=10>
                            <#assign imgUrl><@ofbizContentUrl>${(productCategory.linkOneImageUrl)!}</@ofbizContentUrl></#assign>
                            <@img src=imgUrl target="_blank" width="400px"/>
                    </@cell>
                </@row>
            </#if>
        </#if>
        <#-- SCIPIO: deprecated -->
        <#if (productCategory.linkTwoImageUrl)??>
            <@row>
                <@cell columns=12>
                  <@field type="generic" label=uiLabelMap.ProductLinkTwoImageUrl>
                    <@field type="input" name="linkTwoImageUrl" value=((productCategory.linkTwoImageUrl)!) size="60" maxlength="255"/>                
                  </@field>
                </@cell>
            </@row>

            <#if (productCategory.linkTwoImageUrl)??>
                <@row>
                    <@cell columns=2>
                    </@cell>
                    <@cell columns=10>
                            <#assign imgUrl><@ofbizContentUrl>${(productCategory.linkTwoImageUrl)!}</@ofbizContentUrl></#assign>
                            <@img src=imgUrl target="_blank" width="400px"/>
                    </@cell>
                </@row>
            </#if>
        </#if>
    </div>
        <@row>
            <@cell>
                <#if productCategory?has_content>
                    <@field type="submit" name="Update" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}"/>
                <#else>
                    <@field type="submit" name="Create" text=uiLabelMap.CommonCreate class="+${styles.link_run_sys!} ${styles.action_create!}"/>
                </#if>
            </@cell>
        </@row>
      </@fields>
    </form>
</@section>