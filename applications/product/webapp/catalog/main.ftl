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

<#if !sessionAttributes.userLogin??>
      <@panel>${uiLabelMap.ProductGeneralMessage}</@panel>
</#if>

<#if security.hasEntityPermission("CATALOG", "_VIEW", session)>
    <@grid columns=2>
      <@fields type="default-manual">
        <li>
          <form method="post" action="<@ofbizUrl>EditProdCatalog</@ofbizUrl>" name="EditProdCatalogForm">
            <@pul title=uiLabelMap.ProductEditCatalogWithCatalogId> 
                <@pli><@field type="input" size="20" maxlength="20" name="prodCatalogId" value="" postfix=true/></@pli>
                <@pli><a href="<@ofbizUrl>EditProdCatalog</@ofbizUrl>" class="${styles.tiny!} ${styles.action_nav!} ${styles.action_add!}">${uiLabelMap.ProductCreateNewCatalog}</a></@pli>
            </@pul>
          </form>
        </li>
        <li>
          <form method="post" action="<@ofbizUrl>EditCategory</@ofbizUrl>" name="EditCategoryForm">
            <@pul title=uiLabelMap.ProductEditCategoryWithCategoryId>
                <@pli><@field type="lookup" name="productCategoryId" id="productCategoryId" formName="EditCategoryForm" fieldFormName="LookupProductCategory" postfix=true/></@pli>
                <@pli><a href="<@ofbizUrl>EditCategory</@ofbizUrl>" class="${styles.tiny!} ${styles.action_nav!} ${styles.action_add!}">${uiLabelMap.ProductCreateNewCategory}</a></@pli>
            </@pul>
          </form>
        </li>
          
        <li>
          <form method="post" action="<@ofbizUrl>EditProduct</@ofbizUrl>" name="EditProductForm">
            <@pul title=uiLabelMap.ProductEditProductWithProductId>
                <@pli><@field type="lookup" name="productId" id="productId" formName="EditProductForm" fieldFormName="LookupProduct" postfix=true/></@pli>
                <@pli><a href="<@ofbizUrl>ViewProduct</@ofbizUrl>" class="${styles.tiny!} ${styles.action_nav!} ${styles.action_add!}">${uiLabelMap.ProductCreateNewProduct}</a></@pli>
                <@pli><a href="<@ofbizUrl>CreateVirtualWithVariantsForm</@ofbizUrl>" class="${styles.tiny!}">${uiLabelMap.ProductQuickCreateVirtualFromVariants}</a></@pli>
            </@pul>
          </form>
        </li>
        <li>
          <form method="post" action="<@ofbizUrl>FindProductById</@ofbizUrl>">
            <@pul title=uiLabelMap.ProductFindProductWithIdValue>
                <@pli><@field type="input" size="20" maxlength="20" name="idValue" value="" postfix=true/></@pli>
                <@pli><a href="<@ofbizUrl>UpdateAllKeywords</@ofbizUrl>"> ${uiLabelMap.ProductAutoCreateKeywordsForAllProducts}</a></@pli>
                <@pli><a href="<@ofbizUrl>FastLoadCache</@ofbizUrl>"> ${uiLabelMap.ProductFastLoadCatalogIntoCache}</a></@pli>
            </@pul>
          </form>
        </li>
      </@fields>
    </@grid>
</#if>
