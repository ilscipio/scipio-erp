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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->

<#if !userLogin??>
      <@panel>${uiLabelMap.ProductGeneralMessage}</@panel>
</#if>

<#if security.hasEntityPermission("CATALOG", "_VIEW", request)>
    <@grid columns=2>
      <@fields type="default-manual">
        <li>
          <form method="post" action="<@pageUrl>EditProdCatalog</@pageUrl>" name="EditProdCatalogForm">
            <@pul title=uiLabelMap.ProductEditCatalogWithCatalogId> 
                <@pli><@field type="input" size="20" maxlength="20" name="prodCatalogId" value="" postfix=true/></@pli>
                <@pli><a href="<@pageUrl>EditProdCatalog</@pageUrl>" class="${styles.tiny!} ${styles.action_nav!} ${styles.action_add!}">${uiLabelMap.ProductCreateNewCatalog}</a></@pli>
            </@pul>
          </form>
        </li>
        <li>
          <form method="post" action="<@pageUrl>EditCategory</@pageUrl>" name="EditCategoryForm">
            <@pul title=uiLabelMap.ProductEditCategoryWithCategoryId>
                <@pli><@field type="lookup" name="productCategoryId" id="productCategoryId" formName="EditCategoryForm" fieldFormName="LookupProductCategory" postfix=true/></@pli>
                <@pli><a href="<@pageUrl>EditCategory</@pageUrl>" class="${styles.tiny!} ${styles.action_nav!} ${styles.action_add!}">${uiLabelMap.ProductCreateNewCategory}</a></@pli>
            </@pul>
          </form>
        </li>
          
        <li>
          <form method="post" action="<@pageUrl>EditProduct</@pageUrl>" name="EditProductForm">
            <@pul title=uiLabelMap.ProductEditProductWithProductId>
                <@pli><@field type="lookup" name="productId" id="productId" formName="EditProductForm" fieldFormName="LookupProduct" postfix=true/></@pli>
                <@pli><a href="<@pageUrl>ViewProduct</@pageUrl>" class="${styles.tiny!} ${styles.action_nav!} ${styles.action_add!}">${uiLabelMap.ProductCreateNewProduct}</a></@pli>
                <@pli><a href="<@pageUrl>CreateVirtualWithVariantsForm</@pageUrl>" class="${styles.tiny!}">${uiLabelMap.ProductQuickCreateVirtualFromVariants}</a></@pli>
            </@pul>
          </form>
        </li>
        <li>
          <form method="post" action="<@pageUrl>FindProductById</@pageUrl>">
            <@pul title=uiLabelMap.ProductFindProductWithIdValue>
                <@pli><@field type="input" size="20" maxlength="20" name="idValue" value="" postfix=true/></@pli>
                <@pli><a href="<@pageUrl>UpdateAllKeywords</@pageUrl>"> ${uiLabelMap.ProductAutoCreateKeywordsForAllProducts}</a></@pli>
                <@pli><a href="<@pageUrl>FastLoadCache</@pageUrl>"> ${uiLabelMap.ProductFastLoadCatalogIntoCache}</a></@pli>
            </@pul>
          </form>
        </li>
      </@fields>
    </@grid>
</#if>
