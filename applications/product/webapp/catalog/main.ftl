<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
