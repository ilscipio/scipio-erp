<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
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
