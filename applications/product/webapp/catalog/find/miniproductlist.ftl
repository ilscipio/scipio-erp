<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if productCategory??>
  <#if productCategoryMembers?has_content>
      <#list productCategoryMembers as productCategoryMember>
        <#assign product = productCategoryMember.getRelatedOne("Product", true)>
          <div>
            <a href="<@pageUrl>ViewProductproductId=${product.productId}</@pageUrl>" class="${styles.link_nav_info_name!}">
              <#if product.internalName?has_content>
                ${product.internalName}
              <#else>
                ${product.productName!("${uiLabelMap.CommonNo} ${uiLabelMap.ProductInternalName} / ${uiLabelMap.ProductProductName}")}
              </#if>    
            </a>
            <div>
              <b>${product.productId}</b>
            </div>
          </div>
      </#list>
      <#if (listSize > viewSize)>
          <div>
            <div>NOTE: Only showing the first ${viewSize} of ${listSize} products. To view the rest, use the Products tab for this category.</div>
          </div>
      </#if>
  <#else>
    <div>${uiLabelMap.ProductNoProductsInCategory}.</div>
  </#if>
<#else>
    <div>${uiLabelMap.ProductNoCategorySpecified}.</div>
</#if>
