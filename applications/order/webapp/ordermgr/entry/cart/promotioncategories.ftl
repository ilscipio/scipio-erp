<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#if productPromoCategoryIncludeList?has_content || productPromoCategoryExcludeList?has_content || productPromoCategoryAlwaysList?has_content>
    <@section title=uiLabelMap.OrderPromotionCategories>
        <#if productPromoCategoryIncludeList?has_content>
          <div>${uiLabelMap.OrderPromotionProductsInCategories}:</div>
          <#list productPromoCategoryIncludeList as productPromoCategory>
            <#assign productCategory = productPromoCategory.getRelatedOne("ProductCategory", true)>
            <div>
                -&nbsp;<a href="<@pageUrl>category/~category_id=${productPromoCategory.productCategoryId}</@pageUrl>" class="${styles.link_nav_info_desc!}">${(productCategory.description)!productPromoCategory.productCategoryId}</a>
                <#if (productPromoCategory.includeSubCategories!) == "Y">(${uiLabelMap.OrderIncludeSubCategories})</#if>
            </div>
          </#list>
        </#if>
        <#if productPromoCategoryExcludeList?has_content>
          <div>${uiLabelMap.OrderExcludeCategories}</div>
          <#list productPromoCategoryExcludeList as productPromoCategory>
            <#assign productCategory = productPromoCategory.getRelatedOne("ProductCategory", true)>
            <div>
                -&nbsp;<a href="<@pageUrl>category/~category_id=${productPromoCategory.productCategoryId}</@pageUrl>" class="${styles.link_nav_info_desc!}">${(productCategory.description)!productPromoCategory.productCategoryId}</a>
                <#if (productPromoCategory.includeSubCategories!) == "Y">(${uiLabelMap.OrderIncludeSubCategories})</#if>
            </div>
          </#list>
        </#if>
        <#if productPromoCategoryAlwaysList?has_content>
          <div>${uiLabelMap.OrderAlwaysList}</div>
          <#list productPromoCategoryAlwaysList as productPromoCategory>
            <#assign productCategory = productPromoCategory.getRelatedOne("ProductCategory", true)>
            <div>
                -&nbsp;<a href="<@pageUrl>category/~category_id=${productPromoCategory.productCategoryId}</@pageUrl>" class="${styles.link_nav_info_desc!}">${(productCategory.description)!productPromoCategory.productCategoryId}</a>
                <#if (productPromoCategory.includeSubCategories!) == "Y">(${uiLabelMap.OrderIncludeSubCategories})</#if>
            </div>
          </#list>
        </#if>
    </@section>
</#if>
