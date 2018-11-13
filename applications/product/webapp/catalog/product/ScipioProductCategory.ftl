<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<@section>
    <@table type="fields">
        <#if product.primaryProductCategoryId?has_content>
            <#assign productCategory = product.getRelatedOne("PrimaryProductCategory", true) />
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductPrimaryCategory}
              </@td>
              <@td colspan="3"><a href="<@ofbizUrl>EditCategory?productCategoryId=${product.primaryProductCategoryId}</@ofbizUrl>">${(productCategory.categoryName?default(product.primaryProductCategoryId))!}</a></@td>
            </@tr>
        </#if>
    </@table>


</@section>