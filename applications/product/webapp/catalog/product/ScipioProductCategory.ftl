<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section>
    <@table type="fields">
        <#if product.primaryProductCategoryId?has_content>
            <#assign productCategory = product.getRelatedOne("PrimaryProductCategory", true) />
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductPrimaryCategory}
              </@td>
              <@td colspan="3"><a href="<@pageUrl>EditCategory?productCategoryId=${product.primaryProductCategoryId}</@pageUrl>">${(productCategory.categoryName?default(product.primaryProductCategoryId))!}</a></@td>
            </@tr>
        </#if>
    </@table>


</@section>