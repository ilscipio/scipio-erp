<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section>
   <@table type="fields">
          <#if product.productRating?has_content>
            <@tr>
                <@td class="${styles.grid_large!}2">${uiLabelMap.ProductRating}
                </@td>
                <@td colspan="3">
                    ${product.productRating!}
                    <#if product.ratingTypeEnum?has_content>
                        <#assign ratingEnum = product.getRelatedOne("RatingEnumeration", true)/>
                        ${(ratingEnum.get("description",locale))!product.ratingTypeEnum}
                    </#if>
                </@td>
            </@tr>
        </#if>


    </@table>


</@section>