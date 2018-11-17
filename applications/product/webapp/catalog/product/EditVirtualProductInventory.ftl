<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section title=uiLabelMap.ProductInventorySummary>
        <@table type="data-list" autoAltRows=true>
          <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.ProductProductId}</@th>
                    <#list featureTypeIds as featureTypeId>
                        <#assign featureType = delegator.findOne("ProductFeatureType", {"productFeatureTypeId":featureTypeId}, false)>
                        <@th>${featureType.description}&nbsp;</@th>
                    </#list>
                <@th>${uiLabelMap.ProductQoh}</@th>
                <@th>${uiLabelMap.ProductAtp}</@th>
            </@tr>
          </@thead>
          <@tbody>
            <#list variantInventorySummaries as variantSummary>
              <@tr valign="middle">
                <@td><a href="<@ofbizUrl>EditProductInventoryItems?productId=${variantSummary.productId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${variantSummary.productId}</a></@td>
                    <#list featureTypeIds as featureTypeId>
                        <@td>${(variantSummary[featureTypeId].description)?default(featureTypeId)}</@td>
                    </#list>
                <@td>${variantSummary.quantityOnHandTotal}</@td>
                <@td>${variantSummary.availableToPromiseTotal}</@td>
              </@tr>
            </#list>
          </@tbody>
        </@table>
</@section>
