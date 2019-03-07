<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<#-- SCIPIO: Migrated from orderhistory.ftl -->
<#--<@section>-->
  <#if downloadOrderRoleAndProductContentInfoList?has_content>
    <@table type="data-list" id="availableTitleDownload" summary=uiLabelMap.EcommerceDownloadsAvailableTitle>
      <@thead>
        <@tr>
          <@th>${uiLabelMap.OrderOrder} ${uiLabelMap.CommonNbr}</@th>
          <@th>${uiLabelMap.ProductProductName}</@th>
          <@th>${uiLabelMap.CommonName}</@th>
          <@th>${uiLabelMap.CommonDescription}</@th>
          <@th></@th>
        </@tr>
      </@thead>
      <@tbody>
          <#list downloadOrderRoleAndProductContentInfoList as downloadOrderRoleAndProductContentInfo>
            <@tr>
              <@td>${downloadOrderRoleAndProductContentInfo.orderId}</@td>
              <@td>${downloadOrderRoleAndProductContentInfo.productName}</@td>
              <@td>${downloadOrderRoleAndProductContentInfo.contentName!}</@td>
              <@td>${downloadOrderRoleAndProductContentInfo.description!}</@td>
              <@td>
                <a href="<@pageUrl>downloadDigitalProduct?dataResourceId=${downloadOrderRoleAndProductContentInfo.dataResourceId}</@pageUrl>" class="${styles.link_run_sys!} ${styles.action_export!}">Download</a>
              </@td>
            </@tr>
          </#list>
      </@tbody>
    </@table>
  <#else>
    <@commonMsg type="result-norecord">(${uiLabelMap.CommonNone})</@commonMsg><#--${uiLabelMap.EcommerceDownloadNotFound}-->
  </#if>

  <@commonMsg type="info"><em>${uiLabelMap.CommonNote}: ${uiLabelMap.ShopDownloadsHereOnceOrderCompleted}</em></@commonMsg>
<#--</@section>-->


