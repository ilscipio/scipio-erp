<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<!-- SCIPIO: 2.0.0: didn't make sense to show pickSheetPrintedDate when we try to find orders not picked -->
<@section title=uiLabelMap.OrderOrderList>
      <#if orders?has_content>
        <@table type="data-list">
          <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.OrderOrderId}</@th>
<#--                <@th>${uiLabelMap.FormFieldTitle_orderPickSheetPrintedDate}</@th>-->
                <@th>${uiLabelMap.ProductVerified}</@th>
            </@tr>
           </@thead>
           <@tbody>
                <#list orders as order>
                    <@tr>
                        <@td><a href="<@serverUrl>/ordermgr/control/orderview?orderId=${order.orderId!}</@serverUrl>" class="${styles.link_nav_info_id!}" target="_blank">${order.orderId!}</a></@td>
<#--                        <@td>${order.pickSheetPrintedDate!}</@td>-->
                        <@td><#if "Y" == order.isVerified>${uiLabelMap.CommonY}</#if></@td>
                    </@tr>
                </#list>
           </@tbody>
        </@table>
      <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.OrderNoOrderFound}.</@commonMsg>
      </#if>
</@section>
