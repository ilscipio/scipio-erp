<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<@section title=uiLabelMap.OrderOrderList>
      <#if orders?has_content>
        <@table type="data-list"> <#-- orig: class="basic-table hover-bar" -->
          <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.OrderOrderId}</@th>
                <@th>${uiLabelMap.FormFieldTitle_orderPickSheetPrintedDate}</@th>
                <@th>${uiLabelMap.ProductVerified}</@th>
            </@tr>
           </@thead>
           <@tbody>
                <#list orders?sort_by("pickSheetPrintedDate") as order>
                    <@tr>
                        <@td><a href="<@ofbizInterWebappUrl>/ordermgr/control/orderview?orderId=${order.orderId!}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}" target="_blank">${order.orderId!}</a></@td>
                        <@td>${order.pickSheetPrintedDate!}</@td>
                        <@td><#if "Y" == order.isVerified>${uiLabelMap.CommonY}</#if></@td>
                    </@tr>
                </#list>
           </@tbody>
        </@table>
      <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.OrderNoOrderFound}.</@commonMsg>
      </#if>
</@section>
