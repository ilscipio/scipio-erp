<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
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


