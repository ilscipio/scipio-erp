<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<#include "ordercommon.ftl">

<#-- SCIPIO: Migrated from orderhistory.ftl -->
<#--<@section>-->
  <#if downloadOrderRoleAndProductContentInfoList?has_content>
    <@table type="data-list" id="availableTitleDownload" summary="This table display available title for download.">
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
                <a href="<@ofbizUrl>downloadDigitalProduct?dataResourceId=${downloadOrderRoleAndProductContentInfo.dataResourceId}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_export!}">Download</a>
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


