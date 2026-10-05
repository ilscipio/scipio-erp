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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
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
