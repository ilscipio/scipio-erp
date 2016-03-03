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
