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

<@section id="partyProductStores" title=uiLabelMap.ProductStores>
    <#if productStoreRoles?has_content>
      <@table type="data-list"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
        <@thead>
        <@tr class="header-row">
          <@th>${uiLabelMap.ProductStoreNameId}</@th>
          <@th>${uiLabelMap.PartyRoleType}</@th>
        </@tr>
        </@thead>
        <@tbody>
        <#list productStoreRoles as productStoreRole>
          <#assign productStore = delegator.findOne("ProductStore", {"productStoreId" : productStoreRole.productStoreId}, true) />
          <#assign roleType = delegator.findOne("RoleType", {"roleTypeId" : productStoreRole.roleTypeId}, true) />
          <@tr>
            <@td class="button-col">
              <a href="<@ofbizInterWebappUrl>/catalog/control/FindProductStoreRoles?partyId=${productStoreRole.partyId}&amp;productStoreId=${productStore.productStoreId}</@ofbizInterWebappUrl>">${productStore.storeName!(uiLabelMap.ProductNoDescription)} (${productStore.productStoreId})</a>
            </@td>
            <@td>${roleType.description!}</@td>
          </@tr>
        </#list>
        </@tbody>
      </@table>
    <#else>
      <@commonMsg type="result-norecord">${uiLabelMap.PartyNoProductStoreFoundForThisParty}</@commonMsg>
    </#if>
</@section>