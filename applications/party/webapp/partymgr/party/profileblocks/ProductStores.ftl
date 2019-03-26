<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section id="partyProductStores" title=uiLabelMap.ProductStores>
    <#if productStoreRoles?has_content>
      <@table type="data-list">
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
              <a href="<@serverUrl>/catalog/control/FindProductStoreRoles?partyId=${productStoreRole.partyId}&amp;productStoreId=${productStore.productStoreId}</@serverUrl>">${productStore.storeName!(uiLabelMap.ProductNoDescription)} (${productStore.productStoreId})</a>
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