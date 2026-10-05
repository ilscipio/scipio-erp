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

  <#if savedCartItems?has_content>
    <#macro menuContent menuArgs={}>
      <@menu args=menuArgs>
      <#if security.hasEntityPermission("PARTYMGR", "_UPDATE", request)>
        <#if savedCartListId?has_content>
          <#assign listParam = "&amp;shoppingListId=" + savedCartListId>
        <#else>
          <#assign listParam = "">
        </#if>
        <@menuitem type="link" href=makePageUrl("editShoppingList?partyId=${partyId}${listParam}") text=uiLabelMap.CommonEdit class="+${styles.action_nav!} ${styles.action_update!}" />
      </#if>
      </@menu>
    </#macro>
    <@section id="partyShoppingCart" title=uiLabelMap.PartyCurrentShoppingCart>
        <#if savedCartItems?has_content>
          <@table type="data-list">
           <@thead>
            <@tr class="header-row">
              <@th>${uiLabelMap.PartySequenceId}</@th>
              <@th>${uiLabelMap.PartyProductId}</@th>
              <@th>${uiLabelMap.PartyQuantity}</@th>
              <@th>${uiLabelMap.PartyQuantityPurchased}</@th>
            </@tr>
            </@thead>
            <@tbody>
            <#list savedCartItems as savedCartItem>
              <@tr>
                <@td>${savedCartItem.shoppingListItemSeqId!}</@td>
                <@td class="button-col"><a href="<@serverUrl>/catalog/control/ViewProduct?productId=${savedCartItem.productId}<#if requestAttributes.externalLoginKey?has_content>&amp;externalLoginKey=${requestAttributes.externalLoginKey}</#if></@serverUrl>" class="${styles.link_nav_info_id!}">${savedCartItem.productId!}</a></@td>
                <@td>${savedCartItem.quantity!}</@td>
                <@td>${savedCartItem.quantityPurchased!}</@td>
              </@tr>
            </#list>
            </@tbody>
          </@table>
        <#else>
          <@commonMsg type="result-norecord">${uiLabelMap.PartyNoShoppingCartSavedForParty}</@commonMsg>
        </#if>
    </@section>
  </#if>
