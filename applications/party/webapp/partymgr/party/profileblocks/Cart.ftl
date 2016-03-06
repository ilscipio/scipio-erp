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

  <#if savedCartItems?has_content>
    <#macro menuContent menuArgs={}>
      <@menu args=menuArgs>
      <#if security.hasEntityPermission("PARTYMGR", "_UPDATE", session)>
        <#if savedCartListId?has_content>
          <#assign listParam = "&amp;shoppingListId=" + savedCartListId>
        <#else>
          <#assign listParam = "">
        </#if>
        <@menuitem type="link" href=makeOfbizUrl("editShoppingList?partyId=${partyId}${listParam}") text=uiLabelMap.CommonEdit class="+${styles.action_nav!} ${styles.action_update!}" />
      </#if>
      </@menu>
    </#macro>
    <@section id="partyShoppingCart" title=uiLabelMap.PartyCurrentShoppingCart>
        <#if savedCartItems?has_content>
          <@table type="data-list"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
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
                <@td class="button-col"><a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${savedCartItem.productId}&amp;externalLoginKey=${requestAttributes.externalLoginKey}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${savedCartItem.productId!}</a></@td>
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
