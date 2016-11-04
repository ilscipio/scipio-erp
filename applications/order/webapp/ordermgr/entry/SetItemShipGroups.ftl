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

<#if security.hasEntityPermission("ORDERMGR", "_CREATE", session) || security.hasEntityPermission("ORDERMGR", "_PURCHASE_CREATE", session)>
  <form method="post" action="<@ofbizUrl>finalizeOrder</@ofbizUrl>" name="checkoutsetupform">
    <input type="hidden" name="finalizeMode" value="removeEmptyShipGroups"/>
  </form>

  <@section>
        <#list 1..shoppingCart.getShipGroupSize() as currIndex>
          <#assign shipGroupIndex = currIndex - 1>
          <#assign supplier =  delegator.findOne("PartyGroup", {"partyId":shoppingCart.getSupplierPartyId(shipGroupIndex)}, false)! />
          <#assign sectionTitle>${rawLabel('OrderShipGroup')} ${rawLabel('CommonNbr')} ${currIndex}<#if supplier?has_content> - ${rawLabel('OrderDropShipped')} - ${rawString(supplier.groupName!supplier.partyId)}</#if></#assign>
          <@section title=sectionTitle>
            <@row>
              <@cell>
                <form method="post" action="<@ofbizUrl>assignItemToShipGroups</@ofbizUrl>" name="assignitemtoshipgroup${shipGroupIndex}">
                  <input type="hidden" name="_useRowSubmit" value="N" />
                <@table type="data-list"> <#-- orig: class="basic-table" -->
                  <@thead>
                    <@tr>
                      <@th>
                        ${uiLabelMap.ProductProduct}
                      </@th>
                      <@th>
                        ${uiLabelMap.CommonQuantity}
                      </@th>
                      <@th>
                        ${uiLabelMap.ProductMoveQuantity}
                      </@th>
                      <@th>
                        ${uiLabelMap.OrderShipGroupTo}
                      </@th>
                    </@tr>
                  </@thead>
                  <@tbody>
                  <#assign shipGroupItems = shoppingCart.getShipGroupItems(shipGroupIndex)>
                  <#assign shoppingCartItems = shipGroupItems.keySet().iterator()>
       
                  <#assign rowCount = 0>
                  <#list shoppingCartItems as shoppingCartItem>
                    <#assign cartLineIndex = shoppingCart.getItemIndex(shoppingCartItem)>
                    <#assign shipGroupItemQuantity = shipGroupItems.get(shoppingCartItem)>
                    <@tr>
                      <@td>[${shoppingCartItem.getProductId()}] ${shoppingCartItem.getName()!}: ${shoppingCartItem.getDescription()!}
                           <input type="hidden" name="itemIndex_o_${rowCount}" value="${cartLineIndex}"/>
                           <input type="hidden" name="clearEmptyGroups_o_${rowCount}" value="false"/>
                           <input type="hidden" name="fromGroupIndex_o_${rowCount}" value="${shipGroupIndex}"/>
                      </@td>
                      <@td>${shipGroupItemQuantity}</@td>
                      <@td><input type="text" name="quantity_o_${rowCount}" value="${shipGroupItemQuantity}"/></@td>
                      <@td>
                        <select name="toGroupIndex_o_${rowCount}">
                          <option value="${shipGroupIndex}">---</option>
                          <#list 0..(shoppingCart.getShipGroupSize() - 1) as groupIdx>
                            <#assign groupNumber = groupIdx + 1>
                            <option value="${groupIdx}">${uiLabelMap.CommonGroup} ${uiLabelMap.CommonNbr} ${groupNumber}</option>
                          </#list>
                        </select>
                      </@td>
                    </@tr>
                    <#assign rowCount = rowCount + 1>
                  </#list>
                  </@tbody>
                  <#if (rowCount > 0)>
                  <@tfoot>
                    <@tr>
                      <@td colspan="3">&nbsp;</@td>
                      <@td>
                        <input type="submit" class="${styles.link_run_session!} ${styles.action_update!}" value="${uiLabelMap.CommonSubmit}"/>
                      </@td>
                    </@tr>
                  </@tfoot>
                  </#if>
                </@table>
                  <input type="hidden" name="_rowCount" value="${rowCount}" />
                </form>
              </@cell>
            </@row>
          </@section>
        </#list>
  </@section>

<#else>
  <@commonMsg type="error">${uiLabelMap.OrderViewPermissionError}</@commonMsg>
</#if>
