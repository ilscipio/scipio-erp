<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if security.hasEntityPermission("ORDERMGR", "_CREATE", request) || security.hasEntityPermission("ORDERMGR", "_PURCHASE_CREATE", request)>
  <form method="post" action="<@pageUrl>finalizeOrder</@pageUrl>" name="checkoutsetupform">
    <input type="hidden" name="finalizeMode" value="removeEmptyShipGroups"/>
  </form>

  <@section>
        <#list 1..shoppingCart.getShipGroupSize() as currIndex>
          <#assign shipGroupIndex = currIndex - 1>
          <#assign supplier = delegator.findOne("PartyGroup", {"partyId":shoppingCart.getSupplierPartyId(shipGroupIndex)!}, false)! />
          <#assign sectionTitle>${rawLabel('OrderShipGroup')} ${rawLabel('CommonNbr')} ${currIndex}<#if supplier?has_content> - ${rawLabel('OrderDropShipped')} - ${rawString(supplier.groupName!supplier.partyId)}</#if></#assign>
          <@section title=sectionTitle>
            <@row>
              <@cell>
                <form method="post" action="<@pageUrl>assignItemToShipGroups</@pageUrl>" name="assignitemtoshipgroup${shipGroupIndex}">
                  <input type="hidden" name="_useRowSubmit" value="N" />
                <@table type="data-list">
                  <@thead>
                    <@tr>
                      <@th width="40%">
                        ${uiLabelMap.ProductProduct}
                      </@th>
                      <@th width="20%">
                        ${uiLabelMap.CommonQuantity}
                      </@th>
                      <@th width="20%">
                        ${uiLabelMap.ProductMoveQuantity}
                      </@th>
                      <@th width="20%">
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
