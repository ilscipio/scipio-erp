<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
        <@menuitem type="link" href=makeOfbizUrl("editShoppingList?partyId=${partyId}${listParam}") text=uiLabelMap.CommonEdit class="+${styles.action_nav!} ${styles.action_update!}" />
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
