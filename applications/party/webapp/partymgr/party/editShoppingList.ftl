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

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href="javascript:document.getElementById('createEmptyShoppingList').submit();" text=uiLabelMap.CommonNew class="+${styles.action_run_sys!} ${styles.action_add!}">
      <form id="createEmptyShoppingList" action="<@ofbizUrl>createEmptyShoppingList</@ofbizUrl>" method="post">
        <input type="hidden" name="partyId" value="${partyId!}" />
      </form>
    </@menuitem>
  </@menu>
</#macro>
<@section menuContent=menuContent><#--  title=uiLabelMap.PartyShoppingLists -->
    <#if shoppingLists?has_content>
      <form name="selectShoppingList" method="post" action="<@ofbizUrl>editShoppingList</@ofbizUrl>">
        <@fields type="default-nolabelarea">
        <input type="hidden" name="partyId" value="${partyId!}" />
        <@field type="select" name="shoppingListId">
              <#if shoppingList?has_content>
                <option value="${shoppingList.shoppingListId}">${shoppingList.listName}</option>
                <option value="${shoppingList.shoppingListId}">--</option>
              </#if>
              <#list allShoppingLists as list>
                <option value="${list.shoppingListId}">${list.listName}</option>
              </#list>
        </@field>
        <@field type="submit" submitType="link" href="javascript:document.selectShoppingList.submit();" class="+${styles.link_nav!} ${styles.action_update!}" text=uiLabelMap.CommonEdit />
        </@fields>
      </form>
    <#else>
      <@commonMsg type="result-norecord">${uiLabelMap.PartyNoShoppingListsParty}.</@commonMsg>
    </#if>
</@section>

<#if shoppingList?has_content>

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
      <@menuitem type="link" href="javascript:document.updateList.submit();" text=uiLabelMap.CommonSave class="+${styles.action_run_sys!} ${styles.action_update!}"/>
      <@menuitem type="link" href="javascript:document.createQuoteFromShoppingListForm.submit()" text=uiLabelMap.PartyCreateNewQuote class="+${styles.action_run_sys!} ${styles.action_add!}">
        <form method="post" name="createQuoteFromShoppingListForm" action="<@ofbizInterWebappUrl>/ordermgr/control/createQuoteFromShoppingList</@ofbizInterWebappUrl>">
          <input type="hidden" name="applyStorePromotions" value="N"/>
          <input type="hidden" name="shoppingListId" value="${shoppingList.shoppingListId!}"/>
        </form>
      </@menuitem>
      <@menuitem type="link" href=makeOfbizInterWebappUrl("/ordermgr/control/createCustRequestFromShoppingList?shoppingListId=${shoppingList.shoppingListId!}") text=uiLabelMap.PartyCreateNewCustRequest class="+${styles.action_run_sys!} ${styles.action_add!}" />
      <@menuitem type="link" href=makeOfbizInterWebappUrl("/ordermgr/control/loadCartFromShoppingList?shoppingListId=${shoppingList.shoppingListId!}") text=uiLabelMap.OrderNewOrder class="+${styles.action_run_session!} ${styles.action_add!}" />
  </@menu>
</#macro>
<@section title="${rawLabel('PartyShoppingListDetail')} - ${rawString(shoppingList.listName)}" menuContent=menuContent>
    <form name="updateList" method="post" action="<@ofbizUrl>updateShoppingList</@ofbizUrl>">
      <input type="hidden" name="shoppingListId" value="${shoppingList.shoppingListId}" />
      <input type="hidden" name="partyId" value="${shoppingList.partyId!}" />
        <#assign isAutoSave = ((shoppingList.listName!"") == "auto-save")>
        <@field type="input" label=uiLabelMap.PartyListName size="25" name="listName" value=shoppingList.listName disabled=isAutoSave />
        <@field type="input" label=uiLabelMap.CommonDescription size="70" name="description" value=(shoppingList.description!) disabled=isAutoSave />
        <@field type="select" label=uiLabelMap.PartyListType name="shoppingListTypeId" disabled=isAutoSave>
              <#if shoppingListType??>
                <option value="${shoppingListType.shoppingListTypeId}">${shoppingListType.get("description",locale)?default(shoppingListType.shoppingListTypeId)}</option>
                <option value="${shoppingListType.shoppingListTypeId}">--</option>
              </#if>
              <#list shoppingListTypes as shoppingListType>
                <option value="${shoppingListType.shoppingListTypeId}">${shoppingListType.get("description",locale)?default(shoppingListType.shoppingListTypeId)}</option>
              </#list>
        </@field>
        <@field type="select" label="${rawLabel('PartyPublic')}?" name="isPublic" disabled=isAutoSave>
              <option>${shoppingList.isPublic}</option>
              <option value="${shoppingList.isPublic}">--</option>
              <option value="Y">${uiLabelMap.CommonYes}</option>
              <option value="N">${uiLabelMap.CommonNo}</option>
        </@field>
        <@field type="generic" label=uiLabelMap.PartyParentList>
            <@field type="select" name="parentShoppingListId" disabled=isAutoSave>
              <#if parentShoppingList??>
                <option value="${parentShoppingList.shoppingListId}">${parentShoppingList.listName!parentShoppingList.shoppingListId}</option>
              </#if>
              <option value="">${uiLabelMap.PartyNoParent}</option>
              <#list allShoppingLists as newParShoppingList>
                <option value="${newParShoppingList.shoppingListId}">${newParShoppingList.listName!newParShoppingList.shoppingListId}</option>
              </#list>
            </@field>
            <#if parentShoppingList??>
              <a href="<@ofbizUrl>editShoppingList?shoppingListId=${parentShoppingList.shoppingListId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonGotoParent} (${parentShoppingList.listName!parentShoppingList.shoppingListId})</a>
            </#if>
        </@field>
        <#if !isAutoSave>
          <@field type="submit" submitType="link" href="javascript:document.updateList.submit();" class="+${styles.link_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave />
        </#if>
    </form>
</@section>

<#if childShoppingListDatas?has_content>
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makeOfbizUrl("addListToCart?shoppingListId=${shoppingList.shoppingListId}&includeChild=yes") text=uiLabelMap.PartyAddChildListsToCart class="+${styles.action_run_session!} ${styles.action_add!}" />
  </@menu>
</#macro>
<@section title="${rawLabel('PartyChildShoppingList')} - ${rawString(shoppingList.listName)}" menuContent=menuContent>
    <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
      <@thead>
        <@tr class="header-row">
          <@th>${uiLabelMap.PartyListName}</@th>
          <@th>&nbsp;</@th>
        </@tr>
      </@thead>
      <@tbody>
      <#list childShoppingListDatas as childShoppingListData>
        <#assign childShoppingList = childShoppingListData.childShoppingList>
        <@tr>
          <@td class="button-col"><a href="<@ofbizUrl>editShoppingList?shoppingListId=${childShoppingList.shoppingListId}</@ofbizUrl>">${childShoppingList.listName!childShoppingList.shoppingListId}</a></li></@td>
          <@td class="button-col align-float">
            <a href="<@ofbizUrl>editShoppingList?shoppingListId=${childShoppingList.shoppingListId}</@ofbizUrl>">${uiLabelMap.PartyGotoList}</a>
            <a href="<@ofbizUrl>addListToCart?shoppingListId=${childShoppingList.shoppingListId}</@ofbizUrl>">${uiLabelMap.PartyAddListToCart}</a>
          </@td>
        </@tr>
      </#list>
      </@tbody>
    </@table>
</@section>
</#if>

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
<#-- <@menuitem type="link" href=makeOfbizUrl("addListToCart?shoppingListId=${shoppingList.shoppingListId}") text=uiLabelMap.PartyAddListToCart class="+${styles.action_run_session!} ${styles.action_add!}" /> -->
  </@menu>
</#macro>
<@section title="${rawLabel('PartyListItems')} - ${rawString(shoppingList.listName)}" menuContent=menuContent>
  <#if shoppingListItemDatas?has_content>

    <#assign paramStr = "partyId=" + partyId + "&shoppingListId=" + (shoppingListId!)/>
    <@paginate mode="content" url=makeOfbizUrl("editShoppingList") paramStr=paramStr viewIndex=viewIndex!0 listSize=listSize!0 viewSize=viewSize!1>
      <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
       <@thead>
        <@tr class="header-row">
          <@th>${uiLabelMap.PartyProduct}</@th>
          <@th>${uiLabelMap.PartyQuantity}</@th>
          <@th>${uiLabelMap.PartyQuantityPurchased}</@th>
          <@th>${uiLabelMap.PartyPrice}</@th>
          <@th>${uiLabelMap.PartyTotal}</@th>
          <@th>&nbsp;</@th>
        </@tr>
        </@thead>
        <@tbody>
        <#list shoppingListItemDatas[lowIndex-1..highIndex-1] as shoppingListItemData>
          <#assign shoppingListItem = shoppingListItemData.shoppingListItem>
          <#assign product = shoppingListItemData.product>
          <#assign productContentWrapper = Static["org.ofbiz.product.product.ProductContentWrapper"].makeProductContentWrapper(product, request)>
          <#assign unitPrice = shoppingListItemData.unitPrice>
          <#assign totalPrice = shoppingListItemData.totalPrice>
          <#assign productVariantAssocs = shoppingListItemData.productVariantAssocs!>
          <#assign isVirtual = product.isVirtual?? && product.isVirtual.equals("Y")>
          <@tr valign="middle">
            <@td><a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${shoppingListItem.productId}&amp;externalLoginKey=${requestAttributes.externalLoginKey}</@ofbizInterWebappUrl>">${shoppingListItem.productId} -
              ${productContentWrapper.get("PRODUCT_NAME")!"No Name"}</a> : ${productContentWrapper.get("DESCRIPTION")!}
            </@td>
            <form method="post" action="<@ofbizUrl>removeFromShoppingList</@ofbizUrl>" name="removeform_${shoppingListItem.shoppingListItemSeqId}">
              <input type="hidden" name="shoppingListId" value="${shoppingListItem.shoppingListId}" />
              <input type="hidden" name="shoppingListItemSeqId" value="${shoppingListItem.shoppingListItemSeqId}" />
            </form>
            <form method="post" action="<@ofbizUrl>updateShoppingListItem</@ofbizUrl>" name="listform_${shoppingListItem.shoppingListItemSeqId}">
              <input type="hidden" name="shoppingListId" value="${shoppingListItem.shoppingListId}" />
              <input type="hidden" name="shoppingListItemSeqId" value="${shoppingListItem.shoppingListItemSeqId}" />
              <@td>
                <input size="6" type="text" name="quantity" value="${shoppingListItem.quantity?string.number}" />
              </@td>
              <@td>
                <input size="6" type="text" name="quantityPurchased"
                  <#if shoppingListItem.quantityPurchased?has_content>
                    value="${shoppingListItem.quantityPurchased!?string.number}"
                  </#if> />
              </@td>
            </form>
            <@td class="align-float"><@ofbizCurrency amount=unitPrice isoCode=currencyUomId/></@td>
            <@td class="align-float"><@ofbizCurrency amount=totalPrice isoCode=currencyUomId/></@td>
            <@td class="button-col align-float">
              <a href="javascript:document.listform_${shoppingListItem.shoppingListItemSeqId}.submit();" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
              <a href="javascript:document.removeform_${shoppingListItem.shoppingListItemSeqId}.submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a>
            </@td>
          </@tr>
        </#list>
        </@tbody>
      </@table>
    </@paginate>
  <#else>
    <@commonMsg type="result-norecord">${uiLabelMap.PartyShoppingListEmpty}.</@commonMsg>
  </#if>
</@section>

<@section title=uiLabelMap.PartyQuickAddList>
    <form name="addToShoppingList" method="post" action="<@ofbizUrl>addItemToShoppingList</@ofbizUrl>">
      <input type="hidden" name="shoppingListId" value="${shoppingList.shoppingListId}" />
      <input type="hidden" name="partyId" value="${shoppingList.partyId!}" />
      <@field type="input" name="productId" label=uiLabelMap.PartyProduct value="" />
      <@field type="input" size="5" name="quantity" label=uiLabelMap.CommonQuantity value=requestParameters.quantity?default('1') />
      <@field type="submit" text=uiLabelMap.PartyAddToShoppingList class="+${styles.link_run_sys!} ${styles.action_add!}" />
    </form>
</@section>

</#if>
