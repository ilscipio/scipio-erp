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
    <@menuitem type="link" href="javascript:document.getElementById('createEmptyShoppingList').submit();" text="${uiLabelMap.CommonNew}">
      <form id="createEmptyShoppingList" action="<@ofbizUrl>createEmptyShoppingList</@ofbizUrl>" method="post">
        <input type="hidden" name="partyId" value="${partyId!}" />
      </form>
    </@menuitem>
  </@menu>
</#macro>
<@section title="${uiLabelMap.PartyShoppingLists}" menuContent=menuContent>
    <#if shoppingLists?has_content>
      <form name="selectShoppingList" method="post" action="<@ofbizUrl>editShoppingList</@ofbizUrl>">
        <@fields type="default-nolabels">
        <input type="hidden" name="partyId" value="${partyId!}" />
        <@field type="generic">
            <select name="shoppingListId">
              <#if shoppingList?has_content>
                <option value="${shoppingList.shoppingListId}">${shoppingList.listName}</option>
                <option value="${shoppingList.shoppingListId}">--</option>
              </#if>
              <#list allShoppingLists as list>
                <option value="${list.shoppingListId}">${list.listName}</option>
              </#list>
            </select>
        </@field>
        <@field type="submitarea">
            <a href="javascript:document.selectShoppingList.submit();" class="${styles.link_action!}">${uiLabelMap.CommonEdit}</a>
        </@field>
        </@fields>
      </form>
    <#else>
      <@resultMsg>${uiLabelMap.PartyNoShoppingListsParty}.</@resultMsg>
    </#if>
</@section>

<#if shoppingList?has_content>

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
      <@menuitem type="link" href="javascript:document.updateList.submit();" text="${uiLabelMap.CommonSave}" />
      <@menuitem type="link" href="javascript:document.createQuoteFromShoppingListForm.submit()" text="${uiLabelMap.PartyCreateNewQuote}">
        <form method="post" name="createQuoteFromShoppingListForm" action="/ordermgr/control/createQuoteFromShoppingList">
          <input type= "hidden" name= "applyStorePromotions" value= "N"/>
          <input type= "hidden" name= "shoppingListId" value= "${shoppingList.shoppingListId!}"/>
        </form>
      </@menuitem>
      <@menuitem type="link" href="/ordermgr/control/createCustRequestFromShoppingList?shoppingListId=${shoppingList.shoppingListId!}" text="${uiLabelMap.PartyCreateNewCustRequest}" />
      <@menuitem type="link" href="/ordermgr/control/loadCartFromShoppingList?shoppingListId=${shoppingList.shoppingListId!}" text="${uiLabelMap.OrderNewOrder}" />
  </@menu>
</#macro>
<@section title="${uiLabelMap.PartyShoppingListDetail} - ${shoppingList.listName}" menuContent=menuContent>
    <form name="updateList" method="post" action="<@ofbizUrl>updateShoppingList</@ofbizUrl>">
      <input type="hidden" name="shoppingListId" value="${shoppingList.shoppingListId}" />
      <input type="hidden" name="partyId" value="${shoppingList.partyId!}" />
        <@field type="generic" label="${uiLabelMap.PartyListName}">
            <input type="text" size="25" name="listName" value="${shoppingList.listName}" <#if shoppingList.listName?default("") == "auto-save">disabled="disabled"</#if> />
        </@field>
        <@field type="generic" label="${uiLabelMap.CommonDescription}">
            <input type="text" size="70" name="description" value="${shoppingList.description!}" <#if shoppingList.listName?default("") == "auto-save">disabled="disabled"</#if> />
        </@field>
        <@field type="generic" label="${uiLabelMap.PartyListType}">
            <select name="shoppingListTypeId" <#if shoppingList.listName?default("") == "auto-save">disabled</#if>>
              <#if shoppingListType??>
                <option value="${shoppingListType.shoppingListTypeId}">${shoppingListType.get("description",locale)?default(shoppingListType.shoppingListTypeId)}</option>
                <option value="${shoppingListType.shoppingListTypeId}">--</option>
              </#if>
              <#list shoppingListTypes as shoppingListType>
                <option value="${shoppingListType.shoppingListTypeId}">${shoppingListType.get("description",locale)?default(shoppingListType.shoppingListTypeId)}</option>
              </#list>
            </select>
        </@field>
        <@field type="generic" label="${uiLabelMap.PartyPublic}?">
            <select name="isPublic" <#if shoppingList.listName?default("") == "auto-save">disabled</#if>>
              <option>${shoppingList.isPublic}</option>
              <option value="${shoppingList.isPublic}">--</option>
              <option>${uiLabelMap.CommonYes}</option>
              <option>${uiLabelMap.CommonNo}</option>
            </select>
        </@field>
        <@field type="generic" label="${uiLabelMap.PartyParentList}">
            <select name="parentShoppingListId" <#if shoppingList.listName?default("") == "auto-save">disabled</#if>>
              <#if parentShoppingList??>
                <option value="${parentShoppingList.shoppingListId}">${parentShoppingList.listName?default(parentShoppingList.shoppingListId)}</option>
              </#if>
              <option value="">${uiLabelMap.PartyNoParent}</option>
              <#list allShoppingLists as newParShoppingList>
                <option value="${newParShoppingList.shoppingListId}">${newParShoppingList.listName?default(newParShoppingList.shoppingListId)}</option>
              </#list>
            </select>
            <#if parentShoppingList??>
              <a href="<@ofbizUrl>editShoppingList?shoppingListId=${parentShoppingList.shoppingListId}</@ofbizUrl>" class="${styles.link_action!}">${uiLabelMap.CommonGotoParent} (${parentShoppingList.listName?default(parentShoppingList.shoppingListId)})</a>
            </#if>
        </@field>
        <#if shoppingList.listName?default("") != "auto-save">
          <@field type="submitarea">
              <a href="javascript:document.updateList.submit();" class="${styles.link_action!}">${uiLabelMap.CommonSave}</a>
          </@field>
        </#if>
    </form>
</@section>

<#if childShoppingListDatas?has_content>
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makeOfbizUrl("addListToCart?shoppingListId=${shoppingList.shoppingListId}&amp;includeChild=yes") text="${uiLabelMap.PartyAddChildListsToCart}" />
  </@menu>
</#macro>
<@section title="${uiLabelMap.PartyChildShoppingList} - ${shoppingList.listName}" menuContent=menuContent>
    <@table type="data-list" autoAltRows=true cellspacing="0"> <#-- orig: class="basic-table" -->
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
          <@td class="button-col"><a href="<@ofbizUrl>editShoppingList?shoppingListId=${childShoppingList.shoppingListId}</@ofbizUrl>">${childShoppingList.listName?default(childShoppingList.shoppingListId)}</a></li></@td>
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
<#-- <@menuitem type="link" href=makeOfbizUrl("addListToCart?shoppingListId=${shoppingList.shoppingListId}") text="${uiLabelMap.PartyAddListToCart}" /> -->
  </@menu>
</#macro>
<@section title="${uiLabelMap.PartyListItems} - ${shoppingList.listName}" menuContent=menuContent>
    <#if shoppingListItemDatas?has_content>
        <#-- Pagination -->
        <#include "component://common/webcommon/includes/htmlTemplate.ftl"/>
        <#assign commonUrl = "editShoppingList?partyId=" + partyId + "&shoppingListId="+(shoppingListId!)+"&"/>
        <#assign viewIndexFirst = 0/>
        <#assign viewIndexPrevious = viewIndex - 1/>
        <#assign viewIndexNext = viewIndex + 1/>
        <#assign viewIndexLast = Static["org.ofbiz.base.util.UtilMisc"].getViewLastIndex(listSize, viewSize) />
        <#assign messageMap = Static["org.ofbiz.base.util.UtilMisc"].toMap("lowCount", lowIndex, "highCount", highIndex, "total", listSize)/>
        <#assign commonDisplaying = Static["org.ofbiz.base.util.UtilProperties"].getMessage("CommonUiLabels", "CommonDisplaying", messageMap, locale)/>
        <#macro paginateShoppingListItems>
          <@nextPrev commonUrl=commonUrl ajaxEnabled=false javaScriptEnabled=false paginateStyle="nav-pager" paginateFirstStyle="nav-first" viewIndex=viewIndex highIndex=highIndex listSize=listSize viewSize=viewSize ajaxFirstUrl="" firstUrl="" paginateFirstLabel="" paginatePreviousStyle="nav-previous" ajaxPreviousUrl="" previousUrl="" paginatePreviousLabel="" pageLabel="" ajaxSelectUrl="" selectUrl="" ajaxSelectSizeUrl="" selectSizeUrl="" commonDisplaying=commonDisplaying paginateNextStyle="nav-next" ajaxNextUrl="" nextUrl="" paginateNextLabel="" paginateLastStyle="nav-last" ajaxLastUrl="" lastUrl="" paginateLastLabel="" paginateViewSizeLabel="" />
        </#macro>
        <#assign paginated = true>
        
        <#if paginated>
          <@paginateShoppingListItems />
        </#if>
        
      <@table type="data-list" autoAltRows=true cellspacing="0"> <#-- orig: class="basic-table" -->
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
            <@td><a href="/catalog/control/EditProduct?productId=${shoppingListItem.productId}&amp;externalLoginKey=${requestAttributes.externalLoginKey}">${shoppingListItem.productId} -
              ${productContentWrapper.get("PRODUCT_NAME", "html")?default("No Name")}</a> : ${productContentWrapper.get("DESCRIPTION", "html")!}
            </@td>
            <form method="post" action="<@ofbizUrl>removeFromShoppingList</@ofbizUrl>" name='removeform_${shoppingListItem.shoppingListItemSeqId}'>
              <input type="hidden" name="shoppingListId" value="${shoppingListItem.shoppingListId}" />
              <input type="hidden" name="shoppingListItemSeqId" value="${shoppingListItem.shoppingListItemSeqId}" />
            </form>
            <form method="post" action="<@ofbizUrl>updateShoppingListItem</@ofbizUrl>" name='listform_${shoppingListItem.shoppingListItemSeqId}'>
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
              <a href="javascript:document.listform_${shoppingListItem.shoppingListItemSeqId}.submit();">${uiLabelMap.CommonUpdate}</a>
              <a href="javascript:document.removeform_${shoppingListItem.shoppingListItemSeqId}.submit();">${uiLabelMap.CommonRemove}</a>
            </@td>
          </@tr>
        </#list>
        </@tbody>
      </@table>

      <#if paginated>
        <@paginateShoppingListItems />
      </#if> 
    <#else>
      <@resultMsg>${uiLabelMap.PartyShoppingListEmpty}.</@resultMsg>
    </#if>
</@section>

<@section title="${uiLabelMap.PartyQuickAddList}">
    <form name="addToShoppingList" method="post" action="<@ofbizUrl>addItemToShoppingList</@ofbizUrl>">
      <input type="hidden" name="shoppingListId" value="${shoppingList.shoppingListId}" />
      <input type="hidden" name="partyId" value="${shoppingList.partyId!}" />
      <@field type="input" name="productId" label="${uiLabelMap.PartyProduct}" value="" />
      <@field type="input" size="5" name="quantity" label="${uiLabelMap.CommonQuantity}" value="${requestParameters.quantity?default('1')}" />
      <@field type="submitarea">
        <input type="submit" value="${uiLabelMap.PartyAddToShoppingList}" />
      </@field>
    </form>
</@section>

</#if>
