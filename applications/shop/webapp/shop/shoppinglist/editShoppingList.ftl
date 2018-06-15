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

<@script>
    // function to add extra info for Timestamp format
    function TimestampSubmit(obj) {
       reservStartStr = jQuery(obj).find("input[name='reservStartStr']");
       val1 = reservStartStr.val();
       reservStart = jQuery(obj).find("input[name='reservStart']");
       if (reservStartStr.val().length == 10) {
           reservStart.val(reservStartStr.val() + " 00:00:00.000000000");
       } else {
           reservStart.val(reservStartStr.val());
       }
       jQuery(obj).submit();
    }
    
    function callDocumentByPaginate(info) {
        var str = info.split('~');
        var checkUrl = '<@ofbizUrl>showShoppingListAjaxFired</@ofbizUrl>';
        if(checkUrl.search("http"))
            var ajaxUrl = '<@ofbizUrl>showShoppingListAjaxFired</@ofbizUrl>';
        else
            var ajaxUrl = '<@ofbizUrl>showShoppingListAjaxFiredSecure</@ofbizUrl>';
        //jQuerry Ajax Request
        jQuery.ajax({
            url: ajaxUrl,
            type: 'POST',
            data: {"shoppingListId" : str[0], "VIEW_SIZE" : str[1], "VIEW_INDEX" : str[2]},
            error: function(msg) {
                alert("An error occurred loading content! : " + msg);
            },
            success: function(msg) {
                jQuery('#div3').html(msg);
            }
        });
     }
</@script>
<br />
<#macro paginationControls>
  <#assign viewIndexMax = Static["java.lang.Math"].ceil((listSize)?double / viewSize?double)>
  <#if (viewIndexMax?int > 0)>
    <div class="product-prevnext">
        <#-- Start Page Select Drop-Down -->
        <select name="pageSelect" onchange="callDocumentByPaginate(this[this.selectedIndex].value);">
            <option value="#">${uiLabelMap.CommonPage} ${viewIndex?int} ${uiLabelMap.CommonOf} ${viewIndexMax}</option>
            <#if (viewIndex?int > 1)>
                <#list 0..viewIndexMax as curViewNum>
                     <option value="${shoppingListId!}~${viewSize}~${curViewNum?int + 1}">${uiLabelMap.CommonGotoPage} ${curViewNum + 1}</option>
                </#list>
            </#if>
        </select>
        <#-- End Page Select Drop-Down -->
        
        <#if (viewIndex?int > 1)>
            <a href="javascript: void(0);" onclick="callDocumentByPaginate('${shoppingListId!}~${viewSize}~${viewIndex?int - 1}');" class="${styles.link_nav!}">${uiLabelMap.CommonPrevious}</a> |
        </#if>
        <#if ((listSize?int - viewSize?int) > 0)>
            <span>${lowIndex} - ${highIndex} ${uiLabelMap.CommonOf} ${listSize}</span>
        </#if>
        <#if highIndex?int < listSize?int>
         | <a href="javascript: void(0);" onclick="callDocumentByPaginate('${shoppingListId!}~${viewSize}~${viewIndex?int + 1}');" class="${styles.link_nav!}">${uiLabelMap.CommonNext}</a>
        </#if>
    </div>
</#if>
</#macro>

<form id="createEmptyShoppingList" action="<@ofbizUrl>createEmptyShoppingList</@ofbizUrl>" method="post">
  <input type="hidden" name="productStoreId" value="${productStoreId!}" />
</form>
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href="javascript:document.getElementById('createEmptyShoppingList').submit();" class="+${styles.action_run_sys!} ${styles.action_add!}" text=uiLabelMap.CommonCreateNew/>
  </@menu>
</#macro>
<@section title=uiLabelMap.EcommerceShoppingLists menuContent=menuContent>
    <#if shoppingLists?has_content>
      <form name="selectShoppingList" method="post" action="<@ofbizUrl>editShoppingList</@ofbizUrl>">
        <select name="shoppingListId">
          <#if shoppingList?has_content>
            <option value="${shoppingList.shoppingListId}">${shoppingList.listName}</option>
            <option value="${shoppingList.shoppingListId}">--</option>
          </#if>
          <#list shoppingLists as list>
            <option value="${list.shoppingListId}">${list.listName}</option>
          </#list>
        </select>
        &nbsp;
        <a href="javascript:document.selectShoppingList.submit();" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonEdit}</a>
      </form>
    <#else>
      <div>${uiLabelMap.EcommerceNoShoppingListsCreate}.</div>
      <@menu type="button">
        <@menuitem type="link" href="javascript:document.getElementById('createEmptyShoppingList').submit();" class="+${styles.action_run_sys!} ${styles.action_add!}" text=uiLabelMap.CommonCreateNew/>
      </@menu>
    </#if>
</@section>

<#if shoppingList?has_content>
  <#if canView>

  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
      <@menuitem type="link" href="javascript:document.createCustRequestFromShoppingList.submit()" text=uiLabelMap.OrderCreateCustRequestFromShoppingList>
          <form name="createCustRequestFromShoppingList" method="post" action="<@ofbizUrl>createCustRequestFromShoppingList</@ofbizUrl>">
            <input type="hidden" name="shoppingListId" value="${shoppingList.shoppingListId}"/>
          </form>
      </@menuitem>
      <@menuitem type="link" href="javascript:document.createQuoteFromShoppingList.submit()" text=uiLabelMap.OrderCreateQuoteFromShoppingList>
          <form name="createQuoteFromShoppingList" method="post" action="<@ofbizUrl>createQuoteFromShoppingList</@ofbizUrl>">
            <input type="hidden" name="shoppingListId" value="${shoppingList.shoppingListId}"/>
          </form>
      </@menuitem>
      <@menuitem type="link" href="javascript:document.updateList.submit();" class="+${styles.action_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave />
    </@menu>
  </#macro>
  <@section title="${rawLabel('EcommerceShoppingListDetail')} - ${rawString(shoppingList.listName)}" menuContent=menuContent>
        <form name="updateList" method="post" action="<@ofbizUrl>updateShoppingList</@ofbizUrl>">
            <input type="hidden" name="shoppingListId" value="${shoppingList.shoppingListId}"/>
            <input type="hidden" name="partyId" value="${shoppingList.partyId!}"/>
            <@table type="fields"> <#-- orig: border="0" width="100%" cellspacing="0" cellpadding="0" -->
              <@tr>
                <@td><div class="tableheadtext">${uiLabelMap.EcommerceListName}</div></@td>
                <@td><input type="text" size="25" name="listName" value="${shoppingList.listName}" /></@td>
              </@tr>
              <@tr>
                <@td><div class="tableheadtext">${uiLabelMap.CommonDescription}</div></@td>
                <@td><input type="text" size="70" name="description" value="${shoppingList.description!}" /></@td>
              </@tr>
              <@tr>
                <@td><div class="tableheadtext">${uiLabelMap.OrderListType}</div></@td>
                <@td>
                  <select name="shoppingListTypeId">
                      <#if shoppingListType??>
                      <option value="${shoppingListType.shoppingListTypeId}">${shoppingListType.get("description",locale)?default(shoppingListType.shoppingListTypeId)}</option>
                      <option value="${shoppingListType.shoppingListTypeId}">--</option>
                    </#if>
                    <#list shoppingListTypes as shoppingListType>
                      <option value="${shoppingListType.shoppingListTypeId}">${shoppingListType.get("description",locale)?default(shoppingListType.shoppingListTypeId)}</option>
                    </#list>
                  </select>
                </@td>
              </@tr>
              <@tr>
                <@td><div class="tableheadtext">${uiLabelMap.EcommercePublic}?</div></@td>
                <@td>
                  <select name="isPublic">
                    <#if (((shoppingList.isPublic)!"") == "Y")><option value="Y">${uiLabelMap.CommonYes}</option></#if>
                    <#if (((shoppingList.isPublic)!"") == "N")><option value="N">${uiLabelMap.CommonNo}</option></#if>
                    <option></option>
                    <option value="Y">${uiLabelMap.CommonYes}</option>
                    <option value="N">${uiLabelMap.CommonNo}</option>
                  </select>
                </@td>
              </@tr>
              <@tr>
                <@td><div class="tableheadtext">${uiLabelMap.EcommerceActive}?</div></@td>
                <@td>
                  <select name="isActive">
                    <#if (((shoppingList.isActive)!"") == "Y")><option value="Y">${uiLabelMap.CommonYes}</option></#if>
                    <#if (((shoppingList.isActive)!"") == "N")><option value="N">${uiLabelMap.CommonNo}</option></#if>
                    <option></option>
                    <option value="Y">${uiLabelMap.CommonYes}</option>
                    <option value="N">${uiLabelMap.CommonNo}</option>
                  </select>
                </@td>
              </@tr>
              <@tr>
                <@td><div class="tableheadtext">${uiLabelMap.EcommerceParentList}</div></@td>
                <@td>
                  <select name="parentShoppingListId">
                      <#if parentShoppingList??>
                      <option value="${parentShoppingList.shoppingListId}">${parentShoppingList.listName!parentShoppingList.shoppingListId}</option>
                    </#if>
                    <option value="">${uiLabelMap.EcommerceNoParent}</option>
                    <#list allShoppingLists as newParShoppingList>
                      <option value="${newParShoppingList.shoppingListId}">${newParShoppingList.listName!newParShoppingList.shoppingListId}</option>
                    </#list>
                  </select>
                  <#if parentShoppingList??>
                    <a href="<@ofbizUrl>editShoppingList?shoppingListId=${parentShoppingList.shoppingListId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonGotoParent} (${parentShoppingList.listName!parentShoppingList.shoppingListId})</a>
                  </#if>
                </@td>
              </@tr>
              <@tr>
                <@td><div class="tableheadtext">&nbsp;</div></@td>
                <@td>
                  <a href="javascript:document.updateList.submit();" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonSave}</a>
                </@td>
              </@tr>
            </@table>
        </form>
  </@section>

  <#if shoppingListType?? && shoppingListType.shoppingListTypeId == "SLT_AUTO_REODR">
  <#assign nowTimestamp = Static["org.ofbiz.base.util.UtilDateTime"].monthBegin()>
  <#assign sectionTitle>
    ${uiLabelMap.EcommerceShoppingListReorder} - ${shoppingList.listName}
    <#if shoppingList.isActive?default("N") == "N">
        <font color="yellow">${uiLabelMap.EcommerceOrderNotActive}</font>
    </#if>
  </#assign>
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
      <@menuitem type="link" href="javascript:document.reorderinfo.submit();" class="+${styles.action_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave />
    </@menu>
  </#macro>
  <@section title=wrapAsRaw(sectionTitle, 'htmlmarkup') menuContent=menuContent>
        <form name="reorderinfo" method="post" action="<@ofbizUrl>updateShoppingList</@ofbizUrl>">
        <@fields type="default-manual-widgetonly">
            <input type="hidden" name="shoppingListId" value="${shoppingList.shoppingListId}"/>
            <@table type="fields"> <#-- orig:  width="100%" cellspacing="0" cellpadding="1" border="0" -->
              <@tr>
                <@td><div class="tableheadtext">${uiLabelMap.EcommerceRecurrence}</div></@td>
                <@td>
                  <#if recurrenceInfo?has_content>
                    <#assign recurrenceRule = recurrenceInfo.getRelatedOne("RecurrenceRule", false)!>
                  </#if>
                  <select name="intervalNumber">
                    <option value="">${uiLabelMap.EcommerceSelectInterval}</option>
                    <option value="1" <#if (recurrenceRule.intervalNumber)?default(0) == 1>selected="selected"</#if>>${uiLabelMap.EcommerceEveryDay}</option>
                    <option value="2" <#if (recurrenceRule.intervalNumber)?default(0) == 2>selected="selected"</#if>>${uiLabelMap.EcommerceEveryOther}</option>
                    <option value="3" <#if (recurrenceRule.intervalNumber)?default(0) == 3>selected="selected"</#if>>${uiLabelMap.EcommerceEvery3rd}</option>
                    <option value="6" <#if (recurrenceRule.intervalNumber)?default(0) == 6>selected="selected"</#if>>${uiLabelMap.EcommerceEvery6th}</option>
                    <option value="9" <#if (recurrenceRule.intervalNumber)?default(0) == 9>selected="selected"</#if>>${uiLabelMap.EcommerceEvery9th}</option>
                  </select>
                  &nbsp;
                  <select name="frequency">
                    <option value="">${uiLabelMap.EcommerceSelectFrequency}</option>
                    <option value="4" <#if (recurrenceRule.frequency)?default("") == "DAILY">selected="selected"</#if>>${uiLabelMap.CommonDay}</option>
                    <option value="5" <#if (recurrenceRule.frequency)?default("") == "WEEKLY">selected="selected"</#if>>${uiLabelMap.CommonWeek}</option>
                    <option value="6" <#if (recurrenceRule.frequency)?default("") == "MONTHLY">selected="selected"</#if>>${uiLabelMap.CommonMonth}</option>
                    <option value="7" <#if (recurrenceRule.frequency)?default("") == "YEARLY">selected="selected"</#if>>${uiLabelMap.CommonYear}</option>
                  </select>
                </@td>
                <@td>&nbsp;</@td>
                <@td><div class="tableheadtext">${uiLabelMap.CommonStartDate}</div></@td>
                <@td>
                  <@field type="datetime" name="startDateTime" value=((recurrenceInfo.startDateTime)!) size="25" maxlength="30" id="startDateTime1" />
                </@td>
                <@td>&nbsp;</@td>
                <@td><div class="tableheadtext">${uiLabelMap.CommonEndDate}</div></@td>
                <@td>
                  <@field type="datetime" name="endDateTime" class="+textBox" value=((recurrenceRule.untilDateTime)!) size="25" maxlength="30" id="endDateTime1" />
                </@td>
                <@td>&nbsp;</@td>
              </@tr>
              <@tr type="util"><@td colspan="9"><hr /></@td></@tr>
              <@tr>
                <@td><div class="tableheadtext">${uiLabelMap.OrderShipTo}</div></@td>
                <@td>
                  <select name="contactMechId" onchange="javascript:document.reorderinfo.submit()">
                    <option value="">${uiLabelMap.OrderSelectAShippingAddress}</option>
                    <#if shippingContactMechList?has_content>
                      <#list shippingContactMechList as shippingContactMech>
                        <#assign shippingAddress = shippingContactMech.getRelatedOne("PostalAddress", false)>
                        <option value="${shippingContactMech.contactMechId}"<#if (shoppingList.contactMechId)?default("") == shippingAddress.contactMechId> selected="selected"</#if>>${shippingAddress.address1}</option>
                      </#list>
                    <#else>
                      <option value="">${uiLabelMap.OrderNoAddressesAvailable}</option>
                    </#if>
                  </select>
                </@td>
                <@td>&nbsp;</@td>
                <@td><div class="tableheadtext">${uiLabelMap.OrderShipVia}</div></@td>
                <@td>
                  <select name="shippingMethodString">
                    <option value="">${uiLabelMap.OrderSelectShippingMethod}</option>
                    <#if carrierShipMethods?has_content>
                      <#list carrierShipMethods as shipMeth>
                        <#assign shippingEst = shippingEstWpr.getShippingEstimate(shipMeth)?default(-1)>
                        <#assign shippingMethod = shipMeth.shipmentMethodTypeId + "@" + shipMeth.partyId>
                        <option value="${shippingMethod}"<#if shippingMethod == chosenShippingMethod> selected="selected"</#if>>
                          <#if shipMeth.partyId != "_NA_">
                            ${shipMeth.partyId!}&nbsp;
                          </#if>
                          ${shipMeth.description!}
                          <#if shippingEst?has_content>
                            &nbsp;-&nbsp;
                            <#if (shippingEst > -1)>
                              <@ofbizCurrency amount=shippingEst isoCode=listCart.getCurrency()/>
                            <#else>
                              ${uiLabelMap.OrderCalculatedOffline}
                            </#if>
                          </#if>
                        </option>
                      </#list>
                    <#else>
                      <option value="">${uiLabelMap.OrderSelectAddressFirst}</option>
                    </#if>
                  </select>
                </@td>
                <@td>&nbsp;</@td>
                <@td><div class="tableheadtext">${uiLabelMap.OrderPayBy}</div></@td>
                <@td>
                  <select name="paymentMethodId">
                    <option value="">${uiLabelMap.OrderSelectPaymentMethod}</option>
                    <#list paymentMethodList as paymentMethod>
                      <#if paymentMethod.paymentMethodTypeId == "CREDIT_CARD">
                        <#assign creditCard = paymentMethod.getRelatedOne("CreditCard", false)>
                        <option value="${paymentMethod.paymentMethodId}" <#if (shoppingList.paymentMethodId)?default("") == paymentMethod.paymentMethodId>selected="selected"</#if>>CC:&nbsp;${Static["org.ofbiz.party.contact.ContactHelper"].formatCreditCard(creditCard)}</option>
                      <#elseif paymentMethod.paymentMethodTypeId == "EFT_ACCOUNT">
                        <#assign eftAccount = paymentMethod.getRelatedOne("EftAccount", false)>
                        <option value="${paymentMethod.paymentMethodId}">EFT:&nbsp;${eftAccount.bankName!}: ${eftAccount.accountNumber!}</option>
                      </#if>
                    </#list>
                  </select>
                </@td>
                <@td>&nbsp;</@td>
              </@tr>
              <@tr type="util"><@td colspan="9"><hr /></@td></@tr>
              <@tr>
                <@td align="right" colspan="9">
                    <a href="javascript:document.reorderinfo.submit();" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonSave}</a>
                    <a href="<@ofbizUrl>editcontactmech?preContactMechTypeId=POSTAL_ADDRESS&amp;contactMechPurposeTypeId=SHIPPING_LOCATION&amp;DONE_PAGE=editShoppingList</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.PartyAddNewAddress}</a>
                    <a href="<@ofbizUrl>editcreditcard?DONE_PAGE=editShoppingList</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.EcommerceNewCreditCard}</a>
                    <a href="<@ofbizUrl>editeftaccount?DONE_PAGE=editShoppingList</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.EcommerceNewEFTAccount}</a>
                </@td>
              </@tr>
              <#if shoppingList.isActive?default("N") == "Y">
                <@tr type="util"><@td colspan="9"><hr /></@td></@tr>
                <@tr>
                  <@td colspan="9">
                    <#assign nextTime = recInfo.next(lastSlOrderTime)!>
                    <#if nextTime?has_content>
                      <#assign nextTimeStamp = Static["org.ofbiz.base.util.UtilDateTime"].getTimestamp(nextTime)!>
                      <#if nextTimeStamp?has_content>
                        <#assign nextTimeString = Static["org.ofbiz.base.util.UtilFormatOut"].formatDate(nextTimeStamp)!>
                      </#if>
                    </#if>
                    <#if lastSlOrderDate?has_content>
                      <#assign lastOrderedString = Static["org.ofbiz.base.util.UtilFormatOut"].formatDate(lastSlOrderDate)!>
                    </#if>
                    <div>
                      <@table type="fields"> <#-- orig: cellspacing="2" cellpadding="2" border="0" -->
                        <@tr>
                          <@td><div class="tableheadtext">${uiLabelMap.OrderLastOrderedDate}</div></@td>
                          <@td><div class="tableheadtext">:</div></@td>
                          <@td>${lastOrderedString!(uiLabelMap.OrderNotYetOrdered)}</@td>
                        </@tr>
                        <@tr>
                          <@td><div class="tableheadtext">${uiLabelMap.EcommerceEstimateNextOrderDate}</div></@td>
                          <@td><div class="tableheadtext">:</div></@td>
                          <@td>${nextTimeString!(uiLabelMap.EcommerceNotYetKnown)}</@td>
                        </@tr>
                      </@table>
                    </div>
                  </@td>
                </@tr>
              </#if>
            </@table>
        </@fields>
        </form>
  </@section>
  </#if>

  <#if childShoppingListDatas?has_content>
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
      <@menuitem type="link" href=makeOfbizUrl("addListToCart?shoppingListId=${shoppingList.shoppingListId}&includeChild=yes") class="+${styles.action_run_sys!} ${styles.action_add!}" text=uiLabelMap.EcommerceAddChildListsToCart />
    </@menu>
  </#macro>
  <@section title="${rawLabel('EcommerceChildShoppingList')} - ${rawString(shoppingList.listName)}" menuContent=menuContent>
    <@table type="data-complex"> <#-- orig: width="100%" cellspacing="0" cellpadding="1" border="0" -->
      <@tr>
        <@td><b>${uiLabelMap.EcommerceListName}</b></@td>
        <@td align="right"><b>${uiLabelMap.EcommerceTotalPrice}</b></@td>
        <@td>&nbsp;</@td>
        <@td>&nbsp;</@td>
      </@tr>
      <#list childShoppingListDatas as childShoppingListData>
          <#assign childShoppingList = childShoppingListData.childShoppingList/>
          <#assign totalPrice = childShoppingListData.totalPrice/>
          <@tr>
            <@td nowrap="nowrap">
              <a href="<@ofbizUrl>editShoppingList?shoppingListId=${childShoppingList.shoppingListId}</@ofbizUrl>" class="${styles.link_nav_info_name!}">${childShoppingList.listName!childShoppingList.shoppingListId}</a>
            </@td>
            <@td nowrap="nowrap" align="right"><@ofbizCurrency amount=totalPrice isoCode=currencyUomId/>
            </@td>
            <@td align="right">
              <a href="<@ofbizUrl>editShoppingList?shoppingListId=${childShoppingList.shoppingListId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.EcommerceGoToList}</a>
              <a href="<@ofbizUrl>addListToCart?shoppingListId=${childShoppingList.shoppingListId}</@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_add!}">${uiLabelMap.EcommerceAddListToCart}</a>
            </@td>
          </@tr>
        </form>
      </#list>
      <@tr type="util"><@td colspan="6"><hr /></@td></@tr>
      <@tr>
        <@td>&nbsp;</@td>
        <@td nowrap="nowrap" align="right">
          <div class="tableheadtext"><@ofbizCurrency amount=shoppingListChildTotal isoCode=currencyUomId/></div>
        </@td>
        <@td>&nbsp;</@td>
      </@tr>
    </@table>
  </@section>
  </#if>

  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
      <@menuitem type="link" href=makeOfbizUrl("addListToCart?shoppingListId=${shoppingList.shoppingListId}") class="+${styles.action_run_session!} ${styles.action_add!}" text=uiLabelMap.EcommerceAddListToCart />
    </@menu>
  </#macro>
  <@section title="${rawLabel('EcommerceListItems')} - ${rawString(shoppingList.listName)}" menuContent=menuContent>
    <#if shoppingListItemDatas?has_content>
        <#-- Pagination -->
        <@paginationControls/>
        <@table type="data-complex"> <#-- orig: width="100%" cellspacing="0" cellpadding="1" border="0" -->
          <@tr>
            <@td><b>${uiLabelMap.OrderProduct}</b></@td>
            <@td><@table type="generic"><@tr><@td nowrap="nowrap" align="center"><b>- ${uiLabelMap.EcommerceStartdate} -</b></@td><@td nowrap="nowrap"><b>- ${uiLabelMap.EcommerceNbrOfDays} -</b></@td></@tr><@tr><@td nowrap="nowrap"><b>- ${uiLabelMap.EcommerceNbrOfPersons} -</b></@td><@td nowrap="nowrap" align="center"><b>- ${uiLabelMap.CommonQuantity} -</b></@td></@tr></@table></@td>
            <#-- <@td nowrap="nowrap" align="center"><b>Purchased</b></@td> -->
            <@td align="right"><b>${uiLabelMap.EcommercePrice}</b></@td>
            <@td align="right"><b>${uiLabelMap.OrderTotal}</b></@td>
            <@td>&nbsp;</@td>
          </@tr>
          <#list shoppingListItemDatas[lowIndex-1..highIndex-1] as shoppingListItemData>
            <#assign shoppingListItem = shoppingListItemData.shoppingListItem/>
            <#assign product = shoppingListItemData.product/>
            <#assign productContentWrapper = Static["org.ofbiz.product.product.ProductContentWrapper"].makeProductContentWrapper(product, request)/>
            <#assign unitPrice = shoppingListItemData.unitPrice/>
            <#assign totalPrice = shoppingListItemData.totalPrice/>
            <#assign productVariantAssocs = shoppingListItemData.productVariantAssocs!/>
            <#assign isVirtual = product.isVirtual?? && product.isVirtual.equals("Y")/>
              <@tr>
                <@td>
                     <a href="<@ofbizUrl>product?product_id=${shoppingListItem.productId}</@ofbizUrl>" class="${styles.link_nav_info_idname!}">${shoppingListItem.productId} -
                     ${productContentWrapper.get("PRODUCT_NAME")!("No Name")}</a> : ${productContentWrapper.get("DESCRIPTION")!}
                </@td>
                <@td nowrap="nowrap" align="center">
                  <form method="post" action="<@ofbizUrl>updateShoppingListItem</@ofbizUrl>" name="listform_${shoppingListItem.shoppingListItemSeqId}">
                  <@fields type="default-manual-widgetonly">
                    <input type="hidden" name="shoppingListId" value="${shoppingListItem.shoppingListId}"/>
                    <input type="hidden" name="shoppingListItemSeqId" value="${shoppingListItem.shoppingListItemSeqId}"/>
                    <input type="hidden" name="reservStart"/>
                    <div>
                       <@table type="fields"> <#-- orig: border="0" width="100%" -->
                       <#if product.productTypeId == "ASSET_USAGE" || product.productTypeId == "ASSET_USAGE_OUT_IN">
                            <@tr>
                                <@td width="1%">&nbsp;</@td>
                                <@td><@field type="datetime" name="reservStartStr" value=(shoppingListItem.reservStart!) size="15" maxlength="30" id="reservStartStr_${shoppingListItem.shoppingListItemSeqId}" dateType="date" dateDisplayType="date" /></@td>
                                <@td><input type="text" size="2" name="reservLength" value="${shoppingListItem.reservLength!}"/></@td>
                            </@tr>
                            <@tr open=true close=false />
                            <#if product.productTypeId == "ASSET_USAGE">
                                <@td>&nbsp;</@td>
                                <@td><input type="text" size="3" name="reservPersons" value="${shoppingListItem.reservPersons!}"/></@td>
                            <#else>
                                <@td>&nbsp;</@td>
                                <@td>&nbsp;</@td>
                            </#if>
                                <@td open=true close=false />
                       <#else>
                                <@tr>
                                    <@td width="62%" align="center">--</@td>
                                    <@td align="center">--</@td>
                                </@tr>
                                <@tr open=true close=false />
                                    <@td align="center">--</@td>
                                    <@td open=true close=false /><input type="hidden" name="reservStartStr" value=""/>
                       </#if>
                    <input size="6" type="text" name="quantity" value="${shoppingListItem.quantity?string.number}"/>
                    <@td close=true open=false /></@tr close=true open=false /></@table>
                    </div>
                  </@fields>
                  </form>
                </@td>
                <#--
                <@td nowrap="nowrap" align="center">${shoppingListItem.quantityPurchased?default(0)?string.number}
                </@td>
                -->
                <@td nowrap="nowrap" align="right"><@ofbizCurrency amount=unitPrice isoCode=currencyUomId/>
                </@td>
                <@td nowrap="nowrap" align="right"><@ofbizCurrency amount=totalPrice isoCode=currencyUomId/>
                </@td>
                <@td align="right">
                    <a href="#" onclick="javascript:TimestampSubmit(listform_${shoppingListItem.shoppingListItemSeqId});" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                    <a href="javascript:document.removeFromShoppingList.submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a>
                    <form name="removeFromShoppingList" method="post" action="<@ofbizUrl>removeFromShoppingList</@ofbizUrl>">
                      <fieldset>
                        <input type="hidden" name="shoppingListId" value="${shoppingListItem.shoppingListId!}">
                        <input type="hidden" name="shoppingListItemSeqId" value="${shoppingListItem.shoppingListItemSeqId}">
                      </fieldset>
                    </form>
                  <#if isVirtual && productVariantAssocs?has_content>
                    <#assign replaceItemAction = "/replaceShoppingListItem/" + requestAttributes._CURRENT_VIEW_!>
                    <#assign addToCartAction = "/additem/" + requestAttributes._CURRENT_VIEW_!>
                    <br />
                    <form method="post" action="<@ofbizUrl>${addToCartAction}</@ofbizUrl>" name="listreplform_${shoppingListItem.shoppingListItemSeqId}">
                      <input type="hidden" name="shoppingListId" value="${shoppingListItem.shoppingListId}"/>
                      <input type="hidden" name="shoppingListItemSeqId" value="${shoppingListItem.shoppingListItemSeqId}"/>
                      <input type="hidden" name="quantity" value="${shoppingListItem.quantity}"/>
                      <select name="add_product_id">
                          <#list productVariantAssocs as productVariantAssoc>
                            <#assign variantProduct = productVariantAssoc.getRelatedOne("AssocProduct", true)>
                            <#if variantProduct??>
                            <#assign variantProductContentWrapper = Static["org.ofbiz.product.product.ProductContentWrapper"].makeProductContentWrapper(variantProduct, request)>
                              <option value="${variantProduct.productId}">${variantproductContentWrapper.get("PRODUCT_NAME")!("No Name")} [${variantProduct.productId}]</option>
                            </#if>
                          </#list>
                      </select>
                      <br />
                      <a href="javascript:document.listreplform_${shoppingListItem.shoppingListItemSeqId}.action='<@ofbizUrl>${replaceItemAction}</@ofbizUrl>';document.listreplform_${shoppingListItem.shoppingListItemSeqId}.submit();" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.EcommerceReplaceWithVariation}</a>
                      <br />
                      <a href="javascript:document.listreplform_${shoppingListItem.shoppingListItemSeqId}.action='<@ofbizUrl>${addToCartAction}</@ofbizUrl>';document.listreplform_${shoppingListItem.shoppingListItemSeqId}.submit();" class="${styles.link_run_session!} ${styles.action_add!}">${uiLabelMap.CommonAdd}&nbsp;${shoppingListItem.quantity?string}&nbsp;${uiLabelMap.EcommerceVariationToCart}</a>
                    </form>
                  <#else>
                    <a href="<@ofbizUrl>additem<#if requestAttributes._CURRENT_VIEW_??>/${requestAttributes._CURRENT_VIEW_}</#if>?shoppingListId=${shoppingListItem.shoppingListId}&amp;shoppingListItemSeqId=${shoppingListItem.shoppingListItemSeqId}&amp;quantity=${shoppingListItem.quantity}&amp;reservStart=${shoppingListItem.reservStart!}&amp;reservPersons=${shoppingListItem.reservPersons!}&amp;reservLength=${shoppingListItem.reservLength!}&amp;configId=${shoppingListItem.configId!}&amp;add_product_id=${shoppingListItem.productId}</@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_add!}">${uiLabelMap.CommonAdd}&nbsp;${shoppingListItem.quantity?string}&nbsp;${uiLabelMap.OrderToCart}</a>
                  </#if>
                </@td>
              </@tr>
          </#list>
          <@tr type="util"><@td colspan="6"><hr /></@td></@tr>
          <@tr>
            <@td>&nbsp;</@td>
            <@td>&nbsp;</@td>
            <#--<@td>&nbsp;</@td>-->
            <@td>&nbsp;</@td>
            <@td nowrap="nowrap" align="right">
              <div class="tableheadtext"><@ofbizCurrency amount=shoppingListItemTotal isoCode=currencyUomId/></div>
            </@td>
            <@td>&nbsp;</@td>
          </@tr>
        </@table>
    <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.EcommerceShoppingListEmpty}.</@commonMsg>
    </#if>
  </@section>

  <@section title="${rawLabel('EcommerceShoppingListPriceTotals')} - ${rawString(shoppingList.listName)}">
      <@table type="fields"> <#-- orig: width="100%" border="0" cellspacing="1" cellpadding="1" -->
        <@tr>
          <@td width="5%" nowrap="nowrap">${uiLabelMap.EcommerceChildListTotalPrice}
          </@td>
          <@td align="right" width="5%" nowrap="nowrap"><@ofbizCurrency amount=shoppingListChildTotal isoCode=currencyUomId/>
          </@td>
          <@td width="90%">&nbsp;</@td>
        </@tr>
        <@tr>
          <@td nowrap="nowrap">${uiLabelMap.EcommerceListItemsTotalPrice}&nbsp;
          </@td>
          <@td align="right" nowrap="nowrap"><@ofbizCurrency amount=shoppingListItemTotal isoCode=currencyUomId/>
          </@td>
          <@td>&nbsp;</@td>
        </@tr>
        <@tr>
          <@td nowrap="nowrap">
              <div class="tableheadtext">${uiLabelMap.OrderGrandTotal}</div>
          </@td>
          <@td align="right" nowrap="nowrap">
              <div class="tableheadtext"><@ofbizCurrency amount=shoppingListTotalPrice isoCode=currencyUomId/></div>
          </@td>
          <@td>&nbsp;</@td>
        </@tr>
      </@table>
  </@section>

  <@section title=uiLabelMap.CommonQuickAddList>
    <form name="addToShoppingList" method="post" action="<@ofbizUrl>addItemToShoppingList</@ofbizUrl>">
      <input type="hidden" name="shoppingListId" value="${shoppingList.shoppingListId}"/>
      <input type="text" name="productId" value="${requestParameters.add_product_id!}"/>
      <@table><#if reservStart??><@tr><@td>${uiLabelMap.EcommerceStartDate}</@td><@td><input type="text" size="10" name="reservStart" value="${requestParameters.reservStart!""}" /></@td><@td> ${uiLabelMap.EcommerceLength}:</@td><@td><input type="text" size="2" name="reservLength" value="${requestParameters.reservLength!""}" /></@td></@tr></#if><@tr><#if reservStart??><@td>&nbsp;</@td><@td>&nbsp;</@td><@td>${uiLabelMap.OrderNbrPersons}:</@td><@td><input type="text" size="3" name="reservPersons" value="${requestParameters.reservPersons!"1"}" /></@td></#if><@td nowrap="nowrap">${uiLabelMap.CommonQuantity} :</@td><@td><input type="text" size="5" name="quantity" value="${requestParameters.quantity!"1"}" /></@td></@tr></@table>
      <#-- <input type="text" size="5" name="quantity" value="${requestParameters.quantity!"1"}" />-->
      <input type="submit" class="${styles.link_run_sys!} ${styles.action_add!}" value="${uiLabelMap.OrderAddToShoppingList}"/>
    </form>
  </@section>

  <#else>
    <#-- shoppingList was found, but belongs to a different party -->
    <@commonMsg type="error-perm">${uiLabelMap.EcommerceShoppingListError} ${uiLabelMap.CommonId} ${shoppingList.shoppingListId}) ${uiLabelMap.EcommerceListDoesNotBelong}.</@commonMsg>
  </#if>
</#if>
