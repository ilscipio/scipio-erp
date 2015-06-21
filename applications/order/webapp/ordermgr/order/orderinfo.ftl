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

<@section>
            <#if orderHeader.externalId?has_content>
               <#assign externalOrder = "(" + orderHeader.externalId + ")"/>
            </#if>
            <#assign orderType = orderHeader.getRelatedOne("OrderType", false)/>
            
           <h2><a href="<@ofbizUrl>orderview?orderId=${orderId}</@ofbizUrl>">${orderId}</a> ${externalOrder!}</h2>
           <#--${screens.render("component://order/widget/ordermgr/OrderViewScreens.xml#orderactions")}-->

<table class="basic-table">
            <#if orderHeader.orderName?has_content>
            <tr>
        <td class="${style_grid_large!}2">${uiLabelMap.OrderOrderName}
        </td>
        <td colspan="3">${orderHeader.orderName}</td>
            </tr>
            </#if>
            <tr>
    <td scope="row" class="${style_grid_large!}3">${uiLabelMap.OrderStatusHistory}</td>
    <td colspan="3">
    ${uiLabelMap.OrderCurrentStatus}: ${currentStatus.get("description",locale)} 
    <@modal id="${orderId}_info" label="${uiLabelMap.CommonDetail}">
                <#if orderHeaderStatuses?has_content>
          <ul class="no-bullet">
                  <#list orderHeaderStatuses as orderHeaderStatus>
                    <#assign loopStatusItem = orderHeaderStatus.getRelatedOne("StatusItem", false)>
                    <#assign userlogin = orderHeaderStatus.getRelatedOne("UserLogin", false)>
            
              <li>${loopStatusItem.get("description",locale)} <#if orderHeaderStatus.statusDatetime?has_content>- ${Static["org.ofbiz.base.util.UtilFormatOut"].formatDateTime(orderHeaderStatus.statusDatetime, "", locale, timeZone)?default("0000-00-00 00:00:00")}</#if>
                      &nbsp;
              ${uiLabelMap.CommonBy} - <#--${Static["org.ofbiz.party.party.PartyHelper"].getPartyName(delegator, userlogin.getString("partyId"), true)}--> [${orderHeaderStatus.statusUserLogin}]</li>
                  </#list>
          </ul>
                </#if>
    </@modal>
    
    
              </td>
            </tr>

            <tr>
    <td scope="row" class="${style_grid_large!}3">${uiLabelMap.OrderDateOrdered}</td>
    <td colspan="3">

    <#if orderHeader.orderDate?has_content>${Static["org.ofbiz.base.util.UtilFormatOut"].formatDateTime(orderHeader.orderDate, "", locale, timeZone)!}</#if>
    </td>
            </tr>

            <tr>
    <td scope="row" class="${style_grid_large!}3">${uiLabelMap.CommonCurrency}</td>
    <td colspan="3">${orderHeader.currencyUom?default("???")}</td>
            </tr>

            <#if orderHeader.internalCode?has_content>
            <tr>
        <td scope="row" class="${style_grid_large!}3">${uiLabelMap.OrderInternalCode}</td>
        <td colspan="3">
        ${orderHeader.internalCode}</td>
            </tr>
            </#if>

            <tr>
        <td scope="row" class="${style_grid_large!}3">${uiLabelMap.OrderSalesChannel}</td>
        <td colspan="3">
                  <#if orderHeader.salesChannelEnumId?has_content>
                    <#assign channel = orderHeader.getRelatedOne("SalesChannelEnumeration", false)>
                    ${(channel.get("description",locale))?default("N/A")}
                  <#else>
                    ${uiLabelMap.CommonNA}
                  </#if>
              </td>
            </tr>

            <#if productStore?has_content>
              <tr>
        <td scope="row" class="${style_grid_large!}3">${uiLabelMap.OrderProductStore}</td>
        <td colspan="3">
            ${productStore.storeName!}&nbsp;<a href="/catalog/control/EditProductStore?productStoreId=${productStore.productStoreId}${StringUtil.wrapString(externalKeyParam)}" target="catalogmgr" >(${productStore.productStoreId})</a>
                </td>
              </tr>
            </#if>


            <tr>
        <td scope="row" class="${style_grid_large!}3">${uiLabelMap.OrderOriginFacility}</td>
        <td colspan="3">
                  <#if orderHeader.originFacilityId?has_content>
            <a href="/facility/control/EditFacility?facilityId=${orderHeader.originFacilityId}${StringUtil.wrapString(externalKeyParam)}" target="facilitymgr">${orderHeader.originFacilityId}</a>
                  <#else>
                    ${uiLabelMap.CommonNA}
                  </#if>
              </td>
            </tr>
      
            <tr>
        <td scope="row" class="${style_grid_large!}3">${uiLabelMap.CommonCreatedBy}</td>
        <td colspan="3">
                  <#if orderHeader.createdBy?has_content>
            <a href="/partymgr/control/viewprofile?userlogin_id=${orderHeader.createdBy}${StringUtil.wrapString(externalKeyParam)}" target="partymgr" class="">${orderHeader.createdBy}</a>
                  <#else>
                    ${uiLabelMap.CommonNotSet}
                  </#if>
              </td>
            </tr>

            <#if (orderItem.cancelBackOrderDate)??>
              <tr>
        <td scope="row" class="${style_grid_large!}3">${uiLabelMap.FormFieldTitle_cancelBackOrderDate}</td>
        <td colspan="3">
          <#if orderItem.cancelBackOrderDate?has_content>${Static["org.ofbiz.base.util.UtilFormatOut"].formatDateTime(orderItem.cancelBackOrderDate, "", locale, timeZone)!}</#if>
        </td>
              </tr>
            </#if>



            <#if distributorId??>
            <tr>
        <td scope="row" class="${style_grid_large!}3">${uiLabelMap.OrderDistributor}</td>
        <td colspan="3">
                  <#assign distPartyNameResult = dispatcher.runSync("getPartyNameForDate", Static["org.ofbiz.base.util.UtilMisc"].toMap("partyId", distributorId, "compareDate", orderHeader.orderDate, "userLogin", userLogin))/>
                  ${distPartyNameResult.fullName?default("[${uiLabelMap.OrderPartyNameNotFound}]")}
              </td>
            </tr>
            </#if>

            <#if affiliateId??>
            <tr>
      <td>${uiLabelMap.OrderAffiliate}</td>
      <td colspan="3">
                  <#assign affPartyNameResult = dispatcher.runSync("getPartyNameForDate", Static["org.ofbiz.base.util.UtilMisc"].toMap("partyId", affiliateId, "compareDate", orderHeader.orderDate, "userLogin", userLogin))/>
                  ${affPartyNameResult.fullName?default("[${uiLabelMap.OrderPartyNameNotFound}]")}
                </div>
              </td>
            </tr>
            </#if>

            
            <#if orderContentWrapper.get("IMAGE_URL")?has_content>
            <tr>
              <td>${uiLabelMap.OrderImage}</td>
              <td colspan="3">
                  <a href="<@ofbizUrl>viewimage?orderId=${orderId}&amp;orderContentTypeId=IMAGE_URL</@ofbizUrl>" target="_orderImage" class="button tiny">${uiLabelMap.OrderViewImage}</a>
              </td>
            </tr>
            </#if>

            <#if "SALES_ORDER" == orderHeader.orderTypeId>

                <tr>
                  <td>${uiLabelMap.FormFieldTitle_priority}</td>
                  <td colspan="2">
                     <form name="setOrderReservationPriority" method="post" action="<@ofbizUrl>setOrderReservationPriority</@ofbizUrl>">
                     <input type = "hidden" name="orderId" value="${orderId}"/>
                    <select name="priority">
                      <option value="1" <#if (orderHeader.priority)! == "1">selected="selected" </#if>>${uiLabelMap.CommonHigh}</option>
                      <option value="2" <#if (orderHeader.priority)! == "2">selected="selected" <#elseif !(orderHeader.priority)?has_content>selected="selected"</#if>>${uiLabelMap.CommonNormal}</option>
                      <option value="3" <#if (orderHeader.priority)! == "3">selected="selected" </#if>>${uiLabelMap.CommonLow}</option>
                    </select>
                    </td>
                    <td>
                    <input type="submit" class="smallSubmit" value="${uiLabelMap.FormFieldTitle_reserveInventory}"/>
                    </td>
                    </form>
                  </td>
                </tr>
            </#if>
            <tr>
              <td>${uiLabelMap.AccountingInvoicePerShipment}</td>
              <td colspan="2">
                 <form name="setInvoicePerShipment" method="post" action="<@ofbizUrl>setInvoicePerShipment</@ofbizUrl>">
                 <input type = "hidden" name="orderId" value="${orderId}"/>
                <select name="invoicePerShipment">
                  <option value="Y" <#if (orderHeader.invoicePerShipment)! == "Y">selected="selected" </#if>>${uiLabelMap.CommonYes}</option>
                  <option value="N" <#if (orderHeader.invoicePerShipment)! == "N">selected="selected" </#if>>${uiLabelMap.CommonNo}</option>
                </select>
                </td>
                <td>
                <input type="submit" class="smallSubmit" value="${uiLabelMap.CommonUpdate}"/>
                </td>
                </form>
              </td>
            </tr>
            <#if orderHeader.isViewed?has_content && orderHeader.isViewed == "Y">
            <tr>
              <td>${uiLabelMap.OrderViewed}</td>
              <td colspan="3">
                ${uiLabelMap.CommonYes}
              </td>
            </tr>
            <#else>
            <tr id="isViewed">
              <td>${uiLabelMap.OrderMarkViewed}</td>
              <td colspan="3">
                <form id="orderViewed" action="">
                  <input type="checkbox" name="checkViewed" onclick="javascript:markOrderViewed();"/>
                  <input type="hidden" name="orderId" value="${orderId!}"/>
                  <input type="hidden" name="isViewed" value="Y"/>
                </form>
              </td>
            </tr>
            <tr id="viewed" style="display: none;">
              <td>${uiLabelMap.OrderViewed}</td>
              <td colspan="3">
                ${uiLabelMap.CommonYes}
              </td>
            </tr>
            </#if>
        </table>
    </@section>
