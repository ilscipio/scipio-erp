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

<#if security.hasEntityPermission("FACILITY", "_VIEW", session)>
  <#if !(showWarningForm)>
    <@section title="${uiLabelMap.ProductWeighPackageOnly} ${uiLabelMap.CommonIn} ${facility.facilityName!} [${(facility.facilityId)!}]">
        <#if invoiceIds?has_content>
          <div>
            ${uiLabelMap.CommonView} <a href="<@ofbizUrl>/PackingSlip.pdf?shipmentId=${shipmentId}</@ofbizUrl>" target="_blank" class="${styles.button_default!}">${uiLabelMap.ProductPackingSlip}</a> ${uiLabelMap.CommonOr}
            ${uiLabelMap.CommonView} <a href="<@ofbizUrl>/ShipmentBarCode.pdf?shipmentId=${shipmentId}</@ofbizUrl>" target="_blank" class="${styles.button_default!}">${uiLabelMap.ProductBarcode}</a> ${uiLabelMap.CommonFor} ${uiLabelMap.ProductShipmentId} <a href="<@ofbizUrl>/ViewShipment?shipmentId=${shipmentId}</@ofbizUrl>" class="${styles.button_default!}">${shipmentId}</a>
          </div>
          <#if invoiceIds?? && invoiceIds?has_content>
            <div>
              <p>${uiLabelMap.AccountingInvoices}:</p>
              <@menu type="button">
                <#list invoiceIds as invoiceId>
                  <@menuitem type="generic">
                    ${uiLabelMap.CommonNbr} <a href="/accounting/control/invoiceOverview?invoiceId=${invoiceId}${StringUtil.wrapString(externalKeyParam)}" target="_blank" class="${styles.menu_button_itemlink!}">${invoiceId}</a>
                    (<a href="/accounting/control/invoice.pdf?invoiceId=${invoiceId}${StringUtil.wrapString(externalKeyParam)}" target="_blank" class="${styles.menu_button_itemlink!}">PDF</a>)
                  </@menuitem>
                </#list>
              </@menu>
            </div>
          </#if>
        </#if>
        <#if shipmentPackageRouteSegList?? && shipmentPackageRouteSegList?has_content>
          <#list shipmentPackageRouteSegList as shipmentPackageRouteSeg>
            <form name="viewShipmentPackageRouteSegLabelImageForm_${shipmentPackageRouteSeg_index}" method="post" action="<@ofbizUrl>viewShipmentPackageRouteSegLabelImage</@ofbizUrl>">
              <input type="hidden" name="shipmentId" value ="${(shipmentPackageRouteSeg.shipmentId)!}"/>
              <input type="hidden" name ="shipmentPackageSeqId" value = "${(shipmentPackageRouteSeg.shipmentPackageSeqId)!}"/>
              <input type="hidden" name="shipmentRouteSegmentId" value ="${(shipmentPackageRouteSeg.shipmentRouteSegmentId)!}"/>
              <div>
                <span>${uiLabelMap.ProductPackage}</span> ${(shipmentPackageRouteSeg.shipmentPackageSeqId)!}
                <#if shipmentPackageRouteSeg.labelImage??>
                  <a href="javascript:document.viewShipmentPackageRouteSegLabelImageForm_${shipmentPackageRouteSeg_index}.submit();" class="${styles.button_default!}">${uiLabelMap.ProductViewLabelImage}</a>
                </#if>
              </div>
            </form>
          </#list>
        </#if>
     
        <#if !(orderId?has_content)>
          <@section>
          <form name="selectOrderForm" method="post" action="<@ofbizUrl>WeightPackageOnly</@ofbizUrl>">
            <input type="hidden" name="facilityId" value="${(facility.facilityId)!}" />
              <@field type="generic" label="${uiLabelMap.ProductOrderId}">
                  <input type="text" name="orderId" size="20" maxlength="20" value="${primaryOrderId!}"/>
                  /
                  <input type="text" name="shipGroupSeqId" size="6" maxlength="6" value="${shipGroupSeqId?default("00001")}"/>
              </@field>
              <@field type="submitarea">
                  <a href="javascript:document.selectOrderForm.action='<@ofbizUrl>PackOrder</@ofbizUrl>';document.selectOrderForm.submit();" class="${styles.button_default!}">${uiLabelMap.ProductPackOrder}</a>
                  <a href="javascript:document.selectOrderForm.submit();" class="${styles.button_default!}">${uiLabelMap.ProductWeighPackageOnly}</a>
              </@field>
          </form>
          </@section>
         
          <#-- select picklist bin form -->
          <@section>
          <form name="selectPicklistBinForm" method="post" action="<@ofbizUrl>WeightPackageOnly</@ofbizUrl>">
            <input type="hidden" name="facilityId" value="${(facility.facilityId)!}" />
              <@field type="generic" label="${uiLabelMap.FormFieldTitle_picklistBinId}">
                  <input type="text" name="picklistBinId" size="29" maxlength="60" value="${picklistBinId!}"/>
              </@field>
              <@field type="submitarea">
                  <a href="javascript:document.selectPicklistBinForm.action='<@ofbizUrl>PackOrder</@ofbizUrl>';document.selectPicklistBinForm.submit();" class="${styles.button_default!}">${uiLabelMap.ProductPackOrder}</a>
                  <a href="javascript:document.selectPicklistBinForm.submit();" class="${styles.button_default!}">${uiLabelMap.ProductWeighPackageOnly}</a>
              </@field>
          </form>
          </@section>
        <#else>
          <#assign packedLines = weightPackageSession.getPackedLines(orderId)/>
          <#if !(shipmentPackages?has_content)>
            <@section>
            <#if packedLines?has_content>
              <@table type="data-list" class="${styles.table_default!}" cellpadding="2" cellspacing="0">
                <@thead>
                <@tr>
                  <@th>
                    ${uiLabelMap.ProductPackedWeight} (${("uiLabelMap.ProductShipmentUomAbbreviation_" + defaultWeightUomId)?eval}):
                  </@th>
                  <@th>
                    ${uiLabelMap.CommonDimension} (${("uiLabelMap.ProductShipmentUomAbbreviation_" + defaultDimensionUomId)?eval}):
                  </@th>
                  <@th>
                    ${uiLabelMap.ProductPackageInputBox}:
                  </@th>
                </@tr>
                </@thead>
                <@tbody>
                <#list packedLines as packedLine>
                  <form name="updateWeightPackageForm_${packedLine.getWeightPackageSeqId()}" method="post" action="<@ofbizUrl>updatePackedLine</@ofbizUrl>">
                    <input type="hidden" name="orderId" value ="${orderId!}"/>
                    <input type="hidden" name = "facilityId" value = "${(facility.facilityId)!}"/>
                    <input type="hidden" name="weightPackageSeqId" value ="${packedLine.getWeightPackageSeqId()}"/>
                    <@tr>
                      <@td>
                          ${uiLabelMap.ProductPackage} ${packedLine.getWeightPackageSeqId()}
                          <input type="text" size="7" name="packageWeight" value="${(packedLine.getPackageWeight())!}" />
                      </@td>
                      <@td>
                        <span>${uiLabelMap.CommonLength}<input type="text" name="packageLength" value="${(packedLine.getPackageLength())!}" size="5"/></span>
                        <span>${uiLabelMap.ProductWidth}<input type="text" name="packageWidth" value="${(packedLine.getPackageWidth())!}" size="5"/></span>
                        <span>${uiLabelMap.PartyHeight}<input type="text" name="packageHeight" value="${(packedLine.getPackageHeight())!}" size="5"/></span>
                      </@td>
                      <@td>
                        <select name="shipmentBoxTypeId">
                          <#if shipmentBoxTypes?has_content>
                            <#assign shipmentBoxTypeId = "${(packedLine.getShipmentBoxTypeId())!}"/>
                            <#list shipmentBoxTypes as shipmentBoxType>
                              <#if shipmentBoxTypeId == "${shipmentBoxType.shipmentBoxTypeId}">
                                <option value="${shipmentBoxType.shipmentBoxTypeId}">${shipmentBoxType.description}</option>
                              </#if>
                            </#list>
                            <option value=""></option>
                            <#list shipmentBoxTypes as shipmentBoxType>
                              <option value="${shipmentBoxType.shipmentBoxTypeId}">${shipmentBoxType.description}</option>
                            </#list>
                          </#if>
                        </select>
                      </@td>
                      <@td align="right"><a href="javascript:document.updateWeightPackageForm_${packedLine.getWeightPackageSeqId()}.submit()" class="${styles.button_default!}">${uiLabelMap.CommonUpdate}</a></@td>
                      <@td align="right"><a href="javascript:document.updateWeightPackageForm_${packedLine.getWeightPackageSeqId()}.action='<@ofbizUrl>deletePackedLine</@ofbizUrl>';document.updateWeightPackageForm_${packedLine.getWeightPackageSeqId()}.submit();" class="${styles.button_default!}">${uiLabelMap.CommonDelete}</a></@td>
                    </@tr>
                  </form>
                </#list>
                </@tbody>
              </@table>
              <div align="right">
                <a href="javascript:document.completePackageForm.submit()" class="${styles.button_default!}">${uiLabelMap.ProductComplete}</a>
              </div>
              <form name="completePackageForm" method ="post" action="<@ofbizUrl>completePackage</@ofbizUrl>">
                <input type="hidden" name="orderId" value="${orderId!}"/>
                <input type="hidden" name="shipGroupSeqId" value="${shipGroupSeqId!}"/>
                <input type="hidden" name="facilityId" value="${(facility.facilityId)!}"/>
                <input type="hidden" name="weightUomId" value="${defaultWeightUomId}"/>
                <input type="hidden" name="dimensionUomId" value="${defaultDimensionUomId}"/>
                <input type="hidden" name="shipmentId" value="${(shipment.shipmentId)!}"/>
                <input type="hidden" name="invoiceId" value="${(invoice.invoiceId)!}"/>
                <input type="hidden" name="estimatedShippingCost" value="${estimatedShippingCost!}"/>
                <input type="hidden" name="newEstimatedShippingCost" value="${newEstimatedShippingCost!}"/>
              </form>
            </#if>
            <#if (orderedQuantity > packedLines.size())>
            <@table type="fields" class="${styles.table_default!}" cellpadding="2" cellspacing="0">
              <form name="weightPackageForm" method ="post" action="<@ofbizUrl>setPackageInfo</@ofbizUrl>">
                <input type="hidden" name = "shipGroupSeqId" value = "${shipGroupSeqId!}"/>
                <input type="hidden" name = "facilityId" value = "${(facility.facilityId)!}"/>
                <input type="hidden" name = "orderId" value = "${orderId!}"/>
                <#assign packedLines = weightPackageSession.getPackedLines(orderId)/>
                <#if packedLines?has_content>
                  <hr/>
                </#if>
                <@tr>
                  <@td>${uiLabelMap.ProductPackedWeight} (${("uiLabelMap.ProductShipmentUomAbbreviation_" + defaultWeightUomId)?eval}):
                      <br />
                      ${uiLabelMap.ProductPackage}
                      <input type="text" size="7" name="packageWeight" value=""/>
                  </@td>
                  <@td>
                    <span>${uiLabelMap.CommonDimension} (${("uiLabelMap.ProductShipmentUomAbbreviation_" + defaultDimensionUomId)?eval}):</span>
                    <br />
                    <span>${uiLabelMap.CommonLength}<input type="text" name="packageLength" value="" size="5"/></span>
                    <span>${uiLabelMap.ProductWidth}<input type="text" name="packageWidth" value="" size="5"/></span>
                    <span>${uiLabelMap.PartyHeight}<input type="text" name="packageHeight" value="" size="5"/></span>
                  </@td>
                  <@td>
                    <span>${uiLabelMap.ProductPackageInputBox}:</span>
                    <br />
                    <select name="shipmentBoxTypeId">
                      <#if shipmentBoxTypes?has_content>
                        <option value=""></option>
                        <#list shipmentBoxTypes as shipmentBoxType>
                          <option value="${shipmentBoxType.shipmentBoxTypeId}">${shipmentBoxType.description}</option>
                        </#list>
                      </#if>
                    </select>
                  </@td>
                  <@td align="right"><a href="javascript:document.weightPackageForm.submit()" class="${styles.button_default!}">${uiLabelMap.ProductNextPackage}</a></@td>
                </@tr>
              </form>
            </@table>
            </#if>
            </@section>
          <#else>
            <@section>
            <@table type="data-list" class="${styles.table_default!}" cellpadding="2" cellspacing="0"> 
             <@thead>
             <@tr>
                <@th>
                 ${uiLabelMap.ProductPackedWeight} (${("uiLabelMap.ProductShipmentUomAbbreviation_" + defaultWeightUomId)?eval}):
                </@th>
                 <@th>
                  ${uiLabelMap.CommonDimension} (${("uiLabelMap.ProductShipmentUomAbbreviation_" + defaultDimensionUomId)?eval}):
                </@th>
                <@th>
                  ${uiLabelMap.ProductPackageInputBox}:
               </@th>
              </@tr>
             </@thead>
             <@tbody>
              <form name="completePackForm" method="post" action="<@ofbizUrl>shipNow</@ofbizUrl>">
                <input type="hidden" name="orderId" value="${orderId!}"/>
                <input type="hidden" name="shipGroupSeqId" value="${shipGroupSeqId!}"/>
                <input type="hidden" name="facilityId" value="${(facility.facilityId)!}"/>
                <input type="hidden" name="shipmentId" value="${(shipment.shipmentId)!}"/>
                <input type="hidden" name="invoiceId" value="${(invoice.invoiceId)!}"/> 
                <#list shipmentPackages?sort_by("shipmentPackageSeqId") as shipmentPackage>
                  <@tr>
                    <@td>
                        ${uiLabelMap.ProductPackage} ${(shipmentPackage_index + 1)}
                        <input type="text" size="7" readonly="readonly" name="packageWeight" value="${(shipmentPackage.weight)!}" />
                    </@td>
                    <@td>
                      <span>${uiLabelMap.CommonLength}<input type="text" readonly="readonly" name="packageLength" value="${(shipmentPackage.boxLength)!}" size="5"/></span>
                      <span>${uiLabelMap.ProductWidth}<input type="text" readonly="readonly" name="packageWidth" value="${(shipmentPackage.boxWidth)!}" size="5"/></span>
                      <span>${uiLabelMap.PartyHeight}<input type="text" readonly="readonly" name="packageHeight" value="${(shipmentPackage.boxHeight)!}" size="5"/></span>
                    </@td>
                    <@td>
                      <#if (shipmentPackage.shipmentBoxTypeId)?has_content>
                        <#assign shipmentBoxType = delegator.findOne("ShipmentBoxType", Static["org.ofbiz.base.util.UtilMisc"].toMap("shipmentBoxTypeId", shipmentPackage.shipmentBoxTypeId), true)>
                      </#if>
                      <input type="text" readonly="readonly" name="shipmentBoxTypeId" value="${(shipmentBoxType.description)!}" size="50"/>
                    </@td>
                  </@tr>
                </#list>
              </form>
              </@tbody>
            </@table>
            <div align="right">
              <a href="javascript:document.completePackForm.submit()" class="${styles.button_default!}">${uiLabelMap.ProductComplete}</a>
            </div>
            </@section>
          </#if>
        </#if>
    </@section>
  <#else>
    <@section title="${uiLabelMap.WebtoolsWarningLogLevel}">
        <@alert type="warning">${uiLabelMap.FacilityWarningMessageThereIsMuchDifferenceInShippingCharges}&nbsp;[${uiLabelMap.FacilityEstimatedShippingCharges} = <@ofbizCurrency amount=estimatedShippingCost! isoCode=shipment.currencyUomId!/>, ${uiLabelMap.FacilityActualShippingCharges} = <@ofbizCurrency amount=newEstimatedShippingCost! isoCode=shipment.currencyUomId!/>]</@alert>
        <form name="shipNowForm" method="post" action="<@ofbizUrl>shipNow</@ofbizUrl>">
          <input type="hidden" name="orderId" value="${orderId!}"/>
          <input type="hidden" name="shipGroupSeqId" value="${shipGroupSeqId!}"/>
          <input type="hidden" name="facilityId" value="${(facility.facilityId)!}"/>
          <input type="hidden" name="shipmentId" value="${(shipment.shipmentId)!}"/>
        </form>
        <form name="holdShipmentForm" method="post" action="<@ofbizUrl>HoldShipment</@ofbizUrl>">
          <input type="hidden" name="orderId" value="${orderId!}"/>
          <input type="hidden" name="shipGroupSeqId" value="${shipGroupSeqId!}"/>
          <input type="hidden" name="facilityId" value="${(facility.facilityId)!}"/>
        </form>
        <div>
          <a href="javascript:document.shipNowForm.submit()" class="${styles.button_default!}">${uiLabelMap.FacilityShip} ${uiLabelMap.CommonNow}</a>
          &nbsp;
          <a href="javascript:document.holdShipmentForm.submit()" class="${styles.button_default!}">${uiLabelMap.FacilityHoldShipment}</a>
        </div>
    </@section>
  </#if>
</#if>
