<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@script>
    function clearLine(facilityId, orderId, orderItemSeqId, productId, shipGroupSeqId, inventoryItemId, packageSeqId) {
        document.clearPackLineForm.facilityId.value = facilityId;
        document.clearPackLineForm.orderId.value = orderId;
        document.clearPackLineForm.orderItemSeqId.value = orderItemSeqId;
        document.clearPackLineForm.productId.value = productId;
        document.clearPackLineForm.shipGroupSeqId.value = shipGroupSeqId;
        document.clearPackLineForm.inventoryItemId.value = inventoryItemId;
        document.clearPackLineForm.packageSeqId.value = packageSeqId;
        document.clearPackLineForm.submit();
    }
</@script>

<#if security.hasEntityPermission("FACILITY", "_VIEW", request)>
    <#assign showInput = requestParameters.showInput?default("Y")>    

    <#if (requestParameters.forceComplete?has_content && !invoiceIds?has_content)>
        <#assign forceComplete = "true">
        <#assign showInput = "Y">
    </#if>

    <#macro globalMenuContent menuArgs={}>
        <#if invoiceIds?has_content>
            <@menu type="button">
                <#list invoiceIds as invoiceId>
                    <@menuitem type="link" href=makeServerUrl("/accounting/control/invoiceOverview?invoiceId=" + invoiceId + "&" + raw(externalKeyParam)) class="+${styles.action_nav!} ${styles.action_view!}" text=(uiLabelMap.AccountingInvoice + " " + uiLabelMap.CommonNbr + " " + invoiceId) />
                    (<@menuitem type="link" href=makeServerUrl("/accounting/control/invoice.pdf?invoiceId=" + invoiceId + "&" + raw(externalKeyParam)) class="+${styles.action_nav!} ${styles.action_export!}" text="PDF"  />)
                </#list>
            </@menu>
        </#if>
    </#macro>

    <@section menuContent=globalMenuContent>
        <#-- select order form -->
        <#-- select picklist bin form -->
        <#if !orderId?has_content && !picklistBinId?has_content>
            <@section>
                <form name="selectOrderForm" method="post" action="<@pageUrl>PackOrder</@pageUrl>">
                    <input type="hidden" name="facilityId" value="${facilityId!}" />
                    <@field type="generic" label=uiLabelMap.ProductOrderId>
                        <@field type="lookup" formName="selectOrderForm" name="orderId" id="orderId" size="20" maxlength="20" fieldFormName="LookupOrderHeader"/>
                        /
                        <@field type="input" inline=true name="shipGroupSeqId" size="6" maxlength="6" value=shipGroupSeqId!'00001'/>
                    </@field>
                    <@field type="generic" label=uiLabelMap.FormFieldTitle_picklistBinId>
                        <@field type="input" name="picklistBinId" size="29" maxlength="60" value=(picklistBinId!)/>
                    </@field>
                    <@field type="submitarea">
                        <@field type="submit" submitType="link" href="javascript:document.selectOrderForm.submit();" class="+${styles.link_run_sys!} ${styles.action_update!}" text=uiLabelMap.ProductPackOrder />
                        <@field type="submit" submitType="link" href="javascript:document.selectOrderForm.action='${makePageUrl('WeightPackageOnly')}';document.selectOrderForm.submit();" class="+${styles.link_run_sys!} ${styles.action_verify!}" text=uiLabelMap.ProductWeighPackageOnly />
                    </@field>
                </form>
            </@section>
        </#if>

        <form name="clearPackForm" method="post" action="<@pageUrl>ClearPackAll</@pageUrl>">
          <input type="hidden" name="orderId" value="${orderId!}"/>
          <input type="hidden" name="shipGroupSeqId" value="${shipGroupSeqId!}"/>
          <input type="hidden" name="facilityId" value="${facilityId!}"/>
        </form>
        <form name="incPkgSeq" method="post" action="<@pageUrl>SetNextPackageSeq</@pageUrl>">
          <input type="hidden" name="orderId" value="${orderId!}"/>
          <input type="hidden" name="shipGroupSeqId" value="${shipGroupSeqId!}"/>
          <input type="hidden" name="facilityId" value="${facilityId!}"/>
        </form>
        <form name="clearPackLineForm" method="post" action="<@pageUrl>ClearPackLine</@pageUrl>">
            <input type="hidden" name="facilityId"/>
            <input type="hidden" name="orderId"/>
            <input type="hidden" name="orderItemSeqId"/>
            <input type="hidden" name="productId"/>
            <input type="hidden" name="shipGroupSeqId"/>
            <input type="hidden" name="inventoryItemId"/>
            <input type="hidden" name="packageSeqId"/>
        </form>


    <#if showInput != "N" && ((orderHeader?exists && orderHeader?has_content))>
        <#macro menuContent menuArgs={}>
            <@menu type="button">
                <@menuitem type="link" href=makePageUrl("PackingSlip.pdf?shipmentId=" + shipmentId) class="+${styles.action_nav!} ${styles.action_view!}" text=(uiLabelMap.ProductPackingSlip + " " + uiLabelMap.CommonNbr + " " + invoiceId) />
                <@menuitem type="link" href=makePageUrl("ShipmentBarCode.pdf?shipmentId=" + shipmentId) class="+${styles.action_nav!} ${styles.action_view!}" text=(uiLabelMap.ProductPackingSlip + " " + uiLabelMap.CommonNbr + " " + invoiceId) />

                <@menuitem type="link" href=makePageUrl("ShipmentBarCode.pdf?shipmentId=" + shipmentId) class="+${styles.action_nav!} ${styles.action_view!}" text=(uiLabelMap.ProductPackingSlip + " " + uiLabelMap.CommonNbr + " " + invoiceId) />

                <a href="<@pageUrl>EditShipment?shipmentId=${shipmentId}</@pageUrl>" class="${styles.link_nav_info_id!}">${shipmentId}</a>

            </@menu>
        </#macro>

        <#assign sectionTitle>${getLabel('ProductOrderId')} <a href="<@serverUrl>/ordermgr/control/orderview?orderId=${orderId}</@serverUrl>">${orderId}</a> / ${getLabel('ProductOrderShipGroupId')} #${shipGroupSeqId}</#assign>
        <@section title=wrapAsRaw(sectionTitle, 'htmlmarkup') menuContent=menuContent>
            <#if orderItemShipGroup?has_content>
                <#if (orderItemShipGroup.contactMechId)?has_content>
                    <#assign postalAddress = orderItemShipGroup.getRelatedOne("PostalAddress", false)>
                </#if>
                <#assign carrier = orderItemShipGroup.carrierPartyId!(uiLabelMap.CommonNA)>
                <@row>
                    <@cell columns=4>
                        <#if postalAddress?exists >
                            <@heading><strong>${uiLabelMap.ProductShipToAddress}</strong></@heading>
                            ${uiLabelMap.CommonTo}: ${postalAddress.toName!""}<br />
                            <#if postalAddress.attnName?has_content>
                                ${uiLabelMap.CommonAttn}: ${postalAddress.attnName}<br />
                            </#if>
                            ${postalAddress.address1}<br/>
                            <#if postalAddress.address2?has_content>
                                ${postalAddress.address2}<br/>
                            </#if>
                            ${postalAddress.city!}, ${postalAddress.stateProvinceGeoId!} ${postalAddress.postalCode!}<br />
                            ${postalAddress.countryGeoId!}<br/>
                        </#if>
                    </@cell>                       
                    <@cell columns=4>
                        <@heading><strong>${uiLabelMap.ProductCarrierShipmentMethod}</strong></@heading>
                        <#if carrier == "USPS">
                            <#assign color = "red">
                        <#elseif carrier == "UPS">
                            <#assign color = "green">
                        <#else>
                            <#assign color = "black">
                        </#if>
                        <#if carrier != "_NA_">
                            <font color="${color}">${carrier}</font>&nbsp;
                        </#if>
                        <#if orderItemShipGroup.shipmentMethodTypeId?has_content>
                            <#assign description = (delegator.findOne("ShipmentMethodType", {"shipmentMethodTypeId":orderItemShipGroup.shipmentMethodTypeId}, false)).description>
                        </#if>
                        ${description!"??"}<br/>
                        ${uiLabelMap.ProductEstimatedShipCostForShipGroup}<br />
                        <#if shipmentCostEstimateForShipGroup?exists>
                            <@ofbizCurrency amount=shipmentCostEstimateForShipGroup isoCode=(orderReadHelper.getCurrency()!)/><br />
                        </#if>
                    </@cell>                  
                    <@cell columns=4>
                        <@heading><strong>${uiLabelMap.OrderInstructions}</strong></@heading>
                        ${orderItemShipGroup.shippingInstructions?default("(${uiLabelMap.CommonNone})")}
                    </@cell>
                </@row>                    
            </#if>
    
            <#-- manual per item form -->
            <#assign itemInfos = packingSession.getItemInfos()!>
            <#if showInput != "N" && itemInfos?has_content>
                <#assign sectionTitle="${rawLabel('ProductProduct')} ${rawLabel('ProductToPack')}"/>
                <@section title=sectionTitle>
                    <form name="singlePackForm" method="post" action="<@pageUrl>ProcessPackOrder</@pageUrl>">                    
                        <input type="hidden" name="packageSeq" value="${packingSession.getCurrentPackageSeq()}"/>
                        <input type="hidden" name="orderId" value="${orderId}"/>
                        <input type="hidden" name="shipGroupSeqId" value="${shipGroupSeqId}"/>
                        <input type="hidden" name="facilityId" value="${facilityId!}"/>
                        <@field type="lookup" formName="singlePackForm" name="productId"id="productId" size="20" maxlength="20" fieldFormName="LookupProduct" label=uiLabelMap.ProductProductId/>                        
                        <@field type="input"  name="quantity" size="6" maxlength="6" value="1" label=uiLabelMap.ProductQuantity/>                        
                        <@field type="display" label=uiLabelMap.ProductCurrentPackageSequence>
                            ${packingSession.getCurrentPackageSeq()}
                        </@field>
                        <@field type="submitarea">
                            <@field type="submit" submitType="link" href="javascript:document.singlePackForm.submit();" class="+${styles.link_run_sys!} ${styles.action_update!}" text=uiLabelMap.ProductPackItem />
                            <@field type="submit" submitType="input-button" text=uiLabelMap.ProductNextPackage onClick="javascript:document.incPkgSeq.submit();" />
                        </@field>                    
                    </form>
                </@section>       
    
                <#-- auto grid form -->
                <#assign sectionTitle="${rawLabel('ProductProducts')} ${rawLabel('ProductToPack')}"/>
                <@section title=sectionTitle>
                    <form name="multiPackForm" method="post" action="<@pageUrl>ProcessBulkPackOrder</@pageUrl>">
                        <@fields type="default-manual">
                            <input type="hidden" name="facilityId" value="${facilityId!}" />
                            <input type="hidden" name="orderId" value="${orderId!}" />
                            <input type="hidden" name="shipGroupSeqId" value="${shipGroupSeqId!}" />
                            <input type="hidden" name="originFacilityId" value="${facilityId!}" />
                            <input name="_useRowSubmit" type="hidden" value="Y"/>
                            <@table type="data-list" autoAltRows=true scrollable=true responsive=true>
                                <@thead>
                                    <@tr class="header-row">                                    
                                        <@th>${uiLabelMap.ProductItem} ${uiLabelMap.CommonNbr}</@th>
                                        <@th>${uiLabelMap.ProductProductId}</@th>
                                        <@th>${uiLabelMap.ProductInternalName}</@th>
                                        <@th>${uiLabelMap.ProductOrderedQuantity}</@th>
                                        <@th>${uiLabelMap.ProductQuantityShipped}</@th>
                                        <@th>${uiLabelMap.ProductPackedQty}</@th>
                                        <@th>&nbsp;</@th>
                                        <@th>${uiLabelMap.ProductPackQty}</@th>
                                        <@th>${uiLabelMap.ProductPackedWeight}&nbsp;(${("uiLabelMap.ProductShipmentUomAbbreviation_" + defaultWeightUomId)?eval})</@th>
                                        <@th>${uiLabelMap.ProductPackage}</@th>
                                        <@th>*&nbsp;${uiLabelMap.ProductPackages}</@th>                                    
                                        <@th>${uiLabelMap.ProductPackItem}</@th>
                                    </@tr>
                                </@thead>
                                <#if (itemInfos?has_content)>
                                    <#list itemInfos as itemInfo>
                                        <#assign orderItem = itemInfo.orderItem/>
                                        <#assign shippedQuantity = orderReadHelper.getItemShippedQuantity(orderItem)!>
                                        <#assign orderItemQuantity = itemInfo.quantity/>
                                        <#assign orderProduct = orderItem.getRelatedOne("Product", false)!/>
                                        <#assign product = Static["org.ofbiz.product.product.ProductWorker"].findProduct(delegator, itemInfo.productId)!/>                             
                                        <#assign inputQty = orderItemQuantity - packingSession.getPackedQuantity(orderId, orderItem.orderItemSeqId, shipGroupSeqId, itemInfo.productId)>
                                        <@tr>
                                            <@td>${orderItem.orderItemSeqId}</@td>
                                            <@td>
                                                ${orderProduct.productId!(uiLabelMap.CommonNA)}
                                                <#if orderProduct.productId != product.productId>
                                                    &nbsp;${product.productId!(uiLabelMap.CommonNA)}
                                                </#if>
                                            </@td>
                                            <@td>
                                                <a href="<@serverUrl>/catalog/control/ViewProduct?productId=${orderProduct.productId!}${raw(externalKeyParam)}</@serverUrl>" class="${styles.link_nav_info_name!}" target="_blank">${(orderProduct.internalName)!}</a>
                                                <#if orderProduct.productId != product.productId>
                                                    &nbsp;[<a href="<@serverUrl>/catalog/control/ViewProduct?productId=${product.productId!}${raw(externalKeyParam)}</@serverUrl>" class="${styles.link_nav_info_name!}" target="_blank">${(product.internalName)!}</a>]
                                                </#if>
                                            </@td>
                                            <@td>${orderItemQuantity}</@td>
                                            <@td>${shippedQuantity!0}</@td>
                                            <@td>${packingSession.getPackedQuantity(orderId, orderItem.orderItemSeqId, shipGroupSeqId, itemInfo.productId)}</@td>
                                            <@td>&nbsp;</@td>
                                            <@td>
                                                <@field type="input" size="7" name="qty_o_${itemInfo_index}" value=inputQty />
                                            </@td>
                                            <@td>
                                                <@field type="input" size="7" name="wgt_o_${itemInfo_index}" value="" />
                                            </@td>
                                            <@td>
                                                <@field type="select" name="pkg_o_${itemInfo_index}">
                                                    <#if packingSession.getPackageSeqIds()?exists>
                                                        <#list packingSession.getPackageSeqIds() as packageSeqId>
                                                            <option value="${packageSeqId}">${uiLabelMap.ProductPackage} ${packageSeqId}</option>
                                                        </#list>
                                                        <#assign nextPackageSeqId = packingSession.getPackageSeqIds().size() + 1>
                                                        <option value="${nextPackageSeqId}">${uiLabelMap.ProductNextPackage}</option>
                                                    <#else>
                                                        <option value="1">${uiLabelMap.ProductPackage} 1</option>
                                                        <option value="2">${uiLabelMap.ProductPackage} 2</option>
                                                        <option value="3">${uiLabelMap.ProductPackage} 3</option>
                                                        <option value="4">${uiLabelMap.ProductPackage} 4</option>
                                                        <option value="5">${uiLabelMap.ProductPackage} 5</option>
                                                    </#if>
                                                </@field>
                                            </@td>
                                            <@td>
                                                <@field type="input" size="7" name="numPackages_o_${itemInfo_index}" value="1" />
                                            </@td>
                                            <@td>
                                                <a href="javascript:$('input[name=_rowSubmit_o_${itemInfo_index}]').val('Y');document.multiPackForm.submit();" class="+${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.ProductPackItem}</a>
                                            </@td>
                                            <input type="hidden" name="prd_o_${itemInfo_index}" value="${itemInfo.productId!}"/>
                                            <input type="hidden" name="ite_o_${itemInfo_index}" value="${orderItem.orderItemSeqId}"/>
                                            <input name="_rowSubmit_o_${itemInfo_index}" type="hidden" value="N"/>                     
                                        </@tr>
                                    </#list>
                                </#if>
                                <@tfoot>                                
                                    <@tr>
                                        <@td colspan="12">
                                            <@field type="submitarea">                                            
                                                <@field type="submit" text="${rawLabel('CommonClear')} (${rawLabel('CommonAll')})" onClick="javascript:document.clearPackForm.submit();"/>
                                            </@field>
                                        </@td>
                                    </@tr>
                                </@tfoot>
                            </@table>
                        </@fields>
                    </form>
                </@section>
            </#if>
    
            <#-- complete form -->
            <#assign packageSeqIds = packingSession.getPackageSeqIds()/>

            <#if showInput != "N" && packageSeqIds?has_content>
                ${Debug.log("packageSeqIds ===> " + packageSeqIds)}
                <@section>
                    <form name="completePackForm" method="post" action="<@pageUrl>CompletePack</@pageUrl>">
                        <@fields type="default-manual">
                            <input type="hidden" name="orderId" value="${orderId!}"/>
                            <input type="hidden" name="shipGroupSeqId" value="${shipGroupSeqId!}"/>
                            <input type="hidden" name="facilityId" value="${facilityId!}"/>
                            <input type="hidden" name="forceComplete" value="${forceComplete!'false'}"/>
                            <input type="hidden" name="weightUomId" value="${defaultWeightUomId}"/>
                            <input type="hidden" name="showInput" value="N"/>
                            
    
                            <@table type="fields" class="+${styles.table_spacing_tiny_hint!}">
                                <@thead>
                                    <@tr>
                                        <@th>${uiLabelMap.ProductPackedWeight} (${("uiLabelMap.ProductShipmentUomAbbreviation_" + defaultWeightUomId)?eval})</@th>
                                        <@th>${uiLabelMap.ProductShipmentBoxType}</@th>
                                        <@th>${uiLabelMap.ProductAdditionalShippingCharge}</@th>
                                        <@th>${uiLabelMap.ProductHandlingInstructions}</@th>
                                        <@th>${uiLabelMap.ProductComplete}</@th>                                       
                                    </@tr>
                                </@thead>
                                <#list packageSeqIds as packageSeqId>
                                    <@tr>
                                        <@td>
                                            <#assign packageWeightLabel>${rawLabel('ProductPackage')} ${raw(packageSeqId)}</#assign>
                                            <@field type="input" size="7" name="packageWeight_${packageSeqId}" value=(packingSession.getPackageWeight(packageSeqId?int)!) label=packageWeightLabel/>
                                            <#if orderItemShipGroup?has_content>
                                                <input type="hidden" name="shippingContactMechId" value="${orderItemShipGroup.contactMechId!}"/>
                                                <input type="hidden" name="shipmentMethodTypeId" value="${orderItemShipGroup.shipmentMethodTypeId!}"/>
                                                <input type="hidden" name="carrierPartyId" value="${orderItemShipGroup.carrierPartyId!}"/>
                                                <input type="hidden" name="carrierRoleTypeId" value="${orderItemShipGroup.carrierRoleTypeId!}"/>
                                                <input type="hidden" name="productStoreId" value="${productStoreId!}"/>
                                            </#if>
                                        </@td>
                                        <@td>
                                            <#if carrierShipmentBoxTypes?has_content>
                                                <@field type="select" name="boxType_${packageSeqId}">
                                                    <option value=""></option>
                                                    <#list carrierShipmentBoxTypes as carrierShipmentBoxType>
                                                        <#assign shipmentBoxType = carrierShipmentBoxType.getRelatedOne("ShipmentBoxType", false) />
                                                        <option value="${shipmentBoxType.shipmentBoxTypeId}">${shipmentBoxType.description!shipmentBoxType.shipmentBoxTypeId}</option>
                                                    </#list>
                                                </@field>
                                            </#if>        
                                        </@td>                         
                                        <@td>
                                            <@field type="input" name="additionalShippingCharge" value=(packingSession.getAdditionalShippingCharge()!) size="20"/>
                                            <#if packageSeqIds?has_content>
                                                <a href="javascript:document.completePackForm.action='<@pageUrl>calcPackSessionAdditionalShippingCharge</@pageUrl>';document.completePackForm.submit();" class="${styles.link_run_sys!} ${styles.action_verify!}">${uiLabelMap.ProductEstimateShipCost}</a>                                               
                                            </#if>
                                        </@td>
                                        <@td>
                                            <@field type="textarea" name="handlingInstructions" rows="2" cols="30">${packingSession.getHandlingInstructions()!}</@field>
                                        </@td>
                                        <@td>
                                            <#assign buttonName = uiLabelMap.ProductComplete>
                                            <#if (forceComplete!"false") == "true">
                                                <#assign buttonName = uiLabelMap.ProductCompleteForce>
                                            </#if>
                                            <@field type="submit" text=buttonName onClick="javascript:document.completePackForm.submit();"/>
                                        </@td>
                                    </@tr>
                                </#list>
                            </@table>
                        </@fields>
                    </form>
                </@section>
            </#if>
        </@section>
    
        <#-- display items in packages, per packed package and in order -->
        <#assign linesByPackageResultMap = packingSession.getPackingSessionLinesByPackage()!>
        <#assign packageMap = linesByPackageResultMap.get("packageMap")!>
        <#assign sortedKeys = linesByPackageResultMap.get("sortedKeys")!>
        <#if ((packageMap?has_content) && (sortedKeys?has_content))>
            <@section title="${rawLabel('ProductPackages')} : ${sortedKeys.size()!}">
                <#list sortedKeys as key>
                    <#assign packedLines = packageMap.get(key)>
                    <#if packedLines?has_content>
                        <#assign packedLine = packedLines.get(0)!>
                        ${uiLabelMap.ProductPackage}&nbsp;${packedLine.getPackageSeq()!}
                        <@table type="data-list">
                            <@tr class="header-row">
                                <@td>${uiLabelMap.ProductItem} ${uiLabelMap.CommonNbr}</@td>
                                <@td>${uiLabelMap.ProductProductId}</@td>
                                <@td>${uiLabelMap.ProductProductDescription}</@td>
                                <@td>${uiLabelMap.ProductInventoryItem} ${uiLabelMap.CommonNbr}</@td>
                                <@td>${uiLabelMap.ProductPackedQty}</@td>
                                <@td>${uiLabelMap.ProductPackedWeight}&nbsp;(${("uiLabelMap.ProductShipmentUomAbbreviation_" + defaultWeightUomId)?eval})&nbsp;(${uiLabelMap.ProductPackage})</@td>
                                <@td>${uiLabelMap.ProductPackage} ${uiLabelMap.CommonNbr}</@td>
                                <@td>&nbsp;</@td>
                            </@tr>
                            <#list packedLines as line>
                                <#assign product = Static["org.ofbiz.product.product.ProductWorker"].findProduct(delegator, line.getProductId())/>
                                <@tr>
                                    <@td>${line.getOrderItemSeqId()}</@td>
                                    <@td>${line.getProductId()!(uiLabelMap.CommonNA)}</@td>
                                    <@td>
                                        <a href="<@serverUrl>/catalog/control/ViewProduct?productId=${line.getProductId()!}${raw(externalKeyParam)}</@serverUrl>" class="${styles.link_nav_info_name!}" target="_blank">${product.internalName!("[${uiLabelMap.CommonNA}]")}</a>
                                    </@td>
                                    <@td>${line.getInventoryItemId()}</@td>
                                    <@td>${line.getQuantity()}</@td>
                                    <@td>${line.getWeight()} (${packingSession.getPackageWeight(line.getPackageSeq()?int)!})</@td>
                                    <@td>${line.getPackageSeq()}</@td>
                                    <@td><a href="javascript:clearLine('${facilityId}', '${line.getOrderId()}', '${line.getOrderItemSeqId()}', '${line.getProductId()!""}', '${line.getShipGroupSeqId()}', '${line.getInventoryItemId()}', '${line.getPackageSeq()}')" class="${styles.link_run_sys!} ${styles.action_clear!}">${uiLabelMap.CommonClear}</a></@td>
                                </@tr>
                            </#list>
                        </@table>
                    </#if>
                </#list>
            </@section>
        </#if>
    </#if>
    </@section>
<#else>
    <@commonMsg type="error">${uiLabelMap.ProductFacilityViewPermissionError}</@commonMsg>
</#if>
