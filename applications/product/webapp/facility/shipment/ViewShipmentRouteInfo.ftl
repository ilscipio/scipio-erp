<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#if shipmentRouteSegmentDatas?has_content>
<@section>
        <@table type="data-complex" class="+${styles.table_spacing_tiny_hint!}" autoAltRows=true > <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="2" -->
         <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.ProductSegment}</@th>
                <@th>
                    <div>${uiLabelMap.ProductCarrierShipmentMethod}</div>
                    <div>${uiLabelMap.ProductOriginDestinationFacility}</div>
                    <div>${uiLabelMap.ProductOriginDestinationAddressId}</div>
                    <div>${uiLabelMap.ProductOriginDestinationPhoneId}</div>
                </@th>
                <@th>
                    <div>${uiLabelMap.ProductCarrierStatus}</div>
                    <div>${uiLabelMap.ProductTrackingNumber}</div>
                    <div>${uiLabelMap.ProductEstimatedStartArrive}</div>
                    <div>${uiLabelMap.ProductActualStartArrive}</div>
                </@th>
                <@th>
                    <div>${uiLabelMap.ProductBillingWeightUom}</div>
                    <div>${uiLabelMap.ProductCurrencyUom}</div>
                    <div>${uiLabelMap.ProductActualTransport}</div>
                    <div>${uiLabelMap.ProductActualServices}</div>
                    <div>${uiLabelMap.ProductActualOther}</div>
                    <div>${uiLabelMap.ProductActualTotal}</div>
                </@th>
            </@tr>
            </@thead>
        <#list shipmentRouteSegmentDatas as shipmentRouteSegmentData>
            <#assign shipmentRouteSegment = shipmentRouteSegmentData.shipmentRouteSegment>
            <#assign shipmentPackageRouteSegs = shipmentRouteSegmentData.shipmentPackageRouteSegs!>
            <#assign originFacility = shipmentRouteSegmentData.originFacility!>
            <#assign destFacility = shipmentRouteSegmentData.destFacility!>
            <#assign shipmentMethodType = shipmentRouteSegmentData.shipmentMethodType!>
            <#assign carrierPerson = shipmentRouteSegmentData.carrierPerson!>
            <#assign carrierPartyGroup = shipmentRouteSegmentData.carrierPartyGroup!>
            <#assign originPostalAddress = shipmentRouteSegmentData.originPostalAddress!>
            <#assign destPostalAddress = shipmentRouteSegmentData.destPostalAddress!>
            <#assign originTelecomNumber = shipmentRouteSegmentData.originTelecomNumber!>
            <#assign destTelecomNumber = shipmentRouteSegmentData.destTelecomNumber!>
            <#assign carrierServiceStatusItem = shipmentRouteSegmentData.carrierServiceStatusItem!>
            <#assign currencyUom = shipmentRouteSegmentData.currencyUom!>
            <#assign billingWeightUom = shipmentRouteSegmentData.billingWeightUom!>
            <#assign carrierServiceStatusValidChangeToDetails = shipmentRouteSegmentData.carrierServiceStatusValidChangeToDetails!>
            <@tr valign="middle">
                <@td>${shipmentRouteSegment.shipmentRouteSegmentId}</@td>
                <@td>
                    <span>${(carrierPerson.firstName)!} ${(carrierPerson.middleName)!} ${(carrierPerson.lastName)!} ${(carrierPartyGroup.groupName)!} [${shipmentRouteSegment.carrierPartyId!}]</span>
                    <span>${shipmentMethodType.description?default(shipmentRouteSegment.shipmentMethodTypeId!)}</span>
                    <br />
                    <span>${uiLabelMap.ProductOrigin}</span>
                    <span>${(originFacility.facilityName)!} [${originFacility.facilityId!}]</span>
                    <br />
                    <span>${uiLabelMap.ProductDest}</span>
                    <span>${(destFacility.facilityName)!} [${destFacility.facilityId!}]</span>
                    <br />
                    <span>${uiLabelMap.ProductOrigin}</span>
                    <span><#if originPostalAddress?has_content>${uiLabelMap.CommonTo} : ${originPostalAddress.toName!}, ${uiLabelMap.CommonAttn} : ${originPostalAddress.attnName!}, ${originPostalAddress.address1!}, ${originPostalAddress.address2!}, ${originPostalAddress.city!}, ${originPostalAddress.stateProvinceGeoId!}, ${originPostalAddress.postalCode!}, ${originPostalAddress.countryGeoId!}</#if> [${shipmentRouteSegment.originContactMechId!}]</span>
                    <br />
                    <span>${uiLabelMap.ProductDest}</span>
                    <span><#if destPostalAddress?has_content>${uiLabelMap.CommonTo} : ${destPostalAddress.toName!}, ${uiLabelMap.CommonAttn} : ${destPostalAddress.attnName!}, ${destPostalAddress.address1!}, ${destPostalAddress.address2!}, ${destPostalAddress.city!}, ${destPostalAddress.stateProvinceGeoId!}, ${destPostalAddress.postalCode!}, ${destPostalAddress.countryGeoId!}</#if> [${shipmentRouteSegment.destContactMechId!}]</span>
                    <br />
                    <span>${uiLabelMap.ProductOrigin}</span>
                    <span><#if originTelecomNumber?has_content>${originTelecomNumber.countryCode!}  ${originTelecomNumber.areaCode!} ${originTelecomNumber.contactNumber!}</#if> [${shipmentRouteSegment.originTelecomNumberId!}]</span>
                    <br />
                    <span>${uiLabelMap.ProductDest}</span>
                    <span><#if destTelecomNumber?has_content>${destTelecomNumber.countryCode!}  ${destTelecomNumber.areaCode!} ${destTelecomNumber.contactNumber!}</#if> [${shipmentRouteSegment.destTelecomNumberId!}]</span>
                    <br />
                </@td>
                <@td>
                    <div>${(carrierServiceStatus.description)?default("&nbsp;")}</div>
                    <div>${shipmentRouteSegment.trackingIdNumber?default("&nbsp;")}</div>
                    <div>${(shipmentRouteSegment.estimatedStartDate.toString())!} - ${(shipmentRouteSegment.estimatedArrivalDate.toString())!}</div>
                    <div>${(shipmentRouteSegment.actualStartDate.toString())!} - ${(shipmentRouteSegment.actualArrivalDate.toString())!}</div>
                </@td>
                <@td>
                    <div>${shipmentRouteSegment.billingWeight!} ${(billingWeightUom.get("description",locale))!} [${(billingWeightUom.abbreviation)!}]</div>
                    <div>${(currencyUom.get("description",locale))?default("&nbsp;")}</div>
                    <div>${(shipmentRouteSegment.actualTransportCost)?default("&nbsp;")}</div>
                    <div>${(shipmentRouteSegment.actualServiceCost)?default("&nbsp;")}</div>
                    <div>${(shipmentRouteSegment.actualOtherCost)?default("&nbsp;")}</div>
                    <div>${(shipmentRouteSegment.actualCost)?default("&nbsp;")}</div>
                </@td>
            </@tr>
            <#list shipmentPackageRouteSegs as shipmentPackageRouteSeg>
            <@tr valign="middle" groupLast=true>
                <@td>&nbsp;</@td>
                <@td><span>${uiLabelMap.ProductPackage}</span> ${shipmentPackageRouteSeg.shipmentPackageSeqId}</@td>
                <@td><span>${uiLabelMap.ProductTracking}</span> ${shipmentPackageRouteSeg.trackingCode!}</@td>
                <@td><span>${uiLabelMap.ProductBox}</span> ${shipmentPackageRouteSeg.boxNumber!}</@td>
            </@tr>
            </#list>
        </#list>
        </@table>
</@section>
</#if>