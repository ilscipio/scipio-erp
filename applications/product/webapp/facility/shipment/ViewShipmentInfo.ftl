<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#if shipment??>
<@section title=uiLabelMap.PageTitleViewShipment>
        <@table type="fields" class="+${styles.table_spacing_tiny_hint!}">
          <@tr>
            <@td width="20%" align="right">${uiLabelMap.ProductShipmentId}</@td>
            <@td width="80%">${shipment.shipmentId}</@td>
          </@tr>
          <@tr>
            <@td width="20%" align="right">${uiLabelMap.ProductShipmentType}</@td>
            <@td width="80%">${(shipmentType.get("description",locale))?default(shipment.shipmentTypeId!)}</@td>
          </@tr>
          <@tr>
            <@td width="20%" align="right">${uiLabelMap.CommonStatus}</@td>
            <@td width="80%">${(statusItem.get("description",locale))?default(shipment.statusId!)}</@td>
          </@tr>
          <@tr>
            <@td width="20%" align="right">${uiLabelMap.ProductPrimaryOrderId}</@td>
            <@td width="80%"><#if shipment.primaryOrderId??><a href="<@ofbizInterWebappUrl>/ordermgr/control/orderview?orderId=${shipment.primaryOrderId}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${shipment.primaryOrderId}</a></#if></@td>
          </@tr>
          <@tr>
            <@td width="20%" align="right">${uiLabelMap.ProductPrimaryReturnId}</@td>
            <@td width="80%"><#if shipment.primaryReturnId??><a href="<@ofbizInterWebappUrl>/ordermgr/control/returnMain?returnId=${shipment.primaryReturnId}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${shipment.primaryReturnId}</a></#if></@td>
          </@tr>
          <@tr>
            <@td width="20%" align="right">${uiLabelMap.ProductPrimaryShipGroupSeqId}</@td>
            <@td width="80%">${shipment.primaryShipGroupSeqId!}</@td>
          </@tr>
          <@tr>
            <@td width="20%" align="right">${uiLabelMap.ProductEstimatedDates}</@td>
            <@td width="80%">
              <span>
                <span>${uiLabelMap.CommonReady}:&nbsp;</span>${(shipment.estimatedReadyDate.toString())!}
                <span>${uiLabelMap.ProductEstimatedShipDate}:&nbsp;</span>${(shipment.estimatedShipDate.toString())!}
                <span>${uiLabelMap.ProductArrival}:&nbsp;</span>${(shipment.estimatedArrivalDate.toString())!}
              </span>
            </@td>
          </@tr>
          <@tr>
            <@td width="20%" align="right">${uiLabelMap.ProductLatestCancelDate}</@td>
            <@td width="80%">${(shipment.latestCancelDate.toString())!}</@td>
          </@tr>
          <@tr>
            <@td width="20%" align="right">${uiLabelMap.ProductEstimatedShipCost}</@td>
            <@td width="80%">${(shipment.estimatedShipCost)!}</@td>
          </@tr>
          <@tr>
            <@td width="20%" align="right">${uiLabelMap.ProductAdditionalShippingCharge}</@td>
            <@td width="80%">
                <#if shipment.additionalShippingCharge??>
                    <@ofbizCurrency amount=shipment.additionalShippingCharge isoCode=(shipment.currencyUomId!) />
                </#if>
            </@td>
          </@tr>
          <@tr>
            <@td width="20%" align="right">${uiLabelMap.ProductHandlingInstructions}</@td>
            <@td width="80%">${shipment.handlingInstructions!}</@td>
          </@tr>
          <@tr>
            <@td width="20%" align="right">${uiLabelMap.ProductFacilities}</@td>
            <@td width="80%">
              <div>${uiLabelMap.ProductOrigin}:&nbsp;${(originFacility.facilityName)!}&nbsp;[${(shipment.originFacilityId!)}]</div>
              <div>${uiLabelMap.ProductDestination}:&nbsp;${(destinationFacility.facilityName)!}&nbsp;[${(shipment.destinationFacilityId!)}]</div>
            </@td>
          </@tr>
          <@tr>
            <@td width="20%" align="right">${uiLabelMap.PartyParties}</@td>
            <@td width="80%">
              <span>
                <span>${uiLabelMap.CommonTo}:&nbsp;${(toPerson.firstName)!} ${(toPerson.middleName)!} ${(toPerson.lastName)!} ${(toPartyGroup.groupName)!} [${shipment.partyIdTo!}]</span>
                <span>${uiLabelMap.CommonFrom}:&nbsp;${(fromPerson.firstName)!} ${(fromPerson.middleName)!} ${(fromPerson.lastName)!} ${(fromPartyGroup.groupName)!} [${shipment.partyIdFrom!}]</span>
              </span>
            </@td>
          </@tr>
          <@tr>
            <@td width="20%" align="right">${uiLabelMap.ProductAddresses}</@td>
            <@td width="80%">
              <div>${uiLabelMap.ProductOrigin}:&nbsp;${shipment.originContactMechId!}&nbsp;<#if originPostalAddress?has_content>[${uiLabelMap.CommonTo} : ${originPostalAddress.toName!}, ${uiLabelMap.CommonAttn} : ${originPostalAddress.attnName!}, ${originPostalAddress.address1!}, ${originPostalAddress.address2!}, ${originPostalAddress.city!}, ${originPostalAddress.stateProvinceGeoId!}, ${originPostalAddress.postalCode!}, ${originPostalAddress.countryGeoId!}]</#if></div>
              <div>${uiLabelMap.ProductDestination}:&nbsp;${shipment.destinationContactMechId!}&nbsp;<#if destinationPostalAddress?has_content>[${uiLabelMap.CommonTo} : ${destinationPostalAddress.toName!}, ${uiLabelMap.CommonAttn} : ${destinationPostalAddress.attnName!}, ${destinationPostalAddress.address1!}, ${destinationPostalAddress.address2!}, ${destinationPostalAddress.city!}, ${destinationPostalAddress.stateProvinceGeoId!}, ${destinationPostalAddress.postalCode!}, ${destinationPostalAddress.countryGeoId!}]</#if></div>
            </@td>
          </@tr>
          <@tr>
            <@td width="20%" align="right">${uiLabelMap.ProductPhoneNumbers}</@td>
            <@td width="80%">
              <div>${uiLabelMap.ProductOrigin}:&nbsp;${shipment.originTelecomNumberId!}&nbsp;<#if originTelecomNumber?has_content>[${originTelecomNumber.countryCode!}  ${originTelecomNumber.areaCode!} ${originTelecomNumber.contactNumber!}]</#if></div>
              <div>${uiLabelMap.ProductDestination}:&nbsp;${shipment.destinationTelecomNumberId!}&nbsp;<#if destinationTelecomNumber?has_content>[${destinationTelecomNumber.countryCode!}  ${destinationTelecomNumber.areaCode!} ${destinationTelecomNumber.contactNumber!}]</#if></div>
            </@td>
          </@tr>
          <@tr>
            <@td width="20%" align="right">${uiLabelMap.CommonCreated}</@td>
            <@td width="80%">${uiLabelMap.CommonBy} [${shipment.createdByUserLogin!}] ${uiLabelMap.CommonOn} ${(shipment.createdDate.toString())!}
            </@td>
          </@tr>
          <@tr>
            <@td width="20%" align="right">${uiLabelMap.CommonLastUpdated}</@td>
            <@td width="80%">${uiLabelMap.CommonBy} [${shipment.lastModifiedByUserLogin!}] ${uiLabelMap.CommonOn} ${(shipment.lastModifiedDate.toString())!}
            </@td>
          </@tr>
        </@table>
</@section>
</#if>