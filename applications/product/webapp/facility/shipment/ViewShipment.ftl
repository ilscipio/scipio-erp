<@section>
    <form name="EditShipment" action="updateShipment" method="POST">
        <@field type="select" name="shipmentTypeId" value="" label=uiLabelMap.ProductShipmentTypeId>
            <#list shipmentTypeList as shipmentType>
                <option value="${shipmentType.shipmentTypeId}">${shipmentType.description}</option>
            </#list>
        </@field>
        <@field type="text" name="primaryShipGroupSeqId" value="${(shipment.primaryShipGroupSeqId)!}" label=uiLabelMap.ProductPrimaryShipGroupSeqId />
        <@field type="select" name="statusId" value="" label=uiLabelMap.ProductStatusId>
            <#list statusList as status>
                <option value="${status.statusId}">${status.description}</option>
            </#list>
        </@field>
        <@field type="lookup" name="primaryOrderId" label=uiLabelMap.ProductPrimaryOrderId formName="EditShipment" id="primaryOrderId" fieldFormName="LookupOrderHeader" />        
        <@field type="text" name="primaryReturnId" label=uiLabelMap.ProductPrimaryReturnId />
        <@field type="datetime" name="estimatedReadyDate" label=uiLabelMap.ProductEstimatedReadyDate />
        <@field type="datetime" name="estimatedShipDate" label=uiLabelMap.ProductEstimatedShipDate />
        <@field type="datetime" name="estimatedArrivalDate" label=uiLabelMap.ProductEstimatedArrivalDate />
        <@field type="datetime" name="latestCancelDate" label=uiLabelMap.ProductLatestCancelDate />
        <@field type="select" name="originFacilityId" label=uiLabelMap.ProductOriginFacility>
            <#list facilityList as facility>
                <option value="${facility.facilityId}">${facility.facilityName}</option>
            </#list>
        </@field>
        <@field type="select" name="destinationFacilityId" label=uiLabelMap.ProductDestinationFacility>
            <#list facilityList as facility>
                <option value="${facility.facilityId}">${facility.facilityName}</option>
            </#list>
        </@field>
        <@field type="lookup" name="partyIdFrom" label=uiLabelMap.ProductFromParty formName="EditShipment" id="partyIdFrom" fieldFormName="LookupPartyName" /> 
        <@field type="lookup" name="partyIdTo" label=uiLabelMap.ProductToParty formName="EditShipment" id="partyIdTo" fieldFormName="LookupPartyName" />
        
        <#--TODO: Who the hell remember contact mechs by ids...? This must be done in a different way -->
        <#--
        <field name="originContactMechId" title="${uiLabelMap.ProductOriginPostalAddressId}"
            tooltip="${uiLabelMap.CommonTo}: ${originPostalAddress.toName}, ${uiLabelMap.CommonAttn}: ${originPostalAddress.attnName}, ${originPostalAddress.address1}, ${originPostalAddress.address2}, ${originPostalAddress.city}, ${originPostalAddress.stateProvinceGeoId}, ${originPostalAddress.postalCode}, ${originPostalAddress.countryGeoId}">
            <text/>
        </field>
        <field name="destinationContactMechId" title="${uiLabelMap.ProductDestinationPostalAddressId}" position="2"
            tooltip="${uiLabelMap.CommonTo}: ${destinationPostalAddress.toName}, ${uiLabelMap.CommonAttn}: ${destinationPostalAddress.attnName}, ${destinationPostalAddress.address1}, ${destinationPostalAddress.address2}, ${destinationPostalAddress.city}, ${destinationPostalAddress.stateProvinceGeoId}, ${destinationPostalAddress.postalCode}, ${destinationPostalAddress.countryGeoId}">
            <text/>
        </field>
        <field name="originTelecomNumberId" title="${uiLabelMap.ProductOriginPhoneNumberId}"
            tooltip="${originTelecomNumber.countryCode}  ${originTelecomNumber.areaCode} ${originTelecomNumber.contactNumber}">
            <text/>
        </field>
        <field name="destinationTelecomNumberId" title="${uiLabelMap.ProductDestinationPhoneNumberId}" position="2"
            tooltip="${destinationTelecomNumber.countryCode}  ${destinationTelecomNumber.areaCode} ${destinationTelecomNumber.contactNumber}">
            <text/>
        </field>
        <field name="estimatedShipWorkEffId" title="${uiLabelMap.ProductEstimatedShipWorkEffId}" widget-style="${styles.link_nav_info_id}">
            <hyperlink target="/workeffort/control/WorkEffortSummary" also-hidden="false" description="${shipment.estimatedShipWorkEffId}" target-type="inter-app">
                <parameter param-name="workEffortId" from-field="shipment.estimatedShipWorkEffId"/>
            </hyperlink>
        </field>
        <field name="estimatedArrivalWorkEffId" title="${uiLabelMap.ProductEstimatedArrivalWorkEffId}" widget-style="${styles.link_nav_info_id}" position="2">
            <hyperlink target="/workeffort/control/WorkEffortSummary" also-hidden="false" description="${shipment.estimatedArrivalWorkEffId}" target-type="inter-app">
                <parameter param-name="workEffortId" from-field="shipment.estimatedArrivalWorkEffId"/>
            </hyperlink>
        </field>
        -->
        
        <@field type="text" name="estimatedShipCost" label=uiLabelMap.ProductEstimatedShipCost />
        <@field type="select" name="currencyUomId" label=uiLabelMap.ProductCurrencyUomId>
            <#list uomList as uom>
                <option value="${uom.uomId}" <#if uom.uomId == defaultOrganizationPartyCurrencyUomId>selected="selected"</#if> >${uom.description} - ${uom.abbreviation}</option>
            </#list>
        </@field>
        <@field type="text" name="additionalShippingCharge" label=uiLabelMap.ProductAdditionalShippingCharge />
        <@field type="textarea" name="handlingInstructions" label=uiLabelMap.ProductHandlingInstructions />
        <@field type="submit" submitType="link" href="javascript:document.updateShipment.submit();" class="+${styles.link_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonUpdate />
    </form>
</@section>