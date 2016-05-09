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
            <#list productStoreFacilityList as productStoreFacility>
                <option value="${productStoreFacility.facilityId}">${productStoreFacility.facilityName}</option>
            </#list>
        </@field>

        <#--
        <field name="destinationFacilityId" title="${uiLabelMap.ProductDestinationFacility}" position="2">
            <drop-down allow-empty="true" current="first-in-list">
                <entity-options entity-name="Facility" key-field-name="facilityId" description="${facilityName} [${facilityId}]">
                    <entity-order-by field-name="facilityName"/>
                </entity-options>
            </drop-down>
        </field>
        -->
        
        <@field type="lookup" name="partyIdFrom" label=uiLabelMap.ProductFromParty formName="EditShipment" id="partyIdFrom" fieldFormName="LookupPartyName" /> 
        <@field type="lookup" name="partyIdTo" label=uiLabelMap.ProductToParty formName="EditShipment" id="partyIdTo" fieldFormName="LookupPartyName" />
        
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
        -->
        
        <#--
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
        
        <#--
        <field name="currencyUomId" title="${uiLabelMap.ProductCurrencyUomId}" position="2">
            <drop-down allow-empty="true" no-current-selected-key="${defaultOrganizationPartyCurrencyUomId}">
                <entity-options key-field-name="uomId" description="${description} - ${abbreviation}" entity-name="Uom">
                    <entity-constraint name="uomTypeId" operator="equals" value="CURRENCY_MEASURE"/>
                    <entity-order-by field-name="description"/>
                </entity-options>
            </drop-down>
        </field>
        -->
        <@field type="text" name="additionalShippingCharge" label=uiLabelMap.ProductAdditionalShippingCharge />
        <@field type="textarea" name="handlingInstructions" label=uiLabelMap.ProductHandlingInstructions />

        <@field type="submit" submitType="link" href="javascript:document.updateShipment.submit();" class="+${styles.link_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonUpdate />
        
    </form>

</@section>