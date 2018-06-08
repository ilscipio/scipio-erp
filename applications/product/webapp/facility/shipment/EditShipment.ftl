<@section>
    <#assign formAction = "createShipment"/>    
    <#if shippment?has_content && shipment.shipmentTypeId == "PURCHASE_RETURN">
        <#assign formAction = "createShipmentAndItemsForVendorReturn"/>
    <#else>
        <#assign formAction = "updateShipment"/>
    </#if>    
    <form name="EditShipment" action="${formAction}" method="POST">
        <#if shipment?has_content>
            <@field type="hidden" name="shipmentId" value=shipment.shipmentId />
        </#if>
        <#if !shipment?has_content>
            <@field type="select" name="shipmentTypeId" label=uiLabelMap.ProductShipmentTypeId>
                <#list shipmentTypeList as shipmentType>
                    <option value="${shipmentType.shipmentTypeId}">${shipmentType.description}</option>
                </#list>
            </@field>
        <#else>
            <#assign shipmentType = delegator.findOne("ShipmentType", {"shipmentTypeId":shipment.shipmentTypeId}, true)!/>
            <@field type="display" name="shipmentTypeId" label=uiLabelMap.ProductShipmentTypeId value=shipmentType.description />
        </#if>
        <@field type="text" name="primaryShipGroupSeqId" value=((shipment.primaryShipGroupSeqId)!) label=uiLabelMap.ProductPrimaryShipGroupSeqId />        
        <@field type="select" name="statusId" label=uiLabelMap.ProductStatusId>
            <#if currentStatus?has_content>
                <option value="${currentStatus.statusId}" selected="selected">${currentStatus.description}</option>
            </#if>
            <#if shipment?has_content && shipment.statusId?has_content>
                <#assign statusList = delegator.findByAnd("StatusValidChangeToDetail", {"statusId" : shipment.statusId}, null, false) />
                <#list statusList as status>                    
                    <option value="${status.statusIdTo}">${status.description}</option>                    
                </#list>
            <#else>
                <#assign statusList = delegator.findByAnd("StatusItem", {"statusTypeId" : statusItemTypeId}, null, false) />
                <#list statusList as status>                   
                    <option value="${status.statusId}">${status.description}</option>                   
                </#list>
            </#if>
        </@field>
        <#-- SCIPIO: FIXME: I'm not sure we should allow to change the orderId while editing... -->
        <#if shipment?has_content>
            <#if shipment.primaryOrderId?has_content>
            <@field type="display" name="primaryOrderId" label=uiLabelMap.ProductPrimaryOrderId value=(shipment.primaryOrderId!)>
                <a href="<@ofbizInterWebappUrl>/ordermgr/control/orderview?orderId=${shipment.primaryOrderId}</@ofbizInterWebappUrl>">${shipment.primaryOrderId}</a>
            </@field>
        <#else>
            <@field type="lookup" name="primaryOrderId" label=uiLabelMap.ProductPrimaryOrderId formName="EditShipment" id="primaryOrderId" fieldFormName="LookupOrderHeader" />
            </#if>
        </#if>

        <#if shipment?has_content && (shipment.primaryReturnId?has_content || shipment.statusId == "")>
            <@field type="text" name="primaryReturnId" label=uiLabelMap.ProductPrimaryReturnId value=shipment.primaryReturnId />
        </#if>
        
        <#if shipment?has_content>
            <@field type="datetime" name="estimatedReadyDate" label=uiLabelMap.ProductEstimatedReadyDate value=(shipment.estimatedReadyDate!) />
        <#else>
            <@field type="datetime" name="estimatedReadyDate" label=uiLabelMap.ProductEstimatedReadyDate />
        </#if>

        <#if shipment?has_content>
            <@field type="datetime" name="estimatedShipDate" label=uiLabelMap.ProductEstimatedShipDate value=(shipment.estimatedShipDate!) />
        <#else>
            <@field type="datetime" name="estimatedShipDate" label=uiLabelMap.ProductEstimatedShipDate />
        </#if>
        
        <#if shipment?has_content>
            <@field type="datetime" name="estimatedArrivalDate" label=uiLabelMap.ProductEstimatedArrivalDate value=(shipment.estimatedArrivalDate!) />
        <#else>
            <@field type="datetime" name="estimatedArrivalDate" label=uiLabelMap.ProductEstimatedArrivalDate />
        </#if>

        <#if shipment?has_content>
            <#if shipment.statusId == "SHIPMENT_CANCELLED">
                <@field type="datetime" name="latestCancelDate" label=uiLabelMap.ProductLatestCancelDate value=(shipment.latestCancelDate!) />
            </#if>
        <#else>
            <@field type="datetime" name="latestCancelDate" label=uiLabelMap.ProductLatestCancelDate />
        </#if>

        <@field type="select" name="originFacilityId" label=uiLabelMap.ProductOriginFacility>
            <option value=""></option>
            <#list facilityList as facility>
                <option value="${facility.facilityId}" <#if shipment?has_content && shipment.originFacilityId?has_content && shipment.originFacilityId == facility.facilityId>selected="selected"</#if>>${facility.facilityName}</option>
            </#list>
        </@field>
        <@field type="select" name="destinationFacilityId" label=uiLabelMap.ProductDestinationFacility>
            <option value=""></option>
            <#list facilityList as facility>
                <option value="${facility.facilityId}" <#if shipment?has_content && shipment.destinationFacilityId?has_content && shipment.destinationFacilityId == facility.facilityId>selected="selected"</#if>>${facility.facilityName}</option>
            </#list>
        </@field>
        
        <#if shipment?has_content>
            <@field type="lookup" name="partyIdFrom" label=uiLabelMap.ProductFromParty formName="EditShipment" id="partyIdFrom" fieldFormName="LookupPartyName" value=(shipment.partyIdFrom!) />
        <#else>
            <@field type="lookup" name="partyIdFrom" label=uiLabelMap.ProductFromParty formName="EditShipment" id="partyIdFrom" fieldFormName="LookupPartyName" />
        </#if>
        <#if shipment?has_content>
            <@field type="lookup" name="partyIdTo" label=uiLabelMap.ProductToParty formName="EditShipment" id="partyIdTo" fieldFormName="LookupPartyName" value=(shipment.partyIdTo!) />
        <#else>
            <@field type="lookup" name="partyIdTo" label=uiLabelMap.ProductToParty formName="EditShipment" id="partyIdTo" fieldFormName="LookupPartyName" />
        </#if>
        
        <#--FIXME: Who the hell remember contact mechs by ids...? This must be done in a different way -->
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
        <#if shipment?has_content>
            <@field type="text" name="estimatedShipCost" label=uiLabelMap.ProductEstimatedShipCost value=(shipment.estimatedShipCost!) />
        <#else>
            <@field type="text" name="estimatedShipCost" label=uiLabelMap.ProductEstimatedShipCost />
        </#if>
        <@field type="select" name="currencyUomId" label=uiLabelMap.ProductCurrencyUomId >
            <#list uomList as uom>
                <#assign selected = "" />
                <#if shipment?has_content && shipment.currencyUomId?has_content && uom.uomId == shipment.currencyUomId>
                    <#assign selected = "selected='selected'" />
                <#elseif uom.uomId == defaultOrganizationPartyCurrencyUomId>
                    <#assign selected = "selected='selected'" />
                </#if>
                <option value="${uom.uomId}" ${selected}>${uom.description} - ${uom.abbreviation}</option>
            </#list>
        </@field>
        <#if shipment?has_content>
            <@field type="text" name="additionalShippingCharge" label=uiLabelMap.ProductAdditionalShippingCharge value=(shipment.additionalShippingCharge!)/>
        <#else>
            <@field type="text" name="additionalShippingCharge" label=uiLabelMap.ProductAdditionalShippingCharge />
        </#if>
        <#if shipment?has_content>
            <@field type="textarea" name="handlingInstructions" label=uiLabelMap.ProductHandlingInstructions value=(shipment.handlingInstructions!) />
        <#else>
            <@field type="textarea" name="handlingInstructions" label=uiLabelMap.ProductHandlingInstructions />
        </#if>
        <@field type="submit" submitType="link" href="javascript:document.EditShipment.submit();" class="+${styles.link_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonUpdate />
    </form>
</@section>
