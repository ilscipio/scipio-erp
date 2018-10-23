<input type="hidden" name="facilityId" value="${facilityId!}" />        

<#if !(inventoryTransfer??)>
    <@field type="lookup" label=uiLabelMap.ProductInventoryItemId name="inventoryItemId" size="20" maxlength="20" formName="transferform" id="inventoryItemId" fieldFormName="LookupInventoryItem" postfix=true value=(inventoryItemId!) />   
<#else>
    <@field type="display" label=uiLabelMap.ProductInventoryItemId>
        ${inventoryItemId!}
    </@field>
    <input type="hidden" name="inventoryTransferId" value="${inventoryTransfer.inventoryTransferId!}" />
    <input type="hidden" name="inventoryItemId" value="${inventoryTransfer.inventoryItemId!}" />
    <input type="hidden" name="locationSeqId" value="${(inventoryTransfer.locationSeqId)!}" />
</#if>
<#if !(inventoryItem)??>
    <@commonMsg type="error">${uiLabelMap.ProductInventoryIdNotFound} ${uiLabelMap.ProductInventoryPleaseEnterAValidInventoryId}</@commonMsg> 
<#else>
    <@field type="display" label=uiLabelMap.ProductInventoryItemTypeId>
    <#if inventoryItemType??>
        ${(inventoryItemType.get("description",locale))!}
    </#if>
    </@field>
    <@field type="display" label=uiLabelMap.ProductProductId>
        <#if inventoryItem?? && (inventoryItem.productId)??>
            <a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${(inventoryItem.productId)!}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${(inventoryItem.productId)!}</a>
        </#if>
    </@field>
    <@field type="display" label=uiLabelMap.CommonStatus>
        ${(inventoryStatus.get("description",locale))!("--")}
    </@field>
    
    <@field type="display" label=uiLabelMap.ProductComments>
        ${(inventoryItem.comments)!("--")}
    </@field>
    
    <@field type="display" label=uiLabelMap.ProductSerialAtpQoh>
        <#if inventoryItem?? && inventoryItem.inventoryItemTypeId.equals("NON_SERIAL_INV_ITEM")>
                ${(inventoryItem.availableToPromiseTotal)!}&nbsp;/&nbsp;${(inventoryItem.quantityOnHandTotal)!}
        <#elseif inventoryItem?? && inventoryItem.inventoryItemTypeId.equals("SERIALIZED_INV_ITEM")>
            ${(inventoryItem.serialNumber)!}
        <#elseif inventoryItem??>
            <@commonMsg type="error">${uiLabelMap.ProductErrorType} ${(inventoryItem.inventoryItemTypeId)!} ${uiLabelMap.ProductUnknownSpecifyType}.</@commonMsg>
        </#if>
    </@field>
    
    <@field type="select" label=uiLabelMap.ProductTransferStatus name="statusId">
        <#if (inventoryTransfer.statusId)??>
            <#assign curStatusItem = inventoryTransfer.getRelatedOne("StatusItem", true)>
            <option value="${(inventoryTransfer.statusId)!}">${(curStatusItem.get("description",locale))!}</option>
        </#if>
        <#if statusItems?has_content>
            <#list statusItems as statusItem>
            <option value="${(statusItem.statusId)!}">${(statusItem.get("description",locale))!}</option>
            </#list>
        </#if>
    </@field>
    <@field type="generic" label=uiLabelMap.ProductTransferSendDate>
        <@field type="datetime" dateType="date-time" name="sendDate" value=((inventoryTransfer.sendDate)!) size="22" />        
    </@field>
    <#if !(inventoryTransfer??)>
        <@field type="generic" label=uiLabelMap.ProductToFacilityContainer>
            <@field type="select" name="facilityIdTo" tooltip=(uiLabelMap.ProductSelectFacility!)>
                <#if facilities?has_content>
                    <#list facilities as nextFacility>
                    <option value="${(nextFacility.facilityId)!}">${(nextFacility.facilityName)!} [${(nextFacility.facilityId)!}]</option>
                    </#list>
                </#if>
            </@field>
            <@field type="input" name="containerIdTo" value=((inventoryTransfer.containerIdTo)!) size="20" maxlength="20" tooltip=(uiLabelMap.ProductOrEnterContainerId!)/>
        </@field>
        <@field type="lookup" label=uiLabelMap.ProductToLocation value=((inventoryTransfer.locationSeqIdTo)!) formName="transferform" name="locationSeqIdTo" id="locationSeqIdTo" fieldFormName="LookupFacilityLocation"/>
        <@field type="input" label=uiLabelMap.ProductComments name="comments" size="60" maxlength="250" />
    
        <@field type="generic" label=uiLabelMap.ProductQuantityToTransfer>
            <#if inventoryItem?? && inventoryItem.inventoryItemTypeId.equals("NON_SERIAL_INV_ITEM")>
                <@field type="input" size="5" name="xferQty" value=((inventoryItem.availableToPromiseTotal)!) />
            <#elseif inventoryItem?? && inventoryItem.inventoryItemTypeId.equals("SERIALIZED_INV_ITEM")>
                <input type="hidden" name="xferQty" value="1" />
                <@field type="display" label="1" />
            <#elseif inventoryItem??>
                <@commonMsg type="warning">${uiLabelMap.ProductErrorType} ${(inventoryItem.inventoryItemTypeId)!} ${uiLabelMap.ProductUnknownSpecifyType}.</@commonMsg>
            </#if>
        </@field>
    <#else>
        <@field type="generic" label=uiLabelMap.ProductTransferReceiveDate>
            <@field type="datetime" dateType="date-time" name="receiveDate" value=((inventoryTransfer.receiveDate)!) size="22" />          
        </@field>
        <@field type="display" label=uiLabelMap.ProductToFacilityContainer>
            <#assign fac = delegator.findOne("Facility", {"facilityId":inventoryTransfer.facilityIdTo}, false)> 
            ${(fac.facilityName)!}
        </@field>
        <@field type="lookup" label=uiLabelMap.ProductToLocation value=((inventoryTransfer.locationSeqIdTo)!) formName="transferform" name="locationSeqIdTo" id="locationSeqIdTo" fieldFormName="LookupFacilityLocation?facilityId=${inventoryTransfer.facilityIdTo}"/>
        <@field type="input" label=uiLabelMap.ProductComments name="comments" value=((inventoryTransfer.comments)!) size="60" maxlength="250" />
    </#if>
    <#if !(inventoryTransfer??)>
        <@field type="submit" text=uiLabelMap.ProductTransfer class="+${styles.link_run_sys!} ${styles.action_transfer!}" />
    <#else>
        <@field type="submit" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}" />
    </#if>
</#if>