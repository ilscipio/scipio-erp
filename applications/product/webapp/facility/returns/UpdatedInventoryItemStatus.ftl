<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#assign inventoryItemType = parameters.inventoryItemType?default("NON_SERIAL_INV_ITEM")>
<#assign inventoryItemStatus = parameters.inventoryItemStatus?default("INV_RETURNED")>
<#if inventoryItemType == "NON_SERIAL_INV_ITEM"> 
    <option value="INV_RETURNED" <#if inventoryItemStatus == "INV_RETURNED">selected="selected"</#if>>${uiLabelMap.ProductReturned}</option>
    <option value="INV_AVAILABLE" <#if inventoryItemStatus == "INV_AVAILABLE">selected="selected"</#if>>${uiLabelMap.ProductAvailable}</option>
    <option value="INV_NS_DEFECTIVE" <#if inventoryItemStatus == "INV_DEFECTIVE">selected="selected"</#if>>${uiLabelMap.ProductDefective}</option>
<#else>
    <option value="INV_RETURNED" <#if inventoryItemStatus == "INV_RETURNED">selected="selected"</#if>>${uiLabelMap.ProductReturned}</option>
    <option value="INV_AVAILABLE" <#if inventoryItemStatus == "INV_AVAILABLE">selected="selected"</#if>>${uiLabelMap.ProductAvailable}</option>
    <option value="INV_DEFECTIVE" <#if inventoryItemStatus == "INV_NS_DEFECTIVE">selected="selected"</#if>>${uiLabelMap.ProductDefective}</option>
</#if>