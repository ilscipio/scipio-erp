<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section title=uiLabelMap.ProductCarrierShipmentMethod>
    <form name="addscarr" method="post" action="<@pageUrl>prepareCreateShipMeth</@pageUrl>">
      <@fields type="default-nolabelarea">
        <input type="hidden" name="newShipMethod" value="Y"/>
        <input type="hidden" name="productStoreId" value="${productStoreId!}"/>
        <@field type="select" name="carrierShipmentString">
              <option>${uiLabelMap.ProductSelectOne}</option>
              <#list carrierShipmentMethods as shipmentMethod>
                <option value="${shipmentMethod.partyId}|${shipmentMethod.roleTypeId}|${shipmentMethod.shipmentMethodTypeId}">${shipmentMethod.shipmentMethodTypeId} (${shipmentMethod.partyId}/${shipmentMethod.roleTypeId})</option>
              </#list>
        </@field>
        <@field type="submit" class="+${styles.link_run_sys!} ${styles.action_add!}" text=uiLabelMap.CommonAdd/>
      </@fields>
    </form>
</@section>
