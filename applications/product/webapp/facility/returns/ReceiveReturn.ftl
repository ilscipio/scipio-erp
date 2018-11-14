<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
   
<@section title=uiLabelMap.ProductReceiveReturn>
    <form name="receiveInventoryReturn" method="post" action="<@ofbizUrl>ReceiveInventory</@ofbizUrl>">
        <input type="hidden" name="facilityId" value="${requestParameters.facilityId!}" />
        <input type="hidden" name="initialSelected" value="Y" />
        <@field type="input" label=uiLabelMap.ProductReturnNumber name="returnId" size="20" maxlength="20" value=(requestParameters.returnId!) />
        <@field type="submit" submitType="link" href="javascript:document.receiveInventoryReturn.submit();" class="+${styles.link_run_sys!} ${styles.action_receive!}" text=uiLabelMap.ProductReceiveProduct />
    </form>
</@section>