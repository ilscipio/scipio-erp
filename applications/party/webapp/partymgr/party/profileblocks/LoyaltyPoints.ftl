<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

    <@section id="totalOrders" title=uiLabelMap.PartyLoyaltyPoints>
        <#if monthsToInclude?? && totalSubRemainingAmount?? && totalOrders??>    
          ${totalSubRemainingAmount} ${uiLabelMap.PartyPointsFrom} ${totalOrders} ${uiLabelMap.PartyOrderInLast} ${monthsToInclude} ${uiLabelMap.CommonMonths}.
        </#if>
    </@section>

  