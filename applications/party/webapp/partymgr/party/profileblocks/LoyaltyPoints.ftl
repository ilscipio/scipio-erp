<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

    <@section id="totalOrders" title=uiLabelMap.PartyLoyaltyPoints>
        <#if monthsToInclude?? && totalSubRemainingAmount?? && totalOrders??>    
          ${totalSubRemainingAmount} ${uiLabelMap.PartyPointsFrom} ${totalOrders} ${uiLabelMap.PartyOrderInLast} ${monthsToInclude} ${uiLabelMap.CommonMonths}.
        </#if>
    </@section>

  