<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if orderHeader.statusId == "ORDER_REJECTED">
    ${uiLabelMap.OrderEmailProblemWithYourPaymentMethod} ${orderHeader.orderId}.<br />
    ${uiLabelMap.OrderEmailProblemCancelledNotProcessed}<br />
<#elseif orderHeader.statusId == "ORDER_APPROVED">
    ${uiLabelMap.OrderEmailPaymentOK} ${orderHeader.orderId} ${uiLabelMap.OrderEmailAccepted}
<#else>
      ${uiLabelMap.OrderEmailSorry} ${orderHeader.orderId}.<br />
</#if>
    <br />${uiLabelMap.OrderEmailCustomerService} (info@scipioerp.com)
