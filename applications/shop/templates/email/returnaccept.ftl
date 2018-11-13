<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#-- ReturnHeader and a List of ReturnItem records is available to this template -->
<br />
${uiLabelMap.EcommerceReturnRequest}
<br /><br />
<pre>
<#list returnItems as item>
  <#assign returnReason = item.getRelatedOne("ReturnReason", false)!>
  <#assign returnType = item.getRelatedOne("ReturnType", false)!>
  ${item.description!(uiLabelMap.CommonNA)} - ${item.returnQuantity?string.number} @ ${item.returnPrice}
</#list>
</pre>
<br /><br />

${uiLabelMap.EcommerceReturnRequestAccepted} ${returnHeader.returnId}
<br /><br />
