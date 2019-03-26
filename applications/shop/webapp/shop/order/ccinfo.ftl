<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if !creditCard?has_content>
  <#assign creditCard = requestParameters>
</#if>
<hr />

<@render resource="component://accounting/widget/CommonScreens.xml#creditCardFields" />

