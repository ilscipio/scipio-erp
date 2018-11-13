<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#list creditCardTypes as creditCardType>
  <option value="${creditCardType.enumId}">${creditCardType.enumCode}</option>
</#list>