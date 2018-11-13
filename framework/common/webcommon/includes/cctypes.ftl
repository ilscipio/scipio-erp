<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#list creditCardTypes as creditCardType>
  <option value="${creditCardType.enumId}">${creditCardType.enumCode}</option>
</#list>