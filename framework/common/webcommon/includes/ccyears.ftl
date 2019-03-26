<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#assign stringYear = thisDate?string("yyyy")>
<#assign thisYear = stringYear?number>

<option></option>
<#list 0..10 as i>
    <#assign expireYear = thisYear + i>
    <option value="${expireYear}">${expireYear}</option>
</#list>