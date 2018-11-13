<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#assign stringYear = thisDate?string("yyyy")>
<#assign thisYear = stringYear?number>

<option></option>
<#list 0..10 as i>
    <#assign expireYear = thisYear + i>
    <option value="${expireYear}">${expireYear}</option>
</#list>