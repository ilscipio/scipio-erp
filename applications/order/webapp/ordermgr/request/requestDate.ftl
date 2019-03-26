<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section title=uiLabelMap.CommonDate>
    <@table type="fields">
        <@tr>
            <@td width="35%">${uiLabelMap.OrderRequestDate}</@td>
            <@td width="65%"><@formattedDateTime date=custRequest.custRequestDate! defaultVal="0000-00-00 00:00:00"/></@td>
        </@tr>
        <@tr>
            <@td width="35%">${uiLabelMap.OrderRequestCreatedDate}</@td>
            <@td width="65%"><@formattedDateTime date=custRequest.createdDate! defaultVal="0000-00-00 00:00:00"/></@td>
        </@tr>
        <@tr>
            <@td width="35%">${uiLabelMap.OrderRequestLastModifiedDate}</@td>
            <@td width="65%"><@formattedDateTime date=custRequest.lastModifiedDate! defaultVal="0000-00-00 00:00:00"/></@td>
        </@tr>
    </@table>
</@section>
