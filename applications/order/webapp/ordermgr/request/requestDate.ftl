<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section title=uiLabelMap.CommonDate>
        <@table type="fields"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
            <@tr>
                <@td align="right" valign="top" width="25%">
                    &nbsp;${uiLabelMap.OrderRequestDate}
                </@td>
                <@td width="5%">&nbsp;</@td>
                <@td valign="top" width="70%">
                    ${(custRequest.custRequestDate.toString())!}
                </@td>
            </@tr>
            <@tr type="util"><@td colspan="7"><hr /></@td></@tr>
            <@tr>
                <@td align="right" valign="top" width="25%">
                    &nbsp;${uiLabelMap.OrderRequestCreatedDate}
                </@td>
                <@td width="5%">&nbsp;</@td>
                <@td valign="top" width="70%">
                    ${(custRequest.createdDate.toString())!}
                </@td>
            </@tr>
            <@tr type="util"><@td colspan="7"><hr /></@td></@tr>
            <@tr>
                <@td align="right" valign="top" width="25%">
                    &nbsp;${uiLabelMap.OrderRequestLastModifiedDate}
                </@td>
                <@td width="5%">&nbsp;</@td>
                <@td valign="top" width="70%">
                    ${(custRequest.lastModifiedDate.toString())!}
                </@td>
            </@tr>
        </@table>
</@section>
