<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section title=uiLabelMap.CommonDate>
        <@table type="fields"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
            <@tr>
                <@td align="right" valign="top" width="20%">
                    &nbsp;${uiLabelMap.OrderOrderQuoteIssueDate}
                </@td>
                <@td valign="top" width="80%">
                    ${(quote.issueDate.toString())!}
                </@td>
            </@tr>
            <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
            <@tr>
                <@td align="right" valign="top" width="20%">
                    &nbsp;${uiLabelMap.CommonValidFromDate}
                </@td>
                <@td valign="top" width="80%">
                    ${(quote.validFromDate.toString())!}
                </@td>
            </@tr>
            <@tr type="util"><@td colspan="3"><hr /></@td></@tr>
            <@tr>
                <@td align="right" valign="top" width="20%">
                    &nbsp;${uiLabelMap.CommonValidThruDate}
                </@td>
                <@td valign="top" width="80%">
                    ${(quote.validThruDate.toString())!}
                </@td>
            </@tr>
        </@table>
</@section>
