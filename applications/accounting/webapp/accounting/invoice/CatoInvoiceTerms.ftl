<#-- CATO -->
<@section title=uiLabelMap.AccountingAgreementItemTerms>
    <@table type="fields">
        <@thead>
            <@tr valign="bottom" class="header-row">
                <@th>${uiLabelMap.FormFieldTitle_termDays!}</@th>
                <@th>${uiLabelMap.FormFieldTitle_termTypeId!}</@th>
                <@th>${uiLabelMap.FormFieldTitle_termValue!}</@th>
                <@th>${uiLabelMap.FormFieldTitle_textData!}</@th>
                <@th>${uiLabelMap.FormFieldTitle_textValue!}</@th>
            </@tr>
        </@thead>
       <#list invoiceTerms as item>
            <#assign itemType = item.getRelatedOne("TermType", false)!/>
            <@tr>
                <@td>${item.termDays!}</@td>
                <@td>${(itemType.get("description",locale))!}</@td>
                <@td><@ofbizCurrency isoCode=item.uomId amount=(item.termvalue!)/></@td>
                <@td>${item.FormFieldTitle_textData!}</@td>
                <@td>${item.FormFieldTitle_textValue!}</@td>
            </@tr>
        </#list>

    </@table>
</@section>