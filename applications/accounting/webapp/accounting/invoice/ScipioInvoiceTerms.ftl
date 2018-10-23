<#-- SCIPIO -->
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
                <@td><@ofbizCurrency isoCode=item.uomId amount=(item.termValue!0)/></@td>
                <@td>${item.description!}</@td><#-- TODO: REVIEW: this field name was invalid, I can only guess: ${item.textData!} -->
                <@td>${item.textValue!}</@td>
            </@tr>
        </#list>

    </@table>
</@section>