<#-- SCIPIO -->

<@form name="massInvoiceDownload" action=makePageUrl("massDownloadInvoices.pdf") method="POST">
    <@fields>
        <@field type="hidden" name="partyGroupId" value=myCompanyId! />
        <@field type="datetime" name="fromDate" label=getLabel("CommonFrom") />
        <@field type="datetime" name="thruDate" label=getLabel("CommonThru")/>
        <@field type="select" name="status" label=getLabel("CommonStatus")>
            <option value="">--</option>
            <#list invoiceStatuses as invoiceStatus>
                <option value="${invoiceStatus.statusId}">${invoiceStatus.description}</option>
            </#list>
        </@field>
        <@field type="select" name="type" label=getLabel("CommonType")>
            <option value="">--</option>
            <#list invoiceTypes as invoiceType>
                <option value="${invoiceType.invoiceTypeId}">${invoiceType.description}</option>
            </#list>
        </@field>
        <@field type="lookup" name="partyIdFrom" formName="massInvoiceDownload" id="partyIdFrom" fieldFormName="LookupPartyName" label=uiLabelMap.PartyPartyFrom />
        <@field type="lookup" name="partyIdTo" formName="massInvoiceDownload" id="partyIdTo" fieldFormName="LookupPartyName" label=uiLabelMap.PartyPartyTo />
        <@field type="submit" name="generate" label=getLabel("CommonDownload")/>
    </@fields>
</@form>