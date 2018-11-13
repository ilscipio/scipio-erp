<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#escape x as x?xml>
<fo:block content-width="auto" font-size="10pt">
    <fo:table table-layout="fixed" width="100%">
        <fo:table-column column-width="20mm"/>
        <fo:table-column/>
        <fo:table-body>
        <fo:table-row>
          <fo:table-cell><fo:block>${uiLabelMap.AccountingInvoiceDateAbbr}:</fo:block></fo:table-cell>
          <fo:table-cell><fo:block>${invoiceDate!?date}</fo:block></fo:table-cell>
        </fo:table-row>
        
        <fo:table-row>
          <fo:table-cell><fo:block>${uiLabelMap.AccountingCustNr}:</fo:block></fo:table-cell>
          <fo:table-cell><fo:block><#if billToParty?has_content>${billToParty.partyId}</#if></fo:block></fo:table-cell>
        </fo:table-row>
        
        <fo:table-row>
          <fo:table-cell><fo:block>${uiLabelMap.AccountingInvNr}:</fo:block></fo:table-cell>
          <fo:table-cell><fo:block><#if invoice?has_content>${invoice.invoiceId}</#if></fo:block></fo:table-cell>
        </fo:table-row>
        <#if invoice?has_content && invoice.description?has_content>
          <fo:table-row>
            <fo:table-cell><fo:block>${uiLabelMap.AccountingDescr}:</fo:block></fo:table-cell>
            <fo:table-cell><fo:block>${invoice.description}</fo:block></fo:table-cell>
          </fo:table-row>
        </#if>
        
        <!--fo:table-row>
          <fo:table-cell><fo:block>${uiLabelMap.CommonStatus}</fo:block></fo:table-cell>
          <fo:table-cell><fo:block font-weight="bold">${invoiceStatus.get("description",locale)}</fo:block></fo:table-cell>
        </fo:table-row-->
        </fo:table-body>
    </fo:table>
</fo:block>
</#escape>