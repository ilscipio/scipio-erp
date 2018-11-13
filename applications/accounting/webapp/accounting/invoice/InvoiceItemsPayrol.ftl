<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<form method="post" action="createInvoiceItemPayrol">
    <input type="hidden" name="invoiceId" value="${invoice.invoiceId}" />
    <@row>        
        <#if PayrolGroup?has_content>
            <#assign last=false />
            <#list PayrolGroup as payrolGroup>                
                <#if payrolGroup == PayrolGroup?last>
                    <#assign last = true>
                </#if>
                <@cell columns=4 last=last>            
                    <@section title=payrolGroup.description>                        
                        <@row>
                            <@cell columns=6><strong>Description</strong></@cell>
                            <@cell columns=3><strong>Quantity</strong></@cell>
                            <@cell columns=3 last=true><strong>Amount</strong></@cell>
                        </@row>
                        <#if PayrolList?has_content>
                            <#list PayrolList as payrolList>
                                <#if (payrolList.parentTypeId!) == (payrolGroup.invoiceItemTypeId!)>
                                    <@row>
                                        <@cell columns=6>${payrolList.description}</@cell>
                                        <@cell columns=3><@field type="input" size=10 name="${rawString(payrolList.invoiceItemTypeId)}_Quantity"/></@cell>
                                        <@cell columns=3 last=true><@field type="input" size=10 name="${rawString(payrolList.invoiceItemTypeId)}_Amount"/></@cell>                                
                                    </@row>
                                </#if>
                            </#list>
                        </#if>                        
                    </@section>
                </@cell>
            </#list>
        </#if>
    </@row>
    <@field type="submitarea" label=uiLabelMap.CommonAdd>
        <input type="submit" value="Add" class="${styles.link_run_sys!} ${styles.action_add!}" />        
    </@field>
</form>
