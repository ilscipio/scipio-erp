<#if commEventsUnknown?has_content>
    <@section>
        <form action="<@ofbizUrl>deleteCommunicationEvents</@ofbizUrl>" method="post">        
            <@table type="data-list" autoAltRows=true scrollable=true responsive=true fixedColumnsLeft=1> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
                <#-- Header Begins -->
                <@thead>
                    <@tr>
                        <@th>${uiLabelMap.CommonSubject}</@th>
                        <@th>${uiLabelMap.CommonEntryDate}</@th>
                        <@th>${uiLabelMap.CommonNote}</@th>                       
                        <@th></@th>                        
                    </@tr>
                </@thead>
                <#-- Header Ends-->
                <#list commEventsUnknown as commEvent>
                    <#-- assign status = marketingCampaign.getRelatedOne("StatusItem", false) / -->                   
                    <@tr>
                        <@td>${commEvent.subject!}</@td>
                        <@td>${commEvent.entryDate!}</@td>
                        <@td>${commEvent.note!}</@td>                                                 
                        <@td></@td>
                    </@tr>
                </#list>
            </@table>    
        </form>
    </@section>
</#if>
