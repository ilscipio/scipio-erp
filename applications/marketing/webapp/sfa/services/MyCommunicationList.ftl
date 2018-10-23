<#if commEventList?has_content>
    <@section>
        <form action="<@ofbizUrl>deleteCommunicationEvents</@ofbizUrl>" method="post">        
            <@table type="data-list" autoAltRows=true scrollable=true responsive=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
                <#-- Header Begins -->
                <@thead>
                    <@tr>                        
                        <@th>${uiLabelMap.PartyPartyTo}</@th>
                        <@th>${uiLabelMap.PartySubject}</@th>
                        <@th>${uiLabelMap.MarketingCommunicationStatusId}</@th>
                        <@th>${uiLabelMap.MarketingContactListCommEventTypeId}</@th>
                        <@th>${uiLabelMap.OrderEntryDate}</@th>
                        <@th>${uiLabelMap.CommonNote}</@th>                       
                        <@th>${uiLabelMap.CommonDelete}</@th>                        
                    </@tr>
                </@thead>
                <#-- Header Ends-->
                <#list commEventList as commEvent>
                    <#assign commEventStatus = commEvent.getRelatedOne("StatusItem", true)>
                    <#assign commEventType = commEvent.getRelatedOne("CommunicationEventType", true)>
                    <@tr>                        
                        <@td>
                            <@field name="partyId" type="hidden" value=(commEvent.partyId) />
                            <@field name="roleTypeId" type="hidden" value=(commEvent.roleTypeId) />
                            ${commEvent.partyIdFrom!uiLabelMap.PartyUnknown}
                        </@td>
                        <@td>
                          <#if commEvent.subject?has_content>
                            <#assign subject = commEvent.subject />
                            <#if commEvent.subject?length &gt; 25>
                                <#assign subject = commEvent.subject[0..<25] + "..." />
                            </#if>
                            <a href="<@ofbizInterWebappUrl>/partymgr/control/EditCommunicationEvent?communicationEventId=${commEvent.communicationEventId}&partyId=${commEvent.partyId}</@ofbizInterWebappUrl>">${subject}</a>
                          </#if>
                        </@td>
                        <@td>${commEventStatus.description}</@td>
                        <@td>${commEventType.description}</@td>
                        <@td>${commEvent.entryDate?string("yyyy-mm-dd HH:mm:ss")!}</@td>
                        <@td>${commEvent.note!}</@td>                                                 
                        <@td><a href="javascript:document.deleteCommunicationEvents_${commEvent_index}.submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a></@td>
                    </@tr>
                </#list>
            </@table>    
        </form>
    </@section>
<#else>
    <@commonMsg type="result-norecord">${uiLabelMap.MarketingServiceCommunicationNotFound}.</@commonMsg>    
</#if>
