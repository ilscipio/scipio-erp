<#if listMarketingCampaign?has_content>
    <@section>
        <form action="<@ofbizUrl>UpdateInventoryTransfer</@ofbizUrl>" method="post">
        <input type="hidden" name="facilityId" value="${facilityId!}" />        
        
            <@table type="data-list" autoAltRows=true scrollable=true responsive=true fixedColumnsLeft=1> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
                <#-- Header Begins -->
                <@thead>
                    <@tr>
                        <@th>${uiLabelMap.MarketingCampaignId}</@th>
                        <@th>${uiLabelMap.MarketingCampaignName}</@th>
                        <@th>${uiLabelMap.MarketingParentCampaignId}</@th>                       
                        <@th>${uiLabelMap.CommonStatus}</@th>
                        <@th>${uiLabelMap.CommonDelete}</@th>
                    </@tr>
                </@thead>
                <#-- Header Ends-->
                <#list listMarketingCampaign as marketingCampaign>
                    <#assign status = marketingCampaign.getRelatedOne("StatusItem", false) />                   
                    <@tr>
                        <@td><a href="<@ofbizUrl>EditMarketingCampaign?marketingCampaignId=${marketingCampaign.marketingCampaignId}</@ofbizUrl>">${marketingCampaign.marketingCampaignId}</a></@td>
                        <@td>${marketingCampaign.campaignName!}</@td>
                        <@td>${marketingCampaign.parentCampaignId!}</@td>                                                 
                        <@td>${status.description}</@td>                        
                        <@td><a href="javascript:document.deleteMarketingCampaign_${marketingCampaign_index}.submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a></@td>
                    </@tr>
                </#list>
            </@table>    
        </form>
        <#list listMarketingCampaign as marketingCampaign>
            <form name="deleteMarketingCampaign_${marketingCampaign_index}" action="<@ofbizUrl>removeMarketingCampaign</@ofbizUrl>" method="POST">
                <@field name="marketingCampaignId=" type="hidden" value=marketingCampaign.marketingCampaignId />
            </form>
        </#list>
    </@section>
</#if>