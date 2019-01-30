<#if listMarketingCampaign?has_content>
    <@section> 
        <form action="<@pageUrl>updateMarketingCampaign</@pageUrl>" method="post">
        
            <@table type="data-list" autoAltRows=true scrollable=true responsive=true fixedColumnsLeft=1>
                <#-- Header Begins -->
                <@thead>
                    <@tr>
                        <@th>${uiLabelMap.MarketingCampaignId}</@th>
                        <@th>${uiLabelMap.MarketingCampaignName}</@th>
                        <@th>${uiLabelMap.MarketingParentCampaignId}</@th>                       
                        <@th>${uiLabelMap.CommonStatus}</@th>
                        <#if showActionButtons=="Y">
                            <@th>${uiLabelMap.CommonDelete}</@th>
                        </#if>
                    </@tr>
                </@thead>
                <#-- Header Ends-->
                <#list listMarketingCampaign as marketingCampaign>
                    <#assign status = marketingCampaign.getRelatedOne("StatusItem", false) />                   
                    <@tr>
                        <@td><a href="<@pageUrl>EditMarketingCampaign?marketingCampaignId=${marketingCampaign.marketingCampaignId}</@pageUrl>">${marketingCampaign.marketingCampaignId}</a></@td>
                        <@td>${marketingCampaign.campaignName!}</@td>
                        <@td>${marketingCampaign.parentCampaignId!}</@td>                                                 
                        <@td>${status.description}</@td> 
                        <#if showActionButtons=="Y">                     
                            <@td><a href="javascript:document.deleteMarketingCampaign_${marketingCampaign_index}.submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a></@td>
                        </#if>
                    </@tr>
                </#list>
            </@table>    
        </form>
        <#list listMarketingCampaign as marketingCampaign>
            <form name="deleteMarketingCampaign_${marketingCampaign_index}" action="<@pageUrl>removeMarketingCampaign</@pageUrl>" method="post">
                <@field name="marketingCampaignId" type="hidden" value=marketingCampaign.marketingCampaignId />
            </form>
        </#list>
    </@section>
</#if>