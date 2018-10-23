<@section>
    <form name="FindMarketingCampaign" action="<@ofbizUrl>FindMarketingCampaign</@ofbizUrl>" method="POST">
       <@field type="input" name="marketingCampaignId" label=uiLabelMap.MarketingCampaignId />
       <@field type="input" name="campaignName" label=uiLabelMap.MarketingCampaignName />
       <@field type="input" name="parentCampaignId" label=uiLabelMap.MarketingParentCampaignId />
       <@field type="select" name="statusId" label=uiLabelMap.CommonStatus>
            <#assign statusList=delegator.findByAnd("StatusItem", {"statusTypeId" , "MKTG_CAMP_STATUS"}, null, true)/>
            <option value=""></option>
            <#list statusList as status>
                <option value="${status.statusId}">${status.description}</option>
            </#list>
       </@field>
       <@field type="submit" name="find" text=uiLabelMap.CommonFind class="+${styles.link_run_sys!} ${styles.action_find!}" />
    </form>
</@section>