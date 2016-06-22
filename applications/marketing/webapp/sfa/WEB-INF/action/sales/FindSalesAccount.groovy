
marketingCampaignId = parameters.marketingCampaignId;
campaignName = parameters.campaignName;
parentCampaignId = parameters.parentCampaignId;
statusId = parameters.statusId;

condition=[:];
if (marketingCampaignId)
    condition.put("marketingCampaignId", marketingCampaignId);
if (campaignName)
    condition.put("campaignName", campaignName);
if (parentCampaignId)    
    condition.put("parentCampaignId", parentCampaignId);
if (statusId)
    condition.put("statusId", statusId);
listMarketingCampaign=from("MarketingCampaign").where(condition).queryList();
context.listMarketingCampaign=listMarketingCampaign;