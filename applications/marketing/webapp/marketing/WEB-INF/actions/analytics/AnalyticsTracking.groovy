import org.ofbiz.base.util.Debug
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator

public String findTrackingCodes() {
    marketingCampaignId = parameters.marketingCampaignId;
    trackingCodeId = parameters.trackingCodeId;
//    Debug.log("marketingCampaignId ==========> " + marketingCampaignId + "   trackingCodeId =======> " + trackingCodeId);
    
//    exprs = [:];
//    if (exprs)
//        exprs = EntityCondition.makeCondition("marketingCampaignId", EntityOperator.EQUALS, marketingCampaignId);
//    marketingCampaignList = delegator.findByAnd("MarketingCampaign", exprs , null, false);
//    context.marketingCampaignList = marketingCampaignList;

    if (marketingCampaignId) {       
        exprs = EntityCondition.makeCondition("marketingCampaignId", EntityOperator.EQUALS, marketingCampaignId);        
        trackingCodeList = from("TrackingCode").where(exprs).queryList();
        request.setAttribute("trackingCodeList", trackingCodeList);
    }
    return "success";
}