import org.ofbiz.base.util.Debug
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator

public String findTrackingCodes() {
    marketingCampaignId = parameters.marketingCampaignId;
    trackingCodeId = parameters.trackingCodeId;

    if (marketingCampaignId) {       
        exprs = EntityCondition.makeCondition("marketingCampaignId", EntityOperator.EQUALS, marketingCampaignId);        
        trackingCodeList = from("TrackingCode").where(exprs).queryList();
        request.setAttribute("trackingCodeList", trackingCodeList);
    }
    return "success";
}