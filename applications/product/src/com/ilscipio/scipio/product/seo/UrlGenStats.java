package com.ilscipio.scipio.product.seo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.service.ServiceUtil;

/**
 * Stats for product/category iterating services.
 * TODO: LOCALIZE MESSAGES
 */
public class UrlGenStats {
    public final boolean doProducts;
    public final boolean doCategory;
    public final boolean useSkipped;
    
    public int productSuccess = 0;
    public int productSkipped = 0;
    public int productError = 0;
    
    public int categorySuccess = 0;
    public int categoryError = 0;
    public int categorySkipped = 0;

    public UrlGenStats(boolean doProducts, boolean doCategory, boolean useSkipped) {
        this.doProducts = doProducts;
        this.doCategory = doCategory;
        this.useSkipped = useSkipped;
    }
    
    public UrlGenStats(boolean doProducts, boolean doCategory) {
        this(doProducts, doCategory, true);
    }

    public boolean hasError() {
        return productError > 0 || categoryError > 0;
    }
    
    public Map<String, Object> toMap(Map<String, Object> map) {
        map.put("doProducts", doProducts);
        map.put("doCategory", doCategory);
        map.put("useSkipped", useSkipped);
        
        map.put("productSuccess", productSuccess);
        map.put("productSkipped", productSkipped);
        map.put("productError", productError);
        
        map.put("categorySuccess", categorySuccess);
        map.put("categoryError", categoryError);
        map.put("categorySkipped", categorySkipped);
        return map;
    }
    
    public Map<String, Object> toMap() {
        return toMap(new HashMap<String, Object>());
    }
    
    public void toMsgLists(Locale locale, List<String> msgList, List<String> errMsgList) {
        if (doProducts) {
            msgList.add("Products updated: " + productSuccess);
            if (useSkipped) msgList.add("Products skipped: " + productSkipped);
            if (productError > 0) errMsgList.add("Products failed: " + productError);
        }
        
        if (doCategory) {
            msgList.add("Categories updated: " + categorySuccess);
            if (useSkipped) msgList.add("Categories skipped: " + categorySkipped);
            if (categoryError > 0) errMsgList.add("Categories failed: " + categoryError);
        }
    }
    
    public String toMsg(Locale locale) {
        List<String> msgList = new ArrayList<>();
        List<String> errMsgList = new ArrayList<>();
        toMsgLists(locale, msgList, errMsgList);
        
        List<String> allMsgs = new ArrayList<>();
        allMsgs.addAll(msgList);
        allMsgs.addAll(errMsgList);
        return StringUtils.join(allMsgs, "; ");
    }
    
    public Map<String, Object> toServiceResultSuccessFailure(String msg) {
        return hasError() ? ServiceUtil.returnFailure(msg) : ServiceUtil.returnSuccess(msg);
    }

}