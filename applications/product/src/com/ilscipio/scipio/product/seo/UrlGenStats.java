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
 * TODO: formalize this better, got annoyed with it.
 * TODO: LOCALIZE MESSAGES
 */
public class UrlGenStats {
    public final boolean doProducts;
    public final boolean doCategory;
    public final boolean doContent;
    
    public int productSuccess = 0;
    public int productError = 0;
    public int productSkipped = 0;
    public int productDupSkip = 0;
    
    public int categorySuccess = 0;
    public int categoryError = 0;
    public int categorySkipped = 0;
    public int categoryDupSkip = 0;
    
    public int contentSuccess = 0;
    public int contentError = 0;
    public int contentSkipped = 0;
    public int contentDupSkip = 0;


    public UrlGenStats(boolean doProducts, boolean doCategory, boolean doContent) {
        this.doProducts = doProducts;
        this.doCategory = doCategory;
        this.doContent = doContent;
    }

    public boolean hasError() {
        return productError > 0 || categoryError > 0 || contentError > 0;
    }
    
    public Map<String, Object> toMap(Map<String, Object> map) {
        map.put("doProducts", doProducts);
        map.put("doCategory", doCategory);
        map.put("doContent", doContent);
        
        map.put("productSuccess", productSuccess);
        map.put("productSkipped", productSkipped);
        map.put("productError", productError);
        map.put("productDupSkip", productDupSkip);

        map.put("categorySuccess", categorySuccess);
        map.put("categoryError", categoryError);
        map.put("categorySkipped", categorySkipped);
        map.put("categoryDupSkip", categoryDupSkip);

        map.put("contentSuccess", contentSuccess);
        map.put("contentError", contentError);
        map.put("contentSkipped", contentSkipped);
        map.put("contentDupSkip", contentDupSkip);

        return map;
    }
    
    public Map<String, Object> toMap() {
        return toMap(new HashMap<String, Object>());
    }
    
    public void toMsgLists(Locale locale, List<String> msgList, List<String> errMsgList) {
        if (doProducts) {
            msgList.add("Products updated: " + productSuccess);
            // I think these are mainly useful for debugging after all...
            //if (productSkipped > 0) msgList.add("Products skipped: " + productSkipped);
            //if (productDupSkip > 0) msgList.add("Product duplicates prevented: " + productDupSkip);
            if (productError > 0) errMsgList.add("Products failed: " + productError);
        }
        
        if (doCategory) {
            msgList.add("Categories updated: " + categorySuccess);
            //if (categorySkipped > 0) msgList.add("Categories skipped: " + categorySkipped);
            //if (categoryDupSkip > 0) msgList.add("Category duplicates prevented: " + categoryDupSkip);
            if (categoryError > 0) errMsgList.add("Categories failed: " + categoryError);
        }
        
        if (doContent) {
            msgList.add("Content updated: " + contentSuccess);
            //if (contentSkipped > 0) msgList.add("Content skipped: " + contentSkipped);
            //if (contentDupSkip > 0) msgList.add("Content duplicates prevented: " + contentDupSkip);
            if (contentError > 0) errMsgList.add("Content failed: " + contentError);
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