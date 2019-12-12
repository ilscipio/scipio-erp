package com.ilscipio.scipio.product.seo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
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
    public int productFiltered = 0;

    public int categorySuccess = 0;
    public int categoryError = 0;
    public int categorySkipped = 0;
    public int categoryDupSkip = 0;
    public int categoryFiltered = 0;

    public int contentSuccess = 0;
    public int contentError = 0;
    public int contentSkipped = 0;
    public int contentDupSkip = 0;
    public int contentFiltered = 0;

    public UrlGenStats(boolean doProducts, boolean doCategory, boolean doContent) {
        this.doProducts = doProducts;
        this.doCategory = doCategory;
        this.doContent = doContent;
    }

    /**
     * Copy constructor.
     */
    public UrlGenStats(UrlGenStats other) {
        this.doProducts = other.doProducts;
        this.doCategory = other.doCategory;
        this.doContent = other.doContent;
        this.productSuccess = other.productSuccess;
        this.productError = other.productError;
        this.productSkipped = other.productSkipped;
        this.productDupSkip = other.productDupSkip;
        this.productFiltered = other.productFiltered;
        this.categorySuccess = other.categorySuccess;
        this.categoryError = other.categoryError;
        this.categorySkipped = other.categorySkipped;
        this.categoryDupSkip = other.categoryDupSkip;
        this.categoryFiltered = other.categoryFiltered;
        this.contentSuccess = other.contentSuccess;
        this.contentError = other.contentError;
        this.contentSkipped = other.contentSkipped;
        this.contentDupSkip = other.contentDupSkip;
        this.contentFiltered = other.contentFiltered;
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
        map.put("productFiltered", productFiltered);

        map.put("categorySuccess", categorySuccess);
        map.put("categoryError", categoryError);
        map.put("categorySkipped", categorySkipped);
        map.put("categoryDupSkip", categoryDupSkip);
        map.put("categoryFiltered", categoryFiltered);

        map.put("contentSuccess", contentSuccess);
        map.put("contentError", contentError);
        map.put("contentSkipped", contentSkipped);
        map.put("contentDupSkip", contentDupSkip);
        map.put("contentFiltered", contentFiltered);

        return map;
    }

    public Map<String, Object> toMap() {
        return toMap(new HashMap<>());
    }

    public void toMsgLists(Locale locale, List<String> msgList, List<String> errMsgList) {
        if (doProducts) {
            msgList.add("Products updated: " + productSuccess);
            if (productFiltered > 0) msgList.add("Products filtered: " + productFiltered);
            // I think these are mainly useful for debugging after all...
            //if (productSkipped > 0) msgList.add("Products skipped: " + productSkipped);
            //if (productDupSkip > 0) msgList.add("Product duplicates prevented: " + productDupSkip);
            if (productError > 0) errMsgList.add("Products failed: " + productError);
        }

        if (doCategory) {
            msgList.add("Categories updated: " + categorySuccess);
            if (categoryFiltered > 0) msgList.add("Categories filtered: " + categoryFiltered);
            //if (categorySkipped > 0) msgList.add("Categories skipped: " + categorySkipped);
            //if (categoryDupSkip > 0) msgList.add("Category duplicates prevented: " + categoryDupSkip);
            if (categoryError > 0) errMsgList.add("Categories failed: " + categoryError);
        }

        if (doContent) {
            msgList.add("Content updated: " + contentSuccess);
            if (contentFiltered > 0) msgList.add("Content filtered: " + contentFiltered);
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