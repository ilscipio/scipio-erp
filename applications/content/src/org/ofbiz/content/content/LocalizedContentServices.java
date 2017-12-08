package org.ofbiz.content.content;

import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.PropertyMessageExUtil;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.content.content.LocalizedContentWorker.LocalizedSimpleTextInfo;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * SCIPIO: Dedicated LocalizedContentServices class.
 * Added 2017-12-06.
 */
public abstract class LocalizedContentServices {

    public static final String module = LocalizedContentServices.class.getName();
    
    protected LocalizedContentServices() {
    }

    public static Map<String, Object> replaceContentLocalizedSimpleTexts(DispatchContext dctx, Map<String, ? extends Object> context) {
        // NOTE: error messages kept brief, because caller provides prefix
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        List<Map<String, Object>> entries = UtilGenerics.checkList(context.get("entries"));
        String mainContentId = (String) context.get("mainContentId");

        // TODO?: if needed; always false for now (ignores contentIds)
        // I don't yet see a case where want to honor contentIds
        //boolean strictContent = Boolean.TRUE.equals(context.get("strictContent"));
        
        try {
            LocalizedSimpleTextInfo entriesInfo = LocalizedSimpleTextInfo.fromEntries(entries);

            GenericValue mainContent = null;
            if (UtilValidate.isNotEmpty(mainContentId)) {
                mainContent = delegator.findOne("Content", UtilMisc.toMap("contentId", mainContentId), false);
                if (mainContent == null) {
                    return ServiceUtil.returnError(UtilProperties.getMessage("ContentUiLabels", 
                            "ContentNoContentFound", UtilMisc.toMap("contentId", mainContentId), locale));
                }
            }
            String mainLocaleString = entriesInfo.getMainLocaleString();
            String mainTextData = entriesInfo.getMainTextData();
            
            if (!entriesInfo.isHasTextData()) {
                if (mainContent != null) {
                    // simple case: delete all the ALTERNATE_LOCALE associations, and set
                    // the allContentEmpty flag so the caller may delete the main record (if wanted).
                    LocalizedContentWorker.removeAllAlternateLocaleRecords(delegator, dispatcher, context, mainContentId);
                    // now update the mainContent (for localeString only), because we don't know how caller will handle allContentEmpty,
                    LocalizedContentWorker.updateSimpleTextContent(delegator, dispatcher, mainContent, mainLocaleString, mainTextData);
                } else {
                    ; // trivial case: had nothing, update nothing.
                }
                Map<String, Object> result = ServiceUtil.returnSuccess();
                result.put("allContentEmpty", Boolean.TRUE);
                result.put("mainContentId", mainContentId);
                return result;
            } else {
                mainContent = LocalizedContentWorker.replaceLocalizedContent(delegator, dispatcher, context, mainContent, 
                        mainLocaleString, mainTextData, entriesInfo.getLocaleEntryMap(), true, UtilDateTime.nowTimestamp(), 
                        newContentFields, newDataResourceFields);
                mainContentId = mainContent.getString("contentId");

                Map<String, Object> result = ServiceUtil.returnSuccess();
                result.put("allContentEmpty", Boolean.FALSE);
                result.put("mainContentId", mainContentId);
                return result;
            }
        } catch(Exception e) {
            //return PropertyMessageExUtil.makeServiceErrorResult(e, locale);
            return ServiceUtil.returnError(PropertyMessageExUtil.getExceptionMessage(e, locale));
        }
    }
    
    private static final Map<String, Object> newContentFields = UtilMisc.toMap("description", null);
    private static final Map<String, Object> newDataResourceFields = UtilMisc.toMap("statusId", "CTNT_PUBLISHED");

}
