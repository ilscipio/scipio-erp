package org.ofbiz.content.content;

import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
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
import org.ofbiz.service.ServiceHandler;
import org.ofbiz.service.ServiceUtil;

/**
 * Alternate locale-handling localized content services.
 *
 * <p>SCIPIO: 3.0.0: Migrated services from component://content/script/org/ofbiz/content/content/LocalizedContentServices.xml.</p></p>
 * <p>SCIPIO: 2017-12-06: Added.</p>
 */
public abstract class LocalizedContentServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final Map<String, Object> newContentFields = UtilMisc.toMap("description", null);
    private static final Map<String, Object> newDataResourceFields = UtilMisc.toMap("statusId", "CTNT_PUBLISHED");


    protected LocalizedContentServices() {
    }

    public static Map<String, Object> replaceContentLocalizedSimpleTexts(DispatchContext dctx, Map<String, ? extends Object> context) {
        // NOTE: error messages kept brief, because caller provides prefix
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        List<Map<String, Object>> entries = UtilGenerics.checkList(context.get("entries"));
        String mainContentId = (String) context.get("mainContentId");
        Map<String, Object> newContentFields = UtilGenerics.cast(context.get("newContentFields"));
        if (newContentFields != null) {
            newContentFields = UtilMisc.putAll(new HashMap<>(LocalizedContentServices.newContentFields), newContentFields);
        } else {
            newContentFields = new HashMap<>(LocalizedContentServices.newContentFields);
        }
        Map<String, Object> newDataResourceFields = UtilGenerics.cast(context.get("newDataResourceFields"));
        if (newDataResourceFields != null) {
            newDataResourceFields = UtilMisc.putAll(new HashMap<>(LocalizedContentServices.newDataResourceFields), newDataResourceFields);
        } else {
            newDataResourceFields = new HashMap<>(LocalizedContentServices.newDataResourceFields);
        }

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
                        newContentFields, newDataResourceFields, null, null);
                mainContentId = mainContent.getString("contentId");

                Map<String, Object> result = ServiceUtil.returnSuccess();
                result.put("allContentEmpty", Boolean.FALSE);
                result.put("mainContentId", mainContentId);
                return result;
            }
        } catch (Exception e) {
            //return PropertyMessageExUtil.makeServiceErrorResult(e, locale);
            return ServiceUtil.returnError(PropertyMessageExUtil.getExceptionMessage(e, locale));
        }
    }

    public static class CreateSimpleTextContentForAlternateLocale extends ServiceHandler.Local implements ServiceHandler.Exec {
        @Override
        public Map<String, Object> exec() throws GeneralException {
            try {
                Map<String, Object> createTextCtx = ctx.makeValidInContext("createSimpleTextContent");
                Map<String, Object> createTextResult = ctx.dispatcher().runSync("createSimpleTextContent", createTextCtx);
                if (!ServiceUtil.isSuccess(createTextResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(createTextResult));
                }

                String contentId = (String) createTextResult.get("contentId");
                Map<String, Object> createAssocCtx = UtilMisc.toMap("userLogin", ctx.attr("userLogin"),
                        "locale", ctx.attr("locale"), "timeZone", ctx.attr("timeZone"));
                createAssocCtx.put("contentId", ctx.get("mainContentId"));
                createAssocCtx.put("contentIdTo", contentId);
                createAssocCtx.put("contentAssocTypeId", "ALTERNATE_LOCALE");
                Map<String, Object> createAssocResult = ctx.dispatcher().runSync("createContentAssoc", createAssocCtx);
                if (!ServiceUtil.isSuccess(createAssocResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(createAssocResult));
                }
                return UtilMisc.put(ServiceUtil.returnSuccess(), "contentId", contentId);
            } catch (GeneralException e) {
                return ServiceUtil.returnError(e.toString());
            }
        }
    }

    public static class UpdateSimpleTextContentForAlternateLocale extends ServiceHandler.Local implements ServiceHandler.Exec {
        @Override
        public Map<String, Object> exec() throws GeneralException {
            try {
                List<GenericValue> contentAssocList = ctx.delegator().from("ContentAssoc").where("contentId", ctx.attr("mainContentId"),
                        "contentAssocTypeId", "ALTERNATE_LOCALE", "contentIdTo", ctx.attr("contentId")).filterByDate().queryList();
                if (UtilValidate.isEmpty(contentAssocList)) {
                    String errMsg = UtilProperties.getMessage("ContentUiLabels", "ContentNoContentFound",
                            UtilMisc.toMap("contentId", ctx.attr("contentId")), ctx.locale()) + " (mainContentId: " + ctx.attr("mainContentId") + ")";
                    return ServiceUtil.returnError(errMsg);
                }

                GenericValue content = ctx.delegator().from("Content").where("contentId", ctx.attr("contentId")).queryOne();
                if (content == null) {
                    String errMsg = UtilProperties.getMessage("ContentUiLabels", "ContentNoContentFound",
                            UtilMisc.toMap("contentId", ctx.attr("contentId")), ctx.locale()) + " (mainContentId: " + ctx.attr("mainContentId") + ")";
                    return ServiceUtil.returnError(errMsg);
                }
                content.setNonPKFields(ctx.context(), true); // true because containsKey() logic is used (not null/empty)
                content.store();

                Map<String, Object> updateCtx = ctx.makeValidInContext("updateSimpleTextContent");
                updateCtx.put("textDataResourceId", content.get("dataResourceId"));
                Map<String, Object> updateResult = ctx.dispatcher().runSync("updateSimpleTextContent", updateCtx);
                if (!ServiceUtil.isSuccess(updateResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(updateResult));
                }
                return ServiceUtil.returnSuccess();
            } catch (GeneralException e) {
                return ServiceUtil.returnError(e.toString());
            }
        }
    }

    public static class DeleteSimpleTextContentForAlternateLocale extends ServiceHandler.Local implements ServiceHandler.Exec {
        @Override
        public Map<String, Object> exec() throws GeneralException {
            try {
                List<GenericValue> contentAssocList = ctx.delegator().from("ContentAssoc").where("contentId", ctx.attr("mainContentId"),
                        "contentAssocTypeId", "ALTERNATE_LOCALE", "contentIdTo", ctx.attr("contentId")).filterByDate().queryList();
                if (UtilValidate.isEmpty(contentAssocList)) {
                    String errMsg = UtilProperties.getMessage("ContentUiLabels", "ContentNoContentFound",
                            UtilMisc.toMap("contentId", ctx.attr("contentId")), ctx.locale()) + " (mainContentId: " + ctx.attr("mainContentId") + ")";
                    return ServiceUtil.returnError(errMsg);
                }

                Map<String, Object> removeCtx = ctx.makeValidInContext("removeContentAndRelated");
                Map<String, Object> removeResult = ctx.dispatcher().runSync("removeContentAndRelated", removeCtx);
                if (!ServiceUtil.isSuccess(removeResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(removeResult));
                }
                return ServiceUtil.returnSuccess();
            } catch (GeneralException e) {
                return ServiceUtil.returnError(e.toString());
            }
        }
    }

    public static class CreateUpdateSimpleTextContentForAlternateLocale extends ServiceHandler.Local implements ServiceHandler.Exec {
        @Override
        public Map<String, Object> exec() throws GeneralException {
            try {
                String mainContentId = ctx.attr("mainContentId");
                String contentId = ctx.attr("contentId");
                String localeString = ctx.attr("localeString");

                if (UtilValidate.isNotEmpty(contentId)) {
                    Map<String, Object> updateCtx = ctx.makeValidInContext("updateSimpleTextContentForAlternateLocale");
                    Map<String, Object> updateResult = ctx.dispatcher().runSync("updateSimpleTextContentForAlternateLocale", updateCtx);
                    if (!ServiceUtil.isSuccess(updateResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(updateResult));
                    }
                    return ServiceUtil.returnSuccess();
                } else {
                    GenericValue mainContent = ctx.delegator().from("Content").where("contentId", mainContentId).queryOne();
                    if (mainContent == null) {
                        String errMsg = UtilProperties.getMessage("ContentUiLabels", "ContentNoContentFound",
                                UtilMisc.toMap("contentId", mainContentId), ctx.locale());
                        return ServiceUtil.returnError(errMsg);
                    }

                    if (Objects.equals(localeString, mainContent.getString("localeString"))) {
                        Map<String, Object> updateCtx = UtilMisc.toMap("userLogin", ctx.attr("userLogin"),
                                "locale", ctx.attr("locale"), "timeZone", ctx.attr("timeZone"));
                        updateCtx.put("text", ctx.attr("text"));
                        updateCtx.put("textDataResourceId", mainContent.get("dataResourceId"));
                        Map<String, Object> updateResult = ctx.dispatcher().runSync("updateSimpleTextContent", updateCtx);
                        if (!ServiceUtil.isSuccess(updateResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(updateResult));
                        }
                        return UtilMisc.put(ServiceUtil.returnSuccess(), "contentId", mainContent.get("contentId"));
                    } else {
                        List<GenericValue> contentAssocList = ctx.delegator().from("ContentAssoc").where("contentId", mainContentId,
                                "contentAssocTypeId", "ALTERNATE_LOCALE").filterByDate().queryList();

                        boolean localeFound = false;
                        if (UtilValidate.isNotEmpty(contentAssocList)) {
                            for (GenericValue contentAssoc : contentAssocList) {
                                GenericValue toContent = contentAssoc.getRelatedOne("ToContent", false);
                                String toLocaleString = (toContent != null) ? toContent.getString("localeString") : null;
                                if (Objects.equals(localeString, toLocaleString)) {
                                    localeFound = true;
                                    Map<String, Object> updateTextCtx = UtilMisc.toMap("userLogin", ctx.attr("userLogin"),
                                            "locale", ctx.attr("locale"), "timeZone", ctx.attr("timeZone"));
                                    updateTextCtx.put("text", ctx.attr("text"));
                                    updateTextCtx.put("textDataResourceId", toContent.get("dataResourceId"));
                                    Map<String, Object> updateTextResult = ctx.dispatcher().runSync("updateSimpleTextContent", updateTextCtx);
                                    if (!ServiceUtil.isSuccess(updateTextResult)) {
                                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(updateTextResult));
                                    }
                                    break;
                                }
                            }
                        }

                        if (!localeFound) {
                            Map<String, Object> createCtx = ctx.makeValidInContext("createSimpleTextContentForAlternateLocale");
                            Map<String, Object> createResult = ctx.dispatcher().runSync("createSimpleTextContentForAlternateLocale", createCtx);
                            if (!ServiceUtil.isSuccess(createResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(createResult));
                            }
                            return UtilMisc.put(ServiceUtil.returnSuccess(), "contentId", createResult.get("contentId"));
                        }
                        return ServiceUtil.returnSuccess();
                    }
                }
            } catch (GeneralException e) {
                return ServiceUtil.returnError(e.toString());
            }
        }
    }
}
