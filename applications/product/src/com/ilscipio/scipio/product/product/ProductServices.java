package com.ilscipio.scipio.product.product;

import java.sql.Timestamp;
import java.util.*;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.content.content.CommonContentWrapper;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityListIterator;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.service.*;

/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
public abstract class ProductServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static Set<String> usedProductFields = new HashSet<String>(Arrays.asList("productId", "productTypeId", "introductionDate", "releaseDate",
            "salesDiscontinuationDate", "productRating", "lastModifiedDate", "lastUpdatedStamp"));

    /**
     * SCIPIO: Function to mark a product as discontinued in solr. Sets attribute "salesDiscontinuationDate" to current date.
     * NOTE: This is redundant if solr ECAs are enabled and should not be used in that case.
     */
    public static Map<String, Object> setProductToSalesDiscontinued(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        String productId = (String) context.get("productId");
        if (UtilValidate.isEmpty(productId)) {
            return ServiceUtil.returnError("Empty productId given");
        }
        try {
            GenericValue product = delegator.findOne("Product", false, "productId", productId);
            if (UtilValidate.isEmpty(product)) {
                ServiceUtil.returnError("Could not find product with given id:" + productId);
            }
            product.set("salesDiscontinuationDate", new Timestamp(System.currentTimeMillis()));
            product.set("comments", "Product discontinued (manually disabled)");
            product.store();
            LocalDispatcher dispatcher = dctx.getDispatcher();
            Map<String, Object> params = new HashMap<>();
            params.put("productId", productId);
            try {
                dispatcher.runSync("updateSolrWithProduct", params);
            }
            catch (GenericServiceException e) {
                Debug.logError(e, module);
                ServiceUtil.returnError("Error reindexing product to Solr:" + e.getMessage());
            }
        }
        catch (GenericEntityException e) {
            Debug.logError(e, module);
            ServiceUtil.returnError("Error while disabling product:" + e.getMessage());
        }
        return ServiceUtil.returnSuccess();
    }

    public static class PrecacheProductContentWrapper extends LocalService {
        private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

        protected Collection<String> productContentTypeIdList;
        protected Collection<String> partyIdList;
        protected Collection<String> roleTypeIdList;
        protected Collection<String> encoderTypeList;
        protected Collection<String> mimeTypeIdList;
        protected Collection<Locale> localeList;

        protected int productCount = 0;

        public void init(ServiceContext ctx) throws GeneralException {
            super.init(ctx);
            productContentTypeIdList = ctx.attr("productContentTypeIdList", Collections::emptyList);
            partyIdList = ctx.attr("partyIdList", () -> UtilMisc.toList((String) null));
            roleTypeIdList = ctx.attr("roleTypeIdList", () -> UtilMisc.toList((String) null));
            encoderTypeList = ctx.attr("encoderTypeList", () -> UtilMisc.toList("raw"));
            mimeTypeIdList = ctx.attr("mimeTypeIdList", () -> UtilMisc.toList(CommonContentWrapper.getDefaultMimeTypeId(ctx.delegator())));
            Collection<?> localeObjList = ctx.attr("localeList", Collections::emptyList);
            Collection<Locale> newLocaleList = new ArrayList<>(localeObjList.size());
            for(Object localeObj : localeObjList) {
                if (localeObj instanceof Locale) {
                    newLocaleList.add((Locale) localeObj);
                } else {
                    newLocaleList.add(UtilMisc.parseLocale((String) localeObj));
                }
            }
            if (newLocaleList.isEmpty()) {
                newLocaleList = UtilMisc.toList(Locale.ENGLISH);
            }
            localeList = newLocaleList;
        }

        @Override
        public Map<String, Object> exec() throws ServiceValidationException {
            try(EntityListIterator eli = ctx.delegator().from("Product").queryIterator()) {
                GenericValue product;
                while((product = eli.next()) != null) {
                    //String productId = product.getString("productId");
                    for(String productContentTypeId : productContentTypeIdList) {
                        for(Locale locale : localeList) {
                            for(String partyId : partyIdList) {
                                for(String roleTypeId : roleTypeIdList) {
                                    for(String mimeTypeId : mimeTypeIdList) {
                                        for(String encoderType : encoderTypeList) {
                                            ProductContentWrapper.getProductContentAsText(product, productContentTypeId,
                                                    locale, mimeTypeId, partyId, roleTypeId, ctx.delegator(),
                                                    ctx.dispatcher(),true, encoderType);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    productCount++;
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                return ServiceUtil.returnError(e.toString());
            }
            return ServiceUtil.returnSuccess("Precached ProductContentWrapper for " + productCount + " products");
        }
    }

    public static class CreateUpdateProductSimpleTextContentForAlternateLocale extends LocalService {
        private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

        protected String productId;
        protected String productContentTypeId;
        protected Timestamp productContentFromDate;
        protected String mainContentId;
        protected String contentId;

        @Override
        public void init(ServiceContext ctx) throws GeneralException {
            super.init(ctx);
            productId = ctx.attrNonEmpty("productId");
            productContentTypeId = ctx.attrNonEmpty("productContentTypeId");
            productContentFromDate = ctx.attr("productContentFromDate");
            mainContentId = ctx.attrNonEmpty("mainContentId");
            contentId = ctx.attrNonEmpty("contentId");
        }

        @Override
        public Map<String, Object> exec() {
            try {
                Map<String, Object> productContentFields = UtilMisc.toMap("productId", productId, "productContentTypeId", productContentTypeId);
                boolean filterByDate = true;
                if (productContentFromDate != null) {
                    productContentFields.put("fromDate", productContentFromDate);
                    filterByDate = false;
                }
                if (mainContentId != null) {
                    productContentFields.put("contentId", mainContentId);
                }
                Debug.logInfo("Looking up ProductContent for " + productContentFields, module);
                GenericValue productContent = ctx.delegator().from("ProductContent").where(productContentFields).filterByDate(filterByDate).queryFirst();
                if (productContent != null) {
                    if (productContentFromDate == null) {
                        productContentFromDate = productContent.getTimestamp("fromDate");
                    }
                    if (mainContentId == null) {
                        mainContentId = productContent.getString("contentId");
                    }
                }

                Map<String, Object> updateContentCtx = ctx.makeValidContext("createUpdateSimpleTextContentForAlternateLocale", "IN");
                if (mainContentId != null) {
                    updateContentCtx.put("mainContentId", mainContentId);
                }
                Map<String, Object> updateContentResult = ctx.dispatcher().runSync("createUpdateSimpleTextContentForAlternateLocale", updateContentCtx);
                if (!ServiceUtil.isSuccess(updateContentResult)) {
                    return populateResult(ServiceUtil.returnError(ServiceUtil.getErrorMessage(updateContentResult)));
                }
                mainContentId = UtilValidate.nullIfEmpty(updateContentResult.get("mainContentId"));
                if (mainContentId == null) {
                    return populateResult(ServiceUtil.returnError("No mainContentId available"));
                }
                contentId = (String) updateContentResult.get("contentId");

                // re-lookup (minimize concurrency issues)
                productContentFields.put("contentId", mainContentId);
                productContent = ctx.delegator().from("ProductContent").where(productContentFields).filterByDate(filterByDate).queryFirst();
                if (productContent == null) {
                    productContentFromDate = UtilDateTime.nowTimestamp();
                    productContent = ctx.delegator().makeValue("ProductContent",
                            "productId", productId, "productContentTypeId", productContentTypeId,
                            "contentId", mainContentId, "fromDate", productContentFromDate);
                    productContent = productContent.create();
                } else {
                    productContentFromDate = productContent.getTimestamp("fromDate");
                    if (UtilValidate.isEmpty(productContent.getString("contentId"))) {
                        productContent.set("contentId", mainContentId);
                        productContent.store();
                    }
                }

                return populateResult(ServiceUtil.returnSuccess());
            } catch (GeneralException e) {
                return populateResult(ServiceUtil.returnError(e.toString()));
            }
        }

        protected Map<String, Object> populateResult(Map<String, Object> result) {
            result.put("productContentFromDate", productContentFromDate);
            result.put("mainContentId", mainContentId);
            result.put("contentId", contentId);
            return result;
        }
    }

}
