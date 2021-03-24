/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.product.store;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.TimeZone;

import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.geo.GeoWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.model.DynamicViewEntity;
import org.ofbiz.entity.model.ModelKeyMap;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.party.contact.ContactMechWorker;
import org.ofbiz.product.config.ProductConfigWrapper;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.webapp.website.WebSiteWorker;

/**
 * ProductStoreWorker - Worker class for store related functionality
 */
public final class ProductStoreWorker {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final Map<String, String> defaultProductStoreEmailScreenLocation; // SCIPIO: made final and unmodifiable

    static {
        Map<String, String> emails = new HashMap<>(); // SCIPIO
        
        // SCIPIO: now points to shop
        emails.put("PRDS_ODR_CONFIRM", "component://shop/widget/EmailOrderScreens.xml#OrderConfirmNotice");
        emails.put("PRDS_ODR_COMPLETE", "component://shop/widget/EmailOrderScreens.xml#OrderCompleteNotice");
        emails.put("PRDS_ODR_BACKORDER", "component://shop/widget/EmailOrderScreens.xml#BackorderNotice");
        emails.put("PRDS_ODR_CHANGE", "component://shop/widget/EmailOrderScreens.xml#OrderChangeNotice");

        emails.put("PRDS_ODR_PAYRETRY", "component://shop/widget/EmailOrderScreens.xml#PaymentRetryNotice");

        // SCIPIO: new payment status mails
        emails.put("PRDS_ODR_PAY_CHANGE", "component://shop/widget/EmailOrderScreens.xml#PaymentChangeNotice");
        emails.put("PRDS_ODR_PAY_COMPLT", "component://shop/widget/EmailOrderScreens.xml#PaymentCompletedNotice");

        emails.put("PRDS_RTN_ACCEPT", "component://shop/widget/EmailReturnScreens.xml#ReturnAccept");
        emails.put("PRDS_RTN_COMPLETE", "component://shop/widget/EmailReturnScreens.xml#ReturnComplete");
        emails.put("PRDS_RTN_CANCEL", "component://shop/widget/EmailReturnScreens.xml#ReturnCancel");

        emails.put("PRDS_GC_PURCHASE", "component://shop/widget/EmailGiftCardScreens.xml#GiftCardPurchase");
        emails.put("PRDS_GC_RELOAD", "component://shop/widget/EmailGiftCardScreens.xml#GiftCardReload");

        emails.put("PRDS_QUO_CONFIRM", "component://order/widget/ordermgr/QuoteScreens.xml#ViewQuoteSimple");

        emails.put("PRDS_PWD_RETRIEVE", "component://securityext/widget/EmailSecurityScreens.xml#PasswordEmail");

        emails.put("PRDS_TELL_FRIEND", "component://shop/widget/EmailProductScreens.xml#TellFriend");

        emails.put("PRDS_CUST_REGISTER", "component://securityext/widget/EmailSecurityScreens.xml#PasswordEmail");

        defaultProductStoreEmailScreenLocation = Collections.unmodifiableMap(emails);
    }

    private ProductStoreWorker() {}

    public static GenericValue getProductStore(String productStoreId, Delegator delegator) {
        if (productStoreId == null || delegator == null) {
            return null;
        }
        GenericValue productStore = null;
        try {
            productStore = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", productStoreId).cache().queryOne();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Problem getting ProductStore entity", module);
        }
        return productStore;
    }

    public static GenericValue getProductStore(ServletRequest request) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        String productStoreId = ProductStoreWorker.getProductStoreId(request);
        return ProductStoreWorker.getProductStore(productStoreId, delegator);
    }

    public static String getProductStoreId(ServletRequest request) {
        HttpServletRequest httpRequest = (HttpServletRequest) request;
        HttpSession session = httpRequest.getSession(false);
        // SCIPIO: refactored to prevent multiple accesss
        if (session != null) {
            String productStoreId = (String) session.getAttribute("productStoreId");
            if (productStoreId != null) {
                return productStoreId;
            }
        }
        GenericValue webSite = WebSiteWorker.getWebSite(httpRequest);
        if (webSite != null) {
            String productStoreId = webSite.getString("productStoreId");
            // might be nice to do this, but not needed and has a problem with dependencies: setSessionProductStore(productStoreId, httpRequest);
            return productStoreId;
        }
        return null;
    }

    public static String getStoreCurrencyUomId(HttpServletRequest request) {
        GenericValue productStore = getProductStore(request);
        if (UtilValidate.isEmpty(productStore)) {
            Debug.logError("No product store found in request, cannot set CurrencyUomId!", module);
            return null;
        } else {
            return UtilHttp.getCurrencyUom(request.getSession(), productStore.getString("defaultCurrencyUomId"));
        }
    }

    public static Locale getStoreLocale(HttpServletRequest request) {
        GenericValue productStore = getProductStore(request);
        if (UtilValidate.isEmpty(productStore)) {
            Debug.logError("No product store found in request, cannot set locale!", module);
            return null;
        } else {
            return UtilHttp.getLocale(request, request.getSession(), productStore.getString("defaultLocaleString"));
        }
    }

    public static TimeZone getStoreTimeZone(HttpServletRequest request) {
        GenericValue productStore = getProductStore(request);
        if (UtilValidate.isEmpty(productStore)) {
            Debug.logError("No product store found in request, cannot set timezone!", module);
            return null;
        } else {
            return UtilHttp.getTimeZone(request, request.getSession(), productStore.getString("defaultTimeZoneString"));
        }
    }

    public static String determineSingleFacilityForStore(Delegator delegator, String productStoreId) {
        GenericValue productStore = null;
        try {
            productStore = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", productStoreId).queryOne();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }
        if (productStore != null) {
            if ("Y".equalsIgnoreCase(productStore.getString("oneInventoryFacility"))) {
                return productStore.getString("inventoryFacilityId");
            }
        }
        return null;
    }

    public static boolean autoSaveCart(Delegator delegator, String productStoreId) {
        return autoSaveCart(getProductStore(productStoreId, delegator));
    }

    public static boolean autoSaveCart(GenericValue productStore) {
        return productStore == null ? false : "Y".equalsIgnoreCase(productStore.getString("autoSaveCart"));
    }

    public static String getProductStorePayToPartyId(String productStoreId, Delegator delegator) {
        return getProductStorePayToPartyId(getProductStore(productStoreId, delegator));
    }

    public static String getProductStorePayToPartyId(GenericValue productStore) {
        String payToPartyId = "Company"; // default value
        if (productStore != null && productStore.get("payToPartyId") != null) {
            payToPartyId = productStore.getString("payToPartyId");
        }
        return payToPartyId;
    }

    public static String getProductStorePaymentProperties(ServletRequest request, String paymentMethodTypeId, String paymentServiceTypeEnumId, boolean anyServiceType) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        String productStoreId = ProductStoreWorker.getProductStoreId(request);
        return ProductStoreWorker.getProductStorePaymentProperties(delegator, productStoreId, paymentMethodTypeId, paymentServiceTypeEnumId, anyServiceType);
    }

    public static String getProductStorePaymentProperties(Delegator delegator, String productStoreId, String paymentMethodTypeId, String paymentServiceTypeEnumId, boolean anyServiceType) {
        GenericValue setting = ProductStoreWorker.getProductStorePaymentSetting(delegator, productStoreId, paymentMethodTypeId, paymentServiceTypeEnumId, anyServiceType);

        String payProps = "payment.properties";
        if (setting != null && setting.get("paymentPropertiesPath") != null) {
            payProps =  setting.getString("paymentPropertiesPath");
        }
        return payProps;
    }

    public static GenericValue getProductStorePaymentSetting(Delegator delegator, String productStoreId, String paymentMethodTypeId, String paymentServiceTypeEnumId, boolean anyServiceType) {
        GenericValue storePayment = null;
        try {
            storePayment = EntityQuery.use(delegator).from("ProductStorePaymentSetting").where("productStoreId", productStoreId, "paymentMethodTypeId", paymentMethodTypeId, "paymentServiceTypeEnumId", paymentServiceTypeEnumId).cache().queryOne();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Problems looking up store payment settings", module);
        }

        if (anyServiceType) {
            if (storePayment == null) {
                try {
                    storePayment = EntityQuery.use(delegator).from("ProductStorePaymentSetting").where("productStoreId", productStoreId, "paymentMethodTypeId", paymentMethodTypeId).queryFirst();
                } catch (GenericEntityException e) {
                    Debug.logError(e, "Problems looking up store payment settings", module);
                }
            }

            if (storePayment == null) {
                try {
                    storePayment = EntityQuery.use(delegator).from("ProductStorePaymentSetting").where("productStoreId", productStoreId).queryFirst();
                } catch (GenericEntityException e) {
                    Debug.logError(e, "Problems looking up store payment settings", module);
                }
            }
        }

        return storePayment;
    }

    public static List<GenericValue> getProductStoreShipmentMethods(Delegator delegator, String productStoreId,
                                                             String shipmentMethodTypeId, String carrierPartyId, String carrierRoleTypeId) {
        // check for an external service call

        List<GenericValue> storeShipMethods = null;
        try {
            storeShipMethods = EntityQuery.use(delegator).from("ProductStoreShipmentMeth")
                                   .where("productStoreId", productStoreId, "shipmentMethodTypeId", shipmentMethodTypeId,
                                           "partyId", carrierPartyId, "roleTypeId", carrierRoleTypeId)
                                   .cache(true)
                                   .queryList();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }

        return storeShipMethods;
    }

    public static GenericValue getProductStoreShipmentMethod(Delegator delegator, String productStoreId,
                                                             String shipmentMethodTypeId, String carrierPartyId, String carrierRoleTypeId) {
        // TODO: selecting the first record is a far from optimal solution but, since the productStoreShipmentMethod
        //       is currently used to get the service name to get the online estimate, this should not be a huge deal for now.
        return EntityUtil.getFirst(getProductStoreShipmentMethods(delegator, productStoreId, shipmentMethodTypeId, carrierPartyId, carrierRoleTypeId));
    }

    public static List<GenericValue> getAvailableStoreShippingMethods(Delegator delegator, String productStoreId, GenericValue shippingAddress, List<BigDecimal> itemSizes, Map<String, BigDecimal> featureIdMap, BigDecimal weight, BigDecimal orderTotal) {
        if (featureIdMap == null) {
            featureIdMap = new HashMap<String, BigDecimal>();
        }
        List<GenericValue> shippingMethods = null;
        try {
            shippingMethods = EntityQuery.use(delegator).from("ProductStoreShipmentMethView").where("productStoreId", productStoreId).orderBy("sequenceNumber").cache(true).queryList();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Unable to get ProductStore shipping methods", module);
            return null;
        }

        // clone the list for concurrent modification
        List<GenericValue> returnShippingMethods = UtilMisc.makeListWritable(shippingMethods);

        if (shippingMethods != null) {
            for (GenericValue method: shippingMethods) {

                // test min/max weight first
                BigDecimal minWeight = method.getBigDecimal("minWeight");
                BigDecimal maxWeight = method.getBigDecimal("maxWeight");
                if (minWeight != null && minWeight.compareTo(BigDecimal.ZERO) > 0 && minWeight.compareTo(weight) > 0) {
                    returnShippingMethods.remove(method);
                    continue;
                }
                if (maxWeight != null && maxWeight.compareTo(BigDecimal.ZERO) > 0 && maxWeight.compareTo(weight) < 0) {
                    returnShippingMethods.remove(method);
                    continue;
                }

                // test order total
                BigDecimal minTotal = method.getBigDecimal("minTotal");
                BigDecimal maxTotal = method.getBigDecimal("maxTotal");
                if (minTotal != null && minTotal.compareTo(BigDecimal.ZERO) > 0 && minTotal.compareTo(orderTotal) > 0) {
                    returnShippingMethods.remove(method);
                    continue;
                }
                if (maxTotal != null && maxTotal.compareTo(BigDecimal.ZERO) > 0 && maxTotal.compareTo(orderTotal) < 0) {
                    returnShippingMethods.remove(method);
                    continue;
                }

                // test product sizes
                BigDecimal minSize = method.getBigDecimal("minSize");
                BigDecimal maxSize = method.getBigDecimal("maxSize");
                if (minSize != null && minSize.compareTo(BigDecimal.ZERO) > 0) {
                    boolean allMatch = false;
                    if (itemSizes != null) {
                        allMatch = true;
                        for (BigDecimal size: itemSizes) {
                            if (size.compareTo(minSize) < 0) {
                                allMatch = false;
                            }
                        }
                    }
                    if (!allMatch) {
                        returnShippingMethods.remove(method);
                        continue;
                    }
                }
                if (maxSize != null && maxSize.compareTo(BigDecimal.ZERO) > 0) {
                    boolean allMatch = false;
                    if (itemSizes != null) {
                        allMatch = true;
                        for (BigDecimal size: itemSizes) {
                            if (size.compareTo(maxSize) > 0) {
                                allMatch = false;
                            }
                        }
                    }
                    if (!allMatch) {
                        returnShippingMethods.remove(method);
                        continue;
                    }
                }

                // check USPS address
                String allowUspsAddr = method.getString("allowUspsAddr");
                String requireUspsAddr = method.getString("requireUspsAddr");
                boolean isUspsAddress = ContactMechWorker.isUspsAddress(shippingAddress);
                if ("N".equals(allowUspsAddr) && isUspsAddress) {
                    returnShippingMethods.remove(method);
                    continue;
                }
                if ("Y".equals(requireUspsAddr) && !isUspsAddress) {
                    returnShippingMethods.remove(method);
                    continue;
                }

                // check company address
                String companyPartyId = method.getString("companyPartyId");
                String allowCompanyAddr = method.getString("allowCompanyAddr");
                String requireCompanyAddr = method.getString("requireCompanyAddr");
                boolean isCompanyAddress = ContactMechWorker.isCompanyAddress(shippingAddress, companyPartyId);
                if ("N".equals(allowCompanyAddr) && isCompanyAddress) {
                    returnShippingMethods.remove(method);
                    continue;
                }
                if ("Y".equals(requireCompanyAddr) && !isCompanyAddress) {
                    returnShippingMethods.remove(method);
                    continue;
                }

                // check the items excluded from shipping
                String includeFreeShipping = method.getString("includeNoChargeItems");
                if (includeFreeShipping != null && "N".equalsIgnoreCase(includeFreeShipping)) {
                    if (UtilValidate.isEmpty(itemSizes) && orderTotal.compareTo(BigDecimal.ZERO) == 0) {
                        returnShippingMethods.remove(method);
                        continue;
                    }
                }

                // check the geos
                String includeGeoId = method.getString("includeGeoId");
                String excludeGeoId = method.getString("excludeGeoId");
                if (UtilValidate.isNotEmpty(includeGeoId) || UtilValidate.isNotEmpty(excludeGeoId)) {
                    if (shippingAddress == null) {
                        returnShippingMethods.remove(method);
                        continue;
                    }
                }
                if (UtilValidate.isNotEmpty(includeGeoId)) {
                    List<GenericValue> includeGeoGroup = GeoWorker.expandGeoGroup(includeGeoId, delegator);
                    if (!GeoWorker.containsGeo(includeGeoGroup, shippingAddress.getString("countryGeoId"), delegator) &&
                            !GeoWorker.containsGeo(includeGeoGroup, shippingAddress.getString("stateProvinceGeoId"), delegator) &&
                            !GeoWorker.containsGeo(includeGeoGroup, shippingAddress.getString("postalCodeGeoId"), delegator)) {
                        // not in required included geos
                        returnShippingMethods.remove(method);
                        continue;
                    }
                }
                if (UtilValidate.isNotEmpty(excludeGeoId)) {
                    List<GenericValue> excludeGeoGroup = GeoWorker.expandGeoGroup(excludeGeoId, delegator);
                    if (GeoWorker.containsGeo(excludeGeoGroup, shippingAddress.getString("countryGeoId"), delegator) ||
                            GeoWorker.containsGeo(excludeGeoGroup, shippingAddress.getString("stateProvinceGeoId"), delegator) ||
                            GeoWorker.containsGeo(excludeGeoGroup, shippingAddress.getString("postalCodeGeoId"), delegator)) {
                        // in excluded geos
                        returnShippingMethods.remove(method);
                        continue;
                    }
                }

                // check the features
                String includeFeatures = method.getString("includeFeatureGroup");
                String excludeFeatures = method.getString("excludeFeatureGroup");
                if (UtilValidate.isNotEmpty(includeFeatures)) {
                    List<GenericValue> includedFeatures = null;
                    try {
                        includedFeatures = EntityQuery.use(delegator).from("ProductFeatureGroupAppl").where("productFeatureGroupId", includeFeatures).cache(true).queryList();
                    } catch (GenericEntityException e) {
                        Debug.logError(e, "Unable to lookup ProductFeatureGroupAppl records for group : " + includeFeatures, module);
                    }
                    if (includedFeatures != null) {
                        boolean foundOne = false;
                        for (GenericValue appl: includedFeatures) {
                            if (featureIdMap.containsKey(appl.getString("productFeatureId"))) {
                                foundOne = true;
                                break;
                            }
                        }
                        if (!foundOne) {
                            returnShippingMethods.remove(method);
                            continue;
                        }
                    }
                }
                if (UtilValidate.isNotEmpty(excludeFeatures)) {
                    List<GenericValue> excludedFeatures = null;
                    try {
                        excludedFeatures = EntityQuery.use(delegator).from("ProductFeatureGroupAppl").where("productFeatureGroupId", excludeFeatures).cache(true).queryList();
                    } catch (GenericEntityException e) {
                        Debug.logError(e, "Unable to lookup ProductFeatureGroupAppl records for group : " + excludeFeatures, module);
                    }
                    if (excludedFeatures != null) {
                        for (GenericValue appl: excludedFeatures) {
                            if (featureIdMap.containsKey(appl.getString("productFeatureId"))) {
                                returnShippingMethods.remove(method);
                                continue;
                            }
                        }
                    }
                }
            }
        }

        return returnShippingMethods;
    }

    public static ProductStoreSurveyWrapper getRandomSurveyWrapper(HttpServletRequest request, String groupName) {
        GenericValue productStore = getProductStore(request);
        HttpSession session = request.getSession();
        if (productStore == null) {
            return null;
        }

        GenericValue userLogin = (GenericValue) session.getAttribute("userLogin");
        if (userLogin == null) {
            userLogin = (GenericValue) session.getAttribute("autoUserLogin");
        }

        String partyId = userLogin != null ? userLogin.getString("partyId") : null;
        String origParamMapId = UtilHttp.stashParameterMap(request);
        Map<String, Object> passThruFields = UtilMisc.<String, Object>toMap("_ORIG_PARAM_MAP_ID_", origParamMapId);

        return getRandomSurveyWrapper(productStore.getDelegator(), productStore.getString("productStoreId"), groupName, partyId, passThruFields);
    }

    public static ProductStoreSurveyWrapper getRandomSurveyWrapper(Delegator delegator, String productStoreId, String groupName, String partyId, Map<String, Object> passThruFields) {
        List<GenericValue> randomSurveys = getSurveys(delegator, productStoreId, groupName, null, "RANDOM_POLL", null);
        if (UtilValidate.isNotEmpty(randomSurveys)) {
            Random rand = new Random();
            int index = rand.nextInt(randomSurveys.size());
            GenericValue appl = randomSurveys.get(index);
            return new ProductStoreSurveyWrapper(appl, partyId, passThruFields);
        } else {
            return null;
        }
    }

    public static List<GenericValue> getProductSurveys(Delegator delegator, String productStoreId, String productId, String surveyApplTypeId) {
        return getSurveys(delegator, productStoreId, null, productId, surveyApplTypeId, null);
    }

    public static List<GenericValue> getProductSurveys(Delegator delegator, String productStoreId, String productId, String surveyApplTypeId, String parentProductId) {
        return getSurveys(delegator, productStoreId, null, productId, surveyApplTypeId,parentProductId);
    }

    public static List<GenericValue> getSurveys(Delegator delegator, String productStoreId, String groupName, String productId, String surveyApplTypeId, String parentProductId) {
        List<GenericValue> surveys = new LinkedList<GenericValue>();
        List<GenericValue> storeSurveys = null;
        try {
            storeSurveys = EntityQuery.use(delegator).from("ProductStoreSurveyAppl").where("productStoreId", productStoreId, "surveyApplTypeId", surveyApplTypeId).orderBy("sequenceNum").cache(true).queryList();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Unable to get ProductStoreSurveyAppl for store : " + productStoreId, module);
            return surveys;
        }

        // limit by date
        storeSurveys = EntityUtil.filterByDate(storeSurveys);

        // limit based on group name
        if (UtilValidate.isNotEmpty(groupName)) {
            storeSurveys = EntityUtil.filterByAnd(storeSurveys, UtilMisc.toMap("groupName", groupName));
        }

        if (Debug.infoOn()) {
            Debug.logInfo("getSurvey for product " + productId, module);
        }
        // limit by product
        if (UtilValidate.isNotEmpty(productId) && UtilValidate.isNotEmpty(storeSurveys)) {
            // SCIPIO: 2019-03-06: Moved the following Product lookup to outside the for loop
            GenericValue product = null;
            String virtualProductId = null;
            // if the item is a variant, get its virtual productId
            try {
                product = EntityQuery.use(delegator).from("Product").where("productId", productId).cache().queryOne();
                if ((product != null) && ("Y".equals(product.get("isVariant")))) {
                    if (parentProductId != null) {
                        virtualProductId = parentProductId;
                    } else {
                        virtualProductId = ProductWorker.getVariantVirtualId(product);
                    }
                    if (Debug.infoOn()) {
                        Debug.logInfo("getSurvey for virtual product " + virtualProductId, module);
                    }
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, "Problem finding product from productId " + productId, module);
            }

            for (GenericValue surveyAppl: storeSurveys) {
                // use survey if productId or virtualProductId of the variant product is in the ProductStoreSurveyAppl
                if (surveyAppl.get("productId") != null) {
                    if (surveyAppl.get("productId").equals(productId)) {
                        surveys.add(surveyAppl);
                    } else if ((virtualProductId != null) && (surveyAppl.getString("productId").equals(virtualProductId))) {
                        surveys.add(surveyAppl);
                    }
                } else if (surveyAppl.get("productCategoryId") != null) {
                    List<GenericValue> categoryMembers = null;
                    try {
                        categoryMembers = EntityQuery.use(delegator).from("ProductCategoryMember").where("productCategoryId", surveyAppl.get("productCategoryId")).cache(true).queryList();
                    } catch (GenericEntityException e) {
                        Debug.logError(e, "Unable to get ProductCategoryMember records for survey application : " + surveyAppl, module);
                    }
                    if (categoryMembers != null) {
                        for (GenericValue member: categoryMembers) {
                            if (productId != null && productId.equals(member.getString("productId"))) {
                                surveys.add(surveyAppl);
                                break;
                            } else if ((virtualProductId != null) && (virtualProductId.equals(member.getString("productId")))) { // similarly, check if virtual productId is in category
                                surveys.add(surveyAppl);
                                break;
                            }
                        }
                    }
                }
            }
        } else if (storeSurveys != null) {
            surveys.addAll(storeSurveys);
        }

        return surveys;
    }

    /** Returns the number of responses for this survey by party */
    public static int checkSurveyResponse(HttpServletRequest request, String surveyId) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        String productStoreId = getProductStoreId(request);
        if (userLogin == null) {
            return -1;
        }

        return checkSurveyResponse(delegator, userLogin.getString("partyId"), productStoreId, surveyId);
    }

    /** Returns the number of responses for this survey by party */
    public static int checkSurveyResponse(Delegator delegator, String partyId, String productStoreId, String surveyId) {
        if (delegator == null || partyId == null || productStoreId == null) {
            return -1;
        }

        List<GenericValue> surveyResponse = null;
        try {
            surveyResponse = EntityQuery.use(delegator).from("SurveyResponse").where("surveyId", surveyId, "partyId", partyId).queryList();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return -1;
        }

        if (UtilValidate.isEmpty(surveyResponse)) {
            return 0;
        } else {
            return surveyResponse.size();
        }
    }

    public static boolean isStoreInventoryRequired(ServletRequest request, GenericValue product) {
        return isStoreInventoryRequiredAndAvailable(request, product, null, Boolean.TRUE, null);
    }

    public static boolean isStoreInventoryAvailable(ServletRequest request, GenericValue product, BigDecimal quantity) {
        return isStoreInventoryRequiredAndAvailable(request, product, quantity, null, Boolean.TRUE);
    }

    /**
     * This method is used in the showcart pages to determine whether or not to show the inventory message and
     * in the productdetail pages to determine whether or not to show the item as out of stock.
     *
     * @param request ServletRequest (or HttpServletRequest of course)
     * @param product GenericValue representing the product in question
     * @param quantity Quantity desired.
     * @param wantRequired If true then inventory required must be true for the result to be true, if false must be false; if null don't care
     * @param wantAvailable If true then inventory avilable must be true for the result to be true, if false must be false; if null don't care
     */
    public static boolean isStoreInventoryRequiredAndAvailable(ServletRequest request, GenericValue product, BigDecimal quantity, Boolean wantRequired, Boolean wantAvailable) {
        GenericValue productStore = getProductStore(request);
        if (productStore == null) {
            Debug.logWarning("No ProductStore found, return false for inventory check", module);
            return false;
        }
        if (product == null) {
            Debug.logWarning("No Product passed, return false for inventory check", module);
            return false;
        }

        if (quantity == null) quantity = BigDecimal.ONE;

        String productStoreId = productStore.getString("productStoreId");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");

        try {
            Boolean requiredOkay = null;
            if (wantRequired != null) {
                Map<String, Object> invReqResult = dispatcher.runSync("isStoreInventoryRequired", UtilMisc.toMap("productStoreId", productStoreId, "productId", product.get("productId"), "product", product, "productStore", productStore));
                if (ServiceUtil.isError(invReqResult)) {
                    Debug.logError("Error calling isStoreInventoryRequired service, result is: " + invReqResult, module);
                    return false;
                }
                requiredOkay = wantRequired == "Y".equals(invReqResult.get("requireInventory"));
            }

            Boolean availableOkay = null;
            if (wantAvailable != null) {
                Map<String, Object> invAvailResult = dispatcher.runSync("isStoreInventoryAvailable", UtilMisc.toMap("productStoreId", productStoreId, "productId", product.get("productId"), "product", product, "productStore", productStore, "quantity", quantity, "useInventoryCache", true)); // SCIPIO: useInventoryCache
                if (ServiceUtil.isError(invAvailResult)) {
                    Debug.logError("Error calling isStoreInventoryAvailable service, result is: " + invAvailResult, module);
                    return false;
                }
                availableOkay = wantAvailable == "Y".equals(invAvailResult.get("available"));
            }

            if ((requiredOkay == null || requiredOkay) && (availableOkay == null || availableOkay)) {
                return true;
            } else {
                return false;
            }
        } catch (GenericServiceException e) {
            String errMsg = "Fatal error calling inventory checking services: " + e.toString();
            Debug.logError(e, errMsg, module);
            return false;
        }
    }

    public static boolean isStoreInventoryAvailable(ServletRequest request, ProductConfigWrapper productConfig, BigDecimal quantity) {
        GenericValue productStore = getProductStore(request);

        if (productStore == null) {
            Debug.logWarning("No ProductStore found, return false for inventory check", module);
            return false;
        }

        String productStoreId = productStore.getString("productStoreId");
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        return isStoreInventoryAvailable(productStoreId, productConfig, quantity, delegator, dispatcher);
    }

    /** check inventory availability for the given catalog, product, quantity, etc */
    public static boolean isStoreInventoryAvailable(String productStoreId, ProductConfigWrapper productConfig, BigDecimal quantity, Delegator delegator, LocalDispatcher dispatcher) {
        GenericValue productStore = getProductStore(productStoreId, delegator);

        if (productStore == null) {
            Debug.logWarning("No ProductStore found with id " + productStoreId + ", returning false for inventory available check", module);
            return false;
        }

        // if prodCatalog is set to not check inventory break here
        if ("N".equals(productStore.getString("checkInventory"))) {
            // note: if not set, defaults to yes, check inventory
            if (Debug.verboseOn()) Debug.logVerbose("ProductStore with id " + productStoreId + ", is set to NOT check inventory, returning true for inventory available check", module);
            return true;
        }
        boolean isInventoryAvailable = false;

        if ("Y".equals(productStore.getString("oneInventoryFacility"))) {
            String inventoryFacilityId = productStore.getString("inventoryFacilityId");

            if (UtilValidate.isEmpty(inventoryFacilityId)) {
                Debug.logWarning("ProductStore with id " + productStoreId + " has Y for oneInventoryFacility but inventoryFacilityId is empty, returning false for inventory check", module);
                return false;
            }

            return ProductWorker.isProductInventoryAvailableByFacility(productConfig, inventoryFacilityId, quantity, dispatcher);

        } else {
            GenericValue product = productConfig.getProduct();
            List<GenericValue> productFacilities = null;

            try {
                productFacilities = product.getRelated("ProductFacility", null, null, true);
            } catch (GenericEntityException e) {
                Debug.logWarning(e, "Error invoking getRelated in isCatalogInventoryAvailable", module);
                return false;
            }

            if (UtilValidate.isNotEmpty(productFacilities)) {
                for (GenericValue pfValue: productFacilities) {
                    isInventoryAvailable = ProductWorker.isProductInventoryAvailableByFacility(productConfig, pfValue.getString("facilityId"), quantity, dispatcher);
                    if (isInventoryAvailable == true) {
                        return isInventoryAvailable;
                    }
                }
            }
            return false;
        }
    }

    public static String getDefaultProductStoreEmailScreenLocation(String emailType) {
        return defaultProductStoreEmailScreenLocation.get(emailType);
    }

    private static final List<String> CONTENT_REF_SORT_FIELDS = UtilMisc.toList("defaultPriority", "isContentReference DESC");

    /**
     * SCIPIO: Returns the first of the listed product stores that has isContentReference=Y set ordering by defaultPriority, or if
     * none have isContentReference, the lowest defaultPriority, or null if none.
     * <p>
     * NOTE: 2020-02-03: For compatibility reasons, isContentReference="N" is treated the same as unset.
     */
    public static GenericValue getContentReferenceStore(List<GenericValue> productStores) {
        if (UtilValidate.isEmpty(productStores)) {
            return null;
        }
        return findBestContentReferenceStore(productStores);
    }

    /**
     * SCIPIO: Returns the first of the listed product stores that has isContentReference=Y set ordering by defaultPriority, or if
     * none have isContentReference, the lowest defaultPriority, or the first in list (with a warning) if none.
     * Prints warning if no content reference and multiple stores with different defaultLocaleString.
     * <p>
     * NOTE: 2020-02-03: For compatibility reasons, isContentReference="N" is treated the same as unset.
     */
    public static GenericValue getContentReferenceStoreOrFirst(List<GenericValue> productStores, String multiWarningInfo) {
        if (UtilValidate.isEmpty(productStores)) {
            return null;
        }
        GenericValue productStore = findBestContentReferenceStore(productStores);
        if (productStore != null) {
            return productStore;
        }
        productStore = productStores.get(0);
        if (productStores.size() > 1 && multiWarningInfo != null) {
            Debug.logWarning("Multiple stores found for " + multiWarningInfo + ", but none specify defaultPriority or isContentReference=\"Y\""
                    + "; defaultLocaleString and other content settings may be ambiguous; selecting first store ("
                    + productStore.getString("productStoreId") + ", defaultLocaleString: " + productStore.getString("defaultLocaleString")
                    + ") as content reference", module);
        }
        return productStore;
    }

    /**
     * SCIPIO: Returns the first of the listed product stores that has isContentReference=Y set ordering by defaultPriority, or if
     * none have isContentReference, the lowest defaultPriority, or the first in list (no warning) if none.
     * Does not show warning if no content reference and multiple stores.
     */
    public static GenericValue getContentReferenceStoreOrFirst(List<GenericValue> productStores) {
        return getContentReferenceStoreOrFirst(productStores, null);
    }

    private static GenericValue findBestContentReferenceStore(List<GenericValue> productStores) {
        GenericValue result = null;
        Long bestDefaultPriority = null;
        Boolean bestIsContentReference = null;
        // Find the isContentReference record with the lowest defaultPriority, or otherwise the store with simply the lowest defaultPriority
        for(GenericValue productStore : productStores) {
            Long defaultPriority = productStore.getLong("defaultPriority");
            Boolean isContentReference = productStore.getBoolean("isContentReference");
            if (Boolean.TRUE.equals(isContentReference)) {
                if (result == null || !Boolean.TRUE.equals(bestIsContentReference)) {
                    result = productStore;
                    bestDefaultPriority = defaultPriority;
                    bestIsContentReference = isContentReference;
                } else if (defaultPriority != null && (bestDefaultPriority == null || defaultPriority < bestDefaultPriority)) { // prioritize the isContentReference="Y" records
                    result = productStore;
                    bestDefaultPriority = defaultPriority;
                    bestIsContentReference = isContentReference;
                }
            } else if (!Boolean.TRUE.equals(bestIsContentReference)) {
                if (defaultPriority != null && (bestDefaultPriority == null || defaultPriority < bestDefaultPriority)) { // prioritize the isContentReference="[N]" records
                    result = productStore;
                    bestDefaultPriority = defaultPriority;
                    bestIsContentReference = isContentReference;
                }
            }
        }
        return result;
    }

    /**
     * SCIPIO: Returns the default WebSite for the given store: if there is only one WebSite,
     * that one is always returned; if multiple websites, returns the one marked with isStoreDefault Y.
     * <p>
     * <strong>NOTE:</strong> If there are multiple WebSites but none is marked with isStoreDefault Y,
     * logs a warning and returns null - by design - in a frontend environment there must be no ambiguity as to which
     * store should be used by default! (Otherwise, if ambiguous, could be picking a WebSite
     * intended for backend usage only, e.g. cms preview!)
     * <p>
     * <strong>WARN:</strong> In most cases relying looking up WebSite using productStoreId (like this method) 
     * means there is a design issue in code, and the code should be changed to pass a webSiteId around
     * instead (with this as fallback only).
     * <p>
     * Added 2018-10-02.
     */
    public static GenericValue getStoreDefaultWebSite(Delegator delegator, String productStoreId, boolean useCache) {
        if (UtilValidate.isEmpty(productStoreId)) {
            return null;
        }
        try {
            List<GenericValue> webSiteList = EntityQuery.use(delegator)
                    .from("WebSite").where("productStoreId", productStoreId).cache(useCache).queryList();
            if (webSiteList.size() == 1) {
                return webSiteList.get(0);
            // NOTE: This is technically possible, so a warning is not appropriate.
            //} else if (webSiteList.size() == 0) {
            //    Debug.logWarning("...", module);
            } else if (webSiteList.size() >= 2) {
                List<GenericValue> defaultWebSiteList = EntityUtil.filterByAnd(webSiteList, 
                        UtilMisc.toMap("isStoreDefault", "Y"));
                if (defaultWebSiteList.size() == 1) {
                    return defaultWebSiteList.get(0);
                } else if (defaultWebSiteList.size() >= 2) {
                    Debug.logError("Found multiple WebSites marked default (isStoreDefault Y)"
                            + " for product store '" + productStoreId + "'; only one should be marked default"
                            + "; using first found (" + defaultWebSiteList.get(0).getString("webSiteId") + ")", module);
                    return defaultWebSiteList.get(0);
                } else {
                    Debug.logWarning("Cannot determine a default WebSite for product store '" + productStoreId 
                            + "'; no WebSites marked as store default (isStoreDefault Y)"
                            + "; to rectify this, visit: /catalog/control/EditProductStoreWebSites?productStoreId=" + productStoreId, module);
                }
            }
        } catch(GenericEntityException e) {
            Debug.logError(e, "Cannot determine a WebSite for product store '" 
                    + productStoreId + "'", module);
        }
        return null;
    }

    /**
     * SCIPIO: Returns the default WebSite for the given store: if there is only one WebSite,
     * that one is always returned; if multiple websites, returns the one marked with isStoreDefault Y.
     * <p>
     * <strong>NOTE:</strong> If there are multiple WebSites but none is marked with isStoreDefault Y,
     * logs a warning and returns null - by design - in a frontend environment there must be no ambiguity as to which
     * store should be used by default! (Otherwise, if ambiguous, could be picking a WebSite
     * intended for backend usage only, e.g. cms preview!)
     * <p>
     * <strong>WARN:</strong> In most cases relying looking up WebSite using productStoreId (like this method) 
     * means there is a design issue in code, and the code should be changed to pass a webSiteId around
     * instead (with this as fallback only).
     * <p>
     * Added 2019-01.
     */
    public static GenericValue getStoreDefaultWebSite(Delegator delegator, GenericValue productStore, boolean useCache) {
        if (productStore == null) {
            return null;
        }
        return getStoreDefaultWebSite(delegator, productStore.getString("productStoreId"), useCache);
    }

    /**
     * SCIPIO: Returns the default WebSite for the given store: if there is only one WebSite,
     * that one is always returned; if multiple websites, returns the one marked with isStoreDefault Y.
     * <p>
     * <strong>NOTE:</strong> If there are multiple WebSites but none is marked with isStoreDefault Y, 
     * logs a warning and returns null - by design - in a frontend environment there must be no ambiguity as to which
     * store should be used by default! (Otherwise, if ambiguous, could be picking a WebSite
     * intended for backend usage only, e.g. cms preview!)
     * <p>
     * Added 2018-10-02.
     */
    public static String getStoreDefaultWebSiteId(Delegator delegator, String productStoreId, boolean useCache) {
        GenericValue webSite = getStoreDefaultWebSite(delegator, productStoreId, useCache);
        return (webSite != null) ? webSite.getString("webSiteId") : null;
    }

    /**
     * SCIPIO: Returns the default WebSite for the given store: if there is only one WebSite,
     * that one is always returned; if multiple websites, returns the one marked with isStoreDefault Y.
     * <p>
     * <strong>NOTE:</strong> If there are multiple WebSites but none is marked with isStoreDefault Y, 
     * logs a warning and returns null - by design - in a frontend environment there must be no ambiguity as to which
     * store should be used by default! (Otherwise, if ambiguous, could be picking a WebSite
     * intended for backend usage only, e.g. cms preview!)
     * <p>
     * Added 2019-01.
     */
    public static String getStoreDefaultWebSiteId(Delegator delegator, GenericValue productStore, boolean useCache) {
        if (productStore == null) {
            return null;
        }
        GenericValue webSite = getStoreDefaultWebSite(delegator, productStore.getString("productStoreId"), useCache);
        return (webSite != null) ? webSite.getString("webSiteId") : null;
    }
    
    /**
     * SCIPIO: Returns the logically configured value of catalog.properties#store.email.useStoreDefaultWebSite.
     * <p>
     * Values: "default" or empty means should use the default WebSite only if there is no explicit webSiteId
     * configured; "no" means never use the fallback; "force" means override the webSiteId with the default.
     * <p>
     * Added 2019-01.
     */
    public static String getUseStoreDefaultWebSiteForEmails(Delegator delegator) {
        return EntityUtilProperties.getPropertyValue("catalog", "store.email.useStoreDefaultWebSite", delegator);
    }

    /**
     * SCIPIO: Returns the logically appropriate webSiteId that should be used for an email sent for the given
     * ProductStore (high-level helper method).
     * <p>
     * This consults the catalog.properties#store.email.useStoreDefaultWebSite configuration and the 
     * WebSite.isStoreDefault flag.
     * <p>
     * Added 2019-01.
     *
     * @param delegator The delegator
     * @param productStoreId The ProductStore ID
     * @param webSiteId If the record (e.g. OrderHeader) has an explicitly-recorded webSiteId, it should be passed here
     * @return The webSiteId of the WebSite that should be used for the email for this store
     */
    public static String getStoreWebSiteIdForEmail(Delegator delegator, String productStoreId, String webSiteId, boolean useCache) {
        String useDefaultWebSite = getUseStoreDefaultWebSiteForEmails(delegator);
        if ("never".equals(useDefaultWebSite)) {
            return webSiteId;
        }
        if (!"force".equals(useDefaultWebSite) && UtilValidate.isNotEmpty(webSiteId)) {
            return webSiteId;
        }
        return getStoreDefaultWebSiteId(delegator, productStoreId, useCache);
    }

    /**
     * SCIPIO: Returns the logically appropriate webSiteId that should be used for an email sent for the given
     * ProductStore (high-level helper method).
     * <p>
     * This consults the catalog.properties#store.email.useStoreDefaultWebSite configuration and the 
     * WebSite.isStoreDefault flag.
     * <p>
     * Added 2019-01.
     *
     * @param delegator The delegator
     * @param productStore The ProductStore entity
     * @param webSiteId If the record (e.g. OrderHeader) has an explicitly-recorded webSiteId, it should be passed here
     * @return The webSiteId of the WebSite that should be used for the email for this store
     */
    public static String getStoreWebSiteIdForEmail(Delegator delegator, GenericValue productStore, String webSiteId, boolean useCache) {
        if (productStore == null) {
            return null;
        }
        return getStoreWebSiteIdForEmail(delegator, productStore.getString("productStoreId"), webSiteId, useCache);
    }

    /**
     * SCIPIO: Checks whether a customer has purchased a given product or not
     *
     * @param productStore
     * @param partyId
     * @param productId
     * @return
     */
    public static boolean proofOfPurchase(Delegator delegator, GenericValue productStore, String partyId, String productId) {
        if (UtilValidate.isNotEmpty(partyId) && UtilValidate.isNotEmpty(productStore) && UtilValidate.isNotEmpty(productId)) {
            DynamicViewEntity dve = new DynamicViewEntity();

            dve.addMemberEntity("OHAI", "OrderHeaderAndItems");
            dve.addMemberEntity("ORL", "OrderRole");

            dve.addAlias("OHAI", "orderId", null, null, true, true, null);
            dve.addAlias("OHAI", "orderStatusId", null, null, false, true, null);
            dve.addAlias("OHAI", "productStoreId", null, null, false, true, null);
            dve.addAlias("OHAI", "productId", null, null, false, true, null);
            dve.addAlias("ORL", "orderId", null, null, true, true, null);
            dve.addAlias("ORL", "partyId", null, null, false, true, null);
            dve.addAlias("ORL", "roleTypeId", null, null, false, true, null);

            dve.addViewLink("OHAI", "ORL", true, UtilMisc.toList(new ModelKeyMap("orderId", "orderId")));

            // TODO: Handle virtual -> variant products

            EntityCondition condition = EntityCondition.makeCondition(UtilMisc.toList(
                EntityCondition.makeCondition("partyId", partyId),
                EntityCondition.makeCondition("productId", productId),
                EntityCondition.makeCondition("productStoreId", productStore.getString("productStoreId")),
                EntityCondition.makeCondition("orderStatusId", "ORDER_COMPLETED"),
                EntityCondition.makeCondition("roleTypeId", EntityOperator.IN,
                        UtilMisc.toList("PLACING_CUSTOMER", "END_USER_CUSTOMER", "BILL_TO_CUSTOMER", "SHIP_TO_CUSTOMER", "CUSTOMER"))
            ), EntityOperator.AND);
            try {
                GenericValue productOrdered = EntityQuery.use(delegator)
                    .from(dve)
                    .where(condition).queryFirst();
                if (UtilValidate.isNotEmpty(productOrdered)) {
                    return true;
                }
            } catch (Exception e) {
                Debug.logError(e.getMessage(), module);
            }
        }

        return false;
    }

    /**
     * Returns useVariantStockCalc on ProductStore. NOTE: 2020-02-26: The default is now true.
     */
    public static boolean isUseVariantStockCalc(GenericValue productStore) { // SCIPIO
        if (productStore == null) {
            return true; // TODO: REVIEW: should default be true even when store is missing? Happens in solr... for now let's be consistent
        }
        return !Boolean.FALSE.equals(productStore.getBoolean("useVariantStockCalc"));
    }
}
