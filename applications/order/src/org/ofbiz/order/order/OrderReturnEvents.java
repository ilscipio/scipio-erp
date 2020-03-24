package org.ofbiz.order.order;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.PropertyMessage;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.webapp.event.EventUtil;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

/**
 * Order return events (SCIPIO).
 */
public abstract class OrderReturnEvents {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Combines createReturnHeader + createReturnItem service-multi in separate events in original shop controller,
     * and combines everything in the default java event handler transaction (rolled back by the service calls on error).
     * Requires at least one item to be selected.
     */
    public static String createCustomerReturn(HttpServletRequest request, HttpServletResponse response) {
        return new ReturnHandler(request, response).createCustomerReturn();
    }

    /**
     * Helps create returns, written for storefront.
     * NOTE: Client code should prefer the getters.
     * TODO: Better abstraction using getters/setters on mutable fields to avoid client impact from changes + needless lookups
     */
    public static class ReturnHandler {
        protected HttpServletRequest request;
        protected HttpServletResponse response;
        protected String orderId;
        protected GenericValue orderHeader;
        protected GenericValue productStore;
        protected Delegator delegator;
        protected LocalDispatcher dispatcher;
        protected GenericValue userLogin;
        protected Locale locale;
        protected TimeZone timeZone;
        protected String partyId;
        protected String returnId;
        protected GenericValue returnHeader;
        protected GenericValue orderItem;
        protected Map<String, Object> returnHeaderParams;
        protected Map<String, Object> returnHeaderCtx;
        protected Map<String, Object> returnHeaderResult;
        protected Map<String, Object> returnItemCtx;
        protected Map<String, Object> returnItemParams;
        protected Map<String, Object> returnItemResult;
        protected GenericValue returnItem;
        protected Map<GenericValue, Map<String, Object>> returnableItems;
        protected Map<String, String> customerReturnItemTypeMap;
        protected OrderReadHelper orh;

        protected String createReturnHeaderService = "createReturnHeader";
        protected String createReturnItemService = "createReturnItem";

        public ReturnHandler(HttpServletRequest request, HttpServletResponse response) {
            this.request = request;
            this.response = response;
            this.delegator = (Delegator) request.getAttribute("delegator");
            this.dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
            this.userLogin = UtilHttp.getSessionUserLogin(request);
            this.locale = UtilHttp.getLocale(request);
            this.timeZone = UtilHttp.getTimeZone(request);
            this.partyId = (userLogin != null) ? userLogin.getString("partyId") : null;
        }

        public HttpServletRequest getRequest() { return request; }
        public HttpServletResponse getResponse() { return response; }
        public String getCreateReturnHeaderService() { return createReturnHeaderService; }
        public String getCreateReturnItemService() { return createReturnItemService; }
        public String getOrderId() throws GeneralException { return orderId; }
        public GenericValue getOrderHeader() throws GeneralException { return orderHeader; }
        public GenericValue getProductStore() throws GeneralException { return productStore; }
        public Delegator getDelegator() { return delegator; }
        public LocalDispatcher getDispatcher() { return dispatcher; }
        public Security getSecurity() { return (Security) getRequest().getAttribute("security"); }
        public GenericValue getUserLogin() { return userLogin; }
        public Locale getLocale() { return locale; }
        public TimeZone getTimeZone() { return timeZone; }
        public String getPartyId() throws GeneralException { return partyId; }
        public String getReturnId() throws GeneralException { return returnId; }
        public Map<String, Object> getReturnHeaderParams() { return returnHeaderParams; }
        public Map<String, Object> getReturnHeaderCtx() { return returnHeaderCtx; }
        public Map<String, Object> getReturnHeaderResult() { return returnHeaderResult; }
        public Map<String, Object> getReturnItemCtx() { return returnItemCtx; }
        public Map<String, Object> getReturnItemParams() { return returnItemParams; }
        public Map<String, Object> getReturnItemResult() { return returnItemResult; }
        public GenericValue getReturnItem() throws GeneralException {
            if (returnItem == null && getReturnItemResult() != null) {
                returnItem = getDelegator().from("ReturnItem").where("returnId", getReturnItemResult().get("returnId"),
                        "returnItemSeqId", getReturnItemResult().get("returnItemSeqId")).queryOne();
            }
            return returnItem;
        }
        public Map<GenericValue, Map<String, Object>> getReturnableItems() throws GeneralException { return returnableItems; }
        public GenericValue getReturnHeader() throws GeneralException { return returnHeader; }
        public OrderReadHelper getOrh() throws GeneralException { if (orh == null) { orh = new OrderReadHelper(getDispatcher(), getLocale(), getOrderHeader()); } return orh; }
        public Map<String, String> getCustomerReturnItemTypeMap() throws GeneralException { return customerReturnItemTypeMap; }
        public void setCreateReturnHeaderService(String createReturnHeaderService) { this.createReturnHeaderService = createReturnHeaderService; }
        public void setCreateReturnItemService(String createReturnItemService) { this.createReturnItemService = createReturnItemService; }
        public void setReturnableItems(Map<GenericValue, Map<String, Object>> returnableItems) { this.returnableItems = returnableItems; }

        protected void initForOrder() throws GeneralException {
            if (orderId == null) {
                orderId = (String) request.getAttribute("orderId");
                if (orderId == null) {
                    orderId = request.getParameter("orderId");
                }
            }
            if (UtilValidate.isEmpty(orderId)) {
                throw new IllegalArgumentException("no order ID found in request");
            }
            orderHeader = delegator.from("OrderHeader").where("orderId", orderId).queryOne();
            if (orderHeader == null) {
                throw new IllegalStateException("order '" + orderId + "' not found");
            }
            productStore = orderHeader.getRelatedOne("ProductStore", false);
            if (this.productStore == null) {
                throw new IllegalStateException("Missing store for order '[" + orderHeader.get("orderId") + "]");
            }
            if (!hasReturnPermission()) {
                throw new IllegalStateException("Party [" + partyId + "] cannot modify order '" + orderId + "'");
            }
            Map<String, Object> res = dispatcher.runSync("getReturnableItems", UtilMisc.toMap("orderId", orderId));
            returnableItems = UtilGenerics.cast(res.get("returnableItems"));
            if (returnableItems == null) {
                throw new IllegalStateException("no returnableItems map determined for order '" + orderId + "'");
            }
            customerReturnItemTypeMap = OrderReadHelper.getOrderAdjustmentReturnItemTypeMap(delegator, "CUSTOMER_RETURN");
        }

        public boolean hasReturnPermission() {
            try {
                return hasReturnPermission(getSecurity(), getUserLogin(), getOrderHeader());
            } catch (GeneralException e) {
                Debug.logError(e, module);
                return false;
            }
        }

        /** WARNING: May be moved and deprecated */
        public static boolean hasReturnPermission(Security security, GenericValue userLogin, GenericValue orderHeader) {
            // FIXME: better check somewhere else (the one in OrderReadHelper is for view permission)
            if (security == null || userLogin == null || orderHeader == null) {
                return false;
            }
            if (security.hasEntityPermission("ORDERMGR", "_UPDATE", userLogin)) { // for admins
                return true;
            }
            return (userLogin.getDelegator().from("OrderRole").where("orderId", orderHeader.get("orderId"), "partyId", userLogin.get("partyId"), "roleTypeId", "PLACING_CUSTOMER").queryCountSafe() > 0);
        }

        public String createCustomerReturn() {
            String result = null;
            try {
                try {
                    initForOrder();
                    returnHeaderParams = EventUtil.getValidServiceEventParamMap(getRequest(), createReturnHeaderService);
                    result = createReturnHeader(returnHeaderParams, getOrderHeader());
                    if ("error".equals(result)) {
                        return result;
                    }
                } catch (Exception e) {
                    Debug.logError(e, "createCustomerReturn: " + e.getMessage(), module);
                    setUserErrorMessageAttr(e);
                    result = "error";
                    return result;
                }
                Collection<Map<String, Object>> returnItemCtxList = null;
                try {
                    // This emulates controller service-multi events, in a better way
                    returnItemCtxList = EventUtil.getMultiSubmitServiceContexts(getRequest(), getCreateReturnItemService(), EventUtil.ServiceParamOptions.VALIDATE).values();
                    if (returnItemCtxList.isEmpty()) {
                        getRequest().setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("OrderErrorUiLabels", "OrderNoReturnItemsSelected", UtilHttp.getLocale(request)));
                        result = "error";
                        return result;
                    }
                } catch (Exception e) {
                    Debug.logError(e, "createCustomerReturn: " + e.getMessage(), module);
                    setUserErrorMessageAttr(e);
                    result = "error";
                    return result;
                }
                for (Map<String, Object> returnItemParams : returnItemCtxList) {
                    this.returnItemParams = returnItemParams;
                    try {
                        String orderItemSeqId = (String) returnItemParams.get("orderItemSeqId");
                        orderItem = delegator.from("OrderItem").where("orderId", getOrderId(), "orderItemSeqId", orderItemSeqId).queryOne();
                        if (orderItem == null) {
                            throw new IllegalArgumentException("Could not find OrderItem [orderId=" + getOrderId() + ", orderItemSeqId=" + orderItemSeqId + "]");
                        }
                        result = createReturnItem(returnItemParams, orderItem, getReturnableItemInfo(getReturnableItems(), orderItemSeqId));
                        if ("error".equals(result)) {
                            return result;
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "createCustomerReturn: " + e.getMessage(), module);
                        setUserErrorMessageAttr(e);
                        result = "error";
                        return result;
                    }
                }
                try {
                    result = createExtras();
                    if ("error".equals(result)) {
                        return result;
                    }
                } catch (Exception e) {
                    Debug.logError(e, "createCustomerReturn: " + e.getMessage(), module);
                    setUserErrorMessageAttr(e);
                    result = "error";
                    return result;
                }
            } finally {
                if (!"success".equals(result)) {
                    EventUtil.clearEventAttributes(getRequest()); // remove success message
                }
            }
            /* DO NOT DO THIS in case the transaction times out - let the screen re-query it by returnId
            try {
                request.setAttribute("returnHeader", getReturnHeader());
            } catch (GeneralException e) {
                Debug.logError(e, module);
                setUserErrorMessageAttr(e);
                result = "error";
                return result;
            }
             */
            return "success";
        }

        protected String createReturnHeader(Map<String, Object> returnHeaderParams, GenericValue orderHeader) throws GeneralException {
            returnHeaderCtx = new HashMap<>();
            populateReturnHeader(returnHeaderCtx, returnHeaderParams, orderHeader);
            returnHeaderResult = getDispatcher().runSync(getCreateReturnHeaderService(), returnHeaderCtx);
            EventUtil.setAttributesFromService(getRequest(), returnHeaderResult);
            if (!ServiceUtil.isSuccess(returnHeaderResult)) {
                return "error";
            }
            returnId = (String) returnHeaderResult.get("returnId");
            returnHeader = getDelegator().from("ReturnHeader").where("returnId", returnId).queryOne();
            return "success";
        }

        protected void populateReturnHeader(Map<String, Object> ctx, Map<String, Object> returnHeaderParams, GenericValue orderHeader) throws GeneralException {
            setSystemServiceFields(ctx);
            ctx.put("returnHeaderTypeId", "CUSTOMER_RETURN");
            ctx.put("fromPartyId", getPartyId());
            String toPartyId = getProductStore().getString("payToPartyId");
            if (toPartyId == null) {
                throw new IllegalStateException("Missing payToPartyId for store '[" + getProductStore().get("productStoreId") + "]");
            }
            ctx.put("toPartyId", toPartyId);
            String destinationFacilityId = getProductStore().getString("inventoryFacilityId");
            if (destinationFacilityId == null) {
                throw new IllegalStateException("Missing inventoryFacilityId for store '[" + getProductStore().get("productStoreId") + "]; cannot set destinationFacilityId");
            }
            ctx.put("destinationFacilityId", destinationFacilityId);
            ctx.put("currencyUomId", getOrderHeader().get("currencyUom"));
        };

        protected String createReturnItem(Map<String, Object> returnItemParams, GenericValue orderItem, Map<String, Object> returnableItemInfo) throws GeneralException {
            returnItemCtx = new HashMap<>();
            returnItem = null;
            populateReturnItem(returnItemCtx, returnItemParams, orderItem, returnableItemInfo);
            returnItemResult = getDispatcher().runSync(getCreateReturnItemService(), returnItemCtx);
            if (!ServiceUtil.isSuccess(returnItemResult)) {
                EventUtil.setMessagesFromService(getRequest(), returnItemResult);
                return "error";
            }
            return "success";
        }

        protected void populateReturnItem(Map<String, Object> ctx, Map<String, Object> returnItemParams, GenericValue orderItem, Map<String, Object> returnableItemInfo) throws GeneralException {
            setSystemServiceFields(ctx);
            ctx.put("returnId", getReturnId());
            ctx.put("orderId", getOrderId()); // force order ID
            ctx.put("orderItemSeqId", orderItem.get("orderItemSeqId"));
            ctx.put("productId", orderItem.get("productId"));
            ctx.put("description", orderItem.get("itemDescription"));

            String returnItemTypeId = getCustomerReturnItemTypeMap().get((String) returnableItemInfo.get("itemTypeKey"));
            ctx.put("returnItemTypeId", returnItemTypeId);

            BigDecimal returnableQuantity = (BigDecimal) returnableItemInfo.get("returnableQuantity");
            BigDecimal returnQuantity = (BigDecimal) returnItemParams.get("returnQuantity");
            if (returnQuantity == null || returnableQuantity == null || returnQuantity.compareTo(BigDecimal.ZERO) <= 0 || returnQuantity.compareTo(returnableQuantity) > 0) {
                throwQuantityError(returnQuantity, returnableQuantity);
            }
            ctx.put("returnQuantity", returnQuantity);

            // TODO: REVIEW: is there any reason to accept lower than full price from frontend? Not allowing for now
            //BigDecimal returnablePrice = (BigDecimal) returnableItemInfo.get("returnablePrice");
            //BigDecimal returnPrice = (BigDecimal) returnItemParams.get("returnPrice");
            //ctx.put("returnPrice", returnPrice);
            ctx.put("returnPrice", returnableItemInfo.get("returnablePrice"));

            // FIXME?: Try to validate the following parameters? (minor - mostly client decision)
            ctx.put("returnReasonId", returnItemParams.get("returnReasonId"));
            ctx.put("returnTypeId", returnItemParams.get("returnTypeId"));
        }

        protected String createExtras() throws GeneralException {
            return "success";
        }

        protected Map<String, Object> getReturnableItemInfo(Map<GenericValue, Map<String, Object>> returnableItems, String orderItemSeqId) throws GeneralException {
            Map<String, Object> returnableItemInfo = OrderReadHelper.getReturnableItemInfo(returnableItems, orderItemSeqId);
            if (returnableItemInfo == null) {
                throw new IllegalArgumentException("OrderItem [orderId=" + getOrderId() + ", orderItemSeqId=" + orderItemSeqId + "] is not returnable (or was already returned)");
            }
            return returnableItemInfo;
        }

        public void setSystemServiceFields(Map<String, Object> ctx) {
            ctx.put("userLogin", getUserLogin());
            ctx.put("locale", getLocale());
            ctx.put("timeZone", getTimeZone());
        }

        public String getDefaultErrorMessage() {
            return UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactSupport", UtilHttp.getLocale(request));
        }

        public void setUserErrorMessageAttr(Exception e) {
            if (e instanceof UserException) {
                getRequest().setAttribute("_ERROR_MESSAGE_", ((UserException) e).getPropertyMessage().getMessage(getLocale()));
            } else {
                getRequest().setAttribute("_ERROR_MESSAGE_", getDefaultErrorMessage());
            }
        }

        protected void throwQuantityError(BigDecimal returnQuantity, BigDecimal returnableQuantity) throws GeneralException {
            // FIXME: replace with UserException with localized message
            throw new IllegalArgumentException("Invalid returnQuantity (" + returnQuantity + ") for returnableQuantity (" + returnableQuantity + ")");
        }

        public static class UserException extends GeneralException {
            public UserException(String msg) { super(msg); }
            public UserException(PropertyMessage propMsg) { super(propMsg); }
        }

    }

}
