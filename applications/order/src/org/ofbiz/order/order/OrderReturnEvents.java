package org.ofbiz.order.order;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.webapp.event.EventUtil;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

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
        protected String partyId;
        protected String returnId;
        protected Map<String, Object> returnHeaderParams;
        //protected Map<String, Object> returnItemParams;
        protected Map<String, Object> createReturnHeaderCtx;
        protected Map<String, Object> createReturnHeaderResult;

        public ReturnHandler(HttpServletRequest request, HttpServletResponse response) {
            this.request = request;
            this.response = response;
            this.delegator = (Delegator) request.getAttribute("delegator");
            this.dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
            this.userLogin = UtilHttp.getSessionUserLogin(request);
            this.locale = UtilHttp.getLocale(request);
            this.partyId = (userLogin != null) ? userLogin.getString("partyId") : null;
        }

        public void getCheckOrder() throws GenericEntityException {
            orderId = (String) request.getAttribute("orderId");
            if (orderId == null) {
                orderId = request.getParameter("orderId");
            }
            orderHeader = delegator.from("OrderHeader").where("orderId", orderId).queryOne();
            if (orderHeader == null) {
                throw new IllegalStateException("order ID not found: " + orderId);
            }
            if (delegator.from("OrderRole").where("orderId", orderId, "partyId", partyId, "roleTypeId", "PLACING_CUSTOMER").queryCountSafe() <= 0) {
                throw new IllegalStateException("Party [" + partyId + "] is not the placing customer of orderId '[" + orderId + "]");
            }
            productStore = orderHeader.getRelatedOne("ProductStore", false);
            if (this.productStore == null) {
                throw new IllegalStateException("Missing store for order '[" + orderHeader.get("orderId") + "]");
            }
        }

        public String createCustomerReturn() {
            String result = null;
            try {
                try {
                    getCheckOrder();
                    returnHeaderParams = EventUtil.getValidServiceEventParamMap(request, "createReturnHeader");
                    createReturnHeaderCtx = new HashMap<>();
                    populateReturnHeader(createReturnHeaderCtx, returnHeaderParams);
                    createReturnHeaderResult = dispatcher.runSync("createReturnHeader", createReturnHeaderCtx);
                    EventUtil.setAttributesFromService(request, createReturnHeaderResult);
                    if (!ServiceUtil.isSuccess(createReturnHeaderResult)) {
                        result = "error";
                        return result;
                    }
                    returnId = (String) createReturnHeaderResult.get("returnId");
                } catch (Exception e) {
                    Debug.logError(e, "createCustomerReturn: " + e.getMessage(), module);
                    request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactSupport", UtilHttp.getLocale(request)));
                    result = "error";
                    return result;
                }
                Collection<Map<String, Object>> returnItemCtxList = null;
                try {
                    returnItemCtxList = EventUtil.getMultiSubmitServiceContexts(request, "createReturnItem", EventUtil.ServiceParamOptions.VALIDATE).values();
                    if (returnItemCtxList.isEmpty()) {
                        request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("OrderErrorUiLabels", "OrderNoReturnItemsSelected", UtilHttp.getLocale(request)));
                        result = "error";
                        return result;
                    }
                } catch (Exception e) {
                    Debug.logError(e, "createCustomerReturn: " + e.getMessage(), module);
                    request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactSupport", UtilHttp.getLocale(request)));
                    result = "error";
                    return result;
                }
                for (Map<String, Object> returnItemParams : returnItemCtxList) {
                    try {
                        Map<String, Object> createReturnItemCtx = new HashMap<>();
                        populateReturnItem(createReturnItemCtx, returnItemParams);
                        Map<String, Object> returnItemResult = dispatcher.runSync("createReturnItem", createReturnItemCtx);
                        if (!ServiceUtil.isSuccess(returnItemResult)) {
                            EventUtil.setMessagesFromService(request, returnItemResult);
                            result = "error";
                            return result;
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "createCustomerReturn: " + e.getMessage(), module);
                        request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactSupport", UtilHttp.getLocale(request)));
                        result = "error";
                        return result;
                    }
                }
            } finally {
                if (!"success".equals(result)) {
                    EventUtil.clearEventAttributes(request); // remove success message
                }
            }
            return "success";
        }

        public void populateReturnHeader(Map<String, Object> outCtx, Map<String, Object> returnHeaderParams) throws GenericEntityException {
            outCtx.put("userLogin", userLogin);
            outCtx.put("locale", locale);
            outCtx.put("returnHeaderTypeId", "CUSTOMER_RETURN");
            outCtx.put("fromPartyId", partyId);
            String toPartyId = productStore.getString("payToPartyId");
            if (toPartyId == null) {
                throw new IllegalStateException("Missing payToPartyId for store '[" + productStore.get("productStoreId") + "]");
            }
            outCtx.put("toPartyId", toPartyId);
            String destinationFacilityId = productStore.getString("inventoryFacilityId");
            if (destinationFacilityId == null) {
                throw new IllegalStateException("Missing inventoryFacilityId for store '[" + productStore.get("productStoreId") + "]; cannot set destinationFacilityId");
            }
            outCtx.put("destinationFacilityId", destinationFacilityId);
            outCtx.put("currencyUomId", orderHeader.get("currencyUom"));
        };

        public void populateReturnItem(Map<String, Object> outCtx, Map<String, Object> returnItemParams) throws GenericEntityException {
            outCtx.put("userLogin", userLogin);
            outCtx.put("locale", locale);
            outCtx.put("returnId", returnId);
            outCtx.put("orderId", orderHeader.get("orderId")); // force order ID
            // FIXME: these should be looked up again here from OrderItem instead of from params, but this is same behavior as old code for now and less serious than the rest
            outCtx.put("orderItemSeqId", returnItemParams.get("orderItemSeqId"));
            outCtx.put("description", returnItemParams.get("description"));
            outCtx.put("returnItemTypeId", returnItemParams.get("returnItemTypeId"));
            outCtx.put("returnPrice", returnItemParams.get("returnPrice"));
            outCtx.put("productId", returnItemParams.get("productId"));
            outCtx.put("returnQuantity", returnItemParams.get("returnQuantity"));
            outCtx.put("returnReasonId", returnItemParams.get("returnReasonId"));
            outCtx.put("returnTypeId", returnItemParams.get("returnTypeId"));
        };
    }

}
