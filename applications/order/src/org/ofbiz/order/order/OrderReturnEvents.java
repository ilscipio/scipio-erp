package org.ofbiz.order.order;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.webapp.event.EventUtil;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Collection;
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
    public static String createReturnHeaderAndItems(HttpServletRequest request, HttpServletResponse response) {
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        String result = null;
        try {
            try {
                Map<String, Object> servCtx = EventUtil.getValidServiceEventParamMap(request, "createReturnHeader");
                Map<String, Object> headerResult = dispatcher.runSync("createReturnHeader", servCtx);
                EventUtil.setAttributesFromService(request, headerResult);
                if (!ServiceUtil.isSuccess(headerResult)) {
                    result = "error";
                    return result;
                }
            } catch (Exception e) {
                Debug.logError(e, "createReturnHeaderAndItems: " + e.getMessage(), module);
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
                Debug.logError(e, "createReturnHeaderAndItems: " + e.getMessage(), module);
                request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactSupport", UtilHttp.getLocale(request)));
                result = "error";
                return result;
            }
            for (Map<String, Object> returnItemCtx : returnItemCtxList) {
                try {
                    Map<String, Object> returnItemResult = dispatcher.runSync("createReturnItem", returnItemCtx);
                    if (!ServiceUtil.isSuccess(returnItemResult)) {
                        EventUtil.setMessagesFromService(request, returnItemResult);
                        result = "error";
                        return result;
                    }
                } catch (Exception e) {
                    Debug.logError(e, "createReturnHeaderAndItems: " + e.getMessage(), module);
                    request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactSupport", UtilHttp.getLocale(request)));
                    return "error";
                }
            }
        } finally {
            if (!"success".equals(result)) {
                EventUtil.clearEventAttributes(request); // remove success message
            }
        }
        return "success";
    }

}
