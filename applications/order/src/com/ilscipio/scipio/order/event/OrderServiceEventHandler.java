package com.ilscipio.scipio.order.event;

import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.order.shoppingcart.CartUpdate;
import org.ofbiz.order.shoppingcart.ShoppingCart;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelParam;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceAuthException;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.ServiceValidationException;
import org.ofbiz.webapp.control.ConfigXMLReader.Event;
import org.ofbiz.webapp.control.ConfigXMLReader.RequestMap;
import org.ofbiz.webapp.event.ServiceEventHandler;

/**
 * SCIPIO: Order app-specific service event handler.
 * @deprecated SCIPIO: 2018-11-23: Experimental class, here for reference...
 * <p>
 * Added 2018-11-23.
 */
@Deprecated
public class OrderServiceEventHandler extends ServiceEventHandler {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @Override
    protected Map<String, Object> invokeService(LocalDispatcher dispatcher, ModelService modelService,
            String serviceName, Map<String, Object> serviceContext, String mode, Event event, RequestMap requestMap,
            HttpServletRequest request, HttpServletResponse response)
            throws ServiceAuthException, ServiceValidationException, GenericServiceException {
        if (Boolean.TRUE.equals(modelService.getProperty("shoppingCart.update"))) {
            String cartFieldName = getShoppingCartInParamName(modelService);
            if (cartFieldName != null) {
                try (CartUpdate cartUpdate = new CartUpdate(request)) { // SCIPIO
                    synchronized (cartUpdate.getLockObject()) {
                        ShoppingCart cart = cartUpdate.getCartForUpdate();
                        serviceContext.put(cartFieldName, cart);
                        Map<String, Object> result = super.invokeService(dispatcher, modelService, serviceName, serviceContext, 
                                mode, event, requestMap, request, response);
                        
                        if (result == null || ServiceUtil.isSuccess(result)) {
                            Debug.logInfo("Auto-committing shopping cart update for event service '" + serviceName + "'", module);
                            cartUpdate.commit(cart);
                        } else {
                            Debug.logWarning("Event service '" + serviceName + "' did not succeed; shopping cart changes discarded", module);
                        }
                        return result;
                    }
                }
            } else {
                Debug.logError("Event service '" + serviceName + "' defines \"shoppingCart.update\""
                        + " but does not appear to define any shoppingCart IN attribute; cannot pass cart", module);
            }
        }
        return super.invokeService(dispatcher, modelService, serviceName, serviceContext, 
                mode, event, requestMap, request, response);
    }

    protected static String getShoppingCartInParamName(ModelService modelService) {
        if (modelService.getParam("shoppingCart") != null) { // optimization: usual case
            return "shoppingCart";
        }
        for (ModelParam param : modelService.getInModelParamList()) {
            // FIXME: poor type check
            if (ShoppingCart.class.getName().equals(param.getType())) {
                return param.getName();
            }
        }
        return null;
    }
    
}
