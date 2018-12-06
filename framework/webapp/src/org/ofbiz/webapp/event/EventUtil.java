package org.ofbiz.webapp.event;

import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.service.ModelService;

/**
 * SCIPIO: Event utilities and definitions.
 */
public final class EventUtil {

    public static final String ERROR_MESSAGE_LIST = "_ERROR_MESSAGE_LIST_";
    public static final String ERROR_MESSAGE_MAP = "_ERROR_MESSAGE_MAP_";
    public static final String ERROR_MESSAGE = "_ERROR_MESSAGE_";
    public static final String EVENT_MESSAGE_LIST = "_EVENT_MESSAGE_LIST_";
    public static final String EVENT_MESSAGE = "_EVENT_MESSAGE_";

    private static final Set<String> eventMsgAttrNames = UtilMisc.unmodifiableHashSet(
            EVENT_MESSAGE_LIST, EVENT_MESSAGE);
    
    private static final Set<String> errorMsgAttrNames = UtilMisc.unmodifiableHashSet(
            ERROR_MESSAGE_LIST, ERROR_MESSAGE_MAP, ERROR_MESSAGE);
    
    private static final Set<String> eventErrorMsgAttrNames = UtilMisc.unmodifiableHashSet(
            ERROR_MESSAGE_LIST, ERROR_MESSAGE_MAP, ERROR_MESSAGE,
            EVENT_MESSAGE_LIST, EVENT_MESSAGE);

    private EventUtil() {
    }

    /**
     * Returns the standard event regular (success) AND error message attribute names:
     *  _ERROR_MESSAGE_LIST_, _EVENT_MESSAGE_LIST_, etc.
     */
    public static Set<String> getEventErrorMsgAttrNames() {
        return eventErrorMsgAttrNames;
    }
    
    public static boolean isEventErrorMsgAttrName(String attributeName) {
        return eventErrorMsgAttrNames.contains(attributeName);
    }
    
    /**
     * Returns the standard event message attribute names:
     *  _EVENT_MESSAGE_LIST_, etc.
     */
    public static Set<String> getEventMsgAttrNames() {
        return eventMsgAttrNames;
    }

    public static boolean isEventMsgAttrName(String attributeName) {
        return eventMsgAttrNames.contains(attributeName);
    }
    
    /**
     * Returns the standard error message attribute names:
     *  _ERROR_MESSAGE_LIST_, etc.
     */
    public static Set<String> getErrorMsgAttrNames() {
        return errorMsgAttrNames;
    }
    
    public static boolean isErrorMsgAttrName(String attributeName) {
        return errorMsgAttrNames.contains(attributeName);
    }
    
    public static void setServiceMsgsToEventMsgs(Map<String, Object> serviceResult, Map<String, Object> targetAttributes) {
        targetAttributes.put(ERROR_MESSAGE_LIST, serviceResult.get(ModelService.ERROR_MESSAGE_LIST));
        targetAttributes.put(ERROR_MESSAGE_MAP, serviceResult.get(ModelService.ERROR_MESSAGE_MAP));
        targetAttributes.put(ERROR_MESSAGE, serviceResult.get(ModelService.ERROR_MESSAGE));
    
        targetAttributes.put(EVENT_MESSAGE_LIST, serviceResult.get(ModelService.SUCCESS_MESSAGE_LIST));
        targetAttributes.put(EVENT_MESSAGE, serviceResult.get(ModelService.SUCCESS_MESSAGE));
    }
    
    public static void setServiceMsgsToEventMsgs(Map<String, Object> serviceResult, HttpServletRequest targetRequest) {
        targetRequest.setAttribute(ERROR_MESSAGE_LIST, serviceResult.get(ModelService.ERROR_MESSAGE_LIST));
        targetRequest.setAttribute(ERROR_MESSAGE_MAP, serviceResult.get(ModelService.ERROR_MESSAGE_MAP));
        targetRequest.setAttribute(ERROR_MESSAGE, serviceResult.get(ModelService.ERROR_MESSAGE));
    
        targetRequest.setAttribute(EVENT_MESSAGE_LIST, serviceResult.get(ModelService.SUCCESS_MESSAGE_LIST));
        targetRequest.setAttribute(EVENT_MESSAGE, serviceResult.get(ModelService.SUCCESS_MESSAGE));
    }
}
