package org.ofbiz.service;

import com.ilscipio.scipio.ce.util.collections.ScipioMap;
import org.ofbiz.base.util.PropertyMessage;
import org.ofbiz.base.util.PropertyMessageExUtil;
import org.ofbiz.base.util.UtilGenerics;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Methods implemented by {@link ServiceContext} itself to generate {@link ServiceResult} instances.
 *
 * <p>Based on {@link ServiceUtil} returnError/returnSuccess/returnFailure/returnSys.</p>
 *
 * <p>Mainly intended for use on {@link ServiceContext}, keeps things readable.</p>
 *
 * <p>SCIPIO: 3.0.0: Added, made separate from ServiceContext as mixin to keep it clean.</p>
 */
public interface ServiceResultFactory {

    ModelService service();
    DispatchContext dctx();

    /**
     * Creates a new {@link ServiceResult} instance using the specific result context instance (as-is, unpopulated) (factory method).
     */
    default ServiceResult result(Map<String, ?> resultContext) {
        return ServiceResult.from(service(), dctx(), resultContext);
    }

    /**
     * Copies the given/previous result's system response fields ({@link ModelService#OUT_SYS_PARAMS}), including code and success/error messages
     * into a new service result map.
     *
     * <p>In other words copies only the fields (system service attributes) common to all OUT services by default, unless excluded in the
     * target service (which is the ModelService held by ServiceContext or this instance).</p>
     *
     * <p>This can be used to transfer a service response from an invoked service to the parent service return without having to go through
     * {@link #error}, {@link #fail} or {@link #success} which are often quite redundant and result in loss of information and formatting.
     *
     * @param otherResult   Map of context fields to use as source service system parameters (the actual return values), often a ServiceResult itself
     */
    default ServiceResult responseCopy(Map<String, ?> otherResult) {
        return result(ScipioMap.putKeys(new HashMap<>(), otherResult, ModelService.OUT_SYS_PARAMS, true));
    }

    /**
     * A small routine used all over to improve code efficiency, make a result map with the message and the success response code
     */
    default ServiceResult success() {
        return message(ModelService.RESPOND_SUCCESS, null);
    }

    /**
     * A small routine used all over to improve code efficiency, make a result map with the message and the success response code
     */
    default ServiceResult success(String successMessage) {
        return message(ModelService.RESPOND_SUCCESS, successMessage);
    }

    /**
     * A small routine used all over to improve code efficiency, make a result map with the message and the success response code
     */
    default ServiceResult success(List<String> successMessageList) {
        ServiceResult result = message(ModelService.RESPOND_SUCCESS, null);
        result.put(ModelService.SUCCESS_MESSAGE_LIST, successMessageList);
        return result;
    }

    default ServiceResult error(String errorMessage) {
        return problem(ModelService.RESPOND_ERROR, errorMessage, null, null, null);
    }

    default ServiceResult error(List<?> errorMessageList) {
        return problem(ModelService.RESPOND_ERROR, null, errorMessageList, null, null);
    }

    default ServiceResult error(String errorMessage, List<?> errorMessageList) {
        return problem(ModelService.RESPOND_ERROR, errorMessage, errorMessageList, null, null);
    }

    /**
     * A small routine used all over to improve code efficiency, make a result map with the message and the error response code, also forwards any error messages from the nestedResult
     */
    default ServiceResult error(String errorMessage, List<?> errorMessageList, Map<String, ?> errorMessageMap, Map<String, ?> nestedResult) {
        return problem(ModelService.RESPOND_ERROR, errorMessage, errorMessageList, errorMessageMap, nestedResult);
    }

    /**
     * Creates a service error result map from the given exception using the given localizable intro message
     * combined with a suffix message taken from either a localizable property message via {@link org.ofbiz.base.util.PropertyMessageEx#getPropertyMessage()}
     * stored in the exception if it implements PropertyMessageEx or the exception detail message if any other exception type,
     * in addition to any message lists stored in the exception via {@link org.ofbiz.base.util.PropertyMessageEx#getPropertyMessageList()}.
     * In other words this abstracts and automated the service result building from exception messages.
     */
    default ServiceResult error(PropertyMessage messageIntro, Throwable t, Locale locale) {
        return error(PropertyMessageExUtil.makeServiceMessage(messageIntro, t, locale), PropertyMessageExUtil.getExceptionMessageList(t, locale));
    }

    /**
     * Creates a service error result map from the given exception using the given static intro message
     * combined with a suffix message taken from either a localizable property message via {@link org.ofbiz.base.util.PropertyMessageEx#getPropertyMessage()}
     * stored in the exception if it implements PropertyMessageEx or the exception detail message if any other exception type,
     * in addition to any message lists stored in the exception via {@link org.ofbiz.base.util.PropertyMessageEx#getPropertyMessageList()}.
     * In other words this abstracts and automated the service result building from exception messages.
     */
    default ServiceResult error(String messageIntro, Throwable t, Locale locale) {
        return error(PropertyMessageExUtil.makeServiceMessage(messageIntro, t, locale), PropertyMessageExUtil.getExceptionMessageList(t, locale));
    }

    default ServiceResult fail(String errorMessage) {
        return problem(ModelService.RESPOND_FAIL, errorMessage, null, null, null);
    }

    default ServiceResult fail(String errorMessage, List<?> errorMessageList) {
        return problem(ModelService.RESPOND_FAIL, errorMessage, errorMessageList, null, null);
    }

    default ServiceResult fail(List<?> errorMessageList) {
        return problem(ModelService.RESPOND_FAIL, null, errorMessageList, null, null);
    }

    default ServiceResult fail() {
        return problem(ModelService.RESPOND_FAIL, null, null, null, null);
    }

    /**
     * Creates a service error result map from the given exception using the given localizable intro message
     * combined with a suffix message taken from either a localizable property message via {@link org.ofbiz.base.util.PropertyMessageEx#getPropertyMessage()}
     * stored in the exception if it implements PropertyMessageEx or the exception detail message if any other exception type,
     * in addition to any message lists stored in the exception via {@link org.ofbiz.base.util.PropertyMessageEx#getPropertyMessageList()}.
     * In other words this abstracts and automated the service result building from exception messages.
     */
    default ServiceResult fail(PropertyMessage messageIntro, Throwable t, Locale locale) {
        return fail(PropertyMessageExUtil.makeServiceMessage(messageIntro, t, locale), PropertyMessageExUtil.getExceptionMessageList(t, locale));
    }

    /**
     * Creates a service error result map from the given exception using the given localizable intro message
     * combined with a suffix message taken from either a localizable property message via {@link org.ofbiz.base.util.PropertyMessageEx#getPropertyMessage()}
     * stored in the exception if it implements PropertyMessageEx or the exception detail message if any other exception type,
     * in addition to any message lists stored in the exception via {@link org.ofbiz.base.util.PropertyMessageEx#getPropertyMessageList()}.
     * In other words this abstracts and automated the service result building from exception messages.
     */
    default ServiceResult fail(String messageIntro, Throwable t, Locale locale) {
        return fail(PropertyMessageExUtil.makeServiceMessage(messageIntro, t, locale), PropertyMessageExUtil.getExceptionMessageList(t, locale));
    }

    /**
     * A small routine to make a result map with the message and the response code
     * NOTE: This brings out some bad points to our message convention: we should be using a single message or message list
     * and what type of message that is should be determined by the RESPONSE_MESSAGE (and there's another annoyance, it should be RESPONSE_CODE)
     */
    default ServiceResult message(String code, String message) {
        Map<String, Object> result = new HashMap<>();
        if (code != null) {
            result.put(ModelService.RESPONSE_MESSAGE, code);
        }
        if (message != null) {
            result.put(ModelService.SUCCESS_MESSAGE, message);
        }
        return result(result);
    }

    default ServiceResult problem(String returnType, String errorMessage, List<?> errorMessageList, Map<String, ?> errorMessageMap, Map<String, ?> nestedResult) {
        Map<String, Object> result = new HashMap<>();
        result.put(ModelService.RESPONSE_MESSAGE, returnType);
        if (errorMessage != null) {
            result.put(ModelService.ERROR_MESSAGE, errorMessage);
        }

        List<Object> errorList = new ArrayList<>(); // SCIPIO: switched to ArrayList
        if (errorMessageList != null) {
            errorList.addAll(errorMessageList);
        }

        Map<String, Object> errorMap = new HashMap<>();
        if (errorMessageMap != null) {
            errorMap.putAll(errorMessageMap);
        }

        if (nestedResult != null) {
            if (nestedResult.get(ModelService.ERROR_MESSAGE) != null) {
                errorList.add(nestedResult.get(ModelService.ERROR_MESSAGE));
            }
            if (nestedResult.get(ModelService.ERROR_MESSAGE_LIST) != null) {
                errorList.addAll(UtilGenerics.checkList(nestedResult.get(ModelService.ERROR_MESSAGE_LIST)));
            }
            if (nestedResult.get(ModelService.ERROR_MESSAGE_MAP) != null) {
                errorMap.putAll(UtilGenerics.checkMap(nestedResult.get(ModelService.ERROR_MESSAGE_MAP)));
            }
        }

        if (errorList.size() > 0) {
            result.put(ModelService.ERROR_MESSAGE_LIST, errorList);
        }
        if (errorMap.size() > 0) {
            result.put(ModelService.ERROR_MESSAGE_MAP, errorMap);
        }
        // SCIPIO: NOTE: 2018-08-29: upstream added this, I do not agree with it, enough logs already
        //Debug.logError(result.toString(), module);
        return result(result);
    }

}
