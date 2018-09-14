package org.ofbiz.service;

import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.GeneralException;

/**
 * SCIPIO: Thrown by some methods when a service or service-like function returned an error requiring an exception.
 * <p>
 * Small exception class to pass back service results without requiring the method
 * result to be a service result.
 */
@SuppressWarnings("serial")
public class ServiceErrorException extends GeneralException {

    private final Map<String, Object> serviceResult;

    public ServiceErrorException(String exceptionMessage, Map<String, Object> serviceResult) {
        super(exceptionMessage);
        this.serviceResult = serviceResult;
    }

    public ServiceErrorException(String exceptionMessage, List<String> serviceErrorMessageList) {
        super(exceptionMessage);
        this.serviceResult = ServiceUtil.returnError(serviceErrorMessageList);
    }

    public ServiceErrorException(String exceptionMessage, String serviceErrorMessage) {
        super(exceptionMessage);
        this.serviceResult = ServiceUtil.returnError(serviceErrorMessage);
    }

    public Map<String, Object> getServiceResult() {
        return serviceResult;
    }

}