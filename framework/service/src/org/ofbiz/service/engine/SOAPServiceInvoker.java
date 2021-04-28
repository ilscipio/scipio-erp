package org.ofbiz.service.engine;

import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.ModelService;

import java.util.Map;

public interface SOAPServiceInvoker {

    Map<String, Object> serviceInvoker(ModelService modelService, Map<String, Object> context) throws GenericServiceException;
}
