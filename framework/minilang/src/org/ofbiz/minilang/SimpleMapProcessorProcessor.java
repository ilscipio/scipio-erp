package org.ofbiz.minilang;

import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.MapProcessor;
import org.ofbiz.base.util.UtilGenerics;

/**
 * SCIPIO: A wrapper around simple map processor invocations to fit MapProcessor interface.
 */
public class SimpleMapProcessorProcessor implements MapProcessor {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    protected final String xmlResource;
    protected final String name;
    
    protected SimpleMapProcessorProcessor(String xmlResource, String name) {
        super();
        this.xmlResource = xmlResource;
        this.name = name;
    }

    public static SimpleMapProcessorProcessor getInstance(HttpServletRequest request, String xmlResource, String name) {
        return new SimpleMapProcessorProcessor(xmlResource, name);
    }

    @Override
    public void process(Map<String, Object> inMap, Map<String, Object> results, Map<String, List<String>> fieldErrorMessages, 
            List<String> errorMessages, Locale locale) throws GeneralException {
        //try {
        SimpleMapProcessor.runSimpleMapProcessor(xmlResource, name, inMap, results, UtilGenerics.checkList(errorMessages), locale);
        //} catch (MiniLangException e) {
        //    Debug.logError(e, "Scipio: Error invoking simple map processor '" + name + "' in resource '" + xmlResource + "'", module);
        //    errorMessages.add("Internal error");
        //}
    }

}
