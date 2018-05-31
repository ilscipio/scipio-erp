package com.ilscipio.scipio.ce.service.event;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.ofbiz.base.config.GenericConfigException;
import org.ofbiz.base.start.Config;
import org.ofbiz.base.start.ExtendedStartupLoader;
import org.ofbiz.base.start.StartupException;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceDispatcher;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.config.ServiceConfigUtil;
import org.ofbiz.service.config.model.Engine;
import org.ofbiz.service.config.model.ServiceEngine;
import org.ofbiz.service.engine.XMLRPCClientEngine;

/**
 * Service startup events.
 * <p>
 * Supports command-line-level startup service invocation request in the form (default mode: async):
 * <ul>
 * <li><code>./ant start -Dscipio.startup.service=serviceName -Dscipio.startup.service.mode=[sync|async] -Dscipio.startup.service.params.arg1=val1 -Dscipio.startup.service.params.arg2=val2</code></li>
 * <li><code>./ant start -Dscipio.startup.service.1=serviceName -Dscipio.startup.service.1.mode=[sync|async] -Dscipio.startup.service.1.params.arg1=val1 -Dscipio.startup.service.1.params.arg2=val2
 *                   -Dscipio.startup.service.2=serviceName -Dscipio.startup.service.2.mode=[sync|async] -Dscipio.startup.service.2.params.arg1=val1 -Dscipio.startup.service.2.params.arg2=val2</code></li>
 * </ul>
 * For advanced multi-service control, create a new group service instead.
 * <p>
 * Test cases:
 * <ul>
 * <li><code>
 * ./ant start -Dscipio.startup.service=echoService -Dscipio.startup.service.params.arg1a=val1a -Dscipio.startup.service.params.arg2a=val2a
 *             -Dscipio.startup.service.1=echoService -Dscipio.startup.service.1.params.arg1b=val1b -Dscipio.startup.service.1.params.arg2b=val2b
 *             -Dscipio.startup.service.2=echoService -Dscipio.startup.service.2.mode=sync -Dscipio.startup.service.2.params.arg1c=val1c -Dscipio.startup.service.2.params.arg2c=val2c
 *             -Dscipio.startup.service.3=echoService -Dscipio.startup.service.3.mode=async -Dscipio.startup.service.3.params.arg1d=val1d -Dscipio.startup.service.3.params.arg2d=val2d
 * </code></li>
 * </ul>               
 */
public class ServiceStartupEvents implements ExtendedStartupLoader {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @Override
    public void load(Config config, String[] args) throws StartupException {
    }

    @Override
    public void start() throws StartupException {
    }

    @Override
    public void unload() throws StartupException {
    }

    @Override
    public void execOnRunning() throws StartupException {
        execCheckEngines();
        execStartupServiceAsync();
    }

    protected void execCheckEngines() throws StartupException {
        try {
            for(ServiceEngine serviceEngine : ServiceConfigUtil.getServiceConfig().getServiceEngines()) {
                for(Engine engine : serviceEngine.getEngines()) {
                    if (XMLRPCClientEngine.class.getName().equals(engine.getClassName())) {
                        String urlParam = engine.getParameterValue("url");
                        if (UtilValidate.isEmpty(urlParam)) {
                            Debug.logError("Missing 'url' parameter in service engine config for XML-RPC engine '" 
                                    + engine.getName() + "'", module);
                            continue;
                        }
                        try {
                            URL url = new URL(urlParam);
                            if (!"localhost".equals(url.getHost()) && !"127.0.0.1".equals(url.getHost())) {
                                Debug.logWarning("\n"
                                        + "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                                        + "\nScipio: WARNING: Insecure XML-RPC service engine configuration!"
                                        + "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                                        + "\n"
                                        + "\nScipio has detected you have defined the following XML-RPC"
                                        + " service engine with a non-local host, '" + url.getHost() + "' (serviceengine.xml):"
                                        + "\n"
                                        + "\n    name: " + engine.getName()
                                        + "\n    class: " + engine.getClassName()
                                        + "\n    url: " + urlParam
                                        + "\n"
                                        + "\nThis configuration is insecure; the XML-RPC implementation is based on"
                                        + " Apache XML-RPC (https://ws.apache.org/xmlrpc/), which has not been maintained since"
                                        + " 2010 and contains security issues. It is only safe enough to use on localhost addresses."
                                        + " Please change to a different service engine implementation for network communication."
                                        + "\n"
                                        + "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                                        + "\n", module);
                            }
                        } catch (MalformedURLException e) {
                            Debug.logError("Malformed 'url' parameter in service engine config for XML-RPC engine '" + engine.getName() + "': " 
                                    + urlParam + ": " + e.getMessage(), module);
                        }
                    }
                }
            }
        } catch (GenericConfigException e) {
            Debug.logError("Unable to read service engine config: " + e.getMessage(), module);
        }
    }
        
    protected boolean execStartupServiceAsync() throws StartupException {
        // single service invocation
        String startupService = System.getProperty("scipio.startup.service");
        if (UtilValidate.isNotEmpty(startupService) || UtilValidate.isNotEmpty(System.getProperty("scipio.startup.service.1"))) {
            LocalDispatcher dispatcher = getDispatcher();
            if (dispatcher == null) {
                Debug.logError("Scipio: Cannot exec startup service; could not get dispatcher", module);
                return false;
            }
            Map<String, String> servParams = UtilProperties.getPropertiesWithPrefix(System.getProperties(), "scipio.startup.service.params.");
            String mode = System.getProperty("scipio.startup.service.mode");
            execStartupService(dispatcher, startupService, servParams, mode, null);

            // multiple service invocation
            if (UtilValidate.isNotEmpty(System.getProperty("scipio.startup.service.1"))) {
                Map<String, String> startupServices = UtilProperties.getPropertiesMatching(System.getProperties(), 
                        Pattern.compile("^scipio.startup.service.(\\d+)$"), true);
                List<Integer> serviceKeys = new ArrayList<>();
                for(String key : startupServices.keySet()) serviceKeys.add(Integer.parseInt(key));
                Collections.sort(serviceKeys);
                for(Integer key : serviceKeys) {
                    startupService = startupServices.get(key.toString());
                    servParams = UtilProperties.getPropertiesWithPrefix(System.getProperties(), "scipio.startup.service." + key + ".params.");
                    mode = System.getProperty("scipio.startup.service." + key + ".mode");
                    execStartupService(dispatcher, startupService, servParams, mode, key);
                }
            }
        }
        return false;
    }

    protected LocalDispatcher getDispatcher() {
        return ServiceDispatcher.getLocalDispatcher("default",
                DelegatorFactory.getDelegator("default"));
    }
    
    protected Map<String, Object> execStartupService(LocalDispatcher dispatcher, String serviceName, Map<String, ?> context, String mode, Integer serviceIndex) {
        try {
            if (!"sync".equals(mode) && !"async".equals(mode)) mode = "async";
            if (Debug.infoOn()) {
                final String indexMsg = (serviceIndex != null) ? " [" + serviceIndex + "]" : "";
                Debug.logInfo("Scipio: Running startup service '" + serviceName + "'" + indexMsg + " (" + mode + "), params: " + context, module);
            }
            Map<String, Object> ctx = dispatcher.getDispatchContext().makeValidContext(serviceName, ModelService.IN_PARAM, context);
            if ("async".equals(mode)) {
                dispatcher.runAsync(serviceName, ctx, false);
                return null;
            } else {
                Map<String, Object> result = dispatcher.runSync(serviceName, ctx);
                if (ServiceUtil.isError(result)) {
                    String msg = ServiceUtil.getErrorMessage(result);
                    if (msg != null && !msg.isEmpty()) {
                        Debug.logError("Scipio: Ran startup service '" + serviceName + 
                                "', status: " + result.get(ModelService.RESPONSE_MESSAGE) + ", message: " + msg, module);
                    } else {
                        Debug.logError("Scipio: Ran startup service '" + serviceName + 
                            "', status: " + result.get(ModelService.RESPONSE_MESSAGE), module);
                    }
                } else if (ServiceUtil.isFailure(result)) {
                    String msg = ServiceUtil.getErrorMessage(result);
                    if (msg != null && !msg.isEmpty()) {
                        Debug.logWarning("Scipio: Ran startup service '" + serviceName + 
                                "', status: " + result.get(ModelService.RESPONSE_MESSAGE) + ", message: " + msg, module);
                    } else {
                        Debug.logWarning("Scipio: Ran startup service '" + serviceName + 
                            "', status: " + result.get(ModelService.RESPONSE_MESSAGE), module);
                    }
                } else {
                    String msg = ServiceUtil.getSuccessMessage(result);
                    if (msg != null && !msg.isEmpty()) {
                        Debug.logInfo("Scipio: Ran startup service '" + serviceName + 
                                "', status: " + result.get(ModelService.RESPONSE_MESSAGE) + ", message: " + msg, module);
                    } else {
                        Debug.logInfo("Scipio: Ran startup service '" + serviceName + 
                            "', status: " + result.get(ModelService.RESPONSE_MESSAGE), module);
                    }
                }
                return result;
            }
        } catch (GenericServiceException e) {
            Debug.logError("Scipio: Error running startup service '" + serviceName + "': " + e.getMessage(), module);
            return ServiceUtil.returnError(e.getMessage());
        }
    }
}
