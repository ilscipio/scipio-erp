package com.ilscipio.scipio.ce.service.event;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.regex.Pattern;

import org.ofbiz.base.config.GenericConfigException;
import org.ofbiz.base.start.Config;
import org.ofbiz.base.start.ExtendedStartupLoader;
import org.ofbiz.base.start.StartupException;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
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
 * <li><code>./ant start -Dscipio.startup.service=serviceName -Dscipio.startup.service.mode=[sync|async] -Dscipio.startup.service.params.userLogin=system -Dscipio.startup.service.params.arg1=val1 -Dscipio.startup.service.params.arg2=val2</code></li>
 * <li><code>./ant start -Dscipio.startup.service.1=serviceName -Dscipio.startup.service.1.mode=[sync|async] -Dscipio.startup.service.1.params.arg1=val1 -Dscipio.startup.service.1.params.arg2=val2
 *                   -Dscipio.startup.service.2=serviceName -Dscipio.startup.service.2.mode=[sync|async] -Dscipio.startup.service.2.params.arg1=val1 -Dscipio.startup.service.2.params.arg2=val2</code></li>
 * </ul>
 * For specific user login pass the userLoginId to the params.userLogin parameter (it will be converted to GenericValue).
 * <p>
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
        LocalDispatcher dispatcher = getDispatcher();
        String serviceName;
        if (dispatcher == null) {
            Debug.logError("Scipio: Cannot exec startup service; could not get dispatcher", module);
            return false;
        }

        // properties-based invocation
        Map<String, Map<String, Object>> startupProps = UtilProperties.extractPropertiesWithPrefixAndId(new LinkedHashMap<>(),
                UtilProperties.getMergedPropertiesFromAllComponents("scipio-events"), "scipio.startup.service.");
        if (UtilValidate.isNotEmpty(startupProps)) {
            List<Map<String, Object>> serviceEntryList = new ArrayList<>(startupProps.values());
            serviceEntryList.sort(Comparator.comparingInt(o -> UtilMisc.toInteger(o.get("prio"), 9999999)));
            for(Map<String, Object> serviceEntry : serviceEntryList) {
                serviceName = (String) serviceEntry.get("name");
                String mode = getServiceMode(serviceName, serviceEntry.get("mode"));
                Map<String, String> servParams = UtilProperties.getPropertiesWithPrefix(serviceEntry, "params.");
                if (Debug.infoOn()) {
                    Debug.logInfo("Running startup service [" + serviceName + "] prio [" +
                            serviceEntry.get("prio") + "] mode [" + mode + "] params " + servParams, module);
                }
                execStartupService(dispatcher, serviceName, servParams, mode);
            }
        }

        // command line invocation
        serviceName = System.getProperty("scipio.startup.service");
        if (UtilValidate.isNotEmpty(serviceName) || UtilValidate.isNotEmpty(System.getProperty("scipio.startup.service.1"))) {
            // single service invocation
            Map<String, String> servParams = UtilProperties.getPropertiesWithPrefix(System.getProperties(), "scipio.startup.service.params.");
            String mode = getServiceMode(serviceName, System.getProperty("scipio.startup.service.mode"));
            if (Debug.infoOn()) {
                Debug.logInfo("Running startup service [" + serviceName + "] mode [" + mode + "] params " + servParams, module);
            }
            execStartupService(dispatcher, serviceName, servParams, mode);

            // multiple service invocation
            if (UtilValidate.isNotEmpty(System.getProperty("scipio.startup.service.1"))) {
                Map<String, String> startupServices = UtilProperties.getPropertiesMatching(System.getProperties(),
                        Pattern.compile("^scipio\\.startup\\.service\\.(\\d+)$"), true);
                List<Integer> serviceKeys = new ArrayList<>();
                for(String key : startupServices.keySet()) serviceKeys.add(Integer.parseInt(key));
                Collections.sort(serviceKeys);
                for(Integer key : serviceKeys) {
                    serviceName = startupServices.get(key.toString());
                    servParams = UtilProperties.getPropertiesWithPrefix(System.getProperties(), "scipio.startup.service." + key + ".params.");
                    mode = getServiceMode(serviceName, System.getProperty("scipio.startup.service." + key + ".mode"));
                    if (Debug.infoOn()) {
                        Debug.logInfo("Running startup service [" + serviceName + "] index [" + key + "] mode [" + mode + "] params " + servParams, module);
                    }
                    execStartupService(dispatcher, serviceName, servParams, mode);
                }
            }
        }

        return false;
    }

    protected LocalDispatcher getDispatcher() {
        return ServiceDispatcher.getLocalDispatcher("default",
                DelegatorFactory.getDelegator("default"));
    }

    protected String getServiceMode(String serviceName, Object mode) {
        if ("sync".equals(mode) || "async".equals(mode)) {
            return (String) mode;
        }
        if (UtilValidate.isNotEmpty((String) mode)) {
            Debug.logError("Invalid service mode for startup service [" + serviceName + "], using async: " + mode, module);
        }
        return "async";
    }

    protected Map<String, Object> execStartupService(LocalDispatcher dispatcher, String serviceName, Map<String, ?> context, String mode) {
        try {
            if (!"sync".equals(mode) && !"async".equals(mode)) mode = "async";
            String userLoginAuthId = null;
            if (context.get("userLogin") instanceof String) {
                userLoginAuthId = (String) context.get("userLogin");
                context.remove("userLogin");
            }
            Map<String, Object> ctx = dispatcher.getDispatchContext().makeValidContext(serviceName, ModelService.IN_PARAM, context);
            if (UtilValidate.isNotEmpty(userLoginAuthId)) {
                GenericValue userLogin = EntityQuery.use(dispatcher.getDelegator()).from("UserLogin")
                        .where("userLoginId", userLoginAuthId).queryOne();
                if (userLogin == null) {
                    Debug.logWarning("Could not find UserLogin [" + userLoginAuthId + "] to run startup service [" + serviceName + "]", module);
                } else {
                    ctx.put("userLogin", userLogin);
                }
            }
            if ("async".equals(mode)) {
                dispatcher.runAsync(serviceName, ctx, false);
                return null;
            } else {
                Map<String, Object> result = dispatcher.runSyncNewTrans(serviceName, ctx);
                if (ServiceUtil.isError(result)) {
                    String msg = ServiceUtil.getErrorMessage(result);
                    if (msg != null && !msg.isEmpty()) {
                        Debug.logError("Ran startup service [" + serviceName +
                                "], status: " + result.get(ModelService.RESPONSE_MESSAGE) + ", message: " + msg, module);
                    } else {
                        Debug.logError("Ran startup service [" + serviceName +
                            "], status: " + result.get(ModelService.RESPONSE_MESSAGE), module);
                    }
                } else if (ServiceUtil.isFailure(result)) {
                    String msg = ServiceUtil.getErrorMessage(result);
                    if (msg != null && !msg.isEmpty()) {
                        Debug.logWarning("Ran startup service [" + serviceName +
                                "], status: " + result.get(ModelService.RESPONSE_MESSAGE) + ", message: " + msg, module);
                    } else {
                        Debug.logWarning("Ran startup service [" + serviceName +
                            "], status: " + result.get(ModelService.RESPONSE_MESSAGE), module);
                    }
                } else {
                    String msg = ServiceUtil.getSuccessMessage(result);
                    if (msg != null && !msg.isEmpty()) {
                        Debug.logInfo("Ran startup service [" + serviceName +
                                "], status: " + result.get(ModelService.RESPONSE_MESSAGE) + ", message: " + msg, module);
                    } else {
                        Debug.logInfo("Ran startup service [" + serviceName +
                            "], status: " + result.get(ModelService.RESPONSE_MESSAGE), module);
                    }
                }
                return result;
            }
        } catch (GenericServiceException | GenericEntityException e) {
            Debug.logError("Error running startup service [" + serviceName + "]: " + e.toString(), module);
            return ServiceUtil.returnError(e.toString());
        }
    }
}
