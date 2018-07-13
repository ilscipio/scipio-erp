/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.service.engine;

import java.io.StringReader;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.QName;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamReader;

import org.apache.axiom.om.OMAbstractFactory;
import org.apache.axiom.om.OMElement;
import org.apache.axiom.om.OMFactory;
import org.apache.axiom.om.OMXMLBuilderFactory;
import org.apache.axiom.om.OMXMLParserWrapper;
import org.apache.axis2.AxisFault;
import org.apache.axis2.addressing.EndpointReference;
import org.apache.axis2.client.Options;
import org.apache.axis2.client.ServiceClient;
import org.apache.axis2.context.ConfigurationContext;
import org.apache.axis2.context.ConfigurationContextFactory;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelParam;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceDispatcher;
import org.ofbiz.service.soap.SOAPClientConnectConfig;

/**
 * Generic Service SOAP Interface
 */
public final class SOAPClientEngine extends GenericAsyncEngine {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public SOAPClientEngine(ServiceDispatcher dispatcher) {
        super(dispatcher);
    }

    /**
     * @see org.ofbiz.service.engine.GenericEngine#runSyncIgnore(java.lang.String, org.ofbiz.service.ModelService, java.util.Map)
     */
    @Override
    public void runSyncIgnore(String localName, ModelService modelService, Map<String, Object> context) throws GenericServiceException {
        runSync(localName, modelService, context);
    }

    /**
     * @see org.ofbiz.service.engine.GenericEngine#runSync(java.lang.String, org.ofbiz.service.ModelService, java.util.Map)
     */
    @Override
    public Map<String, Object> runSync(String localName, ModelService modelService, Map<String, Object> context) throws GenericServiceException {
        Map<String, Object> result = serviceInvoker(modelService, context);

        if (result == null)
            throw new GenericServiceException("Service did not return expected result");
        return result;
    }

    // Invoke the remote SOAP service
    private Map<String, Object> serviceInvoker(ModelService modelService, Map<String, Object> context) throws GenericServiceException {
        Delegator delegator = dispatcher.getDelegator();
        if (modelService.location == null || modelService.invoke == null)
            throw new GenericServiceException("Cannot locate service to invoke");

        ServiceClient client = null;
        QName serviceName = null;
        String axis2Repo = "/framework/service/config/axis2";
        String axis2RepoLocation = System.getProperty("ofbiz.home") + axis2Repo;
        String axis2XmlFile = "/framework/service/config/axis2/conf/axis2.xml";
        String axis2XmlFileLocation = System.getProperty("ofbiz.home") + axis2XmlFile;

        try {
            ConfigurationContext configContext = ConfigurationContextFactory.createConfigurationContextFromFileSystem(axis2RepoLocation, axis2XmlFileLocation);
            client = new ServiceClient(configContext, null);
            Options options = new Options();
            configureSOAPHttpClient(client, options); // SCIPIO: new 2018-07-11
            EndpointReference endPoint = new EndpointReference(this.getLocation(modelService));
            options.setTo(endPoint);
            client.setOptions(options);
        } catch (AxisFault e) {
            throw new GenericServiceException("RPC service error", e);
        }

        List<ModelParam> inModelParamList = modelService.getInModelParamList();

        if (Debug.infoOn()) Debug.logInfo("[SOAPClientEngine.invoke] : Parameter length - " + inModelParamList.size(), module);

        if (UtilValidate.isNotEmpty(modelService.nameSpace)) {
            serviceName = new QName(modelService.nameSpace, modelService.invoke);
        } else {
            serviceName = new QName(modelService.invoke);
        }

        int i = 0;

        Map<String, Object> parameterMap = new HashMap<String, Object>();
        for (ModelParam p: inModelParamList) {
            if (Debug.infoOn()) Debug.logInfo("[SOAPClientEngine.invoke} : Parameter: " + p.name + " (" + p.mode + ") - " + i, module);

            // exclude params that ModelServiceReader insert into (internal params)
            if (!p.internal) {
                parameterMap.put(p.name, context.get(p.name));
            }
            i++;
        }

        OMElement parameterSer = null;

        try {
            String xmlParameters = SoapSerializer.serialize(parameterMap);
            XMLStreamReader reader = XMLInputFactory.newInstance().createXMLStreamReader(new StringReader(xmlParameters));
            OMXMLParserWrapper builder = OMXMLBuilderFactory.createStAXOMBuilder(reader);
            parameterSer = builder.getDocumentElement();
        } catch (Exception e) {
            Debug.logError(e, module);
        }

        Map<String, Object> results = null;
        try {
            OMFactory factory = OMAbstractFactory.getOMFactory();
            OMElement payload = factory.createOMElement(serviceName);
            payload.addChild(parameterSer.getFirstElement());
            OMElement respOMElement = client.sendReceive(payload);
            client.cleanupTransport();
            results = UtilGenerics.cast(SoapSerializer.deserialize(respOMElement.toString(), delegator));
        } catch (Exception e) {
            Debug.logError(e, module);
        }
        return results;
    }
    
    /**
     * SCIPIO: Uses the LOCAL definition of a service to invoke a SOAP service on a remote
     * Scipio instance. It is ASSUMED both servers are of same or compatible versions.
     * <p>
     * WARN: LOW-LEVEL implementation that bypasses transactions and most of the service engine!
     * This is only for implementations of higher-level utilities, and should likely not be used by client code.
     * <p>
     * Here, <code>modelService</code> is the LOCAL service definition, not a "soap" engine definition.
     * <p>
     * NOTE: This is not proper WSDL, but should work in typical deployment scenarios.
     * WARN: This bypasses service engine location aliasing.
     * <p>
     * Added 2018-03-15.
     */
    public static Map<String, Object> invokeRemoteMirrorService(LocalDispatcher dispatcher, ModelService modelService, 
            String remoteService, String remoteLocation, String remoteNamespace, Map<String, Object> context, Set<String> alwaysAllowParams, boolean alwaysThrowEx) throws Exception {
        if (remoteService == null) remoteService = modelService.name;
        if (alwaysAllowParams == null) alwaysAllowParams = Collections.emptySet();
        Delegator delegator = dispatcher.getDelegator();
        if (remoteLocation == null || remoteService == null)
            throw new GenericServiceException("Cannot locate service to invoke");

        ServiceClient client = null;
        QName serviceName = null;
        String axis2Repo = "/framework/service/config/axis2";
        String axis2RepoLocation = System.getProperty("ofbiz.home") + axis2Repo;
        String axis2XmlFile = "/framework/service/config/axis2/conf/axis2.xml";
        String axis2XmlFileLocation = System.getProperty("ofbiz.home") + axis2XmlFile;

        try {
            ConfigurationContext configContext = ConfigurationContextFactory.createConfigurationContextFromFileSystem(axis2RepoLocation, axis2XmlFileLocation);
            client = new ServiceClient(configContext, null);
            Options options = new Options();
            configureSOAPHttpClient(client, options); // SCIPIO: new 2018-07-11
            EndpointReference endPoint = new EndpointReference(remoteLocation);
            options.setTo(endPoint);
            client.setOptions(options);
        } catch (AxisFault e) {
            throw new GenericServiceException("RPC service error", e);
        }

        List<ModelParam> inModelParamList = modelService.getInModelParamList();

        if (Debug.infoOn()) Debug.logInfo("[SOAPClientEngine.invoke] : Parameter length - " + inModelParamList.size(), module);

        if (UtilValidate.isNotEmpty(remoteNamespace)) {
            serviceName = new QName(remoteNamespace, remoteService);
        } else {
            serviceName = new QName(remoteService);
        }

        int i = 0;

        Map<String, Object> parameterMap = new HashMap<String, Object>();
        for (ModelParam p: inModelParamList) {
            if (Debug.infoOn()) Debug.logInfo("[SOAPClientEngine.invoke} : Parameter: " + p.name + " (" + p.mode + ") - " + i, module);

            // exclude params that ModelServiceReader insert into (internal params)
            if (!p.internal || (alwaysAllowParams.contains(p.name) && context.containsKey(p.name))) {
                parameterMap.put(p.name, context.get(p.name));
            }
            i++;
        }

        OMElement parameterSer = null;

        try {
            String xmlParameters = SoapSerializer.serialize(parameterMap);
            XMLStreamReader reader = XMLInputFactory.newInstance().createXMLStreamReader(new StringReader(xmlParameters));
            OMXMLParserWrapper builder = OMXMLBuilderFactory.createStAXOMBuilder(reader);
            parameterSer = builder.getDocumentElement();
        } catch (Exception e) {
            if (alwaysThrowEx) throw e;
            else Debug.logError(e, module);
        }

        Map<String, Object> results = null;
        try {
            OMFactory factory = OMAbstractFactory.getOMFactory();
            OMElement payload = factory.createOMElement(serviceName);
            payload.addChild(parameterSer.getFirstElement());
            OMElement respOMElement = client.sendReceive(payload);
            client.cleanupTransport();
            results = UtilGenerics.cast(SoapSerializer.deserialize(respOMElement.toString(), delegator));
        } catch (Exception e) {
            if (alwaysThrowEx) throw e;
            else Debug.logError(e, module);
        }
        return results;
    }

    /**
     * SCIPIO: Configures custom HttpClient4 client and/or connection manager with HTTPS for SOAP (currently axis2 ~1.7.8),
     * based on service.properties soap.connect.* and soap.cert.validation.* configuration.
     * <p>
     * Added 2018-07-11.
     */
    public static void configureSOAPHttpClient(ServiceClient client, Options options) {
        SOAPClientConnectConfig.getDefaultInstance().configureSOAPHttpClient(client, options);
    }
}
