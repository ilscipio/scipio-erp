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
/*
 * Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
 * under the GNU Affero General Public License, version 3, or a commercial
 * license from Ilscipio GmbH (file LICENSE). The original code stays under
 * the Apache License, version 2.0, as stated above.
 */
package org.ofbiz.service.mail;

import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.config.GenericConfigException;
import org.ofbiz.base.config.ResourceHandler;
import com.ilscipio.scipio.ce.base.component.ComponentReflectInfo;
import com.ilscipio.scipio.ce.base.component.ComponentReflectRegistry;
import com.ilscipio.scipio.service.def.mca.Mca;
import com.ilscipio.scipio.service.def.mca.McaAction;
import com.ilscipio.scipio.service.def.mca.McaCondition;
import com.ilscipio.scipio.service.def.mca.McaList;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public final class ServiceMcaUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final UtilCache<String, ServiceMcaRule> mcaCache = UtilCache.createUtilCache("service.ServiceMCAs", 0, 0, false);

    private ServiceMcaUtil() {}

    public static void reloadConfig() {
        mcaCache.clear();
        readConfig();
    }

    public static void readConfig() {
        // TODO: Missing in XSD file.

        // get all of the component resource eca stuff, ie specified in each scipio-component.xml file
        for (ComponentConfig.ServiceResourceInfo componentResourceInfo: ComponentConfig.getAllServiceResourceInfos("mca")) {
            addMcaDefinitions(componentResourceInfo.createResourceHandler());
        }

        // SCIPIO: 4.0.0: Handle annotation definitions
        for (ComponentReflectInfo cri : ComponentReflectRegistry.getReflectInfos()) {
            addMcaDefinitions(cri);
        }
    }

    /**
     * Reads the {@literal @}Mca rules of a component.
     *
     * <p>The rule classes stay DOM-driven, so each annotation is rendered back into an
     * {@code <mca>} element and handed to the same {@link ServiceMcaRule} constructor the XML
     * uses; there is then only one interpretation of a rule.</p>
     *
     * <p>SCIPIO: 4.0.0: Added for annotations support.</p>
     */
    public static void addMcaDefinitions(ComponentReflectInfo cri) {
        Collection<Class<?>> mcaClasses = cri.getReflectQuery().getAnnotatedClasses(List.of(Mca.class, McaList.class));
        if (UtilValidate.isEmpty(mcaClasses)) {
            return;
        }
        Document doc;
        try {
            doc = UtilXml.makeEmptyXmlDocument("service-mca");
        } catch (Exception e) {
            Debug.logError(e, "Could not create MCA document for component ["
                    + cri.getComponent().getGlobalName() + "]", module);
            return;
        }
        int numDefs = 0;
        for (Class<?> mcaClass : mcaClasses) {
            for (Mca mca : mcaClass.getAnnotationsByType(Mca.class)) {
                try {
                    if (UtilValidate.isEmpty(mca.name())) {
                        Debug.logError("@Mca on [" + mcaClass.getName() + "] has no name; skipped", module);
                        continue;
                    }
                    mcaCache.put(mca.name(), new ServiceMcaRule(buildMcaElement(doc, mca)));
                    numDefs++;
                } catch (Exception e) {
                    // Per-rule, so one bad rule cannot hide the rest of the component's rules.
                    Debug.logError(e, "Could not read @Mca [" + mca.name() + "] on ["
                            + mcaClass.getName() + "]", module);
                }
            }
        }
        if (numDefs > 0 && Debug.importantOn()) {
            Debug.logImportant("Loaded " + numDefs + " Service MCA definitions from annotations for component ["
                    + cri.getComponent().getGlobalName() + "]", module);
        }
    }

    /** SCIPIO: 4.0.0: Renders an {@literal @}Mca annotation as the {@code <mca>} element the rule reads. */
    private static Element buildMcaElement(Document doc, Mca mca) {
        Element mcaElement = doc.createElement("mca");
        mcaElement.setAttribute("mail-rule-name", mca.name());
        for (McaCondition condition : mca.conditions()) {
            Element condElement;
            if (UtilValidate.isNotEmpty(condition.serviceName())) {
                condElement = doc.createElement("condition-service");
                condElement.setAttribute("service-name", condition.serviceName());
            } else if (UtilValidate.isNotEmpty(condition.headerName())) {
                condElement = doc.createElement("condition-header");
                condElement.setAttribute("header-name", condition.headerName());
            } else {
                condElement = doc.createElement("condition-field");
                condElement.setAttribute("field-name", condition.fieldName());
            }
            if (UtilValidate.isNotEmpty(condition.operator())) {
                condElement.setAttribute("operator", condition.operator());
            }
            if (UtilValidate.isNotEmpty(condition.value())) {
                condElement.setAttribute("value", condition.value());
            }
            mcaElement.appendChild(condElement);
        }
        for (McaAction action : mca.actions()) {
            Element actionElement = doc.createElement("action");
            actionElement.setAttribute("service", action.service());
            if (UtilValidate.isNotEmpty(action.mode())) {
                actionElement.setAttribute("mode", action.mode());
            }
            if (UtilValidate.isNotEmpty(action.runAsUser())) {
                actionElement.setAttribute("run-as-user", action.runAsUser());
            }
            if (action.persist()) {
                actionElement.setAttribute("persist", "true");
            }
            mcaElement.appendChild(actionElement);
        }
        return mcaElement;
    }

    public static void addMcaDefinitions(ResourceHandler handler) {
        Element rootElement = null;
        try {
            rootElement = handler.getDocument().getDocumentElement();
        } catch (GenericConfigException e) {
            Debug.logError(e, module);
            return;
        }

        int numDefs = 0;
        for (Element e: UtilXml.childElementList(rootElement, "mca")) {
            String ruleName = e.getAttribute("mail-rule-name");
            mcaCache.put(ruleName, new ServiceMcaRule(e));
            numDefs++;
        }

        if (Debug.importantOn()) {
            String resourceLocation = handler.getLocation();
            try {
                resourceLocation = handler.getURL().toExternalForm();
            } catch (GenericConfigException e) {
                Debug.logError(e, "Could not get resource URL", module);
            }
            Debug.logImportant("Loaded " + numDefs + " Service MCA definitions from " + resourceLocation, module);
        }
    }

    public static Collection<ServiceMcaRule> getServiceMcaRules() {
    if (mcaCache.size() == 0) {
        readConfig();
    }
        return mcaCache.values();
    }

    public static void evalRules(LocalDispatcher dispatcher, MimeMessageWrapper wrapper, GenericValue userLogin) throws GenericServiceException {
        Set<String> actionsRun = new TreeSet<>();
        for (ServiceMcaRule rule: getServiceMcaRules()) {
            rule.eval(dispatcher, wrapper, actionsRun, userLogin);
        }
    }
}
