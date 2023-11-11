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
package org.ofbiz.entityext.eca;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;

import com.ilscipio.scipio.ce.base.component.ComponentReflectInfo;
import com.ilscipio.scipio.ce.base.component.ComponentReflectRegistry;
import com.ilscipio.scipio.service.def.Service;
import com.ilscipio.scipio.service.def.eeca.Eeca;
import com.ilscipio.scipio.service.def.eeca.EecaList;
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.concurrent.ExecutionPool;
import org.ofbiz.base.config.GenericConfigException;
import org.ofbiz.base.config.MainResourceHandler;
import org.ofbiz.base.config.ResourceHandler;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilTimer;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityConfException;
import org.ofbiz.entity.config.model.DelegatorElement;
import org.ofbiz.entity.config.model.EntityConfig;
import org.ofbiz.entity.config.model.EntityEcaReader;
import org.ofbiz.entity.config.model.Resource;
import org.w3c.dom.Element;

/**
 * EntityEcaUtil
 * <p>SCIPIO: 2.1.0: Minor fixes, switch to ArrayList.</p>
 */
public final class EntityEcaUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final UtilCache<String, Map<String, Map<String, List<EntityEcaRule>>>> entityEcaReaders = UtilCache.createUtilCache("entity.EcaReaders", 0, 0, false);

    private EntityEcaUtil () {}

    public static Map<String, Map<String, List<EntityEcaRule>>> getEntityEcaCache(String entityEcaReaderName) {
        Map<String, Map<String, List<EntityEcaRule>>> ecaCache = entityEcaReaders.get(entityEcaReaderName);
        if (ecaCache == null) {
            // FIXME: Collections are not thread safe
            ecaCache = new HashMap<>();
            readConfig(entityEcaReaderName, ecaCache);
            ecaCache = entityEcaReaders.putIfAbsentAndGet(entityEcaReaderName, ecaCache);
        }
        return ecaCache;
    }

    public static String getEntityEcaReaderName(String delegatorName) {
        DelegatorElement delegatorInfo = null;
        try {
            delegatorInfo = EntityConfig.getInstance().getDelegator(delegatorName);
        } catch (GenericEntityConfException e) {
            Debug.logWarning(e, "Exception thrown while getting field type config: ", module);
        }
        if (delegatorInfo == null) {
            Debug.logError("BAD ERROR: Could not find delegator config with name: " + delegatorName, module);
            return null;
        }
        return delegatorInfo.getEntityEcaReader();
    }

    private static void readConfig(String entityEcaReaderName, Map<String, Map<String, List<EntityEcaRule>>> ecaCache) {
        EntityEcaReader entityEcaReaderInfo = null;
        try {
            entityEcaReaderInfo = EntityConfig.getInstance().getEntityEcaReader(entityEcaReaderName);
        } catch (GenericEntityConfException e) {
            Debug.logError(e, "Exception thrown while getting entity-eca-reader config with name: " + entityEcaReaderName, module);
        }
        if (entityEcaReaderInfo == null) {
            Debug.logError("BAD ERROR: Could not find entity-eca-reader config with name: " + entityEcaReaderName, module);
            return;
        }

        List<Future<List<EntityEcaRule>>> futures = new ArrayList<Future<List<EntityEcaRule>>>();
        for (Resource eecaResourceElement : entityEcaReaderInfo.getResourceList()) {
            ResourceHandler handler = new MainResourceHandler(EntityConfig.ENTITY_ENGINE_XML_FILENAME, eecaResourceElement.getLoader(), eecaResourceElement.getLocation());
            futures.add(ExecutionPool.GLOBAL_FORK_JOIN.submit(createEcaLoaderCallable(handler)));
        }

        // get all of the component resource eca stuff, ie specified in each scipio-component.xml file
        for (ComponentConfig.EntityResourceInfo componentResourceInfo: ComponentConfig.getAllEntityResourceInfos("eca")) {
            if (entityEcaReaderName.equals(componentResourceInfo.readerName)) {
                futures.add(ExecutionPool.GLOBAL_FORK_JOIN.submit(createEcaLoaderCallable(componentResourceInfo.createResourceHandler())));
            }
        }

        // SCIPIO: 3.0.0: Handle annotation definitions
        for (ComponentReflectInfo cri : ComponentReflectRegistry.getReflectInfos()) {
            futures.add(ExecutionPool.GLOBAL_FORK_JOIN.submit(createEcaLoaderCallable(cri)));
        }

        for (List<EntityEcaRule> oneFileRules: ExecutionPool.getAllFutures(futures)) {
            for (EntityEcaRule rule: oneFileRules) {
                String entityName = rule.getEntityName();
                String eventName = rule.getEventName();
                Map<String, List<EntityEcaRule>> eventMap = ecaCache.get(entityName);
                List<EntityEcaRule> rules = null;
                if (eventMap == null) {
                    eventMap = new HashMap<String, List<EntityEcaRule>>();
                    rules = new ArrayList<>(); // SCIPIO: ArrayList
                    ecaCache.put(entityName, eventMap);
                    eventMap.put(eventName, rules);
                } else {
                    rules = eventMap.get(eventName);
                    if (rules == null) {
                        rules = new ArrayList<>();
                        eventMap.put(eventName, rules);
                    }
                }
                //remove the old rule if found and keep the recent one
                //This will prevent duplicate rule execution along with enabled/disabled eca workflow
                if (rules.remove(rule)) {
                    Debug.logWarning("Duplicate Entity ECA [" + entityName + "] for operation [ " + rule.getOperationName() + "] on [" + eventName + "]", module);
                }
                rules.add(rule);
            }
        }
    }

    private static List<EntityEcaRule> getEcaDefinitions(ResourceHandler handler) {
        List<EntityEcaRule> rules = new ArrayList<EntityEcaRule>();
        Element rootElement = null;
        try {
            rootElement = handler.getDocument().getDocumentElement();
        } catch (GenericConfigException e) {
            Debug.logError(e, module);
            return rules;
        }
        for (Element e: UtilXml.childElementList(rootElement, "eca")) {
            //if (rule.checkInitConditions()) {
            rules.add(new EntityEcaRule(e));
        }
        try {
            Debug.logInfo("Loaded [" + rules.size() + "] Entity ECA definitions from " + handler.getFullLocation() + " in loader " + handler.getLoaderName(), module);
        } catch (GenericConfigException e) {
            Debug.logError(e, module);
        }
        return rules;
    }

    private static List<EntityEcaRule> getEcaDefinitions(ComponentReflectInfo reflectInfo) {
        UtilTimer utilTimer = new UtilTimer();
        utilTimer.timerString("Loading Entity ECA annotations for component [" + reflectInfo.getComponent().getGlobalName() + "]");
        List<EntityEcaRule> ecaRules = new ArrayList<>(); // SCIPIO: switched to ArrayList

        for (Class<?> serviceClass : reflectInfo.getReflectQuery().getAnnotatedClasses(List.of(Eeca.class, EecaList.class))) {
            Service serviceDef = serviceClass.getAnnotation(Service.class);
            EecaList eecaDefList = serviceClass.getAnnotation(EecaList.class);
            if (eecaDefList != null) {
                for (Eeca eecaDef : eecaDefList.value()) {
                    ecaRules.add(new EntityEcaRule(eecaDef, serviceDef, serviceClass, null));
                }
            } else {
                Eeca eecaDef = serviceClass.getAnnotation(Eeca.class);
                if (eecaDef != null) {
                    ecaRules.add(new EntityEcaRule(eecaDef, serviceDef, serviceClass, null));
                }
            }
        }

        for (Method serviceMethod : reflectInfo.getReflectQuery().getAnnotatedMethods(List.of(Eeca.class, EecaList.class))) {
            Service serviceDef = serviceMethod.getAnnotation(Service.class);
            EecaList eecaDefList = serviceMethod.getAnnotation(EecaList.class);
            if (eecaDefList != null) {
                for (Eeca eecaDef : eecaDefList.value()) {
                    ecaRules.add(new EntityEcaRule(eecaDef, serviceDef, null, serviceMethod));
                }
            } else {
                Eeca eecaDef = serviceMethod.getAnnotation(Eeca.class);
                if (eecaDef != null) {
                    ecaRules.add(new EntityEcaRule(eecaDef, serviceDef, null, serviceMethod));
                }
            }
        }

        utilTimer.timerString("Finished Entity ECA annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "] - Total Service ECAs: " + ecaRules.size() + " FINISHED");
        Debug.logInfo("Loaded [" + ecaRules.size() + "] Entity ECA Rules from Entity ECA annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "]", module);
        return ecaRules;
    }

    private static Callable<List<EntityEcaRule>> createEcaLoaderCallable(final ResourceHandler handler) {
        return new Callable<List<EntityEcaRule>>() {
            public List<EntityEcaRule> call() throws Exception {
                return getEcaDefinitions(handler);
            }
        };
    }

    /**
     * Creates Annotations-based loader.
     *
     * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
     */
    private static Callable<List<EntityEcaRule>> createEcaLoaderCallable(ComponentReflectInfo cri) {
        return new Callable<List<EntityEcaRule>>() {
            public List<EntityEcaRule> call() throws Exception {
                return getEcaDefinitions(cri);
            }
        };
    }

    public static Collection<EntityEcaRule> getEntityEcaRules(Delegator delegator, String entityName, String event) {
        Map<String, Map<String, List<EntityEcaRule>>> ecaCache = EntityEcaUtil.getEntityEcaCache(EntityEcaUtil.getEntityEcaReaderName(delegator.getDelegatorName()));
        Map<String, List<EntityEcaRule>> eventMap = ecaCache.get(entityName);
        if (eventMap != null) {
            if (event != null) {
                return eventMap.get(event);
            }
        }
        return null;
    }
}
