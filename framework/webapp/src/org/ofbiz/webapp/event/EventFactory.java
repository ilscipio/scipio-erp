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
package org.ofbiz.webapp.event;

import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletContext;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralRuntimeException;
import org.ofbiz.base.util.ObjectType;
import org.ofbiz.webapp.control.ConfigXMLReader;
import org.ofbiz.webapp.control.ConfigXMLReader.ControllerConfig;
import org.ofbiz.webapp.control.ConfigXMLReader.ControllerConfig.EventHandlerWrapperDef;
import org.ofbiz.webapp.control.ConfigXMLReader.Event;

/**
 * EventFactory - Event Handler Factory
 */
public class EventFactory {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private final Map<String, EventHandler> handlers;
    private final Map<String, List<EventHandlerWrapper>> wrappers; // SCIPIO: added 2018-11-23
    private final List<EventHandlerWrapper> allTriggerWrappers; // SCIPIO: added 2018-11-23

    public EventFactory(ServletContext context, URL controllerConfigURL) {
        // load all the event handlers
        try {
            ControllerConfig controllerConfig = ConfigXMLReader.getControllerConfig(controllerConfigURL);
            Map<String, EventHandler> handlers = new HashMap<>();
            handlers.put("none", NoneEventHandler.DEFAULT); // SCIPIO

            for (Map.Entry<String,String> handlerEntry: controllerConfig.getEventHandlerMap().entrySet()) {
                EventHandler handler = (EventHandler) ObjectType.getInstance(handlerEntry.getValue());
                handler.init(context);
                handlers.put(handlerEntry.getKey(), handler);
            }
            this.handlers = handlers;

            // SCIPIO: handler wrappers
            Map<String, List<EventHandlerWrapper>> wrappers = new HashMap<>();
            
            Set<String> allTriggerNames = new HashSet<>(Event.TRIGGERS);
            // First pass to collect all the known triggers
            for(EventHandlerWrapperDef def : controllerConfig.getEventHandlerWrapperMap().values()) {
                if (def.getTriggers() != null) {
                    allTriggerNames.addAll(def.getTriggers());
                }
            }
            ArrayList<EventHandlerWrapper> allTriggerWrappers = new ArrayList<>();
            for(EventHandlerWrapperDef def : controllerConfig.getEventHandlerWrapperMap().values()) {
                EventHandlerWrapper handler = (EventHandlerWrapper) ObjectType.getInstance(def.getClassName());
                handler.init(context);
                for(String trigger : (def.getTriggers() != null) ? def.getTriggers() : allTriggerNames) {
                    List<EventHandlerWrapper> list = wrappers.get(trigger);
                    if (list == null) {
                        list = new ArrayList<>();
                        wrappers.put(trigger, list);
                    }
                    list.add(handler);
                }
                if (def.getTriggers() == null) {
                    allTriggerWrappers.add(handler);
                }
            }
            for(List<EventHandlerWrapper> list : wrappers.values()) {
                ((ArrayList<EventHandlerWrapper>) list).trimToSize();
            }
            allTriggerWrappers.trimToSize();
            this.wrappers = wrappers;
            this.allTriggerWrappers = allTriggerWrappers;
        } catch (Exception e) {
            Debug.logError(e, module);
            throw new GeneralRuntimeException(e);
        }
    }

    public EventHandler getEventHandler(String type) throws EventHandlerException {
        EventHandler handler = handlers.get(type);
        if (handler == null) {
            throw new EventHandlerException("No handler found for type: " + type);
        }
        return handler;
    }

    public List<EventHandlerWrapper> getEventHandlerWrappersForTrigger(String trigger) { // SCIPIO
        List<EventHandlerWrapper> wrapperList = wrappers.get(trigger);
        return (wrapperList != null) ? wrapperList : allTriggerWrappers;
    }
}
