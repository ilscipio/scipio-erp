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
package org.ofbiz.widget.model;

import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;


/**
 * Widget Library - Tree factory class
 * <p>
 * SCIPIO: now also as instance
 */
@SuppressWarnings("serial")
public class TreeFactory extends WidgetFactory {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final UtilCache<String, Map<String, ModelTree>> treeLocationCache = UtilCache.createUtilCache("widget.tree.locationResource", 0, 0, false);

    public static TreeFactory getTreeFactory() { // SCIPIO: new
        return treeFactory;
    }
    
    /**
     * Gets widget from location or exception. 
     * <p>
     * SCIPIO: now delegating.
     */
    public static ModelTree getTreeFromLocation(String resourceName, String treeName, Delegator delegator, LocalDispatcher dispatcher)
            throws IOException, SAXException, ParserConfigurationException {
        ModelTree modelTree = getTreeFromLocationOrNull(resourceName, treeName, delegator, dispatcher);
        if (modelTree == null) {
            throw new IllegalArgumentException("Could not find tree with name [" + treeName + "] in class resource [" + resourceName + "]");
        }
        return modelTree;
    }
    
    /**
     * SCIPIO: Gets widget from location or null if name not within the location.
     */
    public static ModelTree getTreeFromLocationOrNull(String resourceName, String treeName, Delegator delegator, LocalDispatcher dispatcher)
            throws IOException, SAXException, ParserConfigurationException {
        Map<String, ModelTree> modelTreeMap = treeLocationCache.get(resourceName);
        if (modelTreeMap == null) {
            synchronized (TreeFactory.class) {
                modelTreeMap = treeLocationCache.get(resourceName);
                if (modelTreeMap == null) {
                    ClassLoader loader = Thread.currentThread().getContextClassLoader();
                    if (loader == null) {
                        loader = TreeFactory.class.getClassLoader();
                    }

                    URL treeFileUrl = null;
                    treeFileUrl = FlexibleLocation.resolveLocation(resourceName); //, loader);
                    Document treeFileDoc = UtilXml.readXmlDocument(treeFileUrl, true, true);
                    // SCIPIO: New: Save original location as user data in Document
                    if (treeFileDoc != null) {
                        WidgetDocumentInfo.retrieveAlways(treeFileDoc).setResourceLocation(resourceName); 
                    }
                    modelTreeMap = readTreeDocument(treeFileDoc, delegator, dispatcher, resourceName);
                    treeLocationCache.put(resourceName, modelTreeMap);
                }
            }
        }

        ModelTree modelTree = modelTreeMap.get(treeName);
        return modelTree;
    }

    public static Map<String, ModelTree> readTreeDocument(Document treeFileDoc, Delegator delegator, LocalDispatcher dispatcher, String treeLocation) {
        Map<String, ModelTree> modelTreeMap = new HashMap<String, ModelTree>();
        if (treeFileDoc != null) {
            // read document and construct ModelTree for each tree element
            Element rootElement = treeFileDoc.getDocumentElement();
            for (Element treeElement: UtilXml.childElementList(rootElement, "tree")) {
                ModelTree modelTree = new ModelTree(treeElement, treeLocation);
                modelTreeMap.put(modelTree.getName(), modelTree);
            }
        }
        return modelTreeMap;
    }

    @Override
    public ModelTree getWidgetFromLocation(ModelLocation modelLoc) throws IOException, IllegalArgumentException {
        try {
            DispatchContext dctx = getDefaultDispatchContext();
            return getTreeFromLocation(modelLoc.getResource(), modelLoc.getName(), 
                    dctx.getDelegator(), dctx.getDispatcher());
        } catch (SAXException e) {
            throw new IOException(e);
        } catch (ParserConfigurationException e) {
            throw new IOException(e);
        }
    }

    @Override
    public ModelTree getWidgetFromLocationOrNull(ModelLocation modelLoc) throws IOException {
        try {
            DispatchContext dctx = getDefaultDispatchContext();
            return getTreeFromLocationOrNull(modelLoc.getResource(), modelLoc.getName(), 
                    dctx.getDelegator(), dctx.getDispatcher());
        } catch (SAXException e) {
            throw new IOException(e);
        } catch (ParserConfigurationException e) {
            throw new IOException(e);
        }
    }
}
