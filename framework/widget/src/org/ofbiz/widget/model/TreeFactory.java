/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package org.ofbiz.widget.model;

import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

import com.ilscipio.scipio.ce.base.component.ComponentReflectInfo;
import com.ilscipio.scipio.ce.base.component.ComponentReflectRegistry;
import com.ilscipio.scipio.widget.def.tree.Tree;
import com.ilscipio.scipio.widget.def.tree.TreeAnnotationReader;
import com.ilscipio.scipio.widget.def.tree.TreeList;


/**
 * Widget Library - Tree factory class
 * <p>
 * SCIPIO: now also as instance
 */
@SuppressWarnings("serial")
public class TreeFactory extends WidgetFactory {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final String CLASS_LOCATION_PREFIX = "class://";

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
        // SCIPIO: Handle class:// locations for annotation-based trees
        if (resourceName.startsWith(CLASS_LOCATION_PREFIX)) {
            return getTreeFromClassLocation(resourceName, treeName);
        }

        Map<String, ModelTree> modelTreeMap = treeLocationCache.get(resourceName);
        if (modelTreeMap == null) {
            // SCIPIO: refactored
            synchronized (TreeFactory.class) {
                modelTreeMap = treeLocationCache.get(resourceName);
                if (modelTreeMap == null) {
                    // SCIPIO: 4.0.0: Use unified WidgetLocationResolver for consistent fallback logic
                    URL treeFileUrl = WidgetLocationResolver.resolveWidgetLocation(resourceName, "tree");

                    if (treeFileUrl == null) {
                        // SCIPIO: 4.0.0: no XML at that location; an annotation class may declare the tree
                        // with this location (@Tree.location), which is what the converter writes
                        String classLocation = findAnnotationTreeClassLocation(resourceName, treeName);
                        if (classLocation != null) {
                            return getTreeFromClassLocation(classLocation, treeName);
                        }
                        throw new IllegalArgumentException("Could not resolve tree file location [" + resourceName + "]");
                    }
                    Document treeFileDoc = UtilXml.readXmlDocument(treeFileUrl, true, true);
                    if (treeFileDoc == null) {
                        throw new IllegalArgumentException("Could not read tree file at location [" + resourceName + "]");
                    }
                    // SCIPIO: New: Save original location as user data in Document
                    WidgetDocumentInfo.retrieveAlways(treeFileDoc).setResourceLocation(resourceName);
                    modelTreeMap = readTreeDocument(treeFileDoc, delegator, dispatcher, resourceName);
                    treeLocationCache.put(resourceName, modelTreeMap);
                }
            }
        }

        ModelTree modelTree = modelTreeMap.get(treeName);
        // SCIPIO: now done in non-*OrNull method
        //if (modelTree == null) {
        //    throw new IllegalArgumentException("Could not find tree with name [" + treeName + "] in class resource ["
        //            + resourceName + "]");
        //}
        return modelTree;
    }

    /**
     * SCIPIO: Gets a tree from a class:// location (annotation-based tree).
     *
     * <p>Location format: class://fully.qualified.ClassName#TreeName</p>
     */

    /** SCIPIO: 4.0.0: "location#name" of every annotation tree -> class:// location of its class. */
    private static volatile Map<String, String> annotationTreeLocations = null;

    /**
     * SCIPIO: 4.0.0: Finds the class:// location of the annotation class that declares the tree
     * [treeName] with the XML-style location [resourceName], or null.
     */
    protected static String findAnnotationTreeClassLocation(String resourceName, String treeName) {
        Map<String, String> index = annotationTreeLocations;
        if (index == null) {
            synchronized (TreeFactory.class) {
                index = annotationTreeLocations;
                if (index == null) {
                    Map<String, String> built = new HashMap<>();
                    for (ComponentReflectInfo cri : ComponentReflectRegistry.getReflectInfos()) {
                        try {
                            for (Class<?> cls : cri.getReflectQuery().getAnnotatedClasses(Tree.class)) {
                                registerAnnotationTree(built, cls, cls.getAnnotation(Tree.class));
                            }
                            for (Class<?> cls : cri.getReflectQuery().getAnnotatedClasses(TreeList.class)) {
                                TreeList list = cls.getAnnotation(TreeList.class);
                                if (list != null) {
                                    for (Tree tree : list.value()) {
                                        registerAnnotationTree(built, cls, tree);
                                    }
                                }
                            }
                        } catch (RuntimeException e) {
                            Debug.logWarning(e, "Could not index annotation trees of component [" + cri.getComponent().getGlobalName() + "]", module);
                        }
                    }
                    index = built;
                    annotationTreeLocations = built;
                }
            }
        }
        return index.get(resourceName + "#" + treeName);
    }

    private static void registerAnnotationTree(Map<String, String> index, Class<?> cls, Tree tree) {
        if (tree == null || tree.location().isEmpty() || tree.name().isEmpty()) {
            return;
        }
        index.put(tree.location() + "#" + tree.name(), CLASS_LOCATION_PREFIX + cls.getName() + "#" + tree.name());
    }

    protected static ModelTree getTreeFromClassLocation(String resourceName, String treeName) {
        String cacheKey = resourceName + "#" + treeName;
        Map<String, ModelTree> cachedMap = treeLocationCache.get(cacheKey);
        if (cachedMap != null) {
            return cachedMap.get(treeName);
        }

        synchronized (TreeFactory.class) {
            cachedMap = treeLocationCache.get(cacheKey);
            if (cachedMap != null) {
                return cachedMap.get(treeName);
            }

            try {
                // Parse class:// location
                String classRef = resourceName.substring(CLASS_LOCATION_PREFIX.length());
                String className;
                String annotationTreeName = treeName;

                int hashIndex = classRef.indexOf('#');
                if (hashIndex > 0) {
                    className = classRef.substring(0, hashIndex);
                    annotationTreeName = classRef.substring(hashIndex + 1);
                } else {
                    className = classRef;
                }

                // Load class using context classloader
                Class<?> treeClass = Thread.currentThread().getContextClassLoader().loadClass(className);

                // Check for @Tree or @TreeList annotations
                Tree treeDef = findTreeAnnotation(treeClass, annotationTreeName);
                if (treeDef == null) {
                    Debug.logError("Tree annotation not found: " + annotationTreeName + " in class " + className, module);
                    return null;
                }

                // Use TreeAnnotationReader to create synthetic XML
                TreeAnnotationReader reader = new TreeAnnotationReader();
                Document treeDoc = reader.readTreeDocument(treeClass, annotationTreeName);
                if (treeDoc == null) {
                    Debug.logError("Failed to read tree document from annotations: " + resourceName, module);
                    return null;
                }

                // Build ModelTree from synthetic XML
                Map<String, ModelTree> modelTreeMap = readTreeDocument(treeDoc, null, null, resourceName);
                treeLocationCache.put(cacheKey, modelTreeMap);

                return modelTreeMap.get(annotationTreeName);

            } catch (ClassNotFoundException e) {
                Debug.logError(e, "Could not find class for tree location: " + resourceName, module);
                return null;
            }
        }
    }

    /**
     * SCIPIO: Finds a Tree annotation by name in the given class.
     */
    protected static Tree findTreeAnnotation(Class<?> treeClass, String treeName) {
        // Check for single @Tree annotation
        Tree singleTree = treeClass.getAnnotation(Tree.class);
        if (singleTree != null && treeName.equals(singleTree.name())) {
            return singleTree;
        }

        // Check for @TreeList (multiple trees)
        TreeList treeList = treeClass.getAnnotation(TreeList.class);
        if (treeList != null) {
            for (Tree tree : treeList.value()) {
                if (treeName.equals(tree.name())) {
                    return tree;
                }
            }
        }

        // Check inner interfaces/classes
        for (Class<?> innerClass : treeClass.getDeclaredClasses()) {
            singleTree = innerClass.getAnnotation(Tree.class);
            if (singleTree != null && treeName.equals(singleTree.name())) {
                return singleTree;
            }
        }

        return null;
    }

    public static Map<String, ModelTree> readTreeDocument(Document treeFileDoc, Delegator delegator, LocalDispatcher dispatcher, String treeLocation) {
        Map<String, ModelTree> modelTreeMap = new HashMap<>();
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
    public ModelTree getWidgetFromLocation(ModelLocation modelLoc) throws IOException, IllegalArgumentException { // SCIPIO
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
    public ModelTree getWidgetFromLocationOrNull(ModelLocation modelLoc) throws IOException { // SCIPIO
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
