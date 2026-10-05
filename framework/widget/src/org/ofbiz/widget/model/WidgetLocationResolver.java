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

import java.io.File;
import java.io.InputStream;
import java.net.URL;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;

/**
 * SCIPIO: 4.0.0: Unified widget location resolution with fallback logic.
 * Handles XML/annotation fallback, location aliases, and hashtag parsing.
 */
public class WidgetLocationResolver {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private WidgetLocationResolver() {}

    /**
     * Checks if a URL actually points to an existing resource.
     * FlexibleLocation.resolveLocation() can return URLs for non-existent files.
     */
    private static boolean urlExists(URL url) {
        if (url == null) {
            return false;
        }
        try {
            // For file:// URLs, check if file exists
            if ("file".equals(url.getProtocol())) {
                File file = new File(url.toURI());
                return file.exists() && file.isFile();
            }
            // For other protocols (jar://, http://, etc.), try to open stream
            try (InputStream is = url.openStream()) {
                return is != null;
            }
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Resolves a widget location with comprehensive fallback logic.
     *
     * @param resourceName The resource location (may include or exclude .xml extension)
     * @param widgetType Type hint for logging ("menu", "screen", "form", "tree")
     * @return Resolved URL or null if not found
     */
    public static URL resolveWidgetLocation(String resourceName, String widgetType) {
        try {
            // Step 1: Try exact location (FlexibleLocation now strips hashtags internally)
            URL url = FlexibleLocation.resolveLocation(resourceName);
            if (urlExists(url)) {
                return url;
            }

            // Step 2: Try XML/annotation fallback
            String fallbackResourceName = null;
            if (resourceName.endsWith(".xml")) {
                // Try without .xml extension (annotation-based)
                fallbackResourceName = resourceName.substring(0, resourceName.length() - 4);
            } else {
                // Try with .xml extension (XML file)
                fallbackResourceName = resourceName + ".xml";
            }

            url = FlexibleLocation.resolveLocation(fallbackResourceName);
            if (urlExists(url)) {
                Debug.logInfo(widgetType + " location [" + resourceName + "] not found, using fallback [" +
                    fallbackResourceName + "]", module);
                return url;
            }

            // Step 3: Return null - caller should check location aliases
            return null;

        } catch (Exception e) {
            Debug.logWarning(e, "Error resolving " + widgetType + " location: " + resourceName, module);
            return null;
        }
    }

    /**
     * Extracts widget name from combined location#name format.
     */
    public static String extractWidgetName(String combinedOrName) {
        int hashIndex = combinedOrName.lastIndexOf('#');
        if (hashIndex > 0 && hashIndex + 1 < combinedOrName.length()) {
            return combinedOrName.substring(hashIndex + 1);
        }
        return combinedOrName;
    }

    /**
     * Extracts resource location from combined location#name format.
     */
    public static String extractResourceLocation(String combinedOrLocation) {
        int hashIndex = combinedOrLocation.lastIndexOf('#');
        if (hashIndex > 0) {
            return combinedOrLocation.substring(0, hashIndex);
        }
        return combinedOrLocation;
    }
}
