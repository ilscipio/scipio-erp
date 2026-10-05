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
package org.ofbiz.service.soap;

import java.net.MalformedURLException;

import org.apache.axis2.AxisFault;
import org.apache.axis2.context.ConfigurationContext;
import org.apache.axis2.context.ConfigurationContextFactory;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;

/**
 * SCIPIO: Handles SOAP Axis2 ConfigurationContext management and creation.
 * <p>
 * Addd 2018-07-13.
 */
public abstract class SOAPContextHandler {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // Default axis2 config locations and settings
    // NOTE: There could be more than one set of these in the future (addons, etc.).
    public static final String DEFAULT_REPO = "framework/service/config/axis2";
    public static final String DEFAULT_CFG_FILE = "framework/service/config/axis2/conf/axis2.xml";
    public static final boolean SHARED_CONTEXT = UtilProperties.getPropertyAsBoolean("service", "soap.context.shared", false);

    private static final SOAPContextHandler defaultHandler = createHandlerSafe();

    /**
     * Get the singleton default handler, e.g. used by the {@link org.ofbiz.service.engine.SOAPClientEngine}.
     */
    public static SOAPContextHandler getDefaultHandler() throws AxisFault {
        return defaultHandler;
    }

    /**
     * Create new handler with specific locations and whether sharing ConfigurationContext or not.
     */
    public static SOAPContextHandler createHandler(String axis2Repo, String axis2XmlFile, boolean sharedContext) throws AxisFault {
        if (sharedContext) {
            return new StaticContextHandler(
                    createConfigurationContext(expandLocation(axis2Repo), expandLocation(axis2XmlFile)));
        } else {
            return new SeparateContextHandler(expandLocation(axis2Repo), expandLocation(axis2XmlFile));
        }
    }

    /**
     * Create new handler from default locations and system settings.
     */
    public static SOAPContextHandler createHandler() throws AxisFault {
        return createHandler(DEFAULT_REPO, DEFAULT_CFG_FILE, SHARED_CONTEXT);
    }

    /**
     * Low-level method to re-wrap a ConfigurationContext in a static SOAPContextHandler.
     */
    public static SOAPContextHandler createHandler(ConfigurationContext configContext) throws AxisFault {
        return new StaticContextHandler(configContext);
    }

    public static SOAPContextHandler createHandlerSafe() {
        try {
            return createHandler();
        } catch (Exception e) {
            Debug.logError(e, "SOAP: Error creating ConfigurationContext instance or handler"
                    + " - non-recoverable - returning null (will crash!): " + e.getMessage(), module);
            return null;
        }
    }

    /**
     * Gets a new or shared axis2 ConfigurationContext.
     */
    public abstract ConfigurationContext getConfigurationContext() throws AxisFault;

    protected static ConfigurationContext createConfigurationContext(String axis2RepoLocation, String axis2XmlFileLocation) throws AxisFault {
        return ConfigurationContextFactory.createConfigurationContextFromFileSystem(axis2RepoLocation, axis2XmlFileLocation);
    }

    protected static String expandLocation(String path) {
        if (path.contains("://")) {
            try {
                return FlexibleLocation.resolveFileUrlAsPath(path);
            } catch (MalformedURLException e) {
                throw new IllegalArgumentException(e);
            } catch (IllegalArgumentException e) {
                throw new IllegalArgumentException(e);
            }
        } else {
            if (!path.startsWith("/")) path = "/" + path;
            return System.getProperty("ofbiz.home") + path;
        }
    }

    /**
     * Keeps a single ConfigurationContext for reuse between all users.
     */
    public static class StaticContextHandler extends SOAPContextHandler {
        private final ConfigurationContext configContext;

        protected StaticContextHandler(ConfigurationContext configContext) {
            this.configContext = configContext;
        }

        @Override
        public ConfigurationContext getConfigurationContext() throws AxisFault {
            return configContext;
        }
    }

    /**
     * Creates a new ConfigurationContext at each call (heavy) - legacy ofbiz behavior.
     */
    public static class SeparateContextHandler extends SOAPContextHandler {
        private final String axis2RepoLocation;
        private final String axis2XmlFileLocation;

        protected SeparateContextHandler(String axis2RepoLocation, String axis2XmlFileLocation) {
            this.axis2RepoLocation = axis2RepoLocation;
            this.axis2XmlFileLocation = axis2XmlFileLocation;
        }

        @Override
        public ConfigurationContext getConfigurationContext() throws AxisFault {
            return createConfigurationContext(axis2RepoLocation, axis2XmlFileLocation);
        }
    }
}
