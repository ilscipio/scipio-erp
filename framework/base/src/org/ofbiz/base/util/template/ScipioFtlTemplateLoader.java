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
package org.ofbiz.base.util.template;

import freemarker.cache.MultiTemplateLoader;
import freemarker.cache.TemplateLoader;
import freemarker.cache.URLTemplateLoader;
import freemarker.template.Configuration;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Loads inline templates (as strings), and the rest as URLs; based on {@link freemarker.cache.StringTemplateLoader} and FreeMarkerWorker.FlexibleTemplateLoader (SCIPIO).
 * NOTE: This is ideally a singleton to simplify access, but it's not enforced yet, in case.
 * Thread-safe but not all operations guaranteed to happen atomically.
 */
public class ScipioFtlTemplateLoader extends URLTemplateLoader {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected static final Collection<String> MEMORY_PROTOCOL_NAMES = UtilMisc.unmodifiableArrayList("delegator", "inline", "widget");
    protected static final ScipioFtlTemplateLoader INSTANCE = loadInstance();

    /** The backend "cache" of in-memory scripts, same as StringTemplateLoader
     * TODO: REVIEW: 2020-03-30: Currently FreeMarkerWorker is still not using this because for string templates, using this created too much redundancy in memory,
     *  but likely will be needed/useful at some point. NOTE: It's possible this should be a UtilCache depending on the use (currently caller has to clear entries). */
    protected final Map<String, ScipioFtlTemplateSource> templates = new ConcurrentHashMap<>();

    /** Currently always returns the shared instance, usually good enough. NOTE: With a singleton, we don't even have to go through the Freemarker API... */
    public static ScipioFtlTemplateLoader getInstanceForNewConfig(Configuration newConfig) {
        return getInstance();
    }

    /** Gets the configured instance, expected to match the default instance unless extended. Not expected to return null unless misconfiguration. */
    public static ScipioFtlTemplateLoader getInstance(Configuration config) {
        TemplateLoader loader = config.getTemplateLoader();
        if (loader instanceof MultiTemplateLoader) {
            loader = FreeMarkerWorker.getTemplateLoader((MultiTemplateLoader) loader, ScipioFtlTemplateLoader.class);
        }
        return (ScipioFtlTemplateLoader) loader;
    }

    /** Returns the shared instance, usually good enough. */
    public static ScipioFtlTemplateLoader getInstance() {
        return INSTANCE;
    }

    private static ScipioFtlTemplateLoader loadInstance() {
        // TODO: make configurable for subclassing
        return new ScipioFtlTemplateLoader();
    }

    public Collection<String> getMemoryProtocolNames() {
        return MEMORY_PROTOCOL_NAMES;
    }

    public boolean isMemoryTemplate(String templateName) {
        return MEMORY_PROTOCOL_NAMES.contains(getProtocolName(templateName));
    }

    /** NOTE: This is bastardized to the inverse of {@link #isMemoryTemplate(String)} for the time being, but it's not really correct. */
    public boolean isUrlTemplate(String templateName) {
        String protocol = getProtocolName(templateName);
        return (protocol.length() > 0) && !MEMORY_PROTOCOL_NAMES.contains(protocol);
    }

    /** Returns name of a protocol concerned by this class extracted from the passed template name, or empty string if cannot be determined or unknown;
     * may return invalid (non-existent) protocols.
     * The template name may be a URL or something such as "delegator:[template name]" or "inline:[inline template]".
     * NOTE: This does not recognize unknown or long protocol names; this is to avoid parsing very long template names repeatedly, which may be entire scripts. */
    public String getProtocolName(String templateName) {
        int sep = templateName.indexOf(':');
        return (sep > 0) ? templateName.substring(0, sep) : "";
    }

    protected Map<String, ScipioFtlTemplateSource> getTemplates() {
        return templates;
    }

    /** URLTemplateLoader override. */
    @Override
    protected URL getURL(String name) { //
        if (name == null || isMemoryTemplate(name)) {
            return null; // this is a template stored in the database - or in memory for cached scripts (SCIPIO)
        }
        URL locationUrl = null;
        try {
            locationUrl = FlexibleLocation.resolveLocation(name);
        } catch (Exception e) {
            Debug.logWarning("Unable to locate the template: " + name, module);
        }
        // TODO: REVIEW: It's unclear if it's 100% required to satisfy API to do this I/O lookup, but something else can be left to throw an IOException...
        //  for example the URLTemplateLoader.findTemplateSource calls url.openConnection() right after this, and this method is specific to URLTemplateLoader, so
        //  I see no reason to worry.
        //return (locationUrl != null) && new File(locationUrl.getFile()).exists() ? locationUrl : null;
        return locationUrl;
    }

    @Override
    public Object findTemplateSource(String name) throws IOException {
        Object templateSource = getTemplates().get(name);
        return (templateSource != null) ? templateSource : super.findTemplateSource(name);
    }

    @Override
    public long getLastModified(Object templateSource) {
        if (templateSource instanceof ScipioFtlTemplateSource) {
            return ((ScipioFtlTemplateSource) templateSource).getLastModified();
        } else {
            return super.getLastModified(templateSource);
        }
    }

    @Override
    public Reader getReader(Object templateSource, String encoding) throws IOException {
        if (templateSource instanceof ScipioFtlTemplateSource) {
            return new StringReader(((ScipioFtlTemplateSource) templateSource).getTemplateContent()); // ignores encoding
        } else {
            return super.getReader(templateSource, encoding);
        }
    }

    @Override
    public void closeTemplateSource(Object templateSource) throws IOException {
        if (templateSource instanceof ScipioFtlTemplateSource) {
            getTemplates().remove(((ScipioFtlTemplateSource) templateSource).getName());
        } else {
            super.closeTemplateSource(templateSource);
        }
    }

    public void putTemplate(String name, String templateContent) {
        putTemplate(name, templateContent, System.currentTimeMillis());
    }

    public void putTemplate(String name, String templateContent, long lastModified) {
        getTemplates().put(name, makeTemplateSource(name, templateContent, lastModified));
    }

    public boolean removeTemplate(String name) {
        return (getTemplates().remove(name) != null);
    }

    /** Makes a ScipioFtlTemplateSource, without (yet) adding it to {@link #templates}. */
    protected ScipioFtlTemplateSource makeTemplateSource(String name, String templateContent, long lastModified) {
        return new ScipioFtlTemplateSource(name, templateContent, lastModified);
    }

    protected static class ScipioFtlTemplateSource {
        protected final String name;
        /** Template content, for in-memory templates only (whose names begin with "delegator:", "inline:", ...). */
        protected final String templateContent;
        protected final long lastModified;
        // TODO: REVIEW: We currently cannot wrap the URLTemplateSource given to us by URLTemplateLoader
        //  because URL.openConnection is immediately called for all its instances, and it wouldn't be
        //  wise for us to be responsible for holding a reference to that when there's no guarantee
        //  of ejections in a timely manner.
        //protected final Object urlTemplateSource;

        protected ScipioFtlTemplateSource(String name, String templateContent, long lastModified) {
            if (name == null) {
                throw new IllegalArgumentException("name == null");
            }
            if (templateContent == null) {
                throw new IllegalArgumentException("source == null");
            }
            if (lastModified < -1L) {
                throw new IllegalArgumentException("lastModified < -1L");
            }
            this.name = name;
            this.templateContent = templateContent;
            this.lastModified = lastModified;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + name.hashCode();
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            } else if (obj == null) {
                return false;
            } else if (getClass() != obj.getClass()) {
                return false;
            }
            ScipioFtlTemplateSource other = (ScipioFtlTemplateSource) obj;
            return name.equals(other.name);
        }

        @Override
        public String toString() {
            return name;
        }

        public String getName() {
            return name;
        }

        public String getTemplateContent() {
            return templateContent;
        }

        public long getLastModified() {
            return lastModified;
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(getClass().getName());
        sb.append("(Map { ");
        int cnt = 0;
        for (String name : getTemplates().keySet()) {
            cnt++;
            if (cnt != 1) {
                sb.append(", ");
            }
            if (cnt > 10) {
                sb.append("...");
                break;
            }
            sb.append(freemarker.template.utility.StringUtil.jQuote(name));
            sb.append("=...");
        }
        if (cnt != 0) {
            sb.append(' ');
        }
        sb.append("})");
        return sb.toString();
    }

    /** Originally from FreeMarkerWorker, this is still needed only to bypass FTL cache. */
    public static Reader makeTemplateReader(String templateLocation) throws IOException {
        if (UtilValidate.isEmpty(templateLocation)) {
            throw new IllegalArgumentException("FreeMarker template location null or empty");
        }

        URL locationUrl = null;
        try {
            locationUrl = FlexibleLocation.resolveLocation(templateLocation);
        } catch (MalformedURLException e) {
            throw new IllegalArgumentException(e.getMessage());
        }
        if (locationUrl == null) {
            throw new IllegalArgumentException("FreeMarker file not found at location: " + templateLocation);
        }

        InputStream locationIs = locationUrl.openStream();
        Reader templateReader = new InputStreamReader(locationIs);

        String locationProtocol = locationUrl.getProtocol();
        if ("file".equals(locationProtocol) && Debug.verboseOn()) {
            String locationFile = locationUrl.getFile();
            int lastSlash = locationFile.lastIndexOf('/');
            String locationDir = locationFile.substring(0, lastSlash);
            String filename = locationFile.substring(lastSlash + 1);
            Debug.logVerbose("FreeMarker render: filename=" + filename + ", locationDir=" + locationDir, module);
        }

        return templateReader;
    }
}
