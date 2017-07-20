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
package org.ofbiz.base.location;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilURL;
import org.ofbiz.base.util.UtilValidate;

/**
 * A special location resolver that uses Strings like URLs, but with more options.
 *
 */

public final class FlexibleLocation {

    public static final String module = FlexibleLocation.class.getName();
    private static final Map<String, LocationResolver> locationResolvers;

    static {
        Map<String, LocationResolver> resolverMap = new HashMap<String, LocationResolver>(8);
        LocationResolver standardUrlResolver = new StandardUrlLocationResolver();
        resolverMap.put("http", standardUrlResolver);
        resolverMap.put("https", standardUrlResolver);
        resolverMap.put("ftp", standardUrlResolver);
        resolverMap.put("jar", standardUrlResolver);
        resolverMap.put("file", standardUrlResolver);
        resolverMap.put("classpath", new ClasspathLocationResolver());
        resolverMap.put("ofbizhome", new OFBizHomeLocationResolver());
        resolverMap.put("component", new ComponentLocationResolver());
        try {
            Properties properties = UtilProperties.getProperties("locationresolvers.properties");
            if (properties != null) {
                ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
                for (Entry<Object, Object> entry : properties.entrySet()) {
                    String locationType = (String) entry.getKey();
                    String locationResolverName = (String) entry.getValue();
                    Class<?> lClass = classLoader.loadClass(locationResolverName);
                    resolverMap.put(locationType, (LocationResolver) lClass.newInstance());
                }
            }
        } catch (Throwable e) {
            Debug.logWarning(e, "Error while loading resolvers from locationresolvers.properties: ", module);
        }
        locationResolvers = Collections.unmodifiableMap(resolverMap);
    }

    /**
     * Find the location type descriptor for the passed location String;
     * generally is all text before the first ":" character.
     * If no type descriptor is found, returns the specified default.
     * SCIPIO: 2017-06-15: Modified to overload to return the specified default.
     */
    private static String getLocationType(String location, String defaultValue) {
        int colonIndex = location.indexOf(":");
        if (colonIndex > 0) {
            return location.substring(0, colonIndex);
        } else {
            return defaultValue;
        }
    }
    
    /**
     * Find the location type descriptor for the passed location String;
     * generally is all text before the first ":" character.
     * If no type descriptor is found, defaults to "classpath".
     * SCIPIO: This is the original getLocationType behavior.
     */
    private static String getLocationType(String location) {
        return getLocationType(location, "classpath");
    }

    /**
     * Resolves the gives location into a URL object for use in various ways.
     *
     * The general format of the location is like a URL: {locationType}://location/path/file.ext
     *
     * Supports standard locationTypes like http, https, ftp, jar, & file
     * Supports a classpath location type for when desired to be used like any other URL
     * Supports OFBiz specific location types like ofbizhome and component
     * Supports additional locationTypes specified in the locationresolvers.properties file
     *
     * @param location The location String to parse and create a URL from
     * @return URL object corresponding to the location String passed in
     * @throws MalformedURLException
     */
    public static URL resolveLocation(String location) throws MalformedURLException {
        return resolveLocation(location, null);
    }

    public static URL resolveLocation(String location, ClassLoader loader) throws MalformedURLException {
        if (UtilValidate.isEmpty(location)) {
            return null;
        }
        String locationType = getLocationType(location);
        LocationResolver resolver = locationResolvers.get(locationType);
        if (resolver != null) {
            if (loader != null && resolver instanceof ClasspathLocationResolver) {
                ClasspathLocationResolver cplResolver = (ClasspathLocationResolver) resolver;
                return cplResolver.resolveLocation(location, loader);
            } else {
                return resolver.resolveLocation(location);
            }
        } else {
            throw new MalformedURLException("Could not find a LocationResolver for the location type: " + locationType);
        }
    }
    
    public static String stripLocationType(String location) {
        if (UtilValidate.isEmpty(location)) {
            return "";
        }
        StringBuilder strippedSoFar = new StringBuilder(location);
        // first take care of the colon and everything before it
        int colonIndex = strippedSoFar.indexOf(":");
        if (colonIndex == 0) {
            strippedSoFar.deleteCharAt(0);
        } else if (colonIndex > 0) {
            strippedSoFar.delete(0, colonIndex + 1);
        }
        // now remove any extra forward slashes, ie as long as the first two are forward slashes remove the first one
        while (strippedSoFar.charAt(0) == '/' && strippedSoFar.charAt(1) == '/') {
            strippedSoFar.deleteCharAt(0);
        }
        return strippedSoFar.toString();
    }

    /**
     * SCIPIO: Checks if the given protocol name is among the registered protocol names having resolvers.
     * Added 2017-06-15.
     */
    public static boolean isRegisteredProtocol(String protocolName) {
        return locationResolvers.containsKey(protocolName);
    }
    
    /**
     * SCIPIO: Checks if the given location starts with a protocol that is registered.
     * NOTE: Does not guarantee that the location is a valid URL, nor that the location
     * is in fact 
     * Added 2017-06-15.
     */
    public static boolean isRegisteredProtocolLocation(String location) {
        if (UtilValidate.isEmpty(location)) return false;
        int colonIndex = location.indexOf(":");
        return (colonIndex > 0) && locationResolvers.containsKey(location.substring(0, colonIndex));
    }
    
    /**
     * SCIPIO: Checks if the given location is intended to be a URL (having a protocol prefix), which may be a candidate
     * for the {@link #resolveLocation} methods.
     * <p>
     * Currently, this returns true if and only if either:
     * 1) the location starts with a registered protocol name in lowercase followed by a colon, or
     * 2) the location contains a colon followed by two front slashes.
     * In all other locations, this returns false to indicate maybe a file location was sought.
     * <p>
     * NOTE: This is a best-effort heuristic intended to cope with user input and 
     * automatically try to determine the most likely intended format;
     * but it does not guarantee that the location could not have been a simple file name or path.
     * It is case-sensitive and will only recognize lowercase protocol prefix.
     * <p>
     * Added 2017-06-15.
     */
    public static boolean isUrlLocation(String location) {
        if (UtilValidate.isEmpty(location)) return false; // if empty, resolveLocation always returns null
        String type = getLocationType(location, null);
        if (type == null) return false; 
        if (isRegisteredProtocol(type)) return true;
        if (location.indexOf("//", type.length() + 1) >= 0) return true;
        return false;
    }
    
    /**
     * SCIPIO: Resolves the given location as a URL (having a protocol prefix) or as a filename.
     * If <code>isUrl</code> is true, treats as URL; if false, treats as filename; if null, automatically
     * tries to determined using the {@link #isUrlLocation} heuristic.
     * Added 2017-06-15.
     */
    public static URL resolveLocationAsUrlOrFilename(String location, Boolean isUrl) throws MalformedURLException {
        if (isUrl == Boolean.TRUE) return resolveLocation(location);
        else if (isUrl == Boolean.FALSE) return UtilURL.fromFilename(location);
        else return isUrlLocation(location) ? resolveLocation(location) : UtilURL.fromFilename(location);
    }
    
    /**
     * SCIPIO: Resolves the given location as a URL (having a protocol prefix) or as a filename.
     * Tries to determined using the {@link #isUrlLocation} heuristic.
     * Added 2017-06-15.
     */
    public static URL resolveLocationAsUrlOrFilename(String location) throws MalformedURLException {
        return isUrlLocation(location) ? resolveLocation(location) : UtilURL.fromFilename(location);
    }
    
    /**
     * SCIPIO: Resolves the url parameter interpreting it as a filesystem location ("file://", "component://"
     * and any other that resolves to the local filesystem) and returns its absolute file path.
     * Added 2017-07-14.
     * @throws MalformedURLException if the url is malformed, of unknown type or contains an invalid file path format
     * @throws IllegalArgumentException if the url does not map to a local filesystem location
     */
    public static String resolveFileUrlAsPath(String url) throws MalformedURLException, IllegalArgumentException {
        URL urlInst;
        try {
            urlInst = FlexibleLocation.resolveLocation(url); // here "component://" gets turned into "file://" (example)
        } catch(MalformedURLException e) {
            throw new MalformedURLException("The specified url '" + url + "' is invalid: " + e.getMessage());
        }
        if ("file".equalsIgnoreCase(urlInst.getProtocol())) {
            try {
                return new File(urlInst.toURI()).getPath();
            } catch (Exception e) { // URISyntaxException (toURI) + IllegalArgumentException (File)
                throw new MalformedURLException("The specified local filesystem url '" + url
                        + "' could not be converted to a file path: " + e.getMessage()
                        + " (" + e.getClass().getName() + ")");
            }
        } else throw new IllegalArgumentException("The specified url '" + url
                + "' does not designate or resolve to a local filesystem location (file://, component://, ...)");
    }
    
    /**
     * SCIPIO: If the given value parameter is a url, resolves the value parameter as url 
     * interpreting it as a filesystem location ("file://", "component://" and any other that resolves 
     * to the local filesystem) and returns its absolute file path; if not a url, returns the default value.
     * Added 2017-07-14.
     * @throws MalformedURLException if the url is malformed, of unknown type or contains an invalid file path format
     * @throws IllegalArgumentException if the url does not map to a local filesystem location
     */
    public static String resolveFileUrlAsPathIfUrl(String value, String defaultValue) throws MalformedURLException, IllegalArgumentException {
        return FlexibleLocation.isUrlLocation(value) ? resolveFileUrlAsPath(value) : defaultValue;
    }
    
    private FlexibleLocation() {}
}
