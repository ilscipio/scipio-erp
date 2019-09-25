package org.ofbiz.catalina.container;

import org.apache.catalina.connector.Connector;
import org.ofbiz.base.container.ContainerConfig;
import org.ofbiz.base.util.Debug;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * SCIPIO: A small information class that reflects the configuration found in framework/catalina/scipio-component.xml, initialized
 * by CatalinaContainer, for the "catalina-container" container (considered the default). NOTE: the others may or may not be present
 * here, it depends if they get loaded or not.
 * <p>
 * This is a convenience class for access of some specific info without needed the tomcat or servlet API.
 * Basically abstracts the apache API.
 * <p>
 * TODO: More fields, _or_ remove this class in favor of something else...
 * TODO: in future can support "catalina-container-test" and other containers... not important for now.
 * <p>
 * Added 2019-09-25/2.1.0 (for solr).
 */
public class ScipioConnectorInfo {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String DEFAULT_CONTAINER = "catalina-container";
    public static final String HTTP_CONNECTOR = "http-connector";
    public static final String HTTPS_CONNECTOR = "https-connector";

    private static volatile Map<String, Map<String, ScipioConnectorInfo>> connectorInfoByName = Collections.emptyMap();

    private final ContainerConfig.Container.Property properties;
    //private final Connector connector; // TODO: REVIEW: unclear if a static reference to Tomcat internals could lead to issues or not...
    private final String name;
    private final int port;
    private final String scheme;

    protected ScipioConnectorInfo(ContainerConfig.Container.Property properties, Connector connector) {
        this.properties = properties;
        this.name = properties.name;
        this.port = connector.getPort();
        this.scheme = connector.getScheme();
    }

    protected static void registerConnectors(String containerName, Collection<ScipioConnectorInfo> connectorInfoList) {
        Map<String, ScipioConnectorInfo> nameMap = new LinkedHashMap<>();
        if (connectorInfoList != null) {
            for(ScipioConnectorInfo connectorInfo : connectorInfoList) {
                nameMap.put(connectorInfo.getName(), connectorInfo);
            }
        }
        synchronized(ScipioConnectorInfo.class) {
            Map<String, Map<String, ScipioConnectorInfo>> all = new HashMap<>(connectorInfoByName);
            all.put(containerName, Collections.unmodifiableMap(nameMap));
            connectorInfoByName = Collections.unmodifiableMap(all);
        }
        Debug.logInfo("Scipio: Registered connector info for container [" + containerName + "]: " + nameMap.values(), module);
    }

    public static Collection<ScipioConnectorInfo> getAllConnectorInfo(String containerName) {
        Map<String, ScipioConnectorInfo> map = connectorInfoByName.get(containerName);
        return (map != null) ? map.values() : null;
    }

    public static ScipioConnectorInfo getConnectorInfoByName(String connectorName, String containerName) {
        Map<String, ScipioConnectorInfo> map = connectorInfoByName.get(containerName);
        return (map != null) ? map.get(connectorName) : null;
    }

    public static ScipioConnectorInfo getConnectorInfoByName(String connectorName) {
        return getConnectorInfoByName(connectorName, DEFAULT_CONTAINER);
    }

    public static ScipioConnectorInfo getWebContainer(boolean secure) {
        return getConnectorInfoByName(secure ? HTTPS_CONNECTOR : HTTP_CONNECTOR, DEFAULT_CONTAINER);
    }

    /**
     * NOTE: if scheme is not recognized, defaults to https connector.
     */
    public static ScipioConnectorInfo getWebContainer(String scheme) {
        return getConnectorInfoByName("http".equals(scheme != null ? scheme.toLowerCase() : "") ? HTTP_CONNECTOR : HTTPS_CONNECTOR, DEFAULT_CONTAINER);
    }

    public String getName() {
        return name;
    }

    public int getPort() {
        return port;
    }

    public String getScheme() {
        return scheme;
    }

    public ContainerConfig.Container.Property getProperties() {
        return properties;
    }

    @Override
    public String toString() {
        return "{" +
                "name='" + name + '\'' +
                ", port=" + port +
                ", scheme='" + scheme + '\'' +
                '}';
    }
}
