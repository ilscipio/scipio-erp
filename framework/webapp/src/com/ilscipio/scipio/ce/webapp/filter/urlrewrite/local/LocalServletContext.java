package com.ilscipio.scipio.ce.webapp.filter.urlrewrite.local;

import java.io.File;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.Enumeration;
import java.util.EventListener;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.servlet.Filter;
import javax.servlet.FilterRegistration;
import javax.servlet.RequestDispatcher;
import javax.servlet.Servlet;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRegistration;
import javax.servlet.ServletRegistration.Dynamic;
import javax.servlet.SessionCookieConfig;
import javax.servlet.SessionTrackingMode;
import javax.servlet.descriptor.JspConfigDescriptor;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilURL;

@SuppressWarnings("deprecation")
public class LocalServletContext implements ServletContext {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final int SERVLET_API_MAJOR_VER = 3;
    public static final int SERVLET_API_MINOR_VER = 0;

    protected final LocalServletContainer container;
    
    protected final Map<String, Object> attributes;
    protected final Map<String, String> initParameters;

    public LocalServletContext(LocalServletContainer container, Map<String, Object> attributes,
            Map<String, String> initParameters) {
        this.container = container;
        this.attributes = (attributes != null) ? new HashMap<>(attributes) : new HashMap<>();
        this.initParameters = (initParameters != null) ? new HashMap<>(initParameters) : new HashMap<>();
    }

    @Override
    public String getContextPath() {
        return container.getWebappInfo().getContextPath();
    }

    @Override
    public ServletContext getContext(String uripath) {
        Debug.logWarning("getContext called - unsupported - returning null", module);
        return null;
    }

    @Override
    public int getMajorVersion() {
        return SERVLET_API_MAJOR_VER;
    }

    @Override
    public int getMinorVersion() {
        return SERVLET_API_MINOR_VER;
    }

    @Override
    public int getEffectiveMajorVersion() {
        return SERVLET_API_MAJOR_VER;
    }

    @Override
    public int getEffectiveMinorVersion() {
        return SERVLET_API_MINOR_VER;
    }

    @Override
    public String getMimeType(String file) {
        Debug.logWarning("getMimeType called - unsupported - returning null", module);
        return null;
    }

    @Override
    public Set<String> getResourcePaths(String path) {
        Debug.logWarning("getResourcePaths called - unsupported - returning null", module);
        return null;
    }

    @Override
    public URL getResource(String path) throws MalformedURLException {
        // FIXME: too hardcoded
        String loc = getRealPath(path);
        if (new File(loc).exists()) {
            return UtilURL.fromFilename(loc);
        }
        return null;
    }

    @Override
    public InputStream getResourceAsStream(String path) {
        Debug.logWarning("getResourceAsStream called - unsupported - returning null", module);
        return null;
    }

    @Override
    public RequestDispatcher getRequestDispatcher(String path) {
        Debug.logWarning("getRequestDispatcher called - unsupported - returning null", module);
        return null;
    }

    @Override
    public RequestDispatcher getNamedDispatcher(String name) {
        Debug.logWarning("getNamedDispatcher called - unsupported - returning null", module);
        return null;
    }

    @Override
    public Servlet getServlet(String name) throws ServletException {
        Debug.logWarning("getServlet called - unsupported - returning null", module);
        return null;
    }

    @Override
    public Enumeration<Servlet> getServlets() {
        Debug.logWarning("getServlets called - unsupported - returning null", module);
        return null;
    }

    @Override
    public Enumeration<String> getServletNames() {
        Debug.logWarning("getServletNames called - unsupported - returning null", module);
        return null;
    }

    @Override
    public void log(String msg) {
        Debug.logInfo("[LocalServletContext log event] " + msg, module);
    }

    @Override
    public void log(Exception exception, String msg) {
        Debug.logInfo(exception, "[LocalServletContext log event] " + msg, module);
    }

    @Override
    public void log(String message, Throwable throwable) {
        Debug.logInfo(throwable, "[LocalServletContext log event] " + message, module);
    }

    @Override
    public String getRealPath(String path) {
        return Paths.get(container.getWebappInfo().getWebappInfo().getLocation(), path).toString();
    }

    @Override
    public String getServerInfo() {
        Debug.logWarning("getServerInfo called - unsupported - returning null", module);
        return null;
    }

    @Override
    public String getInitParameter(String name) {
        return initParameters.get(name);
    }

    @Override
    public Enumeration<String> getInitParameterNames() {
        return Collections.enumeration(initParameters.keySet());
    }

    @Override
    public boolean setInitParameter(String name, String value) {
        if (initParameters.containsKey(name)) return false;
        initParameters.put(name, value);
        return true;
    }

    @Override
    public Object getAttribute(String name) {
        return attributes.get(name);
    }

    @Override
    public Enumeration<String> getAttributeNames() {
        return Collections.enumeration(attributes.keySet());
    }

    @Override
    public void setAttribute(String name, Object object) {
        attributes.put(name, object);
    }

    @Override
    public void removeAttribute(String name) {
        attributes.remove(name);
    }

    @Override
    public String getServletContextName() {
        Debug.logWarning("getServletContextName called - returning null", module);
        return null;
    }

    @Override
    public Dynamic addServlet(String servletName, String className) {
        Debug.logWarning("addServlet called - unsupported - returning null", module);
        return null;
    }

    @Override
    public Dynamic addServlet(String servletName, Servlet servlet) {
        Debug.logWarning("addServlet called - unsupported - returning null", module);
        return null;
    }

    @Override
    public Dynamic addServlet(String servletName, Class<? extends Servlet> servletClass) {
        Debug.logWarning("addServlet called - unsupported - returning null", module);
        return null;
    }

    @Override
    public <T extends Servlet> T createServlet(Class<T> clazz) throws ServletException {
        Debug.logWarning("createServlet called - unsupported - returning null", module);
        return null;
    }

    @Override
    public ServletRegistration getServletRegistration(String servletName) {
        Debug.logWarning("getServletRegistration called - unsupported - returning null", module);
        return null;
    }

    @Override
    public Map<String, ? extends ServletRegistration> getServletRegistrations() {
        Debug.logWarning("getServletRegistrations called - unsupported - returning null", module);
        return null;
    }

    @Override
    public javax.servlet.FilterRegistration.Dynamic addFilter(String filterName, String className) {
        Debug.logWarning("addFilter called - unsupported - returning null", module);
        return null;
    }

    @Override
    public javax.servlet.FilterRegistration.Dynamic addFilter(String filterName, Filter filter) {
        Debug.logWarning("addFilter called - unsupported - returning null", module);
        return null;
    }

    @Override
    public javax.servlet.FilterRegistration.Dynamic addFilter(String filterName, Class<? extends Filter> filterClass) {
        Debug.logWarning("addFilter called - unsupported - returning null", module);
        return null;
    }

    @Override
    public <T extends Filter> T createFilter(Class<T> clazz) throws ServletException {
        Debug.logWarning("createFilter called - unsupported - returning null", module);
        return null;
    }

    @Override
    public FilterRegistration getFilterRegistration(String filterName) {
        Debug.logWarning("getFilterRegistration called - unsupported - returning null", module);
        return null;
    }

    @Override
    public Map<String, ? extends FilterRegistration> getFilterRegistrations() {
        Debug.logWarning("getFilterRegistrations called - unsupported - returning empty map", module);
        return new HashMap<>();
    }

    @Override
    public SessionCookieConfig getSessionCookieConfig() {
        Debug.logWarning("getSessionCookieConfig called - unsupported - returning dummy wrapper", module);
        return new SessionCookieConfig() {
            @Override
            public void setName(String name) {
            }

            @Override
            public String getName() {
                return null;
            }

            @Override
            public void setDomain(String domain) {
            }

            @Override
            public String getDomain() {
                return null;
            }

            @Override
            public void setPath(String path) {
            }

            @Override
            public String getPath() {
                return null;
            }

            @Override
            public void setComment(String comment) {
            }

            @Override
            public String getComment() {
                return null;
            }

            @Override
            public void setHttpOnly(boolean httpOnly) {
            }

            @Override
            public boolean isHttpOnly() {
                return false;
            }

            @Override
            public void setSecure(boolean secure) {
            }

            @Override
            public boolean isSecure() {
                return false;
            }

            @Override
            public void setMaxAge(int maxAge) {
            }

            @Override
            public int getMaxAge() {
                return 0;
            }
        };
    }

    @Override
    public void setSessionTrackingModes(Set<SessionTrackingMode> sessionTrackingModes) {
        Debug.logWarning("setSessionTrackingModes called - unsupported - doing nothing", module);
    }

    @Override
    public Set<SessionTrackingMode> getDefaultSessionTrackingModes() {
        Debug.logWarning("getDefaultSessionTrackingModes called - unsupported - returning empty", module);
        return new HashSet<>();
    }

    @Override
    public Set<SessionTrackingMode> getEffectiveSessionTrackingModes() {
        Debug.logWarning("getEffectiveSessionTrackingModes called - unsupported - returning empty", module);
        return new HashSet<>();
    }

    @Override
    public void addListener(String className) {
        Debug.logWarning("addListener called - unsupported - doing nothing", module);
    }

    @Override
    public <T extends EventListener> void addListener(T t) {
        Debug.logWarning("addListener called - unsupported - doing nothing", module);
    }

    @Override
    public void addListener(Class<? extends EventListener> listenerClass) {
        Debug.logWarning("addListener called - unsupported - doing nothing", module);
    }

    @Override
    public <T extends EventListener> T createListener(Class<T> clazz) throws ServletException {
        Debug.logWarning("createListener called - unsupported - must throw exception", module);
        throw new ServletException("createListener called - unsupported - must throw exception");
    }

    @Override
    public JspConfigDescriptor getJspConfigDescriptor() {
        Debug.logWarning("getJspConfigDescriptor called - unsupported - returning null", module);
        return null;
    }

    @Override
    public ClassLoader getClassLoader() {
        // TODO: REVIEW: should get this or the static one?
        return Thread.currentThread().getContextClassLoader();
    }

    @Override
    public void declareRoles(String... roleNames) {
        Debug.logWarning("declareRoles called - unsupported - doing nothing", module);
    }

    @Override
    public String getVirtualServerName() {
        Debug.logWarning("getVirtualServerName called - unsupported - returning null", module);
        return null;
    }
}
