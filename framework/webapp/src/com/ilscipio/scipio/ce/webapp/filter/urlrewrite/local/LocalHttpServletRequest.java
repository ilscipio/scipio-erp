package com.ilscipio.scipio.ce.webapp.filter.urlrewrite.local;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.Principal;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.servlet.AsyncContext;
import javax.servlet.DispatcherType;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletInputStream;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpUpgradeHandler;
import javax.servlet.http.Part;

import org.ofbiz.base.util.Debug;

/**
 * LocalHttpServletRequest - Local servlet API implementation for urlrewrite emulation.
 * <p>
 * DEV NOTE: DO NOT REMOVE THE <code>@SuppressWarnings("deprecation")</code>
 * - due to special library setup with ivy, the IDE does not see the exact same
 * interfaces that are loaded at runtime for Tomcat; the suppress
 * is needed for javac. EDIT: Added "all", as a rare exception for this case.
 */
@SuppressWarnings({ "all", "deprecation" })
public class LocalHttpServletRequest implements HttpServletRequest {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected final LocalServletContainer container;
    
    protected final LocalHttpSession session;
    protected final LocalServletContext servletContext;

    protected final Map<String, Object> attributes;
    protected final Map<String, String[]> parameters;
    protected final Map<String, String[]> headers;
    
    protected final Locale locale;
    protected String charset;
    protected String contentType;
    protected final long creationTime = System.currentTimeMillis();
    
    protected final URL requestURL;
    protected final String servletPath;
    protected final String pathInfo;

    protected LocalHttpServletRequest(LocalServletContainer container, LocalHttpSession session, 
            LocalServletContext servletContext, String requestURL, String servletPath,
            Map<String, Object> attributes, Map<String, String[]> parameters, Map<String, String[]> headers, 
            Locale locale, String charset, String contentType) throws MalformedURLException {
        this.container = container;
        this.session = session;
        this.servletContext = servletContext;
        
        this.attributes = (attributes != null) ? new HashMap<>(attributes) : new HashMap<>();
        this.parameters = (parameters != null) ? Collections.unmodifiableMap(new HashMap<>(parameters)) : Collections.emptyMap();
        this.headers = (headers != null) ? Collections.unmodifiableMap(new HashMap<>(headers)) : Collections.emptyMap();
        
        this.locale = locale;
        this.charset = charset;
        this.contentType = contentType;
        
        this.requestURL = new URL(requestURL);
        servletPath = (servletPath != null) ? servletPath : "";
        this.servletPath = servletPath;
        
        String fullServletPath = getContextPath() + servletPath;
        String reqUri = this.requestURL.getPath();
        String pathInfo = null;
        if (fullServletPath.length() == 0) {
            ;
        } else if (reqUri != null && reqUri.startsWith(fullServletPath) 
                && (reqUri.length() == fullServletPath.length() || reqUri.charAt(fullServletPath.length()) == '/')) {
            pathInfo = reqUri.substring(fullServletPath.length());
            pathInfo = pathInfo.length() > 0 ? pathInfo : null;
        } else {
            Debug.logWarning("Provided request URL '" + requestURL 
                    + "' does not match specified contextPath+servletPath '" + fullServletPath 
                    + "'; using null pathInfo", module);
        }
        this.pathInfo = pathInfo;
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
    public String getCharacterEncoding() {
        return charset;
    }

    @Override
    public void setCharacterEncoding(String env) throws UnsupportedEncodingException {
        this.charset = env;
    }

    @Override
    public int getContentLength() {
        return -1;
    }

    @Override
    public long getContentLengthLong() {
        return -1;
    }

    @Override
    public String getContentType() {
        return contentType;
    }

    @Override
    public ServletInputStream getInputStream() throws IOException {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getParameter(String name) {
        String[] values = getParameterValues(name);
        if (values != null && values.length > 0) return values[0];
        return null;
    }

    @Override
    public Enumeration<String> getParameterNames() {
        return Collections.enumeration(parameters.keySet());
    }

    @Override
    public String[] getParameterValues(String name) {
        return parameters.get(name);
    }

    @Override
    public Map<String, String[]> getParameterMap() {
        return parameters;
    }

    @Override
    public String getProtocol() {
        return "HTTP/1.1";
    }

    @Override
    public String getScheme() {
        return requestURL.getProtocol();
    }

    @Override
    public String getServerName() {
        return requestURL.getHost();
    }

    @Override
    public int getServerPort() {
        return requestURL.getPort();
    }

    @Override
    public BufferedReader getReader() throws IOException {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getRemoteAddr() {
        return "127.0.0.1";
    }

    @Override
    public String getRemoteHost() {
        return "localhost";
    }

    @Override
    public void setAttribute(String name, Object o) {
        attributes.put(name, o);
    }

    @Override
    public void removeAttribute(String name) {
        attributes.remove(name);
    }

    @Override
    public Locale getLocale() {
        return locale;
    }

    @Override
    public Enumeration<Locale> getLocales() {
        return Collections.enumeration(Arrays.asList(new Locale[] {locale}));
    }

    @Override
    public boolean isSecure() {
        return "https".equals(requestURL.getProtocol());
    }

    @Override
    public RequestDispatcher getRequestDispatcher(String path) {
        Debug.logWarning("getRequestDispatcher called - unsupported - returning null", module);
        return null;
    }

    @Override
    public String getRealPath(String path) {
        Debug.logWarning("getRealPath called - unsupported - returning null", module);
        return null;
    }

    @Override
    public int getRemotePort() {
        Debug.logWarning("getRemotePort called - unsupported - returning 0", module);
        return 0;
    }

    @Override
    public String getLocalName() {
        return "localhost";
    }

    @Override
    public String getLocalAddr() {
        return "127.0.0.1";
    }

    @Override
    public int getLocalPort() {
        return requestURL.getPort();
    }

    @Override
    public ServletContext getServletContext() {
        return servletContext;
    }

    @Override
    public AsyncContext startAsync() throws IllegalStateException {
        throw new IllegalStateException();
    }

    @Override
    public AsyncContext startAsync(ServletRequest servletRequest, ServletResponse servletResponse)
            throws IllegalStateException {
        throw new IllegalStateException();
    }

    @Override
    public boolean isAsyncStarted() {
        Debug.logWarning("isAsyncStarted called - unsupported - returning false", module);
        return false;
    }

    @Override
    public boolean isAsyncSupported() {
        Debug.logWarning("isAsyncSupported called - unsupported - returning false", module);
        return false;
    }

    @Override
    public AsyncContext getAsyncContext() {
        Debug.logWarning("isAsyncSupported called - unsupported - returning null", module);
        return null;
    }

    @Override
    public DispatcherType getDispatcherType() {
        return DispatcherType.REQUEST;
    }

    @Override
    public String getAuthType() {
        return null;
    }

    @Override
    public Cookie[] getCookies() {
        return null;
    }

    @Override
    public long getDateHeader(String name) {
        return creationTime;
    }

    @Override
    public String getHeader(String name) {
        if (headers.get(name) != null && headers.get(name).length > 0) return headers.get(name)[0];
        else return null;
    }

    @Override
    public Enumeration<String> getHeaders(String name) {
        if (headers.get(name) != null) return Collections.enumeration(Arrays.asList(headers.get(name)));
        else return Collections.enumeration(Collections.emptyList());
    }

    @Override
    public Enumeration<String> getHeaderNames() {
        return Collections.enumeration(headers.keySet());
    }

    @Override
    public int getIntHeader(String name) {
        return Integer.parseInt(getHeader(name));
    }

    @Override
    public String getMethod() {
        return "GET";
    }

    @Override
    public String getPathInfo() {
        return pathInfo;
    }

    @Override
    public String getPathTranslated() {
        Debug.logWarning("getPathTranslated called - unsupported - returning null", module);
        return null;
    }

    @Override
    public String getContextPath() {
        return container.getWebappInfo().getContextPath();
    }

    @Override
    public String getQueryString() {
        return requestURL.getQuery();
    }

    @Override
    public String getRemoteUser() {
        // don't log; called by org.ofbiz.base.util.UtilHttp.setInitialRequestInfo
        //Debug.logWarning("getRemoteUser called - unsupported - returning null", module);
        return null;
    }

    @Override
    public boolean isUserInRole(String role) {
        Debug.logWarning("isUserInRole called - unsupported - returning false", module);
        return false;
    }

    @Override
    public Principal getUserPrincipal() {
        Debug.logWarning("getUserPrincipal called - unsupported - returning null", module);
        return null;
    }

    @Override
    public String getRequestedSessionId() {
        return session.getId();
    }

    @Override
    public String getRequestURI() {
        return requestURL.getPath();
    }

    @Override
    public StringBuffer getRequestURL() {
        String fullURL = requestURL.toString();
        int i = fullURL.indexOf('?');
        return new StringBuffer((i >= 0) ? fullURL.substring(0, i) : fullURL);
    }

    @Override
    public String getServletPath() {
        return servletPath;
    }

    @Override
    public HttpSession getSession(boolean create) {
        return session;
    }

    @Override
    public HttpSession getSession() {
        return session;
    }

    @Override
    public String changeSessionId() {
        Debug.logWarning("changeSessionId called - unsupported - doing nothing", module);
        return session.getId();
    }

    @Override
    public boolean isRequestedSessionIdValid() {
        Debug.logWarning("isRequestedSessionIdValid called - unsupported - returning false", module);
        return false;
    }

    @Override
    public boolean isRequestedSessionIdFromCookie() {
        Debug.logWarning("isRequestedSessionIdFromCookie called - unsupported - returning false", module);
        return false;
    }

    @Override
    public boolean isRequestedSessionIdFromURL() {
        Debug.logWarning("isRequestedSessionIdFromURL called - unsupported - returning false", module);
        return false;
    }

    @Override
    public boolean isRequestedSessionIdFromUrl() {
        Debug.logWarning("isRequestedSessionIdFromUrl called - unsupported - returning false", module);
        return false;
    }

    @Override
    public boolean authenticate(HttpServletResponse response) throws IOException, ServletException {
        Debug.logWarning("authenticate called - unsupported - doing nothing", module);
        return false;
    }

    @Override
    public void login(String username, String password) throws ServletException {
        Debug.logWarning("login called - unsupported - doing nothing", module);
    }

    @Override
    public void logout() throws ServletException {
        Debug.logWarning("logout called - unsupported - doing nothing", module);
    }

    @Override
    public Collection<Part> getParts() throws IOException, ServletException {
        Debug.logWarning("getParts called - unsupported - returning null", module);
        return null;
    }

    @Override
    public Part getPart(String name) throws IOException, ServletException {
        Debug.logWarning("getPart called - unsupported - returning null", module);
        return null;
    }

    @Override
    public <T extends HttpUpgradeHandler> T upgrade(Class<T> handlerClass) throws IOException, ServletException {
        Debug.logWarning("upgrade called - unsupported - returning null", module);
        return null;
    }

}
