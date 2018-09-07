package com.ilscipio.scipio.ce.webapp.filter.urlrewrite.local;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;

/**
 * LocalHttpSession - Local servlet API implementation for urlrewrite emulation.
 * <p>
 * DEV NOTE: DO NOT REMOVE THE <code>@SuppressWarnings("deprecation")</code>
 * - due to special library setup with ivy, the IDE does not see the exact same
 * interfaces that are loaded at runtime for Tomcat; the suppress
 * is needed for javac.
 */
@SuppressWarnings("deprecation")
public class LocalHttpSession implements HttpSession {
    
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String DUMMY_SESSION_ID = "123456789";
    
    protected final LocalServletContainer container;

    protected final ServletContext servletContext;
    protected final Map<String, Object> attributes;
    protected final long creationTime = System.currentTimeMillis();
    protected final String id;

    public LocalHttpSession(LocalServletContainer container, ServletContext servletContext, String id,
            Map<String, Object> attributes) {
        this.container = container;
        this.servletContext = servletContext;
        this.id = id;
        this.attributes = (attributes != null) ? new HashMap<>(attributes) : new HashMap<>();
    }

    @Override
    public long getCreationTime() {
        return creationTime;
    }

    @Override
    public String getId() {
        if (id != null) return id;
        Debug.logWarning("getId called - returning dummy ID: " + DUMMY_SESSION_ID, module);
        return DUMMY_SESSION_ID;
    }

    @Override
    public long getLastAccessedTime() {
        return creationTime;
    }

    @Override
    public ServletContext getServletContext() {
        return servletContext;
    }

    @Override
    public void setMaxInactiveInterval(int interval) {
    }

    @Override
    public int getMaxInactiveInterval() {
        return 0;
    }

    @Override
    public javax.servlet.http.HttpSessionContext getSessionContext() {
        Debug.logWarning("getSessionContext called - deprecated", module);
        return null;
    }

    @Override
    public Object getAttribute(String name) {
        return attributes.get(name);
    }

    @Override
    public Object getValue(String name) {
        return getAttribute(name);
    }

    @Override
    public Enumeration<String> getAttributeNames() {
        return Collections.enumeration(attributes.keySet());
    }

    @Override
    public String[] getValueNames() {
        return new ArrayList<>(attributes.keySet()).toArray(new String[attributes.size()]);
    }

    @Override
    public void setAttribute(String name, Object value) {
        attributes.put(name, value);
    }

    @Override
    public void putValue(String name, Object value) {
        setAttribute(name, value);
    }

    @Override
    public void removeAttribute(String name) {
        attributes.remove(name);
    }

    @Override
    public void removeValue(String name) {
        removeAttribute(name);
    }

    @Override
    public void invalidate() {
    }

    @Override
    public boolean isNew() {
        return false;
    }

}
