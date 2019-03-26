package com.ilscipio.scipio.ce.webapp.filter.urlrewrite.reqwrap;

import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

import org.ofbiz.base.util.Debug;

/**
 * Can be used to wrap HttpServletRequest for inter-webapp URL rewriting.
 * Shields the original request from attribute modification.
 */
public class WrappedHttpServletRequest extends HttpServletRequestWrapper {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected final Map<String, Object> attributes = new HashMap<>();
    protected Set<String> removedAttributeNames = new HashSet<>();

    public WrappedHttpServletRequest(HttpServletRequest request) {
        super(checkRequest(request));
    }

    private static HttpServletRequest checkRequest(HttpServletRequest request) {
        if (request instanceof WrappedHttpServletRequest) {
            Debug.logWarning("Nested wrapped request detected", module);
        }
        return request;
    }

    @Override
    public Object getAttribute(String name) {
        return attributes.containsKey(name) ? attributes.get(name) : super.getAttribute(name);
    }

    @Override
    public Enumeration<String> getAttributeNames() {
        Set<String> attrNames = new HashSet<>(Collections.list(super.getAttributeNames()));
        attrNames.addAll(attributes.keySet());
        attrNames.removeAll(removedAttributeNames);
        return Collections.enumeration(attrNames);
    }

    @Override
    public void setAttribute(String name, Object o) {
        attributes.put(name, o);
        removedAttributeNames.remove(name);
    }

    @Override
    public void removeAttribute(String name) {
        attributes.put(name, null);
        removedAttributeNames.add(name);
    }
}
