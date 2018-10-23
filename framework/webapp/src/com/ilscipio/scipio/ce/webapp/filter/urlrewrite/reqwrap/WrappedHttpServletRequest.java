package com.ilscipio.scipio.ce.webapp.filter.urlrewrite.reqwrap;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

/**
 * Can be used to wrap HttpServletRequest for inter-webapp URL rewriting.
 * Shields the original request from attribute modification.
 */
public class WrappedHttpServletRequest extends HttpServletRequestWrapper {

    protected final Map<String, Object> attributes = new HashMap<>();
    protected Set<String> attributeNames = null;

    public WrappedHttpServletRequest(HttpServletRequest request) {
        super(request);
    }

    @Override
    public Object getAttribute(String name) {
        return attributes.containsKey(name) ? attributes.get(name) : super.getAttribute(name);
    }

    @Override
    public Enumeration<String> getAttributeNames() {
        if (attributeNames == null) {

        }
        return super.getAttributeNames();
    }

    @Override
    public void setAttribute(String name, Object o) {
        attributes.put(name, o);
    }

    @Override
    public void removeAttribute(String name) {
        attributes.put(name, null);
    }
}
