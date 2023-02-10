package com.ilscipio.scipio.ce.base.component;

import com.ilscipio.scipio.ce.lang.reflect.ReflectQuery;
import org.ofbiz.base.component.ComponentConfig;

import java.net.URL;
import java.util.Collection;

public class ComponentReflectInfo {

    protected final ComponentConfig component;
    protected final ReflectQuery reflectQuery;

    public ComponentReflectInfo(ComponentConfig component, ReflectQuery reflectQuery) {
        this.component = component;
        this.reflectQuery = reflectQuery;
    }

    public ComponentReflectInfo(ComponentConfig component, Collection<URL> jarUrls, boolean useCache) {
        this.component = component;
        this.reflectQuery = ReflectQuery.fromJarUrls(jarUrls, useCache);
    }

    public ComponentConfig getComponent() {
        return component;
    }

    public ReflectQuery getReflectQuery() {
        return reflectQuery;
    }

}
