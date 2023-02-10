package com.ilscipio.scipio.ce.base.component;

import com.ilscipio.scipio.ce.lang.reflect.ReflectQuery;
import org.ofbiz.base.component.ComponentConfig;

import java.net.URL;
import java.util.Collection;

public class WebappReflectInfo {

    protected final ComponentConfig.WebappInfo webappInfo;
    protected final ReflectQuery reflectQuery;

    public WebappReflectInfo(ComponentConfig.WebappInfo webappInfo, ReflectQuery reflectQuery) {
        this.webappInfo = webappInfo;
        this.reflectQuery = reflectQuery;
    }

    public WebappReflectInfo(ComponentConfig.WebappInfo webappInfo, Collection<URL> jarUrls, boolean useCache) {
        this.webappInfo = webappInfo;
        this.reflectQuery = ReflectQuery.fromJarUrls(jarUrls, useCache);
    }

    public ComponentConfig.WebappInfo getWebappInfo() {
        return webappInfo;
    }

    public ReflectQuery getReflectQuery() {
        return reflectQuery;
    }

}
