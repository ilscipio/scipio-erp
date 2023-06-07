package com.ilscipio.scipio.ce.webapp.control.util;

import com.ilscipio.scipio.base.util.AttrHandler;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.util.EntityUtilProperties;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.io.Serializable;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

/**
 * Simple template class for per-webapp cached configurations that can read from both web.xml and EntityUtilProperties with tenant support.
 *
 * <p>Usually static factory methods will simply delegator to these.</p>
 *
 * <p>SCIPIO: 3.0.0: Added.</p>
 */
public abstract class WebappConfig implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static abstract class Factory<C extends WebappConfig> {

        public abstract C make(Delegator delegator, ServletContext servletContext, HttpSession session, HttpServletRequest request);

        public C make(Delegator delegator, Object servletObject) {
            ServletContext servletContext = UtilHttp.getServletContext(servletObject);
            HttpSession session = UtilHttp.getSession(servletObject, false);
            HttpServletRequest request = UtilHttp.getRequest(servletObject);
            return make(delegator, servletContext, session, request);
        }

    }

    public static <C extends WebappConfig> C make(String resource, String property, Class<?> defaultFactoryClass, Delegator delegator, Object servletObject) {
        String className = EntityUtilProperties.getWebappPropertyValue(resource, property, delegator, UtilHttp.getServletContext(servletObject));
        C config;
        Supplier<String> propId = () -> delegator.getDelegatorName() + "@" + UtilHttp.getContextPath(servletObject) + "@" + resource + "#" + property;
        if (UtilValidate.isNotEmpty(className)) {
            config = make(resolveClass(className, propId), delegator, servletObject, propId);
            if (config != null) {
                return config;
            }
        }
        return make(defaultFactoryClass, delegator, servletObject, () -> "default:" + defaultFactoryClass);
    }

    protected static <C extends WebappConfig> C make(Class<?> cls, Delegator delegator, Object servletObject, Supplier<String> propId) {
        if (cls == null) {
            return null;
        }
        try {
            if (Factory.class.isAssignableFrom(cls)) {
                return UtilGenerics.cast(((Factory<?>) cls.getConstructor().newInstance()).make(delegator, servletObject));
            } else {
                try {
                    ServletContext servletContext = UtilHttp.getServletContext(servletObject);
                    HttpSession session = UtilHttp.getSession(servletObject, false);
                    HttpServletRequest request = UtilHttp.getRequest(servletObject);
                    return UtilGenerics.cast(cls.getConstructor(Delegator.class, ServletContext.class, HttpSession.class, HttpServletRequest.class)
                            .newInstance(delegator, servletContext, session, request));
                } catch (NoSuchMethodException e) {
                    try {
                        return UtilGenerics.cast(cls.getConstructor(Delegator.class, Object.class).newInstance(delegator, servletObject));
                    } catch (NoSuchMethodException e2) {
                        return UtilGenerics.cast(cls.getConstructor().newInstance());
                    }
                }
            }
        } catch (ReflectiveOperationException e) {
            Debug.logError(e, "Could not instantiate class [" + cls + "] from property [" + propId.get() + "]", module);
            return null;
        }
    }

    protected static Class<?> resolveClass(String cls, Supplier<String> propId) {
        try {
            return Class.forName(cls);
        } catch (ReflectiveOperationException e) {
            Debug.logError(e, "Could not resolve class name [" + cls + "] from property [" + propId.get() + "]", module);
            return null;
        }
    }

}