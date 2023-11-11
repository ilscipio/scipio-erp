package com.ilscipio.scipio.service.def;

import org.ofbiz.base.util.UtilValidate;

import java.lang.reflect.Method;

public abstract class ServiceDefUtil {

    public static String getServiceName(Service serviceDef, Class<?> serviceClass) {
        return UtilValidate.isNotEmpty(serviceDef.name()) ? serviceDef.name() :
                serviceClass.getSimpleName().substring(0, 1).toLowerCase() +
                        (serviceClass.getSimpleName().length() > 1 ? serviceClass.getSimpleName().substring(1) : "");
    }

    public static String getServiceName(Service serviceDef, Method serviceMethod) {
        return UtilValidate.isNotEmpty(serviceDef.name()) ? serviceDef.name() : serviceMethod.getName();
    }

    public static String getServiceName(Service serviceDef, Class<?> serviceClass, Method serviceMethod) {
        if (serviceClass != null) {
            return getServiceName(serviceDef, serviceClass);
        } else if (serviceMethod != null) {
            return getServiceName(serviceDef, serviceMethod);
        } else {
            throw new IllegalArgumentException("Missing service class or method");
        }
    }

}
