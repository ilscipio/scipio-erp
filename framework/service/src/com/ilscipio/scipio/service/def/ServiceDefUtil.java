/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
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
        if (serviceDef == null) {
            throw new IllegalArgumentException("Missing @Service annotation to derive service name from, for " +
                    (serviceClass != null ? "class " + serviceClass.getName()
                            : serviceMethod != null ? "method " + serviceMethod.getDeclaringClass().getName() + "." + serviceMethod.getName()
                            : "unknown element"));
        }
        if (serviceClass != null) {
            return getServiceName(serviceDef, serviceClass);
        } else if (serviceMethod != null) {
            return getServiceName(serviceDef, serviceMethod);
        } else {
            throw new IllegalArgumentException("Missing service class or method");
        }
    }

}
