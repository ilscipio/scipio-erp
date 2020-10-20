package org.ofbiz.base.util;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;

/**
 * A helper wrapper around the Java {@link Method} class, used to bypass dependency issues.
 */
public class ScipioMethod {
    protected final ClassLoader classLoader;
    protected final String className;
    protected final String methodName;
    protected final Class<?>[] signature;

    protected Method method;

    protected ScipioMethod(ClassLoader classLoader, String className, String methodName, Class<?>[] signature) {
        this.classLoader = classLoader;
        this.className = className;
        this.methodName = methodName;
        this.signature = signature;
    }

    public static ScipioMethod from(ClassLoader classLoader, String className, String methodName, Class<?>... signature) {
        return new ScipioMethod(classLoader, className, methodName, signature);
    }

    public static ScipioMethod from(String className, String methodName, Class<?>... signature) {
        return from(null, className, methodName, signature);
    }

    public static ScipioMethod from(ClassLoader classLoader, String className, String methodName, List<Class<?>> signature) {
        return from(classLoader, className, methodName, signature.toArray(new Class<?>[0]));
    }

    public static ScipioMethod from(String className, String methodName, List<Class<?>> signature) {
        return from(null, className, methodName, signature);
    }

    public <T> T invoke(Object obj, Object... args) throws Exception {
        try {
            return UtilGenerics.cast(getMethod().invoke(obj, args));
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new IllegalStateException(e);
        }
    }

    public <T> T invoke(Object obj, List<?> args) throws Exception {
        return invoke(obj, args.toArray(new Object[0]));
    }

    public <T> T invokeRuntime(Object obj, Object... args) throws RuntimeException {
        try {
            return UtilGenerics.cast(getMethod().invoke(obj, args));
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public <T> T invokeRuntime(Object obj, List<?> args) throws RuntimeException {
        return invokeRuntime(obj, args.toArray(new Object[0]));
    }

    public ClassLoader getClassLoader() {
        ClassLoader classLoader = this.classLoader;
        return (classLoader != null) ? classLoader : Thread.currentThread().getContextClassLoader();
    }

    public Method getMethod() throws IllegalStateException {
        Method method = this.method;
        if (method == null) {
            try {
                method = getClassLoader().loadClass(getClassName()).getMethod(getMethodName(), getSignature());
            } catch (Exception e) {
                throw new IllegalStateException(e);
            }
            this.method = method;
        }
        return method;
    }

    public String getClassName() {
        return className;
    }

    public String getMethodName() {
        return methodName;
    }

    public Class<?>[] getSignature() {
        return signature;
    }
}
