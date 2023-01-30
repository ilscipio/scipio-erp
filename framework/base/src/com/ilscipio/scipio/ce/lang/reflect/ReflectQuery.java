package com.ilscipio.scipio.ce.lang.reflect;

import org.ofbiz.base.util.cache.UtilCache;
import org.reflections.Reflections;
import org.reflections.scanners.FieldAnnotationsScanner;
import org.reflections.scanners.MemberUsageScanner;
import org.reflections.scanners.MethodAnnotationsScanner;
import org.reflections.scanners.MethodParameterNamesScanner;
import org.reflections.scanners.MethodParameterScanner;
import org.reflections.scanners.ResourcesScanner;
import org.reflections.scanners.SubTypesScanner;
import org.reflections.scanners.TypeAnnotationsScanner;
import org.reflections.scanners.TypeElementsScanner;
import org.reflections.util.ConfigurationBuilder;

import java.io.File;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * Annotations reflection scanner - wrapper around org.reflections/org.reflections8 library for simplified use.
 *
 * <p>DEV NOTE: This does not currently fully abstract Reflections; it's intended to add helper calls around expensive
 * cached operations, but some abstractions are available and more may be added.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
public class ReflectQuery {

    private static final UtilCache<String, ReflectQuery> URL_CACHE = UtilCache.createUtilCache("reflect.query");
    public static final ReflectQuery NONE = new ReflectQuery(null);

    protected final Collection<URL> jarUrls;

    /**
     * The Reflections object, which acts as queryable cache for expensive operations.
     * <p>If a method is called which requires type scanners which were not specified at creation, it may be rebuilt.</p>
     */
    protected final Reflections reflections;

    protected ReflectQuery(Collection<URL> jarUrls) {
        this.jarUrls = (jarUrls != null && !jarUrls.isEmpty()) ? jarUrls : Collections.emptySet();
        this.reflections = !this.jarUrls.isEmpty() ? new Reflections(new ConfigurationBuilder()
                .setScanners(new TypeAnnotationsScanner(),
                        new SubTypesScanner(),
                        new MethodAnnotationsScanner(),
                        new FieldAnnotationsScanner(),
                        new MemberUsageScanner(),
                        new MethodParameterNamesScanner(),
                        new ResourcesScanner(),
                        new MethodParameterScanner(),
                        new TypeElementsScanner())
                .setUrls(jarUrls)) : null;
    }

    /*
     * Factory methods
     */

    public static ReflectQuery fromJarUrls(Collection<URL> jarUrls, boolean useCache) {
        if (jarUrls == null || jarUrls.isEmpty()) {
            return NONE;
        } else if (!useCache) {
            return new ReflectQuery(jarUrls);
        }
        Set<String> orderedUrls = new TreeSet<>();
        for(URL url : jarUrls) {
            orderedUrls.add(url.toString());
        }
        String cacheKey = String.join("::", orderedUrls);
        ReflectQuery rq = URL_CACHE.get(cacheKey);
        if (rq == null) {
            rq = new ReflectQuery(jarUrls);
            rq = URL_CACHE.putIfAbsentAndGet(cacheKey, rq);
        }
        return rq;
    }

    public static ReflectQuery fromJarFiles(Collection<File> jarFiles, boolean useCache) {
        return fromJarUrls(getJarUrlsForFiles(jarFiles), useCache);
    }

    public boolean hasDefs() { return reflections != null; }

    public Reflections getReflections() {
        return reflections;
    }

    public Collection<URL> getJarUrls() {
        return jarUrls;
    }

    /*
     * Queries
     */

    public Set<Class<?>> getAnnotatedClasses(Class<? extends Annotation> annCls) {
        if (getReflections() == null) {
            return Collections.emptySet();
        }
        return getReflections().getTypesAnnotatedWith(annCls);
    }

    public Set<Class<?>> getAnnotatedClasses(Collection<Class<? extends Annotation>> annClsList) {
        if (getReflections() == null) {
            return Collections.emptySet();
        }
        Set<Class<?>> allClasses = new LinkedHashSet<>();
        for(Class<? extends Annotation> annotationCls : annClsList) {
            Set<Class<?>> classes = getAnnotatedClasses(annotationCls);
            if (classes != null) {
                allClasses.addAll(classes);
            }
        }
        return allClasses;
    }

    public Set<Method> getAnnotatedMethods(Class<? extends Annotation> annCls) {
        if (getReflections() == null) {
            return Collections.emptySet();
        }
        return getReflections().getMethodsAnnotatedWith(annCls);
    }

    public Set<Method> getAnnotatedMethods(Collection<Class<? extends Annotation>> annClsList) {
        if (getReflections() == null) {
            return Collections.emptySet();
        }
        Set<Method> allMethods = new LinkedHashSet<>();
        for(Class<? extends Annotation> annotationCls : annClsList) {
            Set<Method> methods = getAnnotatedMethods(annotationCls);
            if (methods != null) {
                allMethods.addAll(methods);
            }
        }
        return allMethods;
    }

    public Set<Field> getAnnotatedFields(Class<? extends Annotation> annCls) {
        if (getReflections() == null) {
            return Collections.emptySet();
        }
        return getReflections().getFieldsAnnotatedWith(annCls);
    }

    public Set<Field> getAnnotatedFields(Collection<Class<? extends Annotation>> annClsList) {
        if (getReflections() == null) {
            return Collections.emptySet();
        }
        Set<Field> allFields = new LinkedHashSet<>();
        for(Class<? extends Annotation> annotationCls : annClsList) {
            Set<Field> fields = getAnnotatedFields(annotationCls);
            if (fields != null) {
                allFields.addAll(fields);
            }
        }
        return allFields;
    }

    public static List<URL> getJarUrlsForFiles(Collection<File> jarFiles) {
        return jarFiles.stream().map(file -> {
            try {
                return file.toURI().toURL();
            } catch (MalformedURLException e) {
                throw new IllegalArgumentException(e);
            }
        }).collect(Collectors.toList());
    }

}
