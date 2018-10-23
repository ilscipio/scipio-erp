package org.ofbiz.base.util.template;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilCodec.SimpleEncoder;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;

import freemarker.ext.beans.BeansWrapper;
import freemarker.ext.beans.BeansWrapperConfiguration;
import freemarker.ext.beans.CollectionModel;
import freemarker.ext.beans.StringModel;
import freemarker.ext.util.ModelFactory;
import freemarker.ext.util.WrapperTemplateModel;
import freemarker.template.DefaultObjectWrapper;
import freemarker.template.DefaultObjectWrapperConfiguration;
import freemarker.template.ObjectWrapper;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.Version;

/**
 * SCIPIO: Object wrapper interfaces, classes and implementations.
 * Replaces the legacy Ofbiz ExtendedWrapper that was previously in HtmlWidget.
 * <p>
 * WARN: in this code we treat DefaultObjectWrapper as if it didn't extend BeansWrapper,
 * so we can write sane instanceof statements elsewhere. So ScipioBeansWrapper should NEVER
 * be on the same class as ScipioDefaultObjectWrapper.
 */
public abstract class ScipioFtlWrappers {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final List<CustomFactoryInfo> customWrapperFactoryInfo = Collections.unmodifiableList(readCustomWrapperFactories());
    private static final DefaultScipioObjectWrapperFactory defaultObjectWrapperFactory = new DefaultScipioObjectWrapperFactory();
    private static final ScipioObjectWrapperFactory systemObjectWrapperFactory = readPropertyObjectWrapperFactory("general.properties",
            "render.global.systemObjectWrapperFactory", defaultObjectWrapperFactory);

    protected ScipioFtlWrappers() {
    }


    /*
     **************************************************************
     * Individual model interfaces and base classes
     **************************************************************
     */

    /**
     * SCIPIO: Special interface for template models that perform auto-escaping.
     * This is returned by object wrappers implementing ScipioExtendedObjectWrapper.
     */
    public interface EscapingModel extends WrapperTemplateModel {
    }

    /**
     * Auto-escaping wrapper, originally from HtmlWidget.ExtendedWrapper.
     * <p>
     * FIXME?: in principle this should be able to support detecting the language encoding
     * from the current renderer. originally ofbiz hardcoded these as "html" so having the language
     * in the BeansWrapper was an improvement, but it ends up still hardcoded...
     * but this is also an HtmlWidget decision...
     */
    public static class StringWrapperForFtl extends StringModel implements EscapingModel { // SCIPIO: special interface, and renamed
        public StringWrapperForFtl(String str, ScipioExtendedObjectWrapper wrapper) {
            super(str, (BeansWrapper) wrapper);
        }
        @Override
        public String getAsString() {
            return ((ScipioExtendedObjectWrapper) wrapper).getEncoder().encode(super.getAsString());
        }
    }

    /**
     * Auto-escaping wrapper, originally from HtmlWidget.ExtendedWrapper.
     */
    public static class CollectionWrapperForFtl extends CollectionModel implements EscapingModel { // SCIPIO: special interface, and renamed
        public CollectionWrapperForFtl(Collection<?> collection, ScipioExtendedObjectWrapper wrapper) {
            super(collection, (BeansWrapper) wrapper);
        }

        @Override
        public String getAsString() {
            return ((ScipioExtendedObjectWrapper) wrapper).getEncoder().encode(super.getAsString());
        }
    }


    /*
     **************************************************************
     * Model factories
     **************************************************************
     * NOTE: implementations should be plugged in using config/freemarkerWrapperFactories.properties files.
     * The format is:
     * factoryName.class=org.ofbiz...
     * factoryName.priority=0-1000
     * factoryName.scope=[all|basic|extended|basic-beans|basic-default|extended-beans|extended-default]
     */

    /**
     * Base model factory interface, based off of Freemarker's ModelFactory.
     */
    public interface ScipioModelFactory {
        /**
         * Creates an appropriate model for the object or returns null to delegate wrapping.
         */
        TemplateModel wrap(Object object, ScipioObjectWrapper objectWrapper) throws TemplateModelException;
    }

    /**
     * Turns a Freemarker ModelFactory into a ScipioModelFactory.
     */
    public static class ScipioFtlModelFactoryAdapter implements ScipioModelFactory {
        private final ModelFactory modelFactory;

        public ScipioFtlModelFactoryAdapter(ModelFactory modelFactory) {
            this.modelFactory = modelFactory;
        }

        @Override
        public TemplateModel wrap(Object object, ScipioObjectWrapper objectWrapper) {
            return modelFactory.create(object, objectWrapper);
        }
    }

    /**
     * SCIPIO: 2017-04-03: Reimplements HtmlWidget.ExtendedWrapper using composition.
     * This is plugged into the system through the freemarkerWrapperFactories.properties file.
     */
    public static class AutoEscapingModelFactory implements ScipioModelFactory {
        @Override
        public TemplateModel wrap(Object object, ScipioObjectWrapper objectWrapper) {
            if (object instanceof String) {
                return new StringWrapperForFtl((String) object, (ScipioExtendedObjectWrapper) objectWrapper);
            } else if (object instanceof Collection && !(object instanceof Map)) {
                // An additional wrapper to ensure ${aCollection} is properly encoded for html
                return new CollectionWrapperForFtl((Collection<?>) object, (ScipioExtendedObjectWrapper) objectWrapper);
            }
            return null;
        }
    }


    /*
     **************************************************************
     * Object wrapper general interfaces
     **************************************************************
     * NOTE: These are used mostly in type checking, but custom object
     * wrappers should implement the specific interfaces rather than just these.
     * NOTE: Here we treat "BeansWrapper" and "DefaultObjectWrapper" as EXCLUSIVE -
     * they should NOT be on the same class - unlike what freemarker does with DefaultObjectWrapper
     * extending BeansWrapper - otherwise type checks become hard.
     */

    /**
     * Special base tag interface used to recognized Scipio/OFbiz custom object wrappers.
     */
    public interface ScipioObjectWrapper extends ObjectWrapper {
    }

    /**
     * Special tag interface used to recognized "basic" custom object wrappers - non-extended.
     */
    public interface ScipioBasicObjectWrapper extends ScipioObjectWrapper {
    }

    /**
     * Special tag interface used to recognized the "extended" object wrappers - that provide
     * the legacy ofbiz auto html-escaping. The word "extended" is from legacy Ofbiz code.
     * Typically the wrap function will return EscapingModel instances for string types.
     * <p>
     * TODO?: could support dynamic language?... in ofbiz was hardcoded to "html"...
     */
    public interface ScipioExtendedObjectWrapper extends ScipioObjectWrapper {
        public String getEncodeLang();
        public SimpleEncoder getEncoder();
    }

    /**
     * Special tag interface used to recognized classes that extend BeansWrapper but NOT DefaultObjectWrapper.
     */
    public interface ScipioBeansWrapper extends ScipioObjectWrapper {
    }

    /**
     * Special tag interface used to recognized classes that extend DefaultObjectWrapper.
     */
    public interface ScipioDefaultObjectWrapper extends ScipioObjectWrapper {
    }


    /*
     **************************************************************
     * Object wrapper specific interfaces
     **************************************************************
     * NOTE: Custom object wrappers should implement one of these.
     */

    /**
     * Basic BeansWrapper - corresponds to the stock ofbiz wrapper used in FreeMarkerWorker
     * that performs no automatic html escaping.
     * NOTE: Implementing class must extend BeansWrapper.
     */
    public interface ScipioBasicBeansWrapper extends ScipioBasicObjectWrapper, ScipioBeansWrapper {
    }

    /**
     * Basic DefaultObjectWrapper - a different object wrapper than the one usually used in ofbiz,
     * that can be used to return simple types such as simple maps instead of complex BeanModels.
     * NOTE: Implementing class must extend DefaultObjectWrapper.
     */
    public interface ScipioBasicDefaultObjectWrapper extends ScipioBasicObjectWrapper, ScipioDefaultObjectWrapper {
    }

    /**
     * Extended BeansWrapper - reimplementation of the legacy ofbiz HtmlWidget.ExtendedWrapper that
     * performs html auto-escaping on top of returns complex BeanModels.
     * NOTE: Implementing class must extend BeansWrapper.
     */
    public interface ScipioExtendedBeansWrapper extends ScipioExtendedObjectWrapper, ScipioBeansWrapper {
    }

    /**
     * Extended DefaultObjectWrapper - an extra combination that returns simple types but will
     * perform html auto-escaping on the strings.
     * NOTE: Implementing class must extend DefaultObjectWrapper.
     * <p>
     * WARN: 2017-04-03: NOT YET OFFICIALLY SUPPORTED
     * FIXME: there is code throughout LangFtlUtil and Ofbiz that won't recognize this properly.
     * only ScipioExtendedBeansWrapper can be relied on currently.
     */
    public interface ScipioExtendedDefaultObjectWrapper extends ScipioExtendedObjectWrapper, ScipioDefaultObjectWrapper {
    }


    /*
     **************************************************************
     * Object wrapper implementations (of specific interfaces)
     **************************************************************
     */

    public static class ScipioBasicBeansWrapperImpl extends BeansWrapper implements ScipioBasicBeansWrapper {
        private static final List<ScipioModelFactory> systemWrapperFactories = Collections.unmodifiableList(makeCustomFactoryList(customWrapperFactoryInfo, "basic-beans"));

        private final List<ScipioModelFactory> customWrapperFactories;

        public ScipioBasicBeansWrapperImpl(BeansWrapperConfiguration arg0, boolean arg1, boolean arg2, Collection<? extends ScipioModelFactory> customWrapperFactories) {
            super(arg0, arg1, arg2);
            this.customWrapperFactories = makeCleanFactoryList(customWrapperFactories);
        }

        public ScipioBasicBeansWrapperImpl(BeansWrapperConfiguration bwConf, boolean writeProtected, Collection<? extends ScipioModelFactory> customWrapperFactories) {
            super(bwConf, writeProtected);
            this.customWrapperFactories = makeCleanFactoryList(customWrapperFactories);
        }

        public ScipioBasicBeansWrapperImpl(Version incompatibleImprovements, Collection<? extends ScipioModelFactory> customWrapperFactories) {
            super(incompatibleImprovements);
            this.customWrapperFactories = makeCleanFactoryList(customWrapperFactories);
        }

        public static ScipioBasicBeansWrapperImpl create(Version incompatibleImprovements) {
            return new ScipioBasicBeansWrapperImpl(incompatibleImprovements, systemWrapperFactories);
        }

        public static ScipioBasicBeansWrapperImpl create(Version incompatibleImprovements, Boolean simpleMapWrapper) {
            ScipioBasicBeansWrapperImpl wrapper = new ScipioBasicBeansWrapperImpl(incompatibleImprovements, systemWrapperFactories);
            if (simpleMapWrapper != null) {
                wrapper.setSimpleMapWrapper(simpleMapWrapper);
            }
            return wrapper;
        }

        public static List<ScipioModelFactory> getSystemWrapperFactories() {
            return systemWrapperFactories;
        }

        public List<ScipioModelFactory> getCustomWrapperFactories() {
            return Collections.unmodifiableList(customWrapperFactories);
        }

        protected List<ScipioModelFactory> getCustomWrapperFactoriesInternal() {
            return customWrapperFactories;
        }

        @Override
        public TemplateModel wrap(Object object) throws TemplateModelException {
            if (object != null) {
                for(ScipioModelFactory factory : getCustomWrapperFactoriesInternal()) {
                    TemplateModel model = factory.wrap(object, this);
                    if (model != null) return model;
                }
            }
            return super.wrap(object);
        }
    }

    public static class ScipioBasicDefaultObjectWrapperImpl extends DefaultObjectWrapper implements ScipioBasicDefaultObjectWrapper {
        private static final List<ScipioModelFactory> systemWrapperFactories = Collections.unmodifiableList(makeCustomFactoryList(customWrapperFactoryInfo, "basic-default"));

        private final List<ScipioModelFactory> customWrapperFactories;

        public ScipioBasicDefaultObjectWrapperImpl(BeansWrapperConfiguration bwCfg, boolean writeProtected, Collection<? extends ScipioModelFactory> customWrapperFactories) {
            super(bwCfg, writeProtected);
            this.customWrapperFactories = makeCleanFactoryList(customWrapperFactories);
        }

        public ScipioBasicDefaultObjectWrapperImpl(DefaultObjectWrapperConfiguration dowCfg, boolean writeProtected, Collection<? extends ScipioModelFactory> customWrapperFactories) {
            super(dowCfg, writeProtected);
            this.customWrapperFactories = makeCleanFactoryList(customWrapperFactories);
        }

        public ScipioBasicDefaultObjectWrapperImpl(Version incompatibleImprovements, Collection<? extends ScipioModelFactory> customWrapperFactories) {
            super(incompatibleImprovements);
            this.customWrapperFactories = makeCleanFactoryList(customWrapperFactories);
        }

        public static ScipioBasicDefaultObjectWrapperImpl create(Version incompatibleImprovements) {
            return new ScipioBasicDefaultObjectWrapperImpl(incompatibleImprovements, systemWrapperFactories);
        }

        public static ScipioBasicDefaultObjectWrapperImpl create(Version incompatibleImprovements,
                Boolean simpleMapWrapper, Boolean useAdaptersForContainers) {
            ScipioBasicDefaultObjectWrapperImpl wrapper = new ScipioBasicDefaultObjectWrapperImpl(incompatibleImprovements, systemWrapperFactories);
            if (simpleMapWrapper != null) {
                wrapper.setSimpleMapWrapper(simpleMapWrapper);
            }
            if (useAdaptersForContainers != null) {
                wrapper.setUseAdaptersForContainers(useAdaptersForContainers);
            }
            return wrapper;
        }

        public static List<ScipioModelFactory> getSystemWrapperFactories() {
            return systemWrapperFactories;
        }

        public List<ScipioModelFactory> getCustomWrapperFactories() {
            return Collections.unmodifiableList(customWrapperFactories);
        }

        protected List<ScipioModelFactory> getCustomWrapperFactoriesInternal() {
            return customWrapperFactories;
        }

        @Override
        public TemplateModel wrap(Object object) throws TemplateModelException {
            if (object != null) {
                for(ScipioModelFactory factory : getCustomWrapperFactoriesInternal()) {
                    TemplateModel model = factory.wrap(object, this);
                    if (model != null) return model;
                }
            }
            return super.wrap(object);
        }
    }

    public static class ScipioExtendedBeansWrapperImpl extends BeansWrapper implements ScipioExtendedBeansWrapper {
        private static final List<ScipioModelFactory> systemWrapperFactories = Collections.unmodifiableList(makeCustomFactoryList(customWrapperFactoryInfo, "extended-beans"));

        private final List<ScipioModelFactory> customWrapperFactories;
        private final String encodeLang;
        private final SimpleEncoder encoder;

        public ScipioExtendedBeansWrapperImpl(BeansWrapperConfiguration arg0, boolean arg1, boolean arg2, Collection<? extends ScipioModelFactory> customWrapperFactories, String encodeLang) {
            super(arg0, arg1, arg2);
            this.customWrapperFactories = makeCleanFactoryList(customWrapperFactories);
            this.encodeLang = encodeLang;
            this.encoder = UtilCodec.getEncoder(this.encodeLang);
        }

        public ScipioExtendedBeansWrapperImpl(BeansWrapperConfiguration bwConf, boolean writeProtected, Collection<? extends ScipioModelFactory> customWrapperFactories, String encodeLang) {
            super(bwConf, writeProtected);
            this.customWrapperFactories = makeCleanFactoryList(customWrapperFactories);
            this.encodeLang = encodeLang;
            this.encoder = UtilCodec.getEncoder(this.encodeLang);
        }

        public ScipioExtendedBeansWrapperImpl(Version incompatibleImprovements, Collection<? extends ScipioModelFactory> customWrapperFactories, String encodeLang) {
            super(incompatibleImprovements);
            this.customWrapperFactories = makeCleanFactoryList(customWrapperFactories);
            this.encodeLang = encodeLang;
            this.encoder = UtilCodec.getEncoder(this.encodeLang);
        }

        public static ScipioExtendedBeansWrapperImpl create(Version incompatibleImprovements, String encodeLang) {
            return new ScipioExtendedBeansWrapperImpl(incompatibleImprovements, systemWrapperFactories, encodeLang);
        }

        public static ScipioExtendedBeansWrapperImpl create(Version incompatibleImprovements, String encodeLang, Boolean simpleMapWrapper) {
            ScipioExtendedBeansWrapperImpl wrapper = new ScipioExtendedBeansWrapperImpl(incompatibleImprovements, systemWrapperFactories, encodeLang);
            if (simpleMapWrapper != null) {
                wrapper.setSimpleMapWrapper(simpleMapWrapper);
            }
            return wrapper;
        }

        public static List<ScipioModelFactory> getSystemWrapperFactories() {
            return systemWrapperFactories;
        }

        public List<ScipioModelFactory> getCustomWrapperFactories() {
            return Collections.unmodifiableList(customWrapperFactories);
        }

        protected List<ScipioModelFactory> getCustomWrapperFactoriesInternal() {
            return customWrapperFactories;
        }

        @Override
        public String getEncodeLang() {
            return encodeLang;
        }

        @Override
        public SimpleEncoder getEncoder() {
            return encoder;
        }

        @Override
        public TemplateModel wrap(Object object) throws TemplateModelException {
            if (object != null) {
                for(ScipioModelFactory factory : getCustomWrapperFactoriesInternal()) {
                    TemplateModel model = factory.wrap(object, this);
                    if (model != null) return model;
                }
            }
            return super.wrap(object);
        }
    }

    public static class ScipioExtendedDefaultObjectWrapperImpl extends DefaultObjectWrapper implements ScipioExtendedDefaultObjectWrapper {
        private static final List<ScipioModelFactory> systemWrapperFactories = Collections.unmodifiableList(makeCustomFactoryList(customWrapperFactoryInfo, "extended-default"));

        private final List<ScipioModelFactory> customWrapperFactories;
        private final String encodeLang;
        private final SimpleEncoder encoder;

        public ScipioExtendedDefaultObjectWrapperImpl(BeansWrapperConfiguration bwCfg, boolean writeProtected, Collection<? extends ScipioModelFactory> customWrapperFactories, String encodeLang) {
            super(bwCfg, writeProtected);
            this.customWrapperFactories = makeCleanFactoryList(customWrapperFactories);
            this.encodeLang = encodeLang;
            this.encoder = UtilCodec.getEncoder(this.encodeLang);
        }

        public ScipioExtendedDefaultObjectWrapperImpl(DefaultObjectWrapperConfiguration dowCfg, boolean writeProtected, Collection<? extends ScipioModelFactory> customWrapperFactories, String encodeLang) {
            super(dowCfg, writeProtected);
            this.customWrapperFactories = makeCleanFactoryList(customWrapperFactories);
            this.encodeLang = encodeLang;
            this.encoder = UtilCodec.getEncoder(this.encodeLang);
        }

        public ScipioExtendedDefaultObjectWrapperImpl(Version incompatibleImprovements, Collection<? extends ScipioModelFactory> customWrapperFactories, String encodeLang) {
            super(incompatibleImprovements);
            this.customWrapperFactories = makeCleanFactoryList(customWrapperFactories);
            this.encodeLang = encodeLang;
            this.encoder = UtilCodec.getEncoder(this.encodeLang);
        }

        public static ScipioExtendedDefaultObjectWrapperImpl create(Version incompatibleImprovements, String encodeLang) {
            return new ScipioExtendedDefaultObjectWrapperImpl(incompatibleImprovements, systemWrapperFactories, encodeLang);
        }

        public static ScipioExtendedDefaultObjectWrapperImpl create(Version incompatibleImprovements, String encodeLang,
                Boolean simpleMapWrapper, Boolean useAdaptersForContainers) {
            ScipioExtendedDefaultObjectWrapperImpl wrapper = new ScipioExtendedDefaultObjectWrapperImpl(incompatibleImprovements, systemWrapperFactories, encodeLang);
            if (simpleMapWrapper != null) {
                wrapper.setSimpleMapWrapper(simpleMapWrapper);
            }
            if (useAdaptersForContainers != null) {
                wrapper.setUseAdaptersForContainers(useAdaptersForContainers);
            }
            return wrapper;
        }

        public static List<ScipioModelFactory> getSystemWrapperFactories() {
            return systemWrapperFactories;
        }

        public List<ScipioModelFactory> getCustomWrapperFactories() {
            return Collections.unmodifiableList(customWrapperFactories);
        }

        protected List<ScipioModelFactory> getCustomWrapperFactoriesInternal() {
            return customWrapperFactories;
        }

        @Override
        public String getEncodeLang() {
            return encodeLang;
        }

        @Override
        public SimpleEncoder getEncoder() {
            return encoder;
        }

        @Override
        public TemplateModel wrap(Object object) throws TemplateModelException {
            if (object != null) {
                for(ScipioModelFactory factory : getCustomWrapperFactoriesInternal()) {
                    TemplateModel model = factory.wrap(object, this);
                    if (model != null) return model;
                }
            }
            return super.wrap(object);
        }
    }


    /*
     **************************************************************
     * Object Wrapper Factories
     **************************************************************
     */

    /**
     * Scipio ObjectWrapper Factory - can be used to plug in replacements into the system
     * at the global level.
     * FIXME: FreeMarkerWorker currently forces the return types as indicated, because Ofbiz code does not support
     * anything else and allowing different ones is another major task.
     */
    public interface ScipioObjectWrapperFactory {

        /**
         * FIXME: the result will always be cast to BeansWrapper.
         */
        ScipioObjectWrapper getDefaultOfbizWrapper(Version version);

        /**
         * FIXME: the result will always be cast to BeansWrapper.
         */
        ScipioObjectWrapper getDefaultOfbizSimpleMapWrapper(Version version);

        /**
         * FIXME: the result will always be cast to DefaultObjectWrapper.
         */
        ScipioObjectWrapper getDefaultSimpleTypeWrapper(Version version);

        /**
         * FIXME: the result will always be cast to DefaultObjectWrapper.
         */
        ScipioObjectWrapper getDefaultSimpleTypeCopyingWrapper(Version version);

        /**
         * FIXME: the result will always be cast to BeansWrapper.
         */
        ScipioExtendedObjectWrapper getExtendedWrapper(Version version, String encodeLang);

        /**
         * FIXME: the result will always be cast to BeansWrapper.
         */
        ScipioExtendedObjectWrapper getExtendedSimpleMapWrapper(Version version, String encodeLang);
    }

    public static class DefaultScipioObjectWrapperFactory implements ScipioObjectWrapperFactory {

        @Override
        public ScipioObjectWrapper getDefaultOfbizWrapper(Version version) {
            return ScipioBasicBeansWrapperImpl.create(version);
        }

        @Override
        public ScipioObjectWrapper getDefaultOfbizSimpleMapWrapper(Version version) {
            return ScipioBasicBeansWrapperImpl.create(version, true);
        }

        @Override
        public ScipioObjectWrapper getDefaultSimpleTypeWrapper(Version version) {
            return ScipioBasicDefaultObjectWrapperImpl.create(version, true, true);
        }

        @Override
        public ScipioObjectWrapper getDefaultSimpleTypeCopyingWrapper(Version version) {
            return ScipioBasicDefaultObjectWrapperImpl.create(version, true, false);
        }

        @Override
        public ScipioExtendedObjectWrapper getExtendedWrapper(Version version, String encodeLang) {
            return ScipioExtendedBeansWrapperImpl.create(FreeMarkerWorker.version, encodeLang);
        }

        @Override
        public ScipioExtendedObjectWrapper getExtendedSimpleMapWrapper(Version version, String encodeLang) {
            return ScipioExtendedBeansWrapperImpl.create(FreeMarkerWorker.version, encodeLang, true);
        }

    }

    /**
     * Gets the configured system object wrapper factory.
     */
    public static ScipioObjectWrapperFactory getSystemObjectWrapperFactory() {
        return systemObjectWrapperFactory;
    }

    /**
     * Gets the default Scipio object wrapper factory (not configurable).
     */
    public static ScipioObjectWrapperFactory getDefaultObjectWrapperFactory() {
        return defaultObjectWrapperFactory;
    }

    public static ScipioObjectWrapperFactory readPropertyObjectWrapperFactory(String propertyResource, String propertyName, ScipioObjectWrapperFactory defaultValue) {
        String clsName = UtilProperties.getPropertyValue(propertyResource, propertyName);
        if (clsName == null || clsName.isEmpty()) {
            return defaultValue;
        }
        try {
            Class<?> cls = Thread.currentThread().getContextClassLoader().loadClass(clsName);
            return (ScipioObjectWrapperFactory) cls.newInstance();
        } catch(Exception e) {
            Debug.logError(e, "Error reading or instantiating ScipioObjectWrapperFactory: " + e.getMessage(), module);
            return defaultValue;
        }
    }

    /*
     **************************************************************
     * Utilities
     **************************************************************
     */


    /**
     * Reads freemarkerWrapperFactories.properties files, with entries in the format:
     * factoryName.class=org.ofbiz...
     * factoryName.priority=0-1000
     * factoryName.scope=[all|basic|extended|basic-beans|basic-default|extended-beans|extended-default]
     */
    static List<CustomFactoryInfo> readCustomWrapperFactories() {
        ArrayList<CustomFactoryInfo> infoList = new ArrayList<>();

        ClassLoader loader = Thread.currentThread().getContextClassLoader();
        Enumeration<URL> resources;
        try {
            resources = loader.getResources("freemarkerWrapperFactories.properties");
        } catch (IOException e) {
            Debug.logError(e, "Could not load list of freemarkerWrapperFactories.properties", module);
            throw UtilMisc.initCause(new InternalError(e.getMessage()), e);
        }
        while (resources.hasMoreElements()) {
            URL propertyURL = resources.nextElement();
            Debug.logInfo("loading properties: " + propertyURL, module);
            Properties props = UtilProperties.getProperties(propertyURL);
            if (UtilValidate.isEmpty(props)) {
                Debug.logError("Unable to locate properties file " + propertyURL, module);
            } else {
                for (Iterator<Object> i = props.keySet().iterator(); i.hasNext();) {
                    String key = (String) i.next();
                    if (key.endsWith(".class")) {
                        String prefix = key.substring(0, key.length() - 6);
                        String className = props.getProperty(key);
                        if (className != null) className = className.trim();

                        String priorityStr = props.getProperty(prefix + ".priority");
                        long priority = 1000;
                        if (priorityStr != null) {
                            priorityStr = priorityStr.trim();
                            if (priorityStr != null && priorityStr.length() > 0) {
                                try {
                                    priority = Long.parseLong(priorityStr);
                                } catch (Exception e) {
                                    Debug.logError(e, "Could not parse " + prefix + ".priority: " + priorityStr + " from " + propertyURL, module);
                                }
                            }
                        }

                        // supports: all (default), basic, extended,
                        // basic-beans, basic-default, extended-beans, extended-default
                        String scope = props.getProperty(prefix + ".scope");
                        if (scope != null) scope = scope.trim();
                        if (scope == null || scope.isEmpty()) scope = "all";

                        Class<?> cls = null;
                        try {
                            cls = loader.loadClass(className);
                        } catch(Exception e) {
                            Debug.logError(e, "Could not find class: " + className + " from " + propertyURL, module);
                        }
                        if (cls != null) {
                            ScipioModelFactory factory = null;
                            try {
                                Object inst = cls.newInstance();
                                if (inst instanceof ScipioModelFactory) {
                                    factory = (ScipioModelFactory) inst;
                                } else if (inst instanceof ModelFactory) {
                                    factory = new ScipioFtlModelFactoryAdapter((ModelFactory) inst);
                                } else {
                                    throw new ClassCastException("factory class is not ScipioModelFactory or ModelFactory");
                                }
                            } catch(Exception e) {
                                Debug.logError(e, "Could not instantiate class: " + className + " from " + propertyURL, module);
                            }
                            if (factory != null) {
                                infoList.add(new CustomFactoryInfo(priority, factory, scope));
                                if (Debug.verboseOn()) {
                                    Debug.logVerbose("Registering Freemarker custom wrapper factory " + className + " with priority " + priority, module);
                                }
                            }
                        }
                    }
                }
            }
        }

        sortCustomFactoryInfoList(infoList);
        infoList.trimToSize();

        String out = "Scipio Custom Freemarker Wrapper Factories Loaded (" + infoList.size() + ")\n";
        for(CustomFactoryInfo info : infoList) {
            out += info + "\n";
        }
        Debug.logInfo(out, module);

        return infoList;
    }

    static void sortCustomFactoryInfoList(List<CustomFactoryInfo> infoList) {
        Collections.sort(infoList, new Comparator<CustomFactoryInfo>() {
            @Override
            public int compare(CustomFactoryInfo o1, CustomFactoryInfo o2) {
                return Long.compare(o1.getPriority(), o2.getPriority());
            }
        });
    }

    static List<ScipioModelFactory> makeCustomFactoryList(List<CustomFactoryInfo> infoList, String scope) {
        ArrayList<ScipioModelFactory> factories = new ArrayList<>(infoList.size());
        for(CustomFactoryInfo info : infoList) {
            if ("all".equals(scope) || "all".equals(info.getScope()) || scope.contains(info.getScope())) {
                factories.add(info.getFactory());
            }
        }
        factories.trimToSize();

        String out = "Scipio Custom Freemarker Wrapper Factories Loaded (" + factories.size() + ") for object wrapper [" + scope + "]\n";
        for(ScipioModelFactory factory : factories) {
            out += factory.getClass().getName() + "\n";
        }
        Debug.logInfo(out, module);

        return factories;
    }

    static class CustomFactoryInfo {
        private final long priority;
        private final ScipioModelFactory factory;
        private final String scope;
        public CustomFactoryInfo(long priority, ScipioModelFactory factory, String scope) {
            this.priority = priority;
            this.factory = factory;
            this.scope = scope;
        }
        public long getPriority() {
            return priority;
        }
        public ScipioModelFactory getFactory() {
            return factory;
        }
        public String getScope() {
            return scope;
        }
        @Override
        public String toString() {
            return "[factory: " + factory.getClass().getName() + "; scope: " + scope + "; priority: " + priority + "]";
        }
    }

    static List<ScipioModelFactory> makeCleanFactoryList(Collection<? extends ScipioModelFactory> factories) {
        ArrayList<ScipioModelFactory> list = new ArrayList<>(factories);
        list.trimToSize();
        return list;
    }
}
