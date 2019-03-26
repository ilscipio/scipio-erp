package org.ofbiz.widget.model;

import java.io.Serializable;
import java.util.Map;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;

/**
 * SCIPIO: Classes and utilities for handling screen fallbacks (new in Scipio).
 * <p>
 * These classes help prevent needless code execution when dealing with fallbacks.
 * Models should store SimpleFlexibleScreenFallbackSettings and then call
 * SimpleFlexibleScreenFallbackSettings.getResolvedForScreenLogic before passing to a method
 * which needs to check the settings.
 */
public abstract class ScreenFallback {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final ScreenFallbackSettings defaultFallbackSettings = new SimpleScreenFallbackSettings("", "", null);

    private ScreenFallback() {
    }

    public static ScreenFallbackSettings getDefaultFallbackSettings() {
        return defaultFallbackSettings;
    }

    public interface ScreenFallbackSettings {

        String getName();
        String getLocation();
        Boolean getFallbackIfEmpty();

        boolean isEnabled();
        boolean isEnabledForEmptyLocation();

        /**
         * Optional operation. Usually used for optimization reasons.
         * <p>
         * For some classes (SimpleFlexible*) this does not make much sense (though still possible).
         * Makes most sense for fully-resolving classes such as SmartResolvedFlexible*.
         */
        ScreenFallbackSettings getResolved();
    }

    public static class SimpleScreenFallbackSettings implements ScreenFallbackSettings {

        protected final String name;
        protected final String location;
        protected final Boolean fallbackIfEmpty;

        public SimpleScreenFallbackSettings(String name, String location, Boolean fallbackIfEmpty) {
            this.name = name;
            this.location = location;
            this.fallbackIfEmpty = fallbackIfEmpty;
        }

        @Override
        public String getName() {
            return name;
        }

        @Override
        public String getLocation() {
            return location;
        }

        @Override
        public Boolean getFallbackIfEmpty() {
            return fallbackIfEmpty;
        }

        @Override
        public boolean isEnabled() {
            return UtilValidate.isNotEmpty(location) || UtilValidate.isNotEmpty(name);
        }

        @Override
        public boolean isEnabledForEmptyLocation() {
            return Boolean.TRUE.equals(fallbackIfEmpty) && isEnabled();
        }

        @Override
        public ScreenFallbackSettings getResolved() {
            return this;
        }

    }

    public interface FlexibleScreenFallbackSettings extends ScreenFallbackSettings {

        FlexibleStringExpander getNameExdr();
        FlexibleStringExpander getLocationExdr();

        String getName(Map<String, Object> context);
        String getLocation(Map<String, Object> context);


        /**
         * Returns settings with ALL fields resolved (dumb, not appropriate for screen logic unless
         * doing everything manually using the returned settings).
         */
        ScreenFallbackSettings getResolvedAll(Map<String, Object> context);

        /**
         * Returns settings where fields are resolved in a smart manner appropriate for screen logic.
         * <p>
         * With the returned settings:
         * isEnabled* uses name/location BEFORE flexible expansion to check if should be considered enabled.
         * May delay resolution for performance reasons.
         */
        ScreenFallbackSettings getResolvedForScreenLogic(Map<String, Object> context);

    }

    @SuppressWarnings("serial")
    public static class SimpleFlexibleScreenFallbackSettings implements FlexibleScreenFallbackSettings, Serializable {

        protected final FlexibleStringExpander nameExdr;
        protected final FlexibleStringExpander locationExdr;
        protected final Boolean fallbackIfEmpty;

        public SimpleFlexibleScreenFallbackSettings(String nameExpr,
                String locationExpr, Boolean fallbackIfEmpty) {
            this.nameExdr = FlexibleStringExpander.getInstance(nameExpr);
            this.locationExdr = FlexibleStringExpander.getInstance(locationExpr);
            this.fallbackIfEmpty = fallbackIfEmpty;
        }

        public SimpleFlexibleScreenFallbackSettings(FlexibleStringExpander nameExdr,
                FlexibleStringExpander locationExdr, Boolean fallbackIfEmpty) {
            this.nameExdr = nameExdr;
            this.locationExdr = locationExdr;
            this.fallbackIfEmpty = fallbackIfEmpty;
        }

        /**
         * Copy constructor that uses existing defaults for any values that are null or empty (as appropriate).
         */
        public SimpleFlexibleScreenFallbackSettings(FlexibleScreenFallbackSettings existing, FlexibleStringExpander nameExdr,
                FlexibleStringExpander locationExdr, Boolean fallbackIfEmpty) {
            this(existing, new SimpleFlexibleScreenFallbackSettings(nameExdr, locationExdr, fallbackIfEmpty));
        }

        /**
         * Merge constructor that uses existing defaults for any values that are null or empty (as appropriate).
         */
        public SimpleFlexibleScreenFallbackSettings(FlexibleScreenFallbackSettings existing, FlexibleScreenFallbackSettings overwrite) {
            if (!overwrite.getNameExdr().getOriginal().isEmpty()) {
                this.nameExdr = overwrite.getNameExdr();
            } else {
                this.nameExdr = existing.getNameExdr();
            }
            if (!overwrite.getLocationExdr().getOriginal().isEmpty()) {
                this.locationExdr = overwrite.getLocationExdr();
            } else {
                this.locationExdr = existing.getLocationExdr();
            }
            if (overwrite.getFallbackIfEmpty() != null) {
                this.fallbackIfEmpty = overwrite.getFallbackIfEmpty();
            } else {
                this.fallbackIfEmpty = existing.getFallbackIfEmpty();
            }
        }

        @Override
        public String getName() {
            return getNameExdr().getOriginal();
        }

        @Override
        public String getLocation() {
            return getLocationExdr().getOriginal();
        }

        @Override
        public Boolean getFallbackIfEmpty() {
            return fallbackIfEmpty;
        }

        @Override
        public boolean isEnabled() {
            return !locationExdr.getOriginal().isEmpty() || !nameExdr.getOriginal().isEmpty();
        }

        @Override
        public boolean isEnabledForEmptyLocation() {
            return Boolean.TRUE.equals(this.fallbackIfEmpty) && isEnabled();
        }

        @Override
        public FlexibleStringExpander getNameExdr() {
            return nameExdr;
        }

        @Override
        public FlexibleStringExpander getLocationExdr() {
            return locationExdr;
        }

        @Override
        public String getName(Map<String, Object> context) {
            return nameExdr.expandString(context);
        }

        @Override
        public String getLocation(Map<String, Object> context) {
            return locationExdr.expandString(context);
        }

        @Override
        public ScreenFallbackSettings getResolved() {
            return new SimpleScreenFallbackSettings(nameExdr.getOriginal(),
                    locationExdr.getOriginal(),
                    fallbackIfEmpty
                    );
        }

        @Override
        public ScreenFallbackSettings getResolvedAll(Map<String, Object> context) {
            // TODO Auto-generated method stub
            return new SimpleScreenFallbackSettings(nameExdr.expandString(context),
                    locationExdr.expandString(context),
                    fallbackIfEmpty
                    );
        }

        @Override
        public ScreenFallbackSettings getResolvedForScreenLogic(Map<String, Object> context) {
            return new SmartResolvedFlexibleScreenFallbackSettings(this, context);
        }

    }

    /**
     * Special smart delayed-resolving implementation for screen logic.
     * <p>
     * isEnabled* calls check if name/location strings are empty BEFORE flexible string resolution.
     */
    public static class SmartResolvedFlexibleScreenFallbackSettings implements FlexibleScreenFallbackSettings {

        protected final FlexibleScreenFallbackSettings srcSettings;
        protected final Map<String, Object> context;
        protected ScreenFallbackSettings resolvedSettings = null;

        protected SmartResolvedFlexibleScreenFallbackSettings(FlexibleScreenFallbackSettings srcSettings,
                Map<String, Object> context) {
            this.srcSettings = srcSettings;
            this.context = context;
        }

        @Override
        public ScreenFallbackSettings getResolved() {
            // usually won't reuse the local, so no need to save it
            //resolve();
            //return resolvedSettings;
            return getResolvedAll(context);
        }

        @Override
        public ScreenFallbackSettings getResolvedAll(Map<String, Object> context) {
            return new SimpleScreenFallbackSettings(
                    srcSettings.getNameExdr().expandString(context),
                    srcSettings.getLocationExdr().expandString(context),
                    srcSettings.getFallbackIfEmpty()
                    );
        }

        @Override
        public ScreenFallbackSettings getResolvedForScreenLogic(Map<String, Object> context) {
            return new SmartResolvedFlexibleScreenFallbackSettings(this.srcSettings, context);
        }

        protected void resolve() {
            resolvedSettings = getResolvedAll(context);
        }

        @Override
        public boolean isEnabled() {
            // USES SOURCE SETTINGS to determine if enabled, PRE-resolution
            return srcSettings.isEnabled();
        }

        @Override
        public boolean isEnabledForEmptyLocation() {
            // USES SOURCE SETTINGS to determine if enabled, PRE-resolution
            // also, currently assuming that fallbackIfEmpty can always be resolved statically, as optimization
            // (fallbackIfEmpty does not support flexible expansion; not expected to change)
            return srcSettings.isEnabledForEmptyLocation();
        }

        @Override
        public String getName() {
            if (resolvedSettings == null) {
                resolve();
            }
            return resolvedSettings.getName();
        }

        @Override
        public String getLocation() {
            if (resolvedSettings == null) {
                resolve();
            }
            return resolvedSettings.getLocation();
        }

        @Override
        public Boolean getFallbackIfEmpty() {
            // NOTE: uses SOURCE settings
            // currently assuming that fallbackIfEmpty can always be resolved statically, as optimization
            // (fallbackIfEmpty does not support flexible expansion; not expected to change)
            return srcSettings.getFallbackIfEmpty();
        }


        @Override
        public FlexibleStringExpander getNameExdr() {
            return srcSettings.getNameExdr();
        }

        @Override
        public FlexibleStringExpander getLocationExdr() {
            return srcSettings.getLocationExdr();
        }

        @Override
        public String getName(Map<String, Object> context) {
            return srcSettings.getName(context);
        }

        @Override
        public String getLocation(Map<String, Object> context) {
            return srcSettings.getLocation(context);
        }

    }

}
