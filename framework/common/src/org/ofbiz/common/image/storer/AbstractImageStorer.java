package org.ofbiz.common.image.storer;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.image.AbstractImageOp;
import org.ofbiz.common.image.MediaProfile;
import org.ofbiz.entity.Delegator;

import java.awt.image.RenderedImage;
import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

public abstract class AbstractImageStorer extends AbstractImageOp implements ImageStorer {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected final Set<String> formats;
    protected ProfileMatchInfo profileMatch;

    protected AbstractImageStorer(AbstractImageStorerFactory<? extends AbstractImageStorer> factory, String name, Map<String, Object> confOptions, Map<String, Object> defaultOptions) {
        super(factory, name, confOptions, defaultOptions);
        Set<String> formats = Collections.unmodifiableSet(StringUtil.splitNames(new LinkedHashSet<>(), (String) confOptions.get("formats")));
        if (formats.contains("*")) {
            formats = Collections.emptySet();
        }
        this.formats = formats;
    }

    // NOTE: ugly 2 parameters required to keep hierarchy consistent
    public static abstract class AbstractImageStorerFactory<T extends AbstractImageStorer> extends AbstractImageOp.AbstractImageOpFactory<AbstractImageStorer, ImageStorer> implements ImageStorer.ImageStorerFactory {
        public Map<String, Object> putCommonValidOptions(Map<String, Object> validOptions, Map<String, Object> options) {
            putOption(validOptions, "sequenceNum", UtilMisc.toInteger(options.get("sequenceNum"), null), options);
            putOption(validOptions, "formats", options.get("formats"), options);
            return validOptions;
        }
    }

    @Override
    public boolean isApplicable(RenderedImage im, String formatName, Object output, String mediaProfile, Map<String, Object> options, Delegator delegator) {
        return isApplicableCommon(im, formatName, output, mediaProfile, options, delegator);
    }

    public boolean isApplicableCommon(RenderedImage im, String formatName, Object output, String mediaProfile, Map<String, Object> options, Delegator delegator) {
        return isFormatApplicable(formatName, delegator) && isMediaProfileApplicable(mediaProfile, delegator);
    }

    public Set<String> getFormats() {
        return formats;
    }

    protected boolean isFormatApplicable(String formatName, Delegator delegator) {
        return (getFormats().isEmpty()) ? true : getFormats().contains(formatName);
    }

    protected boolean isMediaProfileApplicable(String mediaProfile, Delegator delegator) {
        return getProfileMatch().isMediaProfileApplicable(mediaProfile, delegator);
    }

    @Override
    public boolean isNativeSupportedDestImagePixelType(int imagePixelType) {
        //return false; // not applicable? class design issue
        throw new UnsupportedOperationException();
    }

    public ProfileMatchInfo getProfileMatch() {
        ProfileMatchInfo profileMatch = this.profileMatch;
        if (profileMatch == null) {
            profileMatch = ProfileMatchInfo.fromExpr((String) getConfiguredAndDefaultOptions().get("mediaProfiles"));
            this.profileMatch = profileMatch;
        }
        return profileMatch;
    }

    protected static class ProfileMatchInfo implements Serializable {
        protected static final ProfileMatchInfo ALL = new ProfileMatchInfo(null, null);
        protected final Set<String> matchExactProfiles;
        protected final Set<String> matchParentProfiles;

        public ProfileMatchInfo(Set<String> matchExactProfiles, Set<String> matchParentProfiles) {
            this.matchExactProfiles = UtilValidate.isNotEmpty(matchExactProfiles) ?
                    Collections.unmodifiableSet(new LinkedHashSet<>(matchExactProfiles)) : null;
            this.matchParentProfiles = UtilValidate.isNotEmpty(matchParentProfiles) ?
                    Collections.unmodifiableSet(new LinkedHashSet<>(matchParentProfiles)) : null;
        }

        public static ProfileMatchInfo fromExpr(String expr) {
            if (UtilValidate.isEmpty(expr)) {
                return ALL;
            }
            Set<String> matchExactProfiles = new LinkedHashSet<>();
            Set<String> matchParentProfiles = new LinkedHashSet<>();
            String[] parts = expr.split(",");
            for(String part : parts) {
                if (part.isEmpty()) {
                    continue;
                } else if (part.endsWith(">*")) {
                    matchParentProfiles.add(part.substring(0, part.length() - ">*".length()));
                } else {
                    matchExactProfiles.add(part);
                }
            }
            if (matchExactProfiles.isEmpty() && matchParentProfiles.isEmpty()) {
                return ALL;
            }
            return new ProfileMatchInfo(matchExactProfiles, matchParentProfiles);
        }

        public boolean isAll() { return matchExactProfiles == null && matchParentProfiles == null; }

        public boolean isMediaProfileApplicable(String mediaProfile, Delegator delegator) {
            if (isAll() || (matchExactProfiles != null && matchExactProfiles.contains(mediaProfile))) {
                return true;
            }
            if (matchParentProfiles == null) {
                return false;
            }
            MediaProfile profile = MediaProfile.getMediaProfile(delegator, mediaProfile);
            if (profile == null) {
                return false;
            }
            Collection<String> parentProfiles = profile.getAncestorProfiles();
            for(String name : matchParentProfiles) {
                if (parentProfiles.contains(name)) {
                    return true;
                }
            }
            return false;
        }
    }

}
