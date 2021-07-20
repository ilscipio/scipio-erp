package org.ofbiz.common.image.storer;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.image.AbstractImageOp;
import org.ofbiz.common.image.ImageProfile;
import org.ofbiz.common.image.MediaProfile;
import org.ofbiz.entity.Delegator;

import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.stream.ImageOutputStream;
import java.awt.image.RenderedImage;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
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
    public boolean isApplicable(RenderedImage im, String formatName, Object output, String imageProfile, Map<String, Object> options, Delegator delegator) {
        return isApplicableCommon(im, formatName, output, imageProfile, options, delegator);
    }

    public boolean isApplicableCommon(RenderedImage im, String formatName, Object output, String imageProfile, Map<String, Object> options, Delegator delegator) {
        return isFormatApplicable(formatName, delegator) && isImageProfileApplicable(imageProfile, delegator);
    }

    public Set<String> getFormats() {
        return formats;
    }

    protected boolean isFormatApplicable(String formatName, Delegator delegator) {
        return (getFormats().isEmpty()) ? true : getFormats().contains(formatName);
    }

    protected boolean isImageProfileApplicable(String imageProfile, Delegator delegator) {
        return getProfileMatch().isImageProfileApplicable(imageProfile, delegator);
    }

    @Override
    public boolean isNativeSupportedDestImagePixelType(int imagePixelType) {
        //return false; // not applicable? class design issue
        throw new UnsupportedOperationException();
    }

    public ProfileMatchInfo getProfileMatch() {
        ProfileMatchInfo profileMatch = this.profileMatch;
        if (profileMatch == null) {
            profileMatch = ProfileMatchInfo.fromExpr((String) getConfiguredAndDefaultOptions().get("imageProfiles"));
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

        public boolean isImageProfileApplicable(String imageProfile, Delegator delegator) {
            if (isAll() || (matchExactProfiles != null && matchExactProfiles.contains(imageProfile))) {
                return true;
            }
            if (matchParentProfiles == null) {
                return false;
            }
            ImageProfile profile = ImageProfile.getImageProfile(delegator, imageProfile, true);
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

    protected void setOutputAndWriteIOImage(ImageWriter writer, ImageWriteParam writeParam, RenderedImage im,
                                            String formatName, Object output, String imageProfile,
                                            Map<String, Object> options, Delegator delegator) throws IOException {
        // Configure the output on the ImageWriter
        ImageOutputStream tempStream = null;
        try {
            if (output instanceof ImageOutputStream) {
                writer.setOutput(output);
            } else if (output instanceof OutputStream) {
                tempStream = ImageIO.createImageOutputStream(output);
                writer.setOutput(tempStream);
            } else if (output instanceof File) {
                ((File) output).delete(); // see ImageIO.write
                tempStream = ImageIO.createImageOutputStream(output);
                writer.setOutput(tempStream);
                //writer.setOutput(new FileImageOutputStream((File) output));
            } else {
                // It'll do it for us if needed
                //throw new IOException("Unsupported output: " + (output != null ? output.getClass().getName() : output));
                writer.setOutput(output);
            }
            // Encode
            writer.write(null, new IIOImage(im, null, null), writeParam);
        } finally {
            if (tempStream != null) {
                tempStream.close();
            }
        }
    }
}
