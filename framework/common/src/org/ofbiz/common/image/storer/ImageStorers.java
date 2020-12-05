package org.ofbiz.common.image.storer;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.common.image.ImageUtil;
import org.ofbiz.entity.Delegator;

import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

/**
 * SCIPIO: ImageStorer instance registry, for image storers/writers/formatters, with helper {@link #write}
 * methods that replace java's ImageIO.write.
 */
public abstract class ImageStorers {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String IMAGE_PROP_SCALER_PREFIX = ImageUtil.IMAGEOP_PROP_PREFIX + "storer.";

    public static final String STORER_NAME_OPT = "storerName";

    private static final Map<String, ImageStorer> STORERS = Collections.unmodifiableMap(readStorers(
            ImageUtil.getAllPropertiesFiles(ImageUtil.IMAGEOP_PROP_RESOURCE), IMAGE_PROP_SCALER_PREFIX));
    private static final List<ImageStorer> STORER_LIST_PRIO = Collections.unmodifiableList(makeStorerPrioList(STORERS));
    private static final ImageStorer DEFAULT_STORER = extractDefaultStorer(STORERS, true, true);

    protected ImageStorers() {
    }

    /*
     * Static helper facade methods, based on ImageIO.write, for client code use
     */

    /**
     * Writes the image to the given output, typically ImageOutputStream, OutputStream or File (as ImageIO.write).
     *
     * @param im the input image
     * @param formatName the image format in short/extension format, as supported by ImageIO.write, usually "jpg", "webp", etc.
     * @param output either ImageOutputStream, OutputStream or File, as supported by ImageIO.write and FileExtension entity
     * @param options options for the ImageStorer and any additional options
     * @param delegator the delegator
     */
    public static boolean write(RenderedImage im, String formatName, Object output, String imageProfile, Map<String, Object> options, Delegator delegator) throws IOException {
        if (options == null) {
            options = Collections.emptyMap();
        }
        return getStorer(im, formatName, output, imageProfile, options, delegator).write(im, formatName, output, imageProfile, options, delegator);
    }

    @Deprecated
    public static boolean write(RenderedImage im, String formatName, Object output, Map<String, Object> options, Delegator delegator) throws IOException {
        return write(im, formatName, output, null, options, delegator);
    }

    @Deprecated
    public static boolean write(RenderedImage im, String formatName, Object output, Delegator delegator) throws IOException {
        return write(im, formatName, output, null, null, delegator);
    }

    public static byte[] writeBytes(BufferedImage image, String formatName, String imageProfile, Map<String, Object> options, Delegator delegator) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        write(image, formatName, baos, imageProfile, options, delegator);
        return baos.toByteArray();
    }

    @Deprecated
    public static byte[] writeBytes(BufferedImage image, String formatName, Delegator delegator) throws IOException {
        return writeBytes(image, formatName, null, null, delegator);
    }

    /**
     * Gets the first applicable storer for the given format and options.
     */
    public static ImageStorer getStorer(RenderedImage im, String formatName, Object output, String imageProfile, Map<String, Object> options, Delegator delegator) {
        for(ImageStorer storer : STORER_LIST_PRIO) {
            if (storer.isApplicable(im, formatName, output, imageProfile, options, delegator)) {
                return storer;
            }
        }
        return DEFAULT_STORER;
    }

    /** Returns an image storer by name. */
    public static ImageStorer getStorer(String name) {
        return STORERS.get(name);
    }

    /** Returns an image storer by name or the default storer if not found. */
    public static ImageStorer getStorerOrDefault(String name) {
        ImageStorer storer = getStorer(name);
        return (storer != null) ? storer : getDefaultStorer();
    }

    public static ImageStorer getDefaultStorer() {
        return DEFAULT_STORER;
    }

    protected static Map<String, ImageStorer> readStorers(Collection<Properties> propList, String propPrefix) {
        return ImageUtil.readImagePropsToImageOpMap(propList, propPrefix, ImageStorer.class);
    }

    protected static List<ImageStorer> makeStorerPrioList(Map<String, ImageStorer> storerMap) {
        Map<String, ImageStorer> filtered = new LinkedHashMap<>(storerMap);
        filtered.remove("default");
        // TODO: REVIEW: should this remove the storer pointed by default? no need for now
        ArrayList<ImageStorer> list = new ArrayList<>(filtered.size());
        for(ImageStorer storer : filtered.values()) {
            int sequenceNum = UtilMisc.toInteger(storer.getConfiguredOptions().get("sequenceNum"), 9999);
            if (sequenceNum >= 0) {
                list.add(storer);
            }
        }
        Collections.sort(list, new Comparator<ImageStorer>() {
            @Override
            public int compare(ImageStorer o1, ImageStorer o2) {
                int prio1 = UtilMisc.toInteger(o1.getConfiguredOptions().get("sequenceNum"), 9999);
                int prio2 = UtilMisc.toInteger(o2.getConfiguredOptions().get("sequenceNum"), 9999);
                return Integer.compare(prio1, prio2);
            }
        });
        list.trimToSize();
        Debug.logInfo("Loaded image storers (resolved order): " + list, module);
        return list;
    }

    protected static ImageStorer extractDefaultStorer(Map<String, ImageStorer> storerMap, boolean fallback, boolean log) {
        ImageStorer storer = storerMap.get("default");
        if (fallback && storer == null) {
            Debug.logWarning("No default storer configured: using stock default", module);
            try {
                storer = new ImageIOStorer.Factory().getImageOpInst("imageio", Collections.emptyMap());
            } catch(Exception e) {
                if (log) Debug.logError(e, "Could not instantiate stock default image storer; image writing will fail: " + e.getMessage(), module);
            }
        }
        if (log && storer != null) Debug.logInfo("Default image storer instance: " + storer.toString(), module);
        return storer;
    }
}
