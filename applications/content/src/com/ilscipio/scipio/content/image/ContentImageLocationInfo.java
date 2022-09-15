package com.ilscipio.scipio.content.image;


import com.ilscipio.scipio.ce.util.PathUtil;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceContainer;

import java.io.File;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public abstract class ContentImageLocationInfo implements Serializable {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private transient Delegator delegator;
    private final String delegatorName;
    private transient LocalDispatcher dispatcher;
    private final String dispatcherName;
    protected final String id;
    protected final ContentImageViewType imageViewType;
    protected final ImageVariantConfig variantConfig;
    protected final String imagePath; // may be path or URL
    protected final String imageFilename;
    protected Collection<String> sizeTypeList;
    protected final boolean useEntityCache;
    protected final boolean useProfileCache;

    protected String imageExtension;
    protected String imageServerPathExpr;
    protected String imageServerPath;
    protected String imageUrlPrefixExpr;
    protected String imageUrlPrefix;
    protected String imageFnFmtExpr;
    protected Map<String, Object> imagePathArgs;
    protected Map<String, ContentImageLocationInfo.VariantLocation> sizeTypeInfoMap;

    protected ContentImageLocationInfo(DispatchContext dctx, String id, ContentImageViewType imageViewType,
                                       ImageVariantConfig variantConfig, String imagePath, Collection<String> sizeTypeList,
                                       boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws IllegalArgumentException {
        this.delegator = dctx.getDelegator();
        this.delegatorName = this.delegator.getDelegatorName();
        this.dispatcher = dctx.getDispatcher();
        this.dispatcherName = this.dispatcher.getName();
        this.id = id;
        this.imageViewType = imageViewType;
        this.variantConfig = variantConfig;
        this.imagePath = imagePath;
        String imageFilename = imagePath;
        if (imageFilename.lastIndexOf("/") != -1) {
            imageFilename = imageFilename.substring(imageFilename.lastIndexOf("/") + 1);
        }
        if (imageFilename.lastIndexOf(".") <= 0 || imageFilename.lastIndexOf(".") >= (imageFilename.length() - 1)) { // SCIPIO: added this to prevent more serious problems
            throw new IllegalArgumentException("Original image filename [" + imagePath + "] has missing or improper file extension (image type)");
        }
        this.imageFilename = imageFilename;
        this.sizeTypeList = sizeTypeList;
        this.useEntityCache = useEntityCache;
        this.useProfileCache = useProfileCache;
    }

//    public abstract Factory<CategoryImageLocationInfo> readFactory();

//    public static CommonImageLocationInfoFactory getFactory() {
//        return FACTORY;
//    }

    public boolean isNull() throws GeneralException {
        return id == null;
    }

    public Delegator getDelegator() throws GeneralException {
        Delegator delegator = this.delegator;
        if (delegator == null) {
            delegator = DelegatorFactory.getDelegator(getDelegatorName());
            this.delegator = delegator;
        }
        return delegator;
    }

    public String getDelegatorName() throws GeneralException {
        return delegatorName;
    }

    public LocalDispatcher getDispatcher() throws GeneralException {
        LocalDispatcher dispatcher = this.dispatcher;
        if (dispatcher == null) {
            dispatcher = ServiceContainer.getLocalDispatcher(getDispatcherName(), this.getDelegator());
            this.dispatcher = dispatcher;
        }
        return dispatcher;
    }

    public String getDispatcherName() throws GeneralException {
        return dispatcherName;
    }

    public DispatchContext getDctx() throws GeneralException {
        return getDispatcher().getDispatchContext();
    }

    public ImageVariantConfig getVariantConfig() throws GeneralException {
        return variantConfig;
    }

    /**
     * Returns either a URL or a file path (used to extract {@link #getImageFilename()}, unreliable.
     */
    public String getImagePath() throws GeneralException {
        return imagePath;
    }

    public String getImageFilename() throws GeneralException {
        return imageFilename;
    }

    public Collection<String> getSizeTypeList() throws GeneralException {
        Collection<String> sizeTypeList = this.sizeTypeList;
        if (sizeTypeList == null) {
            if (getVariantConfig() != null) {
                sizeTypeList = getVariantConfig().getVariantNames();
            } else {
                throw new IllegalArgumentException("Could not determine a sizeTypeList (none passed or missing profile/variant config)");
            }
            this.sizeTypeList = sizeTypeList;
        }
        return sizeTypeList;
    }

    protected boolean isUseEntityCache() throws GeneralException {
        return useEntityCache;
    }

    protected boolean isUseProfileCache() throws GeneralException {
        return useProfileCache;
    }

    public ContentImageViewType getImageViewType() throws GeneralException {
        return imageViewType;
    }

    public String getImageServerPathExpr() throws GeneralException {
        String imageServerPathExpr = this.imageServerPathExpr;
        if (imageServerPathExpr == null) {
            imageServerPathExpr = readImageServerPathExpr();
            this.imageServerPathExpr = imageServerPathExpr;
        }
        return imageServerPathExpr;
    }

    protected String readImageServerPathExpr() throws GeneralException {
        String imageServerPathExpr = EntityUtilProperties.getPropertyValue("catalog", "image.server.path", getDelegator());
        try {
            imageServerPathExpr = FlexibleLocation.resolveFileUrlAsPathIfUrl(imageServerPathExpr, imageServerPathExpr);
        } catch (MalformedURLException e) {
            throw new GeneralException(e);
        }
        return imageServerPathExpr;
    }

    public String getImageServerPath() throws GeneralException {
        String imageServerPath = this.imageServerPath;
        if (imageServerPath == null) {
            imageServerPath = makeImageServerPath(getImageServerPathExpr(), getImagePathArgs());
            this.imageServerPath = imageServerPath;
        }
        return imageServerPath;
    }

    protected String makeImageServerPath(String imageServerPathExpr, Map<String, Object> imageContext) throws GeneralException {
        return PathUtil.removeTrailDelim(FlexibleStringExpander.expandString(imageServerPathExpr, imageContext));
    }

    public String getImageUrlPrefixExpr() throws GeneralException {
        String imageUrlPrefixExpr = this.imageUrlPrefixExpr;
        if (imageUrlPrefixExpr == null) {
            imageUrlPrefixExpr = readImageUrlPrefixExpr();
            this.imageUrlPrefixExpr = imageUrlPrefixExpr;
        }
        return imageUrlPrefixExpr;
    }

    protected String readImageUrlPrefixExpr() throws GeneralException {
        return EntityUtilProperties.getPropertyValue("catalog", "image.url.prefix", getDelegator());
    }

    public String getImageUrlPrefix() throws GeneralException {
        String imageUrlPrefix = this.imageUrlPrefix;
        if (imageUrlPrefix == null) {
            imageUrlPrefix = makeImageUrlPrefix(getImageUrlPrefixExpr(), getImagePathArgs());
            this.imageUrlPrefix = imageUrlPrefix;
        }
        return imageUrlPrefix;
    }

    protected String makeImageUrlPrefix(String imageUrlPrefixExpr, Map<String, Object> imageContext) throws GeneralException {
        return PathUtil.removeTrailDelim(FlexibleStringExpander.expandString(imageUrlPrefixExpr, imageContext));
    }

    public void setImageFnFmtExpr(String imageFnFmtExpr) throws GeneralException {
        this.imageFnFmtExpr = UtilValidate.isNotEmpty(imageFnFmtExpr) ? imageFnFmtExpr : null;
    }

    public String getImageFnFmtExpr() throws GeneralException {
        String imageFnFmt = this.imageFnFmtExpr;
        if (imageFnFmt == null) {
            imageFnFmt = readImageFnFmtExpr();
            this.imageFnFmtExpr = imageFnFmt;
        }
        return imageFnFmt;
    }

    protected String readImageFnFmtExpr() throws GeneralException {
        if (getImageViewType().isMain()) {
            return EntityUtilProperties.getPropertyValue("catalog", "image.filename.format", getDelegator());
        } else {
            return EntityUtilProperties.getPropertyValue("catalog", "image.filename.additionalviewsize.format", getDelegator());
        }
    }

    public String getImageExtension() throws GeneralException {
        String imageExtension = this.imageExtension;
        if (imageExtension == null) {
            imageExtension = imageFilename.substring(imageFilename.lastIndexOf(".") + 1);
            this.imageExtension = imageExtension;
        }
        return imageExtension;
    }

    public Map<String, Object> getImagePathArgs() throws GeneralException {
        Map<String, Object> imagePathArgs = this.imagePathArgs;
        if (imagePathArgs == null) {
            imagePathArgs = makeImagePathArg(new HashMap<>());
            this.imagePathArgs = imagePathArgs;
        }
        return imagePathArgs;
    }

    protected abstract Map<String, Object> makeImagePathArg(Map<String, Object> imagePathArgs) throws GeneralException;

    public Map<String, ContentImageLocationInfo.VariantLocation> getVariantLocations() throws GeneralException {
        Map<String, ContentImageLocationInfo.VariantLocation> sizeTypeInfoMap = this.sizeTypeInfoMap;
        if (sizeTypeInfoMap == null) {
            sizeTypeInfoMap = readVariantLocations();
            this.sizeTypeInfoMap = sizeTypeInfoMap;
        }
        return sizeTypeInfoMap;
    }

    protected Map<String, ContentImageLocationInfo.VariantLocation> readVariantLocations() throws GeneralException {
        if (getVariantConfig() == null) {
            throw new IllegalArgumentException("No variant config available for image");
        }
        Map<String, ContentImageLocationInfo.VariantLocation> sizeTypeInfoMap = new LinkedHashMap<>();
        for (String sizeType : getSizeTypeList()) {
            ImageVariantConfig.VariantInfo variantInfo = getVariantConfig().getVariant(sizeType);
            if (variantInfo == null) {
                throw new IllegalArgumentException("sizeType [" + sizeType + "] not found in image variant config [" + getVariantConfig().getName() + "]");
            }
            String newFileLocation = ContentImageServices.expandImageFnFmt(FlexibleStringExpander.getInstance(getImageFnFmtExpr()), sizeType, getImagePathArgs());
            String targetFileType = (variantInfo.getFormat() != null) ? variantInfo.resolveFormatExt(getDelegator()) : getImageExtension();
            String relativeLocation = newFileLocation + "." + targetFileType;
            sizeTypeInfoMap.put(sizeType, new ContentImageLocationInfo.VariantLocation(variantInfo, relativeLocation,
                    PathUtil.concatPaths(getImageServerPath(), relativeLocation),
                    PathUtil.concatPaths(getImageUrlPrefix(), relativeLocation)));
        }
        return sizeTypeInfoMap;
    }

    public Map<String, ContentImageLocationInfo.VariantLocation> getMissingVariants() throws GeneralException {
        Map<String, ContentImageLocationInfo.VariantLocation> missingVariants = null;
        for (Map.Entry<String, ContentImageLocationInfo.VariantLocation> entry : getVariantLocations().entrySet()) {
            if (!entry.getValue().hasSource()) {
                if (missingVariants == null) {
                    missingVariants = new LinkedHashMap<>();
                }
                missingVariants.put(entry.getKey(), entry.getValue());
            }
        }
        return (missingVariants != null) ? missingVariants : Collections.emptyMap();
    }

    public List<String> getMissingVariantNames() throws GeneralException {
        List<String> missingVariantNames = new ArrayList<>();
        for (Map.Entry<String, ContentImageLocationInfo.VariantLocation> entry : getVariantLocations().entrySet()) {
            if (!entry.getValue().hasSource()) {
                missingVariantNames.add(entry.getKey());
            }
        }
        return missingVariantNames;
    }

    public static class VariantLocation implements Serializable {
        //protected final ImageVariantConfig.VariantInfo variantInfo;
        protected final String name;
        protected final String relativeLocation;
        protected final String fileLocation;
        protected final String url;
        //protected Boolean hasSource;
        //protected Boolean hasSourceFile;

        protected VariantLocation(ImageVariantConfig.VariantInfo variantInfo, String relativeLocation, String fileLocation, String url) {
            this.name = variantInfo.getName();
            this.relativeLocation = relativeLocation;
            this.fileLocation = fileLocation;
            this.url = url;
        }

        public String getName() {
            return name;
        }

        //public ImageVariantConfig.VariantInfo getVariantInfo() {
        //    return variantInfo;
        //}

        public String getRelativeLocation() {
            return relativeLocation;
        }

        public String getFileLocation() {
            return fileLocation;
        }

        public String getUrl() {
            return url;
        }

        /**
         * FIXME: currently only recognizes source files, not source records - this currently works out anyway...
         */
        public boolean hasSource() {
            /*
            Boolean hasSource = this.hasSource;
            if (hasSource == null) {
                hasSource = hasSourceFile();
                this.hasSource = hasSource;
            }
            return hasSource;

             */
            return hasSourceFile();
        }

        public boolean hasSourceFile() {
            /*
            Boolean hasSourceFile = this.hasSourceFile;
            if (hasSourceFile == null) {
                hasSourceFile = checkSourceFile();
                this.hasSourceFile = hasSourceFile;
            }
            return hasSourceFile;
             */
            return checkSourceFile();
        }

        public boolean checkSourceFile() {
            return new File(getFileLocation()).exists();
        }
    }

    public static class ImageContentInfo {
        protected String imageUrl;
        protected GenericValue entityContent;
        protected GenericValue content;

        protected ImageContentInfo(String imageUrl, GenericValue entityContent, GenericValue content) {
            this.imageUrl = imageUrl;
            this.entityContent = entityContent;
            this.content = content;
        }

        public String getImageUrl() {
            return imageUrl;
        }

        public GenericValue getEntityContent() {
            return entityContent;
        }

        public GenericValue getContent() {
            return content;
        }
    }

}
