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
package com.ilscipio.scipio.content.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ImageServices {

    /**
     * Scales a content/generic image file according to size types in ImageProperties.xml - core implementation (caller specifies locations/filenames); also supports component:// and file:// prefixes
     */
    @Service(
        name = "contentImageFileScaleInAllSizeCore",
        engine = "java",
        location = "com.ilscipio.scipio.content.image.ContentImageServices",
        invoke = "contentImageFileScaleInAllSizeCore",
        description = "Scales a content/generic image file according to size types in ImageProperties.xml - core implementation (caller specifies locations/filenames); also supports component:// and file:// prefixes",
        transactionTimeout = "1800",
        attributes = {
            @Attribute(name = "imageOrigPath", type = "String", mode = "IN", optional = "true", description = "Full path of original image in filesystem as input (alternative to imageOrigUrl; if neither specified, auto-determines an original filename under imageServerPath)"),
            @Attribute(name = "imageOrigUrl", type = "String", mode = "IN", optional = "true", description = "URL of image relative to server root of original image in filesystem as input; WARN: 2017-07-04: MUST start with imageUrlPrefix else error; no other mount-points supported (alternative to imageOrigPath; if neither specified, auto-determines an original filename under imageServerPath)"),
            @Attribute(name = "imageOrigFn", type = "String", mode = "IN", optional = "true", description = "Original filename (no directories) of the image - required only if neither imageOrigPath nor imageOrigUrl specified"),
            @Attribute(name = "imageOrigFnFmt", type = "String", mode = "IN", optional = "true", description = "Image filename format string for original only, relative to imageServerPath/imageUrlPrefix, no extension; only useful if copyOrig==true or if imageOrigPath/imageOrigUrl are omitted (default: same as imageFnFmt)"),
            @Attribute(name = "imageServerPath", type = "String", mode = "IN", optional = "true", description = "Full filesystem path of base server image, parameterized with ${tenantId} (default: uses image.server.path / content.properties); also supports component:// and file:// prefixes"),
            @Attribute(name = "imageUrlPrefix", type = "String", mode = "IN", optional = "true", description = "URL prefix for generated images, parameterized with ${tenantId} (default: uses image.url.prefix / content.properties)"),
            @Attribute(name = "imageFnFmt", type = "String", mode = "IN", description = "Image filename format string, relative to imageServerPath/imageUrlPrefix, no extension, parameterized with ${sizetype} (or ${type}), required"),
            @Attribute(name = "imagePathArgs", type = "Map", mode = "IN", optional = "true", description = "Additional args for parameterized paths"),
            @Attribute(name = "imageProfile", type = "Object", mode = "IN", optional = "true", description = "Image profile, now generally required (name or org.ofbiz.common.image.ImageProfile)"),
            @Attribute(name = "defaultImageProfile", type = "Object", mode = "IN", optional = "true", defaultValue = "IMAGE_CONTENT", description = "Default image profile, for wrapping services (name or org.ofbiz.common.image.ImageProfile)"),
            @Attribute(name = "imageVariantConfig", type = "org.ofbiz.common.image.ImageVariantConfig", mode = "IN", optional = "true", description = "Image variant config, alternative to imagePropXmlPath"),
            @Attribute(name = "imagePropXmlPath", type = "String", mode = "IN", optional = "true", description = "Path to ImageProperties.xml file containing size types, from ofbiz home root"),
            @Attribute(name = "imageWriteOptions", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "sizeTypeList", type = "Collection", mode = "IN", optional = "true", description = "List of size types to generate and return (default: all types in file)"),
            @Attribute(name = "copyOrig", type = "Boolean", mode = "IN", optional = "true", description = "If true, also creates copy of the original under the size type \"original\" (default: false)"),
            @Attribute(name = "deleteOld", type = "Boolean", mode = "IN", optional = "true", description = "[TODO: NOT IMPLEMENTED] If true, also deletes old files in target directory (default: false)"),
            @Attribute(name = "scalingOptions", type = "Map", mode = "IN", optional = "true", description = "Scaling options, notably the entry: scalerName (algorithm or library name)"),
            @Attribute(name = "imageUrlMap", type = "Map", mode = "OUT", optional = "true", description = "Map of size types to URLs (relative to server root, with imageUrlPrefix); if copyOrig==true, also contains \"original\""),
            @Attribute(name = "imageInfoMap", type = "Map", mode = "OUT", optional = "true", description = "Map of maps describing url, width, height and variantInfo for each sizeType; also contains \"original\" which contains copyOrig boolean (SCIPIO)"),
            @Attribute(name = "bufferedImage", type = "java.awt.image.BufferedImage", mode = "OUT", optional = "true", description = "Original image contents, for reuse"),
            @Attribute(name = "successCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "failCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "skipCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "reason", type = "String", mode = "OUT", optional = "true", description = "Known values: unsupported-colorspace")
        }
    )
    public interface ContentImageFileScaleInAllSizeCore {}

    /**
     * Scales a content/generic database image according to size types in ImageProperties.xml - core implementation (caller specifies all locations/filenames)
     */
    @Service(
        name = "contentImageDbScaleInAllSizeCore",
        engine = "java",
        location = "com.ilscipio.scipio.content.image.ContentImageServices",
        invoke = "contentImageDbScaleInAllSizeCore",
        description = "Scales a content/generic database image according to size types in ImageProperties.xml - core implementation (caller specifies all locations/filenames)",
        transactionTimeout = "1800",
        attributes = {
            @Attribute(name = "imageOrigContentId", type = "String", mode = "IN", description = "Original image contentId as input (alternative to imageOrigPath)"),
            @Attribute(name = "imageOrigFn", type = "String", mode = "IN", optional = "true", description = "Original filename (no directories) to use for the original image; by default tries to get this from DataResource.objectInfo\n                NOTE: Only required if the original DataResource.objectInfo did not contain a filename or path to a file (sometimes the case)"),
            @Attribute(name = "targetFmtExt", type = "String", mode = "IN", optional = "true", description = "Target format name, same as file extension, e.g. png, jpg, ... (default: inferred from original DataResource.mimeTypeId)"),
            @Attribute(name = "imageProfile", type = "Object", mode = "IN", optional = "true", description = "Image profile, now generally required (name or org.ofbiz.common.image.ImageProfile)"),
            @Attribute(name = "defaultImageProfile", type = "Object", mode = "IN", optional = "true", defaultValue = "IMAGE_CONTENT", description = "Default image profile, for wrapping services (name or org.ofbiz.common.image.ImageProfile)"),
            @Attribute(name = "imageVariantConfig", type = "org.ofbiz.common.image.ImageVariantConfig", mode = "IN", optional = "true", description = "Image variant config, alternative to imagePropXmlPath"),
            @Attribute(name = "imagePropXmlPath", type = "String", mode = "IN", optional = "true", description = "Path to ImageProperties.xml file containing size types, from ofbiz home root"),
            @Attribute(name = "imageWriteOptions", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "sizeTypeList", type = "Collection", mode = "IN", optional = "true", description = "List of size types to generate and return (default: all types in file)"),
            @Attribute(name = "deleteOld", type = "Boolean", mode = "IN", optional = "true", description = "If true, removes all existing related images before creating new ones (default: false)\n                2020-11: AVOID USE: recreateExisting should be specified as true instead, which will update records in-place, which is a lot faster\n                and more friendly toward frontend behavior."),
            @Attribute(name = "recreateExisting", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If false (default), existing files for the size types are ignored and not regenerated; if true, all or give size types are always regenerated (slow)"),
            @Attribute(name = "createdDate", type = "Timestamp", mode = "IN", optional = "true", description = "Optional createdDate for Content and DataResource"),
            @Attribute(name = "contentFields", type = "Map", mode = "IN", optional = "true", description = "Fields to use to populate the created Content entities (all sizeTypes)\n                NOTE: entries can be FlexibleStringExpander instances. defaults: \n                    contentName (default: \"${fields.contentName}_${sizetype}\")"),
            @Attribute(name = "dataResourceFields", type = "Map", mode = "IN", optional = "true", description = "Fields to use to populate the created DataResource entities (all sizeTypes)\n                NOTE: entries can be FlexibleStringExpander instances. defaults: \n                    dataResourceName (default: \"${fields.dataResourceName}_${sizetype}\"),\n                    objectInfo (default: \"${origfn}_${sizetype}.${ext}\" NOTE: origfn comes from objectInfo but may be imageOrigFn w/o ext - if passed)"),
            @Attribute(name = "contentFieldsMap", type = "Map", mode = "IN", optional = "true", description = "Map of fields to use to populate the duplicated original Content entity, if copyOrig true (map key: sizeType)\n                NOTE: entries can be FlexibleStringExpander instances (see contentFields)"),
            @Attribute(name = "dataResourceFieldsMap", type = "Map", mode = "IN", optional = "true", description = "Map of fields to use to populate the duplicated original DataResource entity, if copyOrig true (map key: sizeType)\n                NOTE: entries can be FlexibleStringExpander instances (see dataResourceFields)"),
            @Attribute(name = "fileSizeDataResAttrName", type = "String", mode = "IN", optional = "true", description = "If specified, will add a DataResourceAttribute with this name containing the total filesize"),
            @Attribute(name = "contentAssocTypeId", type = "String", mode = "IN", optional = "true", description = "DEPRECATED: 2020-09: Generally the default is assumed in most circumstances, so setting this to anything else should be avoided.\n                Content assoc type, with support for flexible expressions (default: \"IMGSZ_${sizetype}\")\n                NOTE: the entire string is automatically uppercased"),
            @Attribute(name = "scalingOptions", type = "Map", mode = "IN", optional = "true", description = "Scaling options, notably the entry: scalerName (algorithm or library name)"),
            @Attribute(name = "imageContentIdMap", type = "Map", mode = "OUT", optional = "true", description = "Map of size types to image record contentIds"),
            @Attribute(name = "bufferedImage", type = "java.awt.image.BufferedImage", mode = "OUT", optional = "true", description = "Original image contents, for reuse"),
            @Attribute(name = "successCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "failCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "skipCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "reason", type = "String", mode = "OUT", optional = "true", description = "Known values: unsupported-colorspace")
        }
    )
    public interface ContentImageDbScaleInAllSizeCore {}

    /**
     * Automatically rescales a content image
     */
    @Service(
        name = "contentImageAutoRescale",
        engine = "java",
        location = "com.ilscipio.scipio.content.image.ContentImageServices",
        invoke = "contentImageAutoRescale",
        description = "Automatically rescales a content image",
        transactionTimeout = "1800",
        attributes = {
            @Attribute(name = "contentId", type = "String", mode = "IN"),
            @Attribute(name = "contentDataResource", type = "GenericValue", mode = "IN", optional = "true", description = "Optional ContentDataResourceRequiredView instance matching contentId, if provided (optimization)"),
            @Attribute(name = "sizeTypeList", type = "Collection", mode = "IN", optional = "true", description = "Optional list of size dimension names to restrict resizing to (e.g.: 320x240, small); unlisted are left unchanged"),
            @Attribute(name = "createNew", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "If true, even if the image currently has no variants, attempts to generate new variants;\n                if false, only regenerates if variants already created"),
            @Attribute(name = "recreateExisting", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If false (default), existing files for the size types are ignored and not regenerated; if true, all or give size types are always regenerated (slow)"),
            @Attribute(name = "deleteOld", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "Not recommended unless corruption: delete the old ContentAssoc/Content/DataResource before creating new ones (old behavior)"),
            @Attribute(name = "nonFatal", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, runs in separate transaction and returns failure on image resize fail;\n                if false, runs in current transaction and returns error on on image resize fail"),
            @Attribute(name = "moment", type = "Timestamp", mode = "IN", optional = "true", description = "Specific createdDate for Content and DataResource"),
            @Attribute(name = "doLog", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "progress", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "reason", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "variantSuccessCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "variantFailCount", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "variantSkipCount", type = "Integer", mode = "OUT", optional = "true")
        }
    )
    public interface ContentImageAutoRescale {}

    /**
     * Clear ContentImageVariants caches (SCIPIO)
     */
    @Service(
        name = "contentImageVariantsDistributedClearCaches",
        engine = "jms",
        location = "serviceMessenger",
        invoke = "contentImageVariantsClearCaches",
        description = "Clear ContentImageVariants caches (SCIPIO)",
        auth = "true",
        useTransaction = "false",
        log = "quiet",
        logEca = "quiet"
    )
    public interface ContentImageVariantsDistributedClearCaches {}

    /**
     * Clear ContentImageVariants caches (SCIPIO)
     */
    @Service(
        name = "contentImageVariantsClearCaches",
        engine = "java",
        location = "com.ilscipio.scipio.content.image.ContentImageVariants",
        invoke = "clearCaches",
        description = "Clear ContentImageVariants caches (SCIPIO)",
        auth = "true",
        export = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "distribute", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false")
        }
    )
    public interface ContentImageVariantsClearCaches {}

}
