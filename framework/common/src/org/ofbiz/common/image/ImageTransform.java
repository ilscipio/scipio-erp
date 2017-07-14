/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.common.image;

import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.IndexColorModel;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.common.image.scaler.ImageScalers;
import org.xml.sax.SAXException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;



/**
 * ImageTransform Class
 * <p>
 * Services to apply transformation to images
 */
public class ImageTransform {

    public static final String module = ImageTransform.class.getName();
    public static final String resource = "CommonErrorUiLabels";

    public ImageTransform() {
    }

    /**
     * getBufferedImage
     * <p>
     * Set a buffered image
     *
     * @param   fileLocation    Full file Path or URL
     * @return  URL images for all different size types
     * @throws  IOException Error prevents the document from being fully parsed
     * @throws  JDOMException Errors occur in parsing
     */
    public static  Map<String, Object> getBufferedImage(String fileLocation, Locale locale)
        throws IllegalArgumentException, IOException {

        /* VARIABLES */
        BufferedImage bufImg;
        Map<String, Object> result =  new LinkedHashMap<String, Object>();

        /* BUFFERED IMAGE */
        try {
            bufImg = ImageIO.read(new File(fileLocation));
        } catch (IllegalArgumentException e) {
            String errMsg = UtilProperties.getMessage(resource, "ImageTransform.input_is_null", locale) + " : " + fileLocation + " ; " + e.toString();
            Debug.logError(errMsg, module);
            result.put("errorMessage", errMsg);
            return result;
        } catch (IOException e) {
            String errMsg = UtilProperties.getMessage(resource, "ImageTransform.error_occurs_during_reading", locale) + " : " + fileLocation + " ; " + e.toString();
            Debug.logError(errMsg, module);
            result.put("errorMessage", errMsg);
            return result;
        }

        result.put("responseMessage", "success");
        result.put("bufferedImage", bufImg);
        return result;

    }

    /**
     * scaleImage
     * <p>
     * scale original image related to the ImageProperties.xml dimensions
     * <p>
     * SCIPIO: 2017-07-10: now supports scaling options/algorithm specs.
     *
     * @param   bufImg          Buffered image to scale
     * @param   imgHeight       Original image height
     * @param   imgWidth        Original image width
     * @param   dimensionMap    Image dimensions by size type
     * @param   sizeType        Size type to scale
     * @param   scalingOptions  (SCIPIO) Scaler options, or null for default:
     *                          scalerName: scaler name (algorithm or library name)
     *                          (other): scaler-specific options
     * @return                  New scaled buffered image
     */
    public static Map<String, Object> scaleImage(BufferedImage bufImg, double imgHeight, double imgWidth, Map<String, Map<String, String>> dimensionMap, String sizeType, Locale locale, Map<String, Object> scalingOptions) {

        /* VARIABLES */
        BufferedImage bufNewImg;
        double defaultHeight, defaultWidth, scaleFactor;
        Map<String, Object> result =  new LinkedHashMap<String, Object>();

        /* DIMENSIONS from ImageProperties */
        // A missed dimension is authorized
        if (dimensionMap.get(sizeType).containsKey("height")) {
            defaultHeight = Double.parseDouble(dimensionMap.get(sizeType).get("height").toString());
        } else {
            defaultHeight = -1;
        }
        if (dimensionMap.get(sizeType).containsKey("width")) {
            defaultWidth = Double.parseDouble(dimensionMap.get(sizeType).get("width").toString());
        } else {
            defaultWidth = -1;
        }
        if (defaultHeight == 0.0 || defaultWidth == 0.0) {
            String errMsg = UtilProperties.getMessage(resource, "ImageTransform.one_default_dimension_is_null", locale) + " : defaultHeight = " + defaultHeight + " ; defaultWidth = " + defaultWidth;
            Debug.logError(errMsg, module);
            result.put("errorMessage", errMsg);
            return result;
        }

        /* SCALE FACTOR */
        // find the right Scale Factor related to the Image Dimensions
        if (defaultHeight == -1) {
            scaleFactor = defaultWidth / imgWidth;
            if (scaleFactor == 0.0) {
                String errMsg = UtilProperties.getMessage(resource, "ImageTransform.width_scale_factor_is_null", locale) + "  (defaultWidth = " + defaultWidth + "; imgWidth = " + imgWidth;
                Debug.logError(errMsg, module);
                result.put("errorMessage", errMsg);
                return result;
            }
        } else if (defaultWidth == -1) {
            scaleFactor = defaultHeight / imgHeight;
            if (scaleFactor == 0.0) {
                String errMsg = UtilProperties.getMessage(resource, "ImageTransform.height_scale_factor_is_null", locale) + "  (defaultHeight = " + defaultHeight + "; imgHeight = " + imgHeight;
                Debug.logError(errMsg, module);
                result.put("errorMessage", errMsg);
                return result;
            }
        } else if (imgHeight > imgWidth) {
            scaleFactor = defaultHeight / imgHeight;
            if (scaleFactor == 0.0) {
                String errMsg = UtilProperties.getMessage(resource, "ImageTransform.height_scale_factor_is_null", locale) + "  (defaultHeight = " + defaultHeight + "; imgHeight = " + imgHeight;
                Debug.logError(errMsg, module);
                result.put("errorMessage", errMsg);
                return result;
            }
            // get scaleFactor from the smallest width
            if (defaultWidth < (imgWidth * scaleFactor)) {
                scaleFactor = defaultWidth / imgWidth;
            }
        } else {
            scaleFactor = defaultWidth / imgWidth;
            if (scaleFactor == 0.0) {
                String errMsg = UtilProperties.getMessage(resource, "ImageTransform.width_scale_factor_is_null", locale) + "  (defaultWidth = " + defaultWidth + "; imgWidth = " + imgWidth;
                Debug.logError(errMsg, module);
                result.put("errorMessage", errMsg);
                return result;
            }
            // get scaleFactor from the smallest height
            if (defaultHeight < (imgHeight * scaleFactor)) {
                scaleFactor = defaultHeight / imgHeight;
            }
        }

        if (scaleFactor == 0.0) {
            String errMsg = UtilProperties.getMessage(resource, "ImageTransform.final_scale_factor_is_null", locale) + " = " + scaleFactor;
            Debug.logError(errMsg, module);
            result.put("errorMessage", errMsg);
            return result;
        }
        // SCIPIO: obsolete
//        int bufImgType;
//        if (BufferedImage.TYPE_CUSTOM == bufImg.getType()) {
//            String errMsg = UtilProperties.getMessage(resource, "ImageTransform.unknown_buffered_image_type", locale);
//            Debug.logWarning(errMsg, module);
//            // apply a type for image majority
//            bufImgType = BufferedImage.TYPE_INT_ARGB_PRE;
//        } else {
//            bufImgType = bufImg.getType();
//        }

        // scale original image with new size
        // SCIPIO: 2017-07-10: new configurable scaling; scalerName may be an algorithm name (abstracted) or some other name (3rd-party lib name or other).
        //Image newImg = bufImg.getScaledInstance((int) (imgWidth * scaleFactor), (int) (imgHeight * scaleFactor), Image.SCALE_SMOOTH);
        try {
            scalingOptions = ImageUtil.addImageOpOptionIfNotSet(scalingOptions, "targettype", ImageType.PRESERVE_IF_LOSSLESS); // NOTE: stock ofbiz behavior appeared to try to preserve
            bufNewImg = ImageScalers.getScalerOrDefault(scalingOptions).scaleImage(bufImg, (int) (imgWidth * scaleFactor), (int) (imgHeight * scaleFactor), scalingOptions);
        } catch(IOException e) {
            throw new IllegalArgumentException("Error scaling image: " + e.getMessage(), e);
        }
        
        // SCIPIO: handled by ImageType.PRESERVE
        //bufNewImg = ImageTransform.toBufferedImage(newImg, bufImgType);

        result.put("responseMessage", "success");
        result.put("bufferedImage", bufNewImg);
        result.put("scaleFactor", scaleFactor);
        return result;

    }

    /**
     * scaleImage
     * <p>
     * scale original image related to the ImageProperties.xml dimensions
     *
     * @param   bufImg          Buffered image to scale
     * @param   imgHeight       Original image height
     * @param   imgWidth        Original image width
     * @param   dimensionMap    Image dimensions by size type
     * @param   sizeType        Size type to scale
     * @return                  New scaled buffered image
     */
    public static Map<String, Object> scaleImage(BufferedImage bufImg, double imgHeight, double imgWidth, Map<String, Map<String, String>> dimensionMap, String sizeType, Locale locale) {
        return scaleImage(bufImg, imgHeight, imgWidth, dimensionMap, sizeType, locale, null);
    }
    
    /**
     * getXMLValue
     * <p>
     * From a XML element, get a values map
     *
     * @param fileFullPath      File path to parse
     * @return Map contains asked attribute values by attribute name
     */
    public static  Map<String, Object> getXMLValue(String fileFullPath, Locale locale)
        throws IllegalStateException, IOException {

        /* VARIABLES */
        Document document;
        Element rootElt;
        Map<String, Map<String, String>> valueMap =  new LinkedHashMap<String, Map<String, String>>();
        Map<String, Object> result =  new LinkedHashMap<String, Object>();

        /* PARSING */
        try {
            document = UtilXml.readXmlDocument(new FileInputStream(fileFullPath), fileFullPath);
        } catch (ParserConfigurationException e) {
            String errMsg = UtilProperties.getMessage(resource, "ImageTransform.errors_occurred_during_parsing", locale) +  " ImageProperties.xml " + e.toString();
            Debug.logError(errMsg, module);
            result.put("errorMessage", "error");
            return result;
        } catch (SAXException e) {
            String errMsg = UtilProperties.getMessage(resource, "ImageTransform.errors_occurred_during_parsing", locale) +  " ImageProperties.xml " + e.toString();
            Debug.logError(errMsg, module);
            result.put("errorMessage", "error");
            return result;
        } catch (IOException e) {
            String errMsg = UtilProperties.getMessage(resource, "ImageTransform.error_prevents_the document_from_being_fully_parsed", locale) + e.toString();
            Debug.logError(errMsg, module);
            result.put("errorMessage", "error");
            return result;
        }
        // set Root Element
        try {
            rootElt = document.getDocumentElement();
        } catch (IllegalStateException e) {
            String errMsg = UtilProperties.getMessage(resource, "ImageTransform.root_element_has_not_been_set", locale) + e.toString();
            Debug.logError(errMsg, module);
            result.put("errorMessage", "error");
            return result;
        }

        /* get NAME and VALUE */
        List<? extends Element> children = UtilXml.childElementList(rootElt); // FIXME : despite upgrading to jdom 1.1, it seems that getChildren is pre 1.5 java code (ie getChildren does not retun List<Element> but only List)
        for (Element currentElt : children) {
            Map<String, String> eltMap =  new LinkedHashMap<String, String>();
            List<? extends Element> children2 = UtilXml.childElementList(currentElt);
            if (children2.size() > 0) {
                Map<String, String> childMap =  new LinkedHashMap<String, String>();
                // loop over Children 1st level
                for (Element currentChild : children2) {
                    childMap.put(currentChild.getAttribute("name"), currentChild.getAttribute("value"));
                }
                valueMap.put(currentElt.getAttribute("name"), childMap);
            } else {
                eltMap.put(currentElt.getAttribute("name"), currentElt.getAttribute("value"));
                valueMap.put(currentElt.getNodeName(), eltMap);
            }
        }

        result.put("responseMessage", "success");
        result.put("xml", valueMap);
        return result;

    }

    /**
     * toBufferedImage
     * <p>
     * Transform from an Image instance to a BufferedImage instance (FIXED IMAGE TYPE).
     * <p>
     * SCIPIO: NOTE: This does NOT preserve the image type such as index or color model; always creates as fixed
     * system default type (previously was hardcoded as TYPE_INT_ARGB_PRE; see {@link #DEFAULT_BUFIMAGE_TYPE}
     * for current value).
     * 
     * @param image             Source image
     * @return BufferedImage
     */
    public static BufferedImage toBufferedImage(Image image) {
        // SCIPIO: don't hardcode
        //return ImageTransform.toBufferedImage(image, BufferedImage.TYPE_INT_ARGB_PRE, null); // SCIPIO: new deleg
        return ImageTransform.toBufferedImage(image, ImageType.DEFAULT.getPixelTypeDefault(), null, ImageType.DEFAULT); // SCIPIO: new deleg
    }

    /**
     * toBufferedImage with specific type.
     * @deprecated SCIPIO: 2017-07-11: use {@link #toCompatibleBufferedImage} instead; this
     * method does not preserve enough information from the original images to preserve the types of
     * original images - misses ColorModel and other - so it is unable to work properly 
     * on indexed images properly without color loss and potentially other types.
     */
    @Deprecated
    public static BufferedImage toBufferedImage(Image image, int bufImgType) {
        // SCIPIO: WARN: passing null ColorModel - officially should always be passed
        return toBufferedImage(image, bufImgType, null, new ImageType(bufImgType));
    }
    
    /**
     * toBufferedImage with specific type and color model (used as needed for indexed images).
     * SCIPIO: Modified 2017-07-11.
     * <p>
     * SCIPIO: NOTE: This is a modified legacy Ofbiz function; best to use the new {@link #toCompatibleBufferedImage} instead.
     * 
     * @param image the image (required)
     * @param imageType the original image type (required) - should NOT be a fallback value!
     * @param imageColorModel the original image color model (required)
     * @param fallbackImageType a fallback image type (optional - is a hint and may be ignored)
     */
    public static BufferedImage toBufferedImage(Image image, int imageType, ColorModel imageColorModel, ImageType fallbackImageType) {
        /** Check if the image isn't already a BufferedImage instance */
        if( image instanceof BufferedImage ) {
                return( (BufferedImage)image );
        } else {
                /** Full image loading */
                image = new ImageIcon(image).getImage();

                /** new BufferedImage creation */
                // SCIPIO: 2017-07-11: this does NOT work for indexed images - slaughters them
//                BufferedImage bufferedImage = new BufferedImage(
//                            image.getWidth(null),
//                            image.getHeight(null),
//                            bufImgType);
                BufferedImage bufferedImage = createBufferedImage(
                            image.getWidth(null),
                            image.getHeight(null),
                            imageType,
                            imageColorModel);
                
                copyToBufferedImage(image, bufferedImage); // SCIPIO: factored

                return( bufferedImage );
        }
    }
    
    /**
     * SCIPIO: Creates a new blank BufferedImage with the given type AND color model IF applicable.
     * WARN: we need to preserve the color model if there is one! stock ofbiz did not do this!
     * Needed to support indexed images properly.
     * Added 2017-07-11.
     * NOTE: Better to use {@link #createCompatibleBufferedImage} where possible.
     */
    public static BufferedImage createBufferedImage(int targetWidth, int targetHeight, int imgType, ColorModel colorModel) {
        if (imgType == BufferedImage.TYPE_BYTE_BINARY || imgType == BufferedImage.TYPE_BYTE_INDEXED) {
            if (colorModel instanceof IndexColorModel) {
                return new BufferedImage(targetWidth, targetHeight, imgType, (IndexColorModel) colorModel);
            } else {
                return new BufferedImage(targetWidth, targetHeight, imgType);
            }
        } else if (imgType == BufferedImage.TYPE_CUSTOM) {
            return new BufferedImage(targetWidth, targetHeight, ImageType.DEFAULT.getPixelTypeFor(colorModel));
        } else {
            return new BufferedImage(targetWidth, targetHeight, imgType);
        }
    }
    
    /**
     * SCIPIO: Converts the image to a new BufferedImage <b>IF</b> it's not already one, and preserves
     * the original image parameters as much as possible.
     *
     * @param image the image (required)
     * @param imageColorModel the image color model (required)
     * @param targetWidth target width (optional - if null use from image)
     * @param targetHeight target height (optional - if null use from image)
     * @param fallbackImageType optional fallback target image type; this is a hint and may be ignored
     */
    public static BufferedImage toCompatibleBufferedImage(Image image, int imageType, ColorModel imageColorModel, Integer targetWidth, Integer targetHeight, Integer fallbackImageType) {
        // Check if the image isn't already a BufferedImage instance
        if (image instanceof BufferedImage) {
            return (BufferedImage) image;
        } else {
            /** Full image loading */
            image = new ImageIcon(image).getImage();
            return toCompatibleBufferedImageAlways(image, imageType, imageColorModel, targetWidth, targetHeight, fallbackImageType);
        }
    }
    
    /**
     * SCIPIO: Converts the image to a new BufferedImage <b>IF</b> it's not already one, and preserves
     * the original image parameters as much as possible.
     *
     * @param image the image (required)
     * @param imageType the image type (required)
     * @param imageColorModel the image color model (required)
     */
    public static BufferedImage toCompatibleBufferedImage(Image image, int imageType, ColorModel imageColorModel) {
        return toCompatibleBufferedImage(image, imageType, imageColorModel, null, null, null);
    }
    
    /**
     * SCIPIO: Converts the image to a new BufferedImage <b>ALWAYS</b>, and preserves
     * the original image parameters as much as possible except where explicit.
     * Does not use ImageIcon.
     *
     * @param image the image (required)
     * @param imageType the image type (required)
     * @param imageColorModel the image color model (required)
     * @param targetWidth target width (optional - if null use from image)
     * @param targetHeight target height (optional - if null use from image)
     * @param fallbackImageType fallback target image type (optional - this is a hint and may be ignored)
     */
    public static BufferedImage toCompatibleBufferedImageAlways(Image image, int imageType, ColorModel imageColorModel, Integer targetWidth, Integer targetHeight, Integer fallbackImageType) {
        /** new BufferedImage creation */
        BufferedImage bufferedImage = createCompatibleBufferedImage(image, imageType, imageColorModel, targetWidth, targetHeight);
        copyToBufferedImage(image, bufferedImage); // SCIPIO: factored
        return( bufferedImage );
    }
    
    /**
     * SCIPIO: Converts the image to a new BufferedImage <b>ALWAYS</b>, and preserves
     * the original image parameters as much as possible.
     *
     * @param image the image (required)
     * @param imageType the image type (required)
     * @param imageColorModel the image color model (required)
     */
    public static BufferedImage toCompatibleBufferedImageAlways(Image image, int imageType, ColorModel imageColorModel) {
        return toCompatibleBufferedImageAlways(image, imageType, imageColorModel, null, null, null);
    }

    /**
     * SCIPIO: This reconverts the given modified image (after some operation) back to the original
     * input image's type as best as possible, always.
     * Does not use ImageIcon.
     * WARN: this adds an extra conversion step after every op, and better avoided where possible.
     */
    public static BufferedImage toOrigCompatibleBufferedImageAfterOpAlways(Image origImage, int origImageType, ColorModel origColorModel, BufferedImage modifiedImage, Integer fallbackImgType) {
        // TODO: REVIEW: do we still need this?
        ///** Full image loading */
        //origImage = new ImageIcon(origImage).getImage();
        /** new BufferedImage creation */
        BufferedImage resultImage = ImageTransform.createCompatibleBufferedImage(origImage, origImageType, origColorModel, modifiedImage.getWidth(), modifiedImage.getHeight());
        ImageTransform.copyToBufferedImage(modifiedImage, resultImage);
        return resultImage;
    }
    
    /**
     * SCIPIO: This reconverts the given modified image (after some operation) back to the original
     * input image's type as best as possible, always.
     * Does not use ImageIcon.
     * WARN: this adds an extra conversion step after every op, and better avoided where possible.
     */
    public static BufferedImage toOrigCompatibleBufferedImageAfterOpAlways(BufferedImage origImage, BufferedImage modifiedImage, Integer fallbackImgType) {
        // TODO: REVIEW: do we still need this?
        ///** Full image loading */
        //origImage = new ImageIcon(origImage).getImage();
        /** new BufferedImage creation */
        BufferedImage resultImage = ImageTransform.createCompatibleBufferedImage(origImage, modifiedImage.getWidth(), modifiedImage.getHeight());
        ImageTransform.copyToBufferedImage(modifiedImage, resultImage);
        return resultImage;
    }
    
    /**
     * SCIPIO: Improved method for creating a compatible BufferedImage.
     * Based on mortennobel {@link com.mortennobel.imagescaling.AdvancedResizeOp#createCompatibleDestImage}.
     * Added 2017-07-12.
     * @param image the image (required)
     * @param imageType the image type (required)
     * @param imageColorModel the image color model (required)
     * @param targetWidth target width (optional - if null use from image)
     * @param targetHeight target height (optional - if null use from image)
     */
    public static BufferedImage createCompatibleBufferedImage(Image image, int imageType, ColorModel imageColorModel, Integer targetWidth, Integer targetHeight) {
        return new BufferedImage(imageColorModel, imageColorModel.createCompatibleWritableRaster(
                targetWidth != null ? targetWidth : image.getWidth(null), targetHeight != null ? targetHeight : image.getHeight(null)),
                imageColorModel.isAlphaPremultiplied(), null);
    }
    
    /**
     * SCIPIO: Improved method for creating a compatible BufferedImage.
     * Based on mortennobel {@link com.mortennobel.imagescaling.AdvancedResizeOp#createCompatibleDestImage}.
     * Added 2017-07-12.
     * @param image the image (required)
     * @param imageType the image type (required)
     * @param imageColorModel the image color model (required)
     */
    public static BufferedImage createCompatibleBufferedImage(Image image, int imageType, ColorModel imageColorModel) {
        return createCompatibleBufferedImage(image, imageType, imageColorModel, null, null);
    }
    
    /**
     * SCIPIO: Improved method for creating a compatible BufferedImage.
     * Based on mortennobel {@link com.mortennobel.imagescaling.AdvancedResizeOp#createCompatibleDestImage}.
     * This version automatically gets the color model from the buffered image.
     * Added 2017-07-12.
     * @param image the image (required, includes color model)
     * @param targetWidth target width (optional - if null use from image)
     * @param targetHeight target height (optional - if null use from image)
     */
    public static BufferedImage createCompatibleBufferedImage(BufferedImage image, Integer targetWidth, Integer targetHeight) {
        ColorModel colorModel = image.getColorModel();
        return new BufferedImage(colorModel, 
                //image.getRaster().createCompatibleWritableRaster(targetWidth != null ? targetWidth : image.getWidth(null), targetHeight != null ? targetHeight : image.getHeight(null)),
                colorModel.createCompatibleWritableRaster(targetWidth != null ? targetWidth : image.getWidth(null), targetHeight != null ? targetHeight : image.getHeight(null)),
                colorModel.isAlphaPremultiplied(), null);
    }
    
    /**
     * SCIPIO: Improved method for creating a compatible BufferedImage.
     * Based on mortennobel {@link com.mortennobel.imagescaling.AdvancedResizeOp#createCompatibleDestImage}.
     * This version automatically gets the color model from the buffered image.
     * Added 2017-07-12.
     * @param image the image (required, includes color model)
     */
    public static BufferedImage createCompatibleBufferedImage(BufferedImage image) {
        return createCompatibleBufferedImage(image, null, null);
    }
    
    /**
     * SCIPIO: Simple copy of a source image to a destination buffered image.
     * Added 2017-07-12.
     * @return the destImage
     */
    public static BufferedImage copyToBufferedImage(Image srcImage, BufferedImage destImage, RenderingHints renderingHints) {
        Graphics2D g = destImage.createGraphics();
        try {
            if (renderingHints != null) g.setRenderingHints(renderingHints);
            g.drawImage(srcImage, 0, 0, null);
        } finally { // SCIPIO: added finally
            g.dispose();
        }
        return destImage;
    }
    
    /**
     * SCIPIO: Simple copy of a source image to a destination buffered image.
     * Added 2017-07-12.
     * @return the destImage
     */
    public static BufferedImage copyToBufferedImage(Image srcImage, BufferedImage destImage) {
        return copyToBufferedImage(srcImage, destImage, null);
    }
    
    /**
     * SCIPIO: Attempts to create an exact copy of the original image in a new instance.
     * WARN: TODO: currently not guaranteed to work for all images.
     * Added 2017-07-14.
     * @return the cloned image
     */
    public static BufferedImage cloneBufferedImage(BufferedImage image) {
        ColorModel colorModel = image.getColorModel();
        return new BufferedImage(colorModel, 
                //image.copyData(image.getRaster().createCompatibleWritableRaster()),
                image.copyData(colorModel.createCompatibleWritableRaster(image.getWidth(null), image.getHeight(null))),
                colorModel.isAlphaPremultiplied(), null);
    }

}
