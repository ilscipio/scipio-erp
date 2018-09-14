/*
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
 */
package org.ofbiz.common.qrcode;

import java.awt.Graphics2D;
import java.awt.geom.AffineTransform;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.FileUtil;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.image.ImageTransform;
import org.ofbiz.common.image.ImageTransform.ImageScaleSpec;
import org.ofbiz.common.image.scaler.ImageScalers;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.google.zxing.BarcodeFormat;
import com.google.zxing.ChecksumException;
import com.google.zxing.DecodeHintType;
import com.google.zxing.EncodeHintType;
import com.google.zxing.FormatException;
import com.google.zxing.MultiFormatWriter;
import com.google.zxing.NotFoundException;
import com.google.zxing.WriterException;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.common.DecoderResult;
import com.google.zxing.common.DetectorResult;
import com.google.zxing.qrcode.decoder.Decoder;
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel;
import com.google.zxing.qrcode.detector.Detector;

import freemarker.template.utility.StringUtil;

/**
 * Services for QRCode.
 */
public class QRCodeServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String QRCODE_DEFAULT_WIDTH = UtilProperties.getPropertyValue("qrcode", "qrcode.default.width", "200");

    public static final int QRCODE_DEFAULT_WIDTH_INT = UtilProperties.asInteger(QRCODE_DEFAULT_WIDTH, 200); // SCIPIO: 2018-08-22

    public static final String QRCODE_DEFAULT_HEIGHT = UtilProperties.getPropertyValue("qrcode", "qrcode.default.height", "200");

    public static final int QRCODE_DEFAULT_HEIGHT_INT = UtilProperties.asInteger(QRCODE_DEFAULT_HEIGHT, 200); // SCIPIO: 2018-08-22

    public static final String QRCODE_DEFAULT_FORMAT = UtilProperties.getPropertyValue("qrcode", "qrcode.default.format", "jpg");

    public static final String QRCODE_FORMAT_SUPPORTED = UtilProperties.getPropertyValue("qrcode", "qrcode.format.supported", "jpg|png|bmp");

    public static final String QRCODE_DEFAULT_LOGOIMAGE = UtilProperties.getPropertyValue("qrcode", "qrcode.default.logoimage");

    public static final BufferedImage defaultLogoImage;

    private static final String[] FORMAT_NAMES = StringUtil.split(QRCODE_FORMAT_SUPPORTED, '|');

    private static final List<String> FORMATS_SUPPORTED = Arrays.asList(FORMAT_NAMES);

    public static final int MIN_SIZE = 20;

    public static final int MAX_SIZE = 500;

    private static final int BLACK = 0xFF000000;

    private static final int WHITE = 0xFFFFFFFF;

    static {
        // SCIPIO: this block rearranged for final field and not swallowing exceptions
        BufferedImage img = null;
        if (UtilValidate.isNotEmpty(QRCODE_DEFAULT_LOGOIMAGE)) {
            try {
                Map<String, Object> logoImageResult = ImageTransform.getBufferedImage(FileUtil.getFile(QRCODE_DEFAULT_LOGOIMAGE).getAbsolutePath(), Locale.getDefault());
                img = (BufferedImage) logoImageResult.get("bufferedImage");
                if (UtilValidate.isEmpty(img)) {
                    Debug.logError("Your logo image file (" + QRCODE_DEFAULT_LOGOIMAGE + ") cannot be read by javax.imageio.ImageIO. Please use png, jpeg formats instead of ico and etc.", module);
                }
            } catch (Exception e) {
                Debug.logError(e, "Could not load logo image file: " + QRCODE_DEFAULT_LOGOIMAGE, module);
            }
        }
        defaultLogoImage = img;
    }

    // SCIPIO: 2018-08-22: new fields

    public static final String QRCODE_DEFAULT_ECLEVEL;

    private static final ErrorCorrectionLevel defaultEcLevel;
    static {
        String ecLevel = UtilProperties.getPropertyValueOrNull("qrcode", "qrcode.default.eclevel");
        String defaultEcLevelStr = null;
        ErrorCorrectionLevel defaultEcLevelEnum = null;
        try {
            defaultEcLevelEnum = (ecLevel != null) ?
                    ErrorCorrectionLevel.valueOf(ecLevel) : null;
            defaultEcLevelStr = ecLevel;
        } catch(Exception e) {
            Debug.logError("Invalid value for qrcode.default.eclevel: " + e.toString(), module);
        }
        QRCODE_DEFAULT_ECLEVEL = defaultEcLevelStr;
        defaultEcLevel = defaultEcLevelEnum;
    }

    public static final String QRCODE_SCALER = UtilProperties.getPropertyValueOrNull("qrcode", "qrcode.scaler");

    public static final String QRCODE_DEFAULT_LOGOSIZE = UtilProperties.getPropertyValueOrNull("qrcode", "qrcode.default.logoSize");
    public static final String QRCODE_DEFAULT_LOGOMAXSIZE = UtilProperties.getPropertyValueOrNull("qrcode", "qrcode.default.logoMaxSize");

    /** Streams QR Code to the result. */
    public static Map<String, Object> generateQRCodeImage(DispatchContext ctx,Map<String, Object> context) {
        Locale locale = (Locale) context.get("locale");
        String message = (String) context.get("message");
        Integer width = (Integer) context.get("width");
        Integer height = (Integer) context.get("height");
        String format = (String) context.get("format");
        String encoding = (String) context.get("encoding");
        Boolean verifyOutput = (Boolean) context.get("verifyOutput");
        String logoImage = (String) context.get("logoImage");
        Integer logoImageMaxWidth = (Integer) context.get("logoImageMaxWidth");
        Integer logoImageMaxHeight = (Integer) context.get("logoImageMaxHeight");

        if (UtilValidate.isEmpty(message)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("QRCodeUiLabels", "ParameterCannotEmpty", new Object[] { "message" }, locale));
        }
        if (width == null) {
            //width = Integer.parseInt(QRCODE_DEFAULT_WIDTH);
            width = QRCODE_DEFAULT_WIDTH_INT; // SCIPIO
        }
        if (width < MIN_SIZE || width > MAX_SIZE) {
            return ServiceUtil.returnError(UtilProperties.getMessage("QRCodeUiLabels", "SizeOutOfBorderError", new Object[] {"width", String.valueOf(width), String.valueOf(MIN_SIZE), String.valueOf(MAX_SIZE)}, locale));
        }
        if (height == null) {
            //height = Integer.parseInt(QRCODE_DEFAULT_HEIGHT);
            height = QRCODE_DEFAULT_HEIGHT_INT; // SCIPIO
        }
        if (height < MIN_SIZE || height > MAX_SIZE) {
            return ServiceUtil.returnError(UtilProperties.getMessage("QRCodeUiLabels", "SizeOutOfBorderError",
                    new Object[] { "height", String.valueOf(height), String.valueOf(MIN_SIZE), String.valueOf(MAX_SIZE) }, locale));
        }
        if (UtilValidate.isEmpty(format)) {
            format = QRCODE_DEFAULT_FORMAT;
        }
        if (!FORMATS_SUPPORTED.contains(format)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("QRCodeUiLabels", "ErrorFormatNotSupported", new Object[] { format }, locale));
        }
        Map<EncodeHintType, Object> encodeHints = null;
        if (UtilValidate.isNotEmpty(encoding)) {
            encodeHints = new EnumMap<>(EncodeHintType.class);
            encodeHints.put(EncodeHintType.CHARACTER_SET, encoding);
        }

        // SCIPIO: 2018-08-22: error correct level
        String ecLevelStr = (String) context.get("ecLevel");
        ErrorCorrectionLevel ecLevel = null;
        if (UtilValidate.isNotEmpty(ecLevelStr)) {
            try {
                ecLevel = ErrorCorrectionLevel.valueOf(ecLevelStr);
            } catch(Exception e) {
                // TODO: localize
                return ServiceUtil.returnError("Invalid error correction level");
            }
        } else {
            ecLevel = defaultEcLevel;
        }
        if (ecLevel != null) {
            if (encodeHints == null) {
                encodeHints = new EnumMap<>(EncodeHintType.class);
            }
            encodeHints.put(EncodeHintType.ERROR_CORRECTION, ecLevel);
        }
        // SCIPIO: logo controls
        boolean useLogo = !Boolean.FALSE.equals(context.get("useLogo"));
        String logoImageSize = (String) context.get("logoImageSize");
        String logoImageMaxSize = (String) context.get("logoImageMaxSize");
        Map<String, Object> scalingOptions = UtilGenerics.checkMap(context.get("scalingOptions"));
        // use default size only if caller specified nothing at all
        if (useLogo) {
            if (logoImageSize == null && logoImageMaxSize == null
                    && logoImageMaxWidth == null && logoImageMaxHeight == null) {
                logoImageSize = QRCODE_DEFAULT_LOGOSIZE;
                logoImageMaxSize = QRCODE_DEFAULT_LOGOMAXSIZE;
            }
        }
        if ("none".equals(logoImageSize)) {
            logoImageSize = null;
        }
        if ("none".equals(logoImageMaxSize)) {
            logoImageMaxSize = null;
        }

        try {
            BitMatrix bitMatrix = new MultiFormatWriter().encode(message, BarcodeFormat.QR_CODE, width, height, encodeHints);
            BufferedImage bufferedImage = toBufferedImage(bitMatrix, format);
            BufferedImage logoBufferedImage = null;
            if (useLogo) { // SCIPIO: 2018-08-22: useLogoImage flag to prevent logo, when false
                if (UtilValidate.isNotEmpty(logoImage)) {
                    Map<String, Object> logoImageResult;
                    try {
                        logoImageResult = ImageTransform.getBufferedImage(FileUtil.getFile(logoImage).getAbsolutePath(), locale);
                        logoBufferedImage = (BufferedImage) logoImageResult.get("bufferedImage");
                    } catch (IllegalArgumentException | IOException e) {
                        Debug.logError(e, module);
                    }
                }
                if (UtilValidate.isEmpty(logoBufferedImage)) {
                    logoBufferedImage = defaultLogoImage;
                }
            }

            BufferedImage newBufferedImage = null;
            if (UtilValidate.isNotEmpty(logoBufferedImage)) {
                // SCIPIO: 2018-08-23: new more versatile code to handle logo image scaling
                ImageScaleSpec logoScaleMaxSpec;
                try {
                    logoScaleMaxSpec = ImageScaleSpec.fromExpr(logoImageMaxSize, locale);
                    if (logoScaleMaxSpec == null) {
                        // legacy ofbiz
                        if (UtilValidate.isNotEmpty(logoImageMaxWidth) || UtilValidate.isNotEmpty(logoImageMaxHeight)) {
                            logoScaleMaxSpec = ImageScaleSpec.fromFixed(logoImageMaxWidth, logoImageMaxHeight);
                        }
                    }
                } catch(Exception e) {
                    return ServiceUtil.returnError("Invalid logo max scaling specifications: " + e.toString()); // TODO: localize
                }
                ImageScaleSpec logoScaleSpec;
                try {
                    logoScaleSpec = ImageScaleSpec.fromExpr(logoImageSize, locale);
                } catch(Exception e) {
                    return ServiceUtil.returnError("Invalid logo scaling specifications: " + e.toString()); // TODO: localize
                }
                if (logoScaleSpec != null || logoScaleMaxSpec != null) {
                    try {
                        if (QRCODE_SCALER != null) {
                            if (scalingOptions == null) {
                                scalingOptions = new HashMap<>();
                                scalingOptions.put(ImageScalers.SCALER_NAME_OPT, QRCODE_SCALER);
                            } else {
                                if (!scalingOptions.containsKey(ImageScalers.SCALER_NAME_OPT)) {
                                    scalingOptions = new HashMap<>(scalingOptions);
                                    scalingOptions.put(ImageScalers.SCALER_NAME_OPT, QRCODE_SCALER);
                                }
                            }
                        }
                        Map<String, Object> logoImageResult = ImageTransform.scaleImageVersatile(logoBufferedImage, (double) logoBufferedImage.getHeight(), (double) logoBufferedImage.getWidth(),
                                bufferedImage.getHeight(), bufferedImage.getWidth(), logoScaleSpec, logoScaleMaxSpec, locale, scalingOptions);
                        if (ServiceUtil.isError(logoImageResult)) {
                            return ServiceUtil.returnError(UtilProperties.getMessage("QRCodeUiLabels", "ErrorGenerateQRCode", new Object[] { ServiceUtil.getErrorMessage(logoImageResult) }, locale));
                        }
                        if (logoImageResult.get("bufferedImage") != null) {
                            logoBufferedImage = (BufferedImage) logoImageResult.get("bufferedImage");
                        }
                    } catch(Exception e) {
                        return ServiceUtil.returnError(UtilProperties.getMessage("QRCodeUiLabels", "ErrorGenerateQRCode", new Object[] { e.getMessage() }, locale));
                    }
                }
                /*
                if (logoImageSize != null) { // SCIPIO
                    if (!logoImageSize.endsWith("%")) {
                        return ServiceUtil.returnError("logoImageSize only currently support percentage (%), must be suffixed with %: " + logoImageSize); // TODO: localize
                    }
                    // SCIPIO: 2018-08-23:
                    double sizePercent;
                    try {
                        sizePercent = Double.parseDouble(logoImageSize.substring(0, logoImageSize.length() - 1));
                        if (sizePercent <= 0 || sizePercent > 100) {
                            throw new NumberFormatException();
                        }
                    } catch(NumberFormatException e) {
                        return ServiceUtil.returnError("logoImageSize % must be between 0 and 100, but got: " + logoImageSize); // TODO: localize
                    }
                    double scalex = ((double) bufferedImage.getWidth()) / logoBufferedImage.getWidth();
                    double scaley = ((double) bufferedImage.getHeight()) / logoBufferedImage.getHeight();
                    double finalScale = Math.min(scalex, scaley) * (sizePercent / 100.0);
                    Map<String, Object> logoImageResult = ImageTransform.scaleImageExact(logoBufferedImage, (int) (logoBufferedImage.getHeight() * finalScale), (int) (logoBufferedImage.getWidth() * finalScale), locale, null);
                    if (ServiceUtil.isError(logoImageResult)) {
                        return ServiceUtil.returnError(UtilProperties.getMessage("QRCodeUiLabels", "ErrorGenerateQRCode", new Object[] { ServiceUtil.getErrorMessage(logoImageResult) }, locale));
                    }
                    logoBufferedImage = (BufferedImage) logoImageResult.get("bufferedImage");
                    // TODO: optimize: we make 2 scaleImage calls if both size and max params are specified; should calculate together...
                }
                */
                /*
                if (UtilValidate.isNotEmpty(logoImageMaxWidth) && UtilValidate.isNotEmpty(logoImageMaxHeight) && (logoBufferedImage.getWidth() > logoImageMaxWidth || logoBufferedImage.getHeight() > logoImageMaxHeight)) {
                    Map<String, String> typeMap = new HashMap<String, String>();
                    typeMap.put("width", logoImageMaxWidth.toString());
                    typeMap.put("height", logoImageMaxHeight.toString());
                    Map<String, Map<String, String>> dimensionMap = new HashMap<String, Map<String, String>>();
                    dimensionMap.put("QRCode", typeMap);
                    // SCIPIO: 2018-08-23: turns out this overload's height and width are backward, so this was a stock ofbiz bug of sorts...
                    //Map<String, Object> logoImageResult = ImageTransform.scaleImage(logoBufferedImage, (double) logoBufferedImage.getWidth(), (double) logoBufferedImage.getHeight(), dimensionMap, "QRCode", locale);
                    Map<String, Object> logoImageResult = ImageTransform.scaleImage(logoBufferedImage, (double) logoBufferedImage.getHeight(), (double) logoBufferedImage.getWidth(), dimensionMap, "QRCode", locale);
                    if (ServiceUtil.isError(logoImageResult)) { // SCIPIO: 2018-08-23
                        return ServiceUtil.returnError(UtilProperties.getMessage("QRCodeUiLabels", "ErrorGenerateQRCode", new Object[] { ServiceUtil.getErrorMessage(logoImageResult) }, locale));
                    }
                    logoBufferedImage = (BufferedImage) logoImageResult.get("bufferedImage");
                }*/

                BitMatrix newBitMatrix = bitMatrix.clone();
                newBufferedImage = toBufferedImage(newBitMatrix, format);
                Graphics2D graphics = newBufferedImage.createGraphics();
                graphics.drawImage(logoBufferedImage, new AffineTransformOp(AffineTransform.getTranslateInstance(1, 1), null), (newBufferedImage.getWidth() - logoBufferedImage.getWidth())/2, (newBufferedImage.getHeight() - logoBufferedImage.getHeight())/2);
                graphics.dispose();
            }

            if (UtilValidate.isNotEmpty(verifyOutput) && verifyOutput) {
                Decoder decoder = new Decoder();
                Map<DecodeHintType, Object> decodeHints = new EnumMap<>(DecodeHintType.class);
                decodeHints.put(DecodeHintType.TRY_HARDER, Boolean.TRUE);
                if (UtilValidate.isNotEmpty(encoding)) {
                    decodeHints.put(DecodeHintType.CHARACTER_SET, encoding);
                }
                DetectorResult detectorResult = null;
                if (UtilValidate.isNotEmpty(newBufferedImage)) {
                    BitMatrix newBitMatrix = createMatrixFromImage(newBufferedImage);
                    DecoderResult result = null;
                    try {
                        detectorResult = new Detector(newBitMatrix).detect(decodeHints);
                        result = decoder.decode(detectorResult.getBits(), decodeHints);
                    } catch (ChecksumException | FormatException | NotFoundException e) {
                        Debug.logError(e, module);
                    }
                    if (UtilValidate.isNotEmpty(result) && !result.getText().equals(message)) {
                        detectorResult = new Detector(bitMatrix).detect(decodeHints);
                        result = decoder.decode(detectorResult.getBits(), decodeHints);
                        if (!result.getText().equals(message)) {
                            return ServiceUtil.returnError(UtilProperties.getMessage("QRCodeUiLabels", "GeneratedTextNotMatchOriginal", new Object[]{result.getText(), message}, locale));
                        }
                    } else {
                        bufferedImage = newBufferedImage;
                    }
                } else {
                    detectorResult = new Detector(bitMatrix).detect(decodeHints);
                    DecoderResult result = decoder.decode(detectorResult.getBits(), decodeHints);
                    if (!result.getText().equals(message)) {
                        return ServiceUtil.returnError(UtilProperties.getMessage("QRCodeUiLabels", "GeneratedTextNotMatchOriginal", new Object[]{result.getText(), message}, locale));
                    }
                }
            } else if (UtilValidate.isNotEmpty(newBufferedImage)) {
                bufferedImage = newBufferedImage;
            }

            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("bufferedImage", bufferedImage);
            return result;
        } catch (WriterException e) {
            return ServiceUtil.returnError(UtilProperties.getMessage("QRCodeUiLabels", "ErrorGenerateQRCode", new Object[] { e.toString() }, locale));
        } catch (ChecksumException e) {
            return ServiceUtil.returnError(UtilProperties.getMessage("QRCodeUiLabels", "ErrorVerifyQRCode", new Object[] { e.toString() }, locale));
        } catch (FormatException e) {
            return ServiceUtil.returnError(UtilProperties.getMessage("QRCodeUiLabels", "ErrorVerifyQRCode", new Object[] { e.toString() }, locale));
        } catch (NotFoundException e) {
            return ServiceUtil.returnError(UtilProperties.getMessage("QRCodeUiLabels", "ErrorVerifyQRCode", new Object[] { e.toString() }, locale));
        }
    }

    /**
     * Renders a {@link BitMatrix} as an image, where "false" bits are rendered
     * as white, and "true" bits are rendered as black.
     *
     * This is to replace MatrixToImageWriter.toBufferedImage(bitMatrix) if you
     * find the output image is not right, you can change BufferedImage image =
     * new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB); to
     * BufferedImage image = new BufferedImage(width, height,
     * BufferedImage.TYPE_INT_RGB); or others to make it work correctly.
     */
    private static BufferedImage toBufferedImage(BitMatrix matrix, String format) {
        int width = matrix.getWidth();
        int height = matrix.getHeight();
        BufferedImage image = null;
        String osName = System.getProperty("os.name").toLowerCase(Locale.getDefault());
        image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        if (osName.startsWith("mac os") && format.equals("png")) {
            image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
        }
        for (int x = 0; x < width; x++) {
            for (int y = 0; y < height; y++) {
                image.setRGB(x, y, matrix.get(x, y) ? BLACK : WHITE);
            }
        }
        return image;
    }

    private static BitMatrix createMatrixFromImage(BufferedImage image) {
        int width = image.getWidth();
        int height = image.getHeight();
        int[] pixels = new int[width * height];
        image.getRGB(0, 0, width, height, pixels, 0, width);

        BitMatrix matrix = new BitMatrix(width, height);
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                int pixel = pixels[y * width + x];
                int luminance = (306 * ((pixel >> 16) & 0xFF) +
                    601 * ((pixel >> 8) & 0xFF) +
                    117 * (pixel & 0xFF)) >> 10;
                if (luminance <= 0x7F) {
                    matrix.set(x, y);
                }
            }
        }
        return matrix;
    }
}
