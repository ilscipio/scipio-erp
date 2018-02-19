package com.ilscipio.scipio.common.util.fileType;

import java.math.BigDecimal;
import java.util.Locale;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilFormatOut;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;

public class FileTypeUtil {
    
    private static final String module = "FileTypeUtil";

    public static final String FILE_SIZE_ATTRIBUTE_NAME = "fileSize";
    // 2017-02-07: not using; not abstracted enough
    //public static final String MAGIC_NUMBER_ATTRIBUTE_NAME = "magicNumber";
    public static final String MEDIA_TYPE_ATTRIBUTE_NAME = "mediaType";
    
    public static int sizeOfFloat(float f) {
        return Float.floatToIntBits(f) / Byte.SIZE;
    }

    public static long sizeOfDouble(double d) {
        return Double.doubleToLongBits(d) / Byte.SIZE;
    }

    public static int sizeOfInt(int i) {
        return Integer.toBinaryString(i).length() / Byte.SIZE;
    }

    public static int sizeOfLong(long l) {
        return Long.toBinaryString(l).length() / Byte.SIZE;
    }

    public static int sizeOfChar(char c) {
        return Integer.toBinaryString(Character.getNumericValue(c)).length() / Byte.SIZE;
    }

    public static byte[] hexStringToByteArray(String s) {
        if (UtilValidate.isNotEmpty(s)) {
            int len = s.length();
            // This must be an even string length (every hex value represents 4
            // bit so length can't be odd)
            if (len % 2 == 0) {
                byte[] data = new byte[len / 2];
                for (int i = 0; i < len; i += 2) {
                    int digitChar1 = (s.charAt(i) != 'x') ? (Character.digit(s.charAt(i), 16) << 4) : 0;
                    int digitChar2 = (s.charAt(i + 1) != 'x') ? (Character.digit(s.charAt(i + 1), 16)) : 0;

                    data[i / 2] = (byte) (digitChar1 + digitChar2);
                }
                return data;
            }
        }
        return null;
    }

    public static String getFileExtension(String fileName) {
        int fileExtensionDot = fileName.lastIndexOf(".");
        if (fileExtensionDot > 1) {
            if ((fileExtensionDot + 1) < fileName.length())
                return fileName.substring(fileExtensionDot + 1);
        }
        return null;
    }

    public static String formatFileSize(String fileSize, LocalDispatcher dispatcher, Locale locale) {
        Delegator delegator = dispatcher.getDelegator();
        BigDecimal bytes = new BigDecimal(fileSize);
        long bytesLong = bytes.longValue();
        double convertedFileSize = bytesLong;

        String uomIdTo = "DATA_B";
        if (bytesLong > 1024) {
            uomIdTo = "DATA_KB";
            convertedFileSize = convertedFileSize / 1024;
            if (bytesLong > 1024 * 1024) {
                uomIdTo = "DATA_MB";
                convertedFileSize = convertedFileSize / 1024;
                if (bytesLong > 1024 * 1024 * 1024) {
                    uomIdTo = "DATA_GB";
                    convertedFileSize = convertedFileSize / 1024;
                    if (bytesLong > 1024 * 1024 * 1024 * 1024) {
                        convertedFileSize = convertedFileSize / 1024;
                        uomIdTo = "DATA_TB";
                    }
                    // I highly doubt somebody will support files larger than a
                    // TeraByte max size
                }
            }

        }

        // BigDecimal convertedUom = UomWorker.convertUom(bytes, "DATA_B",
        // uomIdTo, dispatcher);
        GenericValue uom = null;
        try {
            uom = delegator.findOne("UomAndType", true, UtilMisc.toMap("typeUomTypeId", "DATA_MEASURE", "uomId", uomIdTo));
        } catch (GenericEntityException e) {
            Debug.logError(e.getMessage(), module);
        }

        if (UtilValidate.isNotEmpty(convertedFileSize) && UtilValidate.isNotEmpty(uom)) {
            String template = UtilProperties.getPropertyValue("arithmetic", "accounting-number.format", "#,##0.00;(#,##0.00)");
            String formattedFileSize = UtilFormatOut.formatDecimalNumber(convertedFileSize, template, locale);

            return formattedFileSize + " " + uom.getString("abbreviation");
        }

        return null;
    }

}
