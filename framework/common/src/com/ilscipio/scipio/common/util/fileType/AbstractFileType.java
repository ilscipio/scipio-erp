package com.ilscipio.scipio.common.util.fileType;

import java.util.Arrays;
import java.util.List;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

public abstract class AbstractFileType {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    protected abstract List<MagicNumber> getMagicNumbers();

    public static class MagicNumber {
        private final String magicNumber;
        private final byte[] magicNumberMask;
        private final String trailer;
        private final List<String> extensions;
        private final int offset;
        private final String description;

        public MagicNumber(String magicNumber, String trailer, String description, int offset) {
            this(magicNumber, null, trailer, description, offset, (String[]) null);
        }

        public MagicNumber(String magicNumber, String trailer, String description, int offset, String... extensions) {
            this(magicNumber, null, trailer, description, offset, extensions);
        }

        public MagicNumber(String magicNumber, byte[] magicNumberMask, String trailer, String description, int offset) {
            this(magicNumber, magicNumberMask, description, trailer, offset, (String[]) null);
        }

        public MagicNumber(String magicNumber, byte[] magicNumberMask, String trailer, String description, int offset, String... extensions) {
            if (UtilValidate.isEmpty(magicNumberMask)) {
                // Fill the mask with the max 8 bit value (255)
                magicNumberMask = new byte[FileTypeUtil.hexStringToByteArray(magicNumber).length];
                for (int i = 0; i < magicNumberMask.length; i++) {
                    magicNumberMask[i] = Byte.MAX_VALUE;
                }
            }

            this.magicNumber = magicNumber;
            this.magicNumberMask = magicNumberMask;
            this.trailer = trailer;
            this.offset = offset;
            this.description = description;
            if (UtilValidate.isNotEmpty(extensions))
                this.extensions = Arrays.asList(extensions);
            else
                this.extensions = null;
        }

        public String getMagicNumber() {
            return magicNumber;
        }

        public byte[] getMagicNumberMask() {
            return magicNumberMask;
        }

        public String getTrailer() {
            return trailer;
        }

        public int getOffset() {
            return offset;
        }

        public List<String> getExtensions() {
            return extensions;
        }

        public String getDescription() {
            return description;
        }
        
        /**
         * WARN: this does not perform coercions such as video/ prefix to audio/ and vice versa...
         * see FileTypeResolver.adjustMediaTypeManual.
         * @deprecated shouldn't need anymore
         */
        @Deprecated
        public String getMediaType(Delegator delegator, String extension) {
            if (UtilValidate.isEmpty(extension) || !extensions.contains(extension)) {
                return null;
            }
            try {
                GenericValue fileTypeExtension = delegator.findOne("FileExtension", true, UtilMisc.toMap("fileExtensionId", extension));
                if (UtilValidate.isNotEmpty(fileTypeExtension)) {
                    List<GenericValue> mimeTypeList = fileTypeExtension.getRelated("MimeType", null, null, true);
                    if (mimeTypeList.size() > 0)
                        return mimeTypeList.get(0).getString("mimeTypeId");
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, "Could not get media/mime type from database via FileExtension for extension: " + extension, module);
            }
            return null;
        }
    }

}
