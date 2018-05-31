package com.ilscipio.scipio.common.util.fileType.image;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.ofbiz.entity.Delegator;

import com.ilscipio.scipio.common.util.fileType.AbstractFileType;
import com.ilscipio.scipio.common.util.fileType.FileTypeResolver;

/**
 * File type resolver focused on resolving image types. This is likely to be
 * moved to an entity at some point.
 * 
 * @author jsoto
 *
 */
public class ImageFileTypeResolver extends FileTypeResolver {

    protected static final List<AbstractFileType> fileTypes;
    static {
        List<AbstractFileType> ft = new ArrayList<>();
        ft.add(new JpegFileType());
        ft.add(new GifFileType());
        ft.add(new PngFileType());
        ft.add(new IcoFileType());
        fileTypes = Collections.unmodifiableList(ft);
    }
    protected final Set<String> manualSupportedMediaTypes;
    
    protected ImageFileTypeResolver(Delegator delegator, ResolverConfig resolverConfig) {
        super(delegator, resolverConfig);
        
        manualSupportedMediaTypes = Collections.unmodifiableSet(collectManualSupportedMediaTypes(delegator, fileTypes));
    }
    
    public static ImageFileTypeResolver getInstance(Delegator delegator, ResolverConfig resolverConfig) {
        return new ImageFileTypeResolver(delegator, resolverConfig);
    }

    public static class JpegFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public JpegFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("FFD8FFE0xxxx4A46494600", new byte[] { (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x00, (byte) 0x00, (byte) 0xFF,
                    (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF }, "FFD9", "Standard JPEG/JFIF file", 0, "jfif", "jpe", "jpeg", "jpg"),

            new MagicNumber("FFD8FFE1xxxx4578696600", new byte[] { (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x00, (byte) 0x00, (byte) 0xFF,
                    (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF }, "FFD9", "Standard JPEG/Exif file", 0, "jpg"),

            new MagicNumber("FFD8FFE2", null, "FFD9", 0, "Canon EOS-1D JPEG file", "jpg"),

            new MagicNumber("FFD8FFE3", null, "FFD9", 0, "Samsung D500 JPEG file", "jpg"),

            new MagicNumber(
                    "FFD8FFE8xxxx535049464600", new byte[] { (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x00, (byte) 0x00, (byte) 0xFF,
                            (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF },
                    "FFD9", "Still Picture Interchange File Format (SPIFF)", 0, "jpg"),

            new MagicNumber("FFD8FFDB", null, "FFD9", 0, "Samsung D807 JPEG file", "jpg")

            );
        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }

    }

    public static class GifFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public GifFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("474946383761", null, "003B", 0, "Graphics interchange format file", "gif"),

            new MagicNumber("474946383961", null, "003B", 0, "Graphics interchange format file", "gif")

            );
        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }

    }

    public static class PngFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public PngFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("89504E470D0A1A0A", null, "49454E44AE426082", 0, "Portable Network Graphics file", "png")

            );

        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }

    }

    public static class IcoFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public IcoFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("00000100", null, null, 0, "Windows icon file", "ico")

            );

        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }

    }

    @Override
    public String getProvidedType() {
        return IMAGE_TYPE;
    }
    
    @Override
    public boolean isAllowedMediaTypePermissive(String mediaType) {
        return mediaTypeOrAliasHasPrefix(mediaType, "image/");
    }

    @Override
    protected List<AbstractFileType> getFileTypes() {
        return fileTypes;
    }
    
    @Override
    protected Set<String> getManualSupportedMediaTypes() {
        return manualSupportedMediaTypes;
    }
    
    @Override
    public String adjustMediaTypeManual(String mediaType) {
        return mediaType;
    }
}
