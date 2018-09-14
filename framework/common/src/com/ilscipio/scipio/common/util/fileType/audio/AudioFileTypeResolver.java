package com.ilscipio.scipio.common.util.fileType.audio;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.ofbiz.entity.Delegator;

import com.ilscipio.scipio.common.util.fileType.AbstractFileType;
import com.ilscipio.scipio.common.util.fileType.FileTypeResolver;

/**
 * File type resolver focused on resolving audio types. This is likely to be
 * moved to an entity at some point.
 *
 * @author jsoto
 *
 */
public class AudioFileTypeResolver extends FileTypeResolver {

    protected static final List<AbstractFileType> fileTypes;
    static {
        List<AbstractFileType> ft = new ArrayList<>();
        ft.add(new Mp3FileType());
        ft.add(new OggFileType());
        ft.add(new M4aFileType());
        ft.add(new WavFileType());
        fileTypes = Collections.unmodifiableList(ft);
    }
    protected final Set<String> manualSupportedMediaTypes;

    protected AudioFileTypeResolver(Delegator delegator, ResolverConfig resolverConfig) {
        super(delegator, resolverConfig);

        manualSupportedMediaTypes = Collections.unmodifiableSet(collectManualSupportedMediaTypes(delegator, fileTypes));
    }

    public static AudioFileTypeResolver getInstance(Delegator delegator, ResolverConfig resolverConfig) {
        return new AudioFileTypeResolver(delegator, resolverConfig);
    }

    @Override
    public String getProvidedType() {
        return AUDIO_TYPE;
    }

    public static class Mp3FileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public Mp3FileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("FFEx", new byte[] { (byte) 0xFF, (byte) 0xF0 }, null, "MPEG audio file frame", 0, "mpeg", "mpg", "mp3"),

            new MagicNumber("FFFx", new byte[] { (byte) 0xFF, (byte) 0xF0 }, null, "MPEG audio file frame", 0, "mpeg", "mpg", "mp3"),

            new MagicNumber("494433", null, null, 0, "MPEG-1 Audio Layer 3 (MP3) audio file", "mp3")

            );
        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }

    }

    public static class OggFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public OggFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("4F67675300020000000000000000", null, null, 0, "Ogg Vorbis Codec compressed Multimedia file", "oga", "ogg", "ogv", "ogx")

            );
        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }

    }

    public static class M4aFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public M4aFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("00000020667479704D344120", null, null, "Apple Lossless Audio Codec file", 0, "m4a"),

            new MagicNumber("667479704D344120", null, null, "Apple Lossless Audio Codec file", 4, "m4a")

            );
        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }

    }

    public static class WavFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public WavFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("52494646xxxxxxxx57415645666D7420",
                    new byte[] { (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0xFF,
                            (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF },
                    null, "Apple Lossless Audio Codec file", 0, "wav"), // "m4a"??

            new MagicNumber("667479704D344120", null, null, "Resource Interchange File Format -- Audio for Windowsfile", 0, "wav")

            );
        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }

    }

    @Override
    public boolean isAllowedMediaTypePermissive(String mediaType) {
        return mediaTypeOrAliasHasPrefix(mediaType, "audio/");
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
        if (mediaType.startsWith("video/")) {
            mediaType = mediaType.replaceFirst("^video/", "audio/");
        }
        return mediaType;
    }

}
