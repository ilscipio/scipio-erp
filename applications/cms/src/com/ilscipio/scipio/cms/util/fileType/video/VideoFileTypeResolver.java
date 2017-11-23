package com.ilscipio.scipio.cms.util.fileType.video;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.ofbiz.entity.Delegator;

import com.ilscipio.scipio.cms.util.fileType.AbstractFileType;
import com.ilscipio.scipio.cms.util.fileType.FileTypeResolver;

/**
 * File type resolver focused on resolving video types. This is likely to be
 * moved to an entity at some point.
 * 
 * @author jsoto
 *
 */
public class VideoFileTypeResolver extends FileTypeResolver {

    protected static final List<AbstractFileType> fileTypes;
    static {
        List<AbstractFileType> ft = new ArrayList<>();
        ft.add(new Mp4FileType());
        ft.add(new AviFileType());
        ft.add(new MkvFileType());
        ft.add(new MovFileType());
        ft.add(new MpgFileType());
        ft.add(new WmvFileType());
        ft.add(new OggFileType());
        ft.add(new Gpp3FileType());
        ft.add(new WebMFileType());
        fileTypes = Collections.unmodifiableList(ft);
    }
    protected final Set<String> manualSupportedMediaTypes;
    
    protected VideoFileTypeResolver(Delegator delegator, ResolverConfig resolverConfig) {
        super(delegator, resolverConfig);
        
        Set<String> mediaTypes = collectManualSupportedMediaTypes(delegator, fileTypes);
        // 2017-02-08: not needed with new tika code; for manual, just define new type instead...
        //mediaTypes.add("video/x-flv");
        manualSupportedMediaTypes = Collections.unmodifiableSet(mediaTypes);
    }
    
    public static VideoFileTypeResolver getInstance(Delegator delegator, ResolverConfig resolverConfig) {
        return new VideoFileTypeResolver(delegator, resolverConfig);
    }

    public static class Mp4FileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public Mp4FileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("000000146674797069736F6D", null, "ISO Base Media file (MPEG-4) v1", 0, "mp4"),

            new MagicNumber("000000186674797033677035", null, null, 0, "MPEG-4 video files", "mp4"),

            new MagicNumber("00000018667479706D703432", null, null, 0, "MPEG-4 video/QuickTime file", "m4v"),

            new MagicNumber("0000001C667479704D534E56012900464D534E566D703432", null, null, 0, "MPEG-4 video file", "mp4"),

            new MagicNumber("6674797033677035", null, null, 4, "MPEG-4 video file", "mp4"),

            new MagicNumber("6674797069736F6D", null, "ISO Base Media file (MPEG-4) v1", 4, "mp4"),

            new MagicNumber("667479704D534E56", null, null, 4, "MPEG-4 video file", "mp4"),

            new MagicNumber("667479706D703432", null, null, 4, "MPEG-4 video/QuickTime file", "m4v")

            );
        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }

    }

    public static class AviFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public AviFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("52494646xxxxxxxx415649204C495354",
                    new byte[] { (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0xFF,
                            (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF },
                    null, "Resource Interchange File Format", 0, "avi")

            );
        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }

    }

    public static class MkvFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public MkvFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("1A45DFA3934282886D6174726F736B61", null, null, 0, "Matroska stream file", "mkv")

            );
        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }

    }

    public static class MovFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public MovFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("6674797071742020", null, null, 4, "QuickTime movie file", "mov")

            );
        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }

    }

    public static class WmvFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public WmvFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("3026B2758E66CF11A6D900AA0062CE6C", null, null, 0, "Microsoft Windows Media Audio/Video File", "wmv", "wma", "asf")

            );
        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }

    }

    public static class MpgFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public MpgFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("000001Bx", new byte[] { (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xF0 }, "000001B7", 
                    "MPEG video file", 0, "mpeg", "mpg")

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

    public static class Gpp3FileType extends AbstractFileType {

        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public Gpp3FileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("66747970336770", null, null, 4, "3rd Generation Partnership Project 3GPP multimedia files", "3gg", "3gp", "3g2"),

            new MagicNumber("0000002066747970336770", null, null, 0, "3rd Generation Partnership Project 3GPP multimedia files", "3gg", "3gp", "3g2")

            );
        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }

    }

    public static class WebMFileType extends AbstractFileType {

        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public WebMFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("1A45DFA3", null, null, 0, "WebM video file", "webm")

            );
        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }

    }

    @Override
    public String getProvidedType() {
        return VIDEO_TYPE;
    }

    @Override
    public boolean isAllowedMediaTypePermissive(String mediaType) {
        return mediaTypeOrAliasHasPrefix(mediaType, "video/");
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
        if (mediaType.startsWith("audio/")) {
            mediaType = mediaType.replaceFirst("^audio/", "video/");
        }
        return mediaType;
    }
}
