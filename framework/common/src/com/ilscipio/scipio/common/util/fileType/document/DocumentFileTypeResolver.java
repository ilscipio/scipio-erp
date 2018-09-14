package com.ilscipio.scipio.common.util.fileType.document;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.common.util.fileType.AbstractFileType;
import com.ilscipio.scipio.common.util.fileType.FileTypeResolver;

/**
 * File type resolver focused on resolving document types. This is likely to be
 * moved to an entity at some point.
 *
 * @author jsoto
 */
public class DocumentFileTypeResolver extends FileTypeResolver {

    protected static final List<AbstractFileType> fileTypes;
    static {
        List<AbstractFileType> ft = new ArrayList<>();
        ft.add(new PdfFileType());
        ft.add(new DocFileType());
        ft.add(new XlsFileType());
        ft.add(new PptFileType());
        ft.add(new OoxmlFileType());
        ft.add(new PsdFileType());
        fileTypes = Collections.unmodifiableList(ft);
    }
    protected final Set<String> manualSupportedMediaTypes;

    protected DocumentFileTypeResolver(Delegator delegator, ResolverConfig resolverConfig) {
        super(delegator, resolverConfig);

        Set<String> mediaTypes = collectManualSupportedMediaTypes(delegator, fileTypes);
        // 2017-02-08: not needed with new tika code; for manual, just define new type instead...
        //mediaTypes.add("image/x-psd");
        manualSupportedMediaTypes = Collections.unmodifiableSet(mediaTypes);
    }

    public static DocumentFileTypeResolver getInstance(Delegator delegator, ResolverConfig resolverConfig) {
        return new DocumentFileTypeResolver(delegator, resolverConfig);
    }

    @Override
    public GenericValue findMimeType(ByteBuffer byteBuffer, String fileName) throws GeneralException {
        GenericValue mimeType = super.findMimeType(byteBuffer, fileName);

        // SPECIAL: for documents, if we couldn't determine a mime-type,
        // or the mime-type determined is not registered in MimeType entity,
        // just get as octet-stream.
        if (mimeType == null) {
            mimeType = delegator.findOne("MimeType", true, UtilMisc.toMap("mimeTypeId", "application/octet-stream"));
        }

        return mimeType;
    }

    public static class PdfFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public PdfFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("25504446", "0A2525454F46", "Adobe Portable Document Format, Forms Document Format, and Illustrator graphics files", 0, "pdf",
                    "fdf", "ai")

            );
        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }
    }

    public static class DocFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public DocFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            // This is actually a subheader, the header is generic and used for
            // office suite apps

            new MagicNumber("ECA5C100", null, "Word document subheader (MS Office)", 512, "doc"),

            new MagicNumber("D0CF11E0A1B11AE1", null,
                    "An Object Linking and Embedding (OLE) Compound File (CF) (i.e., OLECF) file format, known as Compound Binary File format by Microsoft, "
                            + "used by Microsoft Office 97-2003 applications (Word, Powerpoint, Excel, Wizard)",
                    0, "doc")

            );
        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }
    }

    public static class XlsFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public XlsFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            // These are actually subheaders, the header is generic and used for
            // office suite apps

            new MagicNumber("0908100000060500", null, "Excel spreadsheet subheader (MS Office)", 512, "xls"),

            new MagicNumber("FDFFFFFFxx00", new byte[] { (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x00, (byte) 0xFF }, null,
                    "Excel spreadsheet subheader (MS Office)", 512, "xls"),

            new MagicNumber("FDFFFFFFxx02", new byte[] { (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x00, (byte) 0xFF }, null,
                    "Excel spreadsheet subheader (MS Office)", 512, "xls"),

            new MagicNumber("FDFFFFFF20000000", null, "Excel spreadsheet subheader (MS Office)", 512, "xls")

            );

        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }
    }

    public static class PptFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public PptFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            // NOTE: These are actually subheaders, the header is generic and
            // used for office suite apps

            new MagicNumber("006E1EF0", null, "PowerPoint presentation subheader (MS Office)", 512, "ppt"),

            new MagicNumber("0F00E803", null, "PowerPoint presentation subheader (MS Office)", 512, "ppt"),

            new MagicNumber("A0461DF0", null, "PowerPoint presentation subheader (MS Office)", 512, "ppt"),

            new MagicNumber("FDFFFFFFxxxx0000",
                    new byte[] { (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x00, (byte) 0x00, (byte) 0xFF, (byte) 0xFF }, null,
                    "PowerPoint presentation subheader (MS Office)", 512, "ppt")

            );

        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }
    }

    public static class OoxmlFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public OoxmlFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            // NOTE (extracted from
            // http://www.garykessler.net/library/file_sigs.html): There is no
            // subheader for MS OOXML files as there is with
            // DOC, PPT, and XLS files. To better understand the format of these
            // files, rename any OOXML file to have a .ZIP extension and then
            // unZIP the file; look at the resultant file named
            // [Content_Types].xml to see the content types. In particular, look
            // for the <Override PartName=tag, where you will find word, ppt, or
            // xl, respectively.

            // TODO: (optional) Figure out how to handle this case so we can
            // assert for sure what the file type is by proceeding as the note
            // about says

            // FIXME: Not sure why this whole magic number don't work, anyhow
            // this should be improved so it can handle complex cases like MS
            // Office common file types

            // new MagicNumber("504B030414000600", null, "504B0506", 0,
            // "Microsoft Office Open XML Format (OOXML) Document",
            // "docx", "pptx", "xlsx")

            new MagicNumber("504B03041400", null, "504B0506", 0, "Microsoft Office Open XML Format (OOXML) Document", "docx", "pptx", "xlsx")

            );

        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }
    }

    @Override
    public String getProvidedType() {
        return DOCUMENT_TYPE;
    }

    public static class PsdFileType extends AbstractFileType {
        private final List<MagicNumber> MAGIC_NUMBERS = new ArrayList<>();

        public PsdFileType() {
            Collections.addAll(MAGIC_NUMBERS,

            new MagicNumber("38425053", null, "Photoshop image file", 0, "psd")

            );
        }

        @Override
        public List<MagicNumber> getMagicNumbers() {
            return MAGIC_NUMBERS;
        }
    }

    @Override
    public boolean isAllowedMediaTypePermissive(String mediaType) {
        // SPECIAL: no condition here: Document will allow anything:
        //return mediaTypeOrAliasHasPrefix(mediaType, "application/");
        return true;
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
