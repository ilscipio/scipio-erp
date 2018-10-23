package com.ilscipio.scipio.ce.webapp.ftl.doc;

import java.io.File;
import java.util.HashMap;
import java.util.IllegalFormatException;
import java.util.LinkedHashMap;
import java.util.Map;

import com.ilscipio.scipio.ce.webapp.ftl.doc.FtlDocException.ParseException;

public abstract class FtlDocFileParser {

    protected MsgHandler msgHandler = new MsgHandler.VoidMsgHandler();

    protected final String libFilename;
    protected final File srcFile;
    protected final String inFileExtension;
    protected final String outFileExtension;

    protected FtlDocFileParser(String libFilename, File srcFile, String inFileExtension, String outFileExtension) {
        this.libFilename = libFilename;
        this.srcFile = srcFile;
        this.inFileExtension = (inFileExtension != null) ? inFileExtension : "";
        this.outFileExtension = (outFileExtension != null) ? outFileExtension : "";
    }

    public void setMsgHandler(MsgHandler msgHandler) {
        this.msgHandler = msgHandler;
    }

    public static FtlDocFileParser getInstance(String libFilename, File srcFile,
            String inFileExtension, String outFileExtension, String defaultLibFormat) {
        // TODO?: currently only supports one defaultLibFormat
        if (FtlDocCompiler.SCIPIO_LIB_FORMAT.equals(defaultLibFormat)) {
            return new ScipioLibFtlDocFileParser(libFilename, srcFile, inFileExtension, outFileExtension);
        }
        else {
            throw new IllegalArgumentException("Invalid lib format: " + defaultLibFormat);
        }
    }

    public abstract void parseLib(Map<String, Object> dataModel, String text) throws ParseException;

    public void setLibProperties(Map<String, Object> dataModel) throws IllegalFormatException {
        // get file name only
        String libTopName = srcFile.getName();
        // make a separate hash as well, easier to pass around
        Map<String, Object> libInfo = makeObjectMap();
        libInfo.put("libTopName", FtlDocUtil.replaceExtension(libTopName, ""));
        libInfo.put("libFilename", getLibFilename());
        libInfo.put("libFormat", getLibFormat());
        libInfo.put("libName", getLibName());
        libInfo.put("libDocPath", getLibDocPath());
        // map for easy passing
        dataModel.put("libInfo", libInfo);
        // putall for easy access
        dataModel.putAll(libInfo);
    }

    public String getLibFilename() {
        return libFilename;
    }

    public String getLibName() {
        return FtlDocUtil.replaceExtension(libFilename, "");
    }

    public String getLibDocPath() {
        return FtlDocUtil.replaceExtension(libFilename, outFileExtension);
    }

    public abstract String getLibFormat();


    protected static <T> Map<String, T> makeDataMap() {
        return new LinkedHashMap<>();
    }

    protected static <T> Map<String, T> makeDataMap(Map<String, ? extends T> from) {
        return new LinkedHashMap<>(from);
    }

    protected static <T> Map<String, T> makeObjectMap() {
        return new HashMap<>();
    }

    protected static <T> Map<String, T> makeObjectMap(Map<String, ? extends T> from) {
        return new HashMap<>(from);
    }
}