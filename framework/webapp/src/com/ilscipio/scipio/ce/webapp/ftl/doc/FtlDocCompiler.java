package com.ilscipio.scipio.ce.webapp.ftl.doc;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.ilscipio.scipio.ce.webapp.ftl.doc.FtlDocException.ParseException;

import freemarker.ext.beans.BeansWrapper;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;

/**
 * SCIPIO: Customized Freemarker library documentation compiler and generator.
 * Transforms a set of source FTL files into HTML documentation files.
 * <p>
 * <strong>REQUIRES FREEMARKER 2.3.22 OR LATER IN CLASSPATH FOR INVOCATION.</strong>
 * <p>
 * Main command line arguments (minimum 5 arguments needed for invocation):<br/>
 * <code>[lib format] [doc purpose] [base source folder] [output folder] [FTL doc-formatting template location] [series of relative source file paths, at least one]</code><br/>
 * The only currently supported lib format is "scipio-lib".
 * <p>
 * TODO: This is currently compiled as part of the Ofbiz compilation, but
 * it should be in its own separate project.
 * <p>
 * FIXME?: Only supports one lib format for all sources.
 * <p>
 * DEV NOTE: <strong>WARN</strong>: Do not use Ofbiz libraries here.
 * <p>
 * TODO: Could consider use of markdown (or other) in descriptions.
 * <p>
 * TODO: Replace MsgHandler by log4j
 * <p>
 * TODO: License
 * <p>
 * WARN: "\n" is intentionally hardcoded and source files should only have LF line endings.
 * <p>
 * WARN: Input documentation formatting (parsing) is sensitive to whitespace, presence and number of asterisks (*),
 *    and line endings. Must be spaces-only and LF-only line endings.
 */
public class FtlDocCompiler {

    public static boolean DEBUG = false;
    
    public static final String EXTENSION_NONE = "_NONE_";
    
    public static final String SCIPIO_LIB_FORMAT = "scipio-lib";
    public static final Charset DEFAULT_FILE_ENCODING = StandardCharsets.UTF_8;

    protected MsgHandler msgHandler = new MsgHandler.VoidMsgHandler();
    
    protected String inFileExtension = ".ftl";
    protected String outFileExtension = ".html";
    
    protected Configuration cfg = null;
    
    protected String docPurpose = "templating";
    
    protected String defaultLibFormat = SCIPIO_LIB_FORMAT;
    protected Charset defaultEncoding = DEFAULT_FILE_ENCODING;
    
    // IN
    protected String srcFolderPath = null;
    protected File srcFolderPathFile = null;
    protected List<String> libFilenames = null;
    protected List<File> libFiles = null;
    protected String templatePath = null;
    protected File templatePathFile = null;
    protected String outFolderPath = null;
    protected File outFolderPathFile = null;


    // OUT
    protected String targetLibName = null; // for single render mode(s)
    protected Map<String, Object> targetDataModel = null; // for single render mode(s)

    // RESULTS (CACHED/INTERMEDIATE)
    protected Map<String, Map<String, Object>> srcFileDataModels = null; // this is the motherload
    

    public FtlDocCompiler() {
    }
    
    /**
     * Gets default instance, which parses Ilscipio Scipio macro doc format.
     * <p>
     * FIXME: this may not make sense anymore, but whatever
     */
    public static FtlDocCompiler getInstance() {
        return new FtlDocCompiler();
    }
    
    
    public static void main(String[] args) {
        MsgHandler msgHandler = new MsgHandler.SysOutMsgHandler();
        
        if (args.length >= 6) {
            String defaultLibFormat = args[0];
            String docPurpose = args[1];
            String srcFolderPath = args[2];
            String outFolderPath = args[3];
            String outFileExtension = args[4];
            String templatePath = args[5];
            List<String> libFilenames = Arrays.asList(args).subList(6, args.length);

            if (EXTENSION_NONE.equals(outFileExtension)) {
                outFileExtension = "";
            }
                    
            try {
                FtlDocCompiler compiler = FtlDocCompiler.getInstance();
                compiler.setMsgHandler(msgHandler);
                compiler.execBasic(defaultLibFormat, docPurpose, srcFolderPath, libFilenames, outFileExtension, templatePath, outFolderPath);
            } catch (Throwable t) {
                msgHandler.logError(t.getMessage());
                t.printStackTrace();
                System.exit(1);
            }
        }
        else {
            msgHandler.logError("Insufficient arguments");
            System.exit(1);
        }
    }
    
    /* Getters */
    
    public MsgHandler getMsgHandler() {
        return msgHandler;
    }

    public String getDocPurpose() {
        return docPurpose;
    }

    public String getDefaultLibFormat() {
        return defaultLibFormat;
    }

    public Charset getDefaultEncoding() {
        return defaultEncoding;
    }
    
    public String getInFileExtension() {
        return inFileExtension;
    }

    public String getOutFileExtension() {
        return outFileExtension;
    }

    public String getSrcFolderPath() {
        return srcFolderPath;
    }

    public File getSrcFolderPathFile() {
        return srcFolderPathFile;
    }

    public List<String> getLibFilenames() {
        return libFilenames;
    }

    public List<File> getLibFiles() {
        return libFiles;
    }

    public String getTemplatePath() {
        return templatePath;
    }

    public File getTemplatePathFile() {
        return templatePathFile;
    }

    public String getOutFolderPath() {
        return outFolderPath;
    }

    public File getOutFolderPathFile() {
        return outFolderPathFile;
    }

    public String getTargetLibName() {
        return targetLibName;
    }

    public Map<String, Object> getTargetDataModel() {
        return targetDataModel;
    }

    
    /* exec */

    public void execBasic(String defaultLibFormat, String docPurpose, String srcFolderPath, List<String> libFilenames, 
            String outFileExtension, String templatePath, String outFolderPath) throws IllegalArgumentException, IOException, TemplateException, ParseException {
        //msgHandler.logInfo("Setting sources and output...");
        setDefaultLibFormat(defaultLibFormat);
        setDocPurpose(docPurpose);
        setSources(srcFolderPath, libFilenames);
        setOutFileExtension(outFileExtension);
        setTemplatePath(templatePath);
        setOutputFolder(outFolderPath);
        
        init();
        compile();
    }
    
    public void execDataLoadOnly(String defaultLibFormat, String docPurpose, String srcFolderPath, List<String> libFilenames, 
            String targetLibName, Map<String, Object> targetDataModel) throws IllegalArgumentException, IOException, TemplateException, ParseException {
        //msgHandler.logInfo("Setting sources...");
        setDefaultLibFormat(defaultLibFormat);
        setDocPurpose(docPurpose);
        setSources(srcFolderPath, libFilenames);
        
        setTargetLibName(targetLibName);
        setTargetDataModel(targetDataModel);
        
        initDataOnly();
        compileDataOnly();
    }
    
    public void setMsgHandler(MsgHandler msgHandler) {
        this.msgHandler = msgHandler;
    }

    public void setDefaultLibFormat(String defaultLibFormat) {
        this.defaultLibFormat = defaultLibFormat;
    }
    
    public void setDocPurpose(String docPurpose) {
        this.docPurpose = docPurpose;
    }
    
    public void setInFileExtension(String inFileExtension) {
        if (inFileExtension == null) {
            inFileExtension = "";
        }
        this.inFileExtension = inFileExtension;
    }

    public void setOutFileExtension(String outFileExtension) {
        if (outFileExtension == null) {
            outFileExtension = "";
        }
        this.outFileExtension = outFileExtension;
    }

    public void setSources(String srcFolderPath, List<String> libFilenames) throws FileNotFoundException, IllegalArgumentException {
        this.srcFolderPath = validateFilename(srcFolderPath);
        this.srcFolderPathFile = new File(this.srcFolderPath);
        if (!this.srcFolderPathFile.exists()) {
            throw new FileNotFoundException("Base source folder '" + this.srcFolderPathFile + "' does not exist");
        }
        if (!this.srcFolderPathFile.isDirectory()) {
            throw new IllegalArgumentException("Base source folder '" + this.srcFolderPathFile + "' is not a directory");
        }
        
        this.libFilenames = new ArrayList<>(libFilenames.size());
        this.libFiles = new ArrayList<>(libFilenames.size());
        for(String filename : libFilenames) {
            String validFn = validateFilename(filename);
            if (!validFn.endsWith(inFileExtension)) {
                throw new IllegalArgumentException("Input file " + filename + " does not end in " + inFileExtension);
            }
            File libFile = new File(this.srcFolderPathFile, validFn);
            if (!libFile.exists()) {
                throw new FileNotFoundException("Source file '" + libFile.getPath() + "' does not exist");
            }
            if (!libFile.isFile()) {
                throw new IllegalArgumentException("Source file '" + libFile.getPath() + "' is not a file");
            }
            this.libFilenames.add(validateFilename(filename));
            this.libFiles.add(libFile);
        }
    }
    
    public void setTemplatePath(String templatePath) throws FileNotFoundException, IllegalArgumentException {
        this.templatePath = validateFilename(templatePath);
        this.templatePathFile = new File(this.templatePath);
        if (!this.templatePathFile.exists()) {
            throw new FileNotFoundException("Template '" + this.templatePathFile + "' does not exist");
        }
        if (!this.templatePathFile.isFile()) {
            throw new IllegalArgumentException("Template '" + this.templatePathFile + "' is not a file");
        }
    }
    
    public void setOutputFolder(String outFolderPath) throws FileNotFoundException, IllegalArgumentException {
        this.outFolderPath = validateFilename(outFolderPath);
        this.outFolderPathFile = new File(this.outFolderPath);
        // If doesn't exist, will create, but if exists, must be a folder
        if (this.outFolderPathFile.exists()) {
            if (!this.outFolderPathFile.isDirectory()) {
                throw new IllegalArgumentException("Output folder '" + this.outFolderPathFile + "' is not a directory");
            }
        }
    }
    
    public void setTargetLibName(String targetLibName) {
        this.targetLibName = targetLibName;
    }

    public void setTargetDataModel(Map<String, Object> targetDataModel) {
        this.targetDataModel = targetDataModel;
    }
    
    /**
     * Gets the data model motherload, to cache externally.
     */
    public Map<String, Map<String, Object>> getSrcFileDataModels() {
        return srcFileDataModels;
    }

    /**
     * Sets the data model motherload, so this method can reuse value that was cached externally.
     */
    public void setSrcFileDataModels(Map<String, Map<String, Object>> srcFileDataModels) {
        this.srcFileDataModels = srcFileDataModels;
    }

    public void init() throws IOException {
        if (cfg == null) {
            msgHandler.logInfo("Initializing...");
            cfg = new Configuration(Configuration.VERSION_2_3_22);
            cfg.setDirectoryForTemplateLoading(new File("."));
            cfg.setDefaultEncoding(defaultEncoding.name());
            cfg.setTemplateExceptionHandler(TemplateExceptionHandler.HTML_DEBUG_HANDLER);
            // support Static like ofbiz does
            cfg.setSharedVariable("Static", ((BeansWrapper) cfg.getObjectWrapper()).getStaticModels());
        }
    }
    
    public void initDataOnly() throws IOException {
        //msgHandler.logInfo("Initializing...");
    }
    
    public void compile() throws IOException, ParseException, TemplateException {
        msgHandler.logInfo("Begin compilation...");
        if (this.srcFileDataModels == null) {
            msgHandler.logInfo("Parsing to data models...");
            this.srcFileDataModels = parseLibs(this.defaultLibFormat);
            msgHandler.logInfo("Got " + srcFileDataModels.size() + " data models");
        } else {
            msgHandler.logInfo("Got " + srcFileDataModels.size() + " data models from existing parse result (cached)");
        }
        
        msgHandler.logInfo("Loading FTL doc template...");
        Template template = cfg.getTemplate(this.templatePath);
        
        msgHandler.logInfo("Rendering with FTL...");
        render(template);
        msgHandler.logInfo("Compilation complete.");
    }
    
    public void compileDataOnly() throws IOException, ParseException, TemplateException {
        msgHandler.logInfo("Begin compilation...");
        if (this.srcFileDataModels == null) {
            msgHandler.logInfo("Parsing to data models...");
            this.srcFileDataModels = parseLibs(this.defaultLibFormat);
            msgHandler.logInfo("Got " + srcFileDataModels.size() + " data models");

        } else {
            msgHandler.logInfo("Got " + srcFileDataModels.size() + " data models from existing parse result (cached)");
        }
        
        msgHandler.logInfo("Populating data model...");
        populateDataModel(targetDataModel, targetLibName);
        msgHandler.logInfo("Data model ready.");
    }
    
    /*
     **********************************************************
     * PARSING (TO DATA MODEL) *
     **********************************************************
     */
    
    /**
     * For each input file, creates a data model. 
     * 
     * @return a map of file names to data models
     * @throws ParseException 
     */
    protected Map<String, Map<String, Object>> parseLibs(String defaultLibFormat) throws IOException, ParseException {
        Map<String, Map<String, Object>> srcFileDataModels = FtlDocFileParser.makeDataMap();
        
        for(String libFilename : libFilenames) {
            File srcFile = new File(srcFolderPathFile, libFilename);
            
            msgHandler.logInfo("Parsing " + srcFile.toString());
            Map<String, Object> dataModel = parseLib(libFilename, srcFile, defaultLibFormat);
            String libName = (String) dataModel.get("libName");
            
            srcFileDataModels.put(libName, dataModel);
        }
        return srcFileDataModels;
    }
    
    protected Map<String, Object> parseLib(String libFilename, File srcFile, String defaultLibFormat) throws ParseException, IOException {
        Map<String, Object> dataModel = FtlDocFileParser.makeObjectMap();
        
        // Super lazy, load entire file as a string.
        String text = FtlDocUtil.readFileAsString(srcFile.getPath(), this.defaultEncoding);
        
        try {
            FtlDocFileParser parser = FtlDocFileParser.getInstance(libFilename, srcFile, inFileExtension, outFileExtension, defaultLibFormat);
            parser.setMsgHandler(msgHandler);
            parser.setLibProperties(dataModel);
            parser.parseLib(dataModel, text);
        }
        catch (ParseException e) {
            throw new ParseException("Error parsing file '" + srcFile.toString() + "': " + e.getMessage(), e);
        }
        catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("Error parsing file '" + srcFile.toString() + "': " + e.getMessage(), e);
        }
        return dataModel;
    }
    
    
    /*
     **********************************************************
     * RENDERING *
     **********************************************************
     */
    
    protected void render(Template template) throws TemplateException, IOException {
        
        // Create out folder if doesn't exist
        File outFolderFile = new File(this.outFolderPath);
        outFolderFile.mkdirs();
        
        for(Map.Entry<String, Map<String, Object>> entry : srcFileDataModels.entrySet()) {
            Map<String, Object> srcFileDataModel = entry.getValue();
            
            String libDocPath = (String) srcFileDataModel.get("libDocPath");
            
            // Get out file path
            File outFile = new File(outFolderFile, libDocPath);
            
            msgHandler.logInfo("Rendering to output file " + outFile.toString());
            
            // Create any missing sub-folders
            outFile.getParentFile().mkdirs();
            
            FileOutputStream fos = null;
            OutputStreamWriter out = null;
            
            try {
                fos = new FileOutputStream(outFile);
                out = new OutputStreamWriter(fos, this.defaultEncoding);
                
                render(template, srcFileDataModel, out);
            }
            finally {
                if (out != null) {
                    out.close();
                }
                if (fos != null) {
                    fos.close();
                }
            }
        }
    }
    
    protected void render(Template template, Map<String, Object> srcFileDataModel, Writer out) throws TemplateException, IOException {
        Map<String, Object> dataModel = FtlDocFileParser.makeObjectMap();
        populateDataModel(dataModel, srcFileDataModel);
        template.process(dataModel, out);
    }
    
    protected void populateDataModel(Map<String, Object> dataModel, String libName) throws TemplateException, IOException {
        if (libName != null && !srcFileDataModels.containsKey(libName)) {
            throw new IOException("Library name " + libName + " is not within data model input files");
        }
        populateDataModel(dataModel, libName != null ? srcFileDataModels.get(libName) : null);
    }
    
    protected void populateDataModel(Map<String, Object> dataModel, Map<String, Object> srcFileDataModel) throws TemplateException, IOException {
        dataModel.put("libMap", srcFileDataModels);
        if (srcFileDataModel != null) {
            dataModel.putAll(srcFileDataModel);
        }
        
        TemplateHelper tmplHelper = TemplateHelper.getInstance((String) dataModel.get("libFormat"),
                inFileExtension, outFileExtension);
        tmplHelper.setMsgHandler(msgHandler);
        dataModel.put("tmplHelper", tmplHelper);
        dataModel.put("docPurpose", this.docPurpose);
        dataModel.put("docInFileExt", inFileExtension);
        dataModel.put("docOutFileExt", outFileExtension);
    }

    /*
     **********************************************************
     * CLASS UTILITIES *
     **********************************************************
     */

    
    protected static String validateFilename(String fn) {
        if (fn == null) {
            throw new IllegalArgumentException("Missing filename");
        }
        fn = fn.trim();
        if (fn.isEmpty()) {
            throw new IllegalArgumentException("Missing filename");
        }
        return fn;
    }
}
