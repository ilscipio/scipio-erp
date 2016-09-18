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
    
    public static final String SCIPIO_LIB_FORMAT = "scipio-lib";
    public static final Charset DEFAULT_FILE_ENCODING = StandardCharsets.UTF_8;

    protected MsgHandler msgHandler = new MsgHandler.VoidMsgHandler();
    
    protected String srcFolderPath = null;
    protected File srcFolderPathFile = null;
    protected List<String> libFilenames = null;
    protected List<File> libFiles = null;
    protected String templatePath = null;
    protected File templatePathFile = null;
    protected String outFolderPath = null;
    protected File outFolderPathFile = null;
    
    protected static final String inFileExtension = ".ftl";
    protected static final String outFileExtension = ".html";
    
    protected Configuration cfg = null;
    
    protected String docPurpose = "templating";
    
    protected String defaultLibFormat = SCIPIO_LIB_FORMAT;
    protected Charset defaultEncoding = DEFAULT_FILE_ENCODING;


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
            String templatePath = args[4];
            List<String> libFilenames = Arrays.asList(args).subList(5, args.length);

            try {
                FtlDocCompiler compiler = FtlDocCompiler.getInstance();
                compiler.setMsgHandler(msgHandler);
                compiler.execBasic(defaultLibFormat, docPurpose, srcFolderPath, libFilenames, templatePath, outFolderPath);
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
    
    public void execBasic(String defaultLibFormat, String docPurpose, String srcFolderPath, List<String> libFilenames, 
            String templatePath, String outFolderPath) throws IllegalArgumentException, IOException, TemplateException, ParseException {
        msgHandler.logInfo("Setting sources and output...");
        setDefaultLibFormat(defaultLibFormat);
        setDocPurpose(docPurpose);
        setSources(srcFolderPath, libFilenames, templatePath);
        setOutputFolder(outFolderPath);
        
        msgHandler.logInfo("Initializing...");
        init();
        
        msgHandler.logInfo("Begin compiling...");
        compile();
        msgHandler.logInfo("Compilation complete.");
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
    
    public void setSources(String srcFolderPath, List<String> libFilenames, String templatePath) throws FileNotFoundException, IllegalArgumentException {
        this.srcFolderPath = validateFilename(srcFolderPath);
        this.srcFolderPathFile = new File(this.srcFolderPath);
        if (!this.srcFolderPathFile.exists()) {
            throw new FileNotFoundException("Base source folder '" + this.srcFolderPathFile + "' does not exist");
        }
        if (!this.srcFolderPathFile.isDirectory()) {
            throw new IllegalArgumentException("Base source folder '" + this.srcFolderPathFile + "' is not a directory");
        }
        
        this.libFilenames = new ArrayList<String>(libFilenames.size());
        this.libFiles = new ArrayList<File>(libFilenames.size());
        for(String filename : libFilenames) {
            String validFn = validateFilename(filename);
            if (!validFn.endsWith(inFileExtension)) {
                throw new IllegalArgumentException("Input file " + filename + " does not end in .ftl");
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
    
    public void init() throws IOException {
         cfg = new Configuration(Configuration.VERSION_2_3_22);
         cfg.setDirectoryForTemplateLoading(new File("."));
         cfg.setDefaultEncoding(defaultEncoding.name());
         cfg.setTemplateExceptionHandler(TemplateExceptionHandler.HTML_DEBUG_HANDLER);
    }
    
    public void compile() throws IOException, ParseException, TemplateException {
        msgHandler.logInfo("Parsing to data models...");
        Map<String, Map<String, Object>> srcFileDataModels = parseLibs(this.defaultLibFormat);
        msgHandler.logInfo("Got " + srcFileDataModels.size() + " data models");
        
        msgHandler.logInfo("Loading FTL doc template...");
        Template template = cfg.getTemplate(this.templatePath);
        
        msgHandler.logInfo("Rendering...");
        render(template, srcFileDataModels);
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
            FtlDocFileParser parser = FtlDocFileParser.getInstance(libFilename, srcFile, defaultLibFormat);
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
    
    protected void render(Template template, Map<String, Map<String, Object>> srcFileDataModels) throws TemplateException, IOException {
        
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
                
                render(template, srcFileDataModel, srcFileDataModels, out);
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
    
    protected void render(Template template, Map<String, Object> srcFileDataModel, Map<String, Map<String, Object>> srcFileDataModels, 
            Writer out) throws TemplateException, IOException {
        
        Map<String, Object> dataModel = FtlDocFileParser.makeObjectMap();
        dataModel.put("libMap", srcFileDataModels);
        dataModel.putAll(srcFileDataModel);
        
        TemplateHelper tmplHelper = TemplateHelper.getInstance((String) dataModel.get("libFormat"));
        tmplHelper.setMsgHandler(msgHandler);
        dataModel.put("tmplHelper", tmplHelper);
        dataModel.put("docPurpose", this.docPurpose);
        
        template.process(dataModel, out);
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
