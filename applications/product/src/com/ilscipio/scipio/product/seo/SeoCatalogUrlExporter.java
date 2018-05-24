package com.ilscipio.scipio.product.seo;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.content.data.SpecDataResEntityInfo;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;

import com.ilscipio.scipio.content.content.ContentTraverser;
import com.ilscipio.scipio.content.content.ContentTraverser.ContentTraverserConfig;

public class SeoCatalogUrlExporter extends SeoCatalogTraverser {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    public static final String DATAFILECONFIGS_RESOURCE = "seo-urls";
    
    static final String logPrefix = "Seo: Alt URL Export: ";

    // FIXME: copy-pasted this from CMS
    protected static final Map<String, Set<String>> entityCdataFields = Collections.unmodifiableMap(makeEntityCdataFieldsMap());
    
    public static final List<String> EXPORT_ENTITY_NAMES;
    
    static {
        ArrayList<String> names = new ArrayList<>(100);
        names.addAll(UtilMisc.unmodifiableArrayList("DataResource", "DataResourceAttribute", 
                "DataResourceRole", "ElectronicText"));
        names.addAll(SpecDataResEntityInfo.getEntityNameList());
        names.addAll(UtilMisc.unmodifiableArrayList("Content", "ContentAttribute", "ContentRole", 
                "ContentAssoc", "ContentKeyword", "ProductContent",  "ProductCategoryContent"));
        names.trimToSize();
        EXPORT_ENTITY_NAMES = Collections.unmodifiableList(names);
    }
    
    /**
     * The original output, e.g. StringWriter.
     */
    protected Writer outWriter;
    /**
     * PrintWriter-wrapped output.
     * NOTE: caller can pass this pre-wrapped, but caller himself might run into problems converting back.
     */
    protected PrintWriter outPrintWriter;
    
    protected final SeoContentExporter contentExporter;
    
    protected Map<String, List<GenericValue>> entityTypeValues = null;
    
    public SeoCatalogUrlExporter(Delegator delegator, LocalDispatcher dispatcher, ExportTraversalConfig travConfig,
            Writer outWriter) throws GeneralException {
        super(delegator, dispatcher, travConfig);
        setOutWriter(outWriter);
        ContentTraverserConfig travContentConfig = new ContentTraverserConfig()
                .setIncludeKeyword(false)
                .setIncludeRole(false)
                // NOTE: we need recursive but forces it to disable all parent flag - see ContentTraverser
                .setIncludeChildContentAssoc(true)
                .setIncludeChildContentRecursive(true)
                .setIncludeParentContentAssoc(false)
                .setIncludeParentContentRecursive(false)
                .setFilterByDate(travConfig.isFilterByDate())
                .setMoment(travConfig.getMoment());
        this.contentExporter = new SeoContentExporter(delegator, dispatcher, travContentConfig);
        this.reset();
    }

    public enum RecordGrouping { // NOTE: imported from CMS
        NONE,
        ENTITY_TYPE,
        MAJOR_OBJECT;
        
        public static final RecordGrouping DEFAULT = NONE;
        public static RecordGrouping fromString(String str) { return UtilValidate.isNotEmpty(str) ? RecordGrouping.valueOf(str) : null; }
        public static RecordGrouping fromStringSafe(String str) {
            try {
                return UtilValidate.isNotEmpty(str) ? RecordGrouping.valueOf(str) : null;
            } catch(Exception e) { return null; }
        }
        public static RecordGrouping fromStringOrDefault(String str) { return UtilValidate.isNotEmpty(str) ? RecordGrouping.valueOf(str) : DEFAULT; }
        public static RecordGrouping fromStringOrDefaultSafe(String str) {
            try {
                return UtilValidate.isNotEmpty(str) ? RecordGrouping.valueOf(str) : DEFAULT;
            } catch(Exception e) { return DEFAULT; }
        }
        public static RecordGrouping getDefault() { return DEFAULT; }
    }
    
    public static class ExportTraversalConfig extends SeoTraversalConfig {
        protected Map<String, ?> servCtxOpts;
        protected boolean doChildProducts = true;
        protected String linePrefix = "";
        protected RecordGrouping recordGrouping = RecordGrouping.DEFAULT;
        protected List<String> exportEntityNames = EXPORT_ENTITY_NAMES;
        private boolean includeVariant = true;
        
        public Map<String, ?> getServCtxOpts() {
            return servCtxOpts;
        }

        public ExportTraversalConfig setServCtxOpts(Map<String, ?> servCtxOpts) {
            this.servCtxOpts = servCtxOpts;
            this.includeVariant = !Boolean.FALSE.equals(servCtxOpts.get("includeVariant"));
            return this;
        }
        
        public boolean isDoChildProducts() {
            return doChildProducts;
        }

        public ExportTraversalConfig setDoChildProducts(boolean doChildProducts) {
            this.doChildProducts = doChildProducts;
            return this;
        }

        public boolean isIncludeVariant() {
            return includeVariant;
        }
        
        public String getLinePrefix() {
            return linePrefix;
        }

        public ExportTraversalConfig setLinePrefix(String linePrefix) {
            this.linePrefix = linePrefix;
            return this;
        }

        public RecordGrouping getRecordGrouping() {
            return recordGrouping;
        }

        public ExportTraversalConfig setRecordGrouping(RecordGrouping recordGrouping) {
            this.recordGrouping = (recordGrouping != null) ? recordGrouping : RecordGrouping.DEFAULT;
            return this;
        }
        
        public ExportTraversalConfig setRecordGrouping(String recordGrouping) {
            this.recordGrouping = RecordGrouping.fromStringOrDefault(recordGrouping);
            return this;
        }


        public List<String> getExportEntityNames() {
            return exportEntityNames;
        }

        public ExportTraversalConfig setExportEntityNames(List<String> exportEntityNames) {
            this.exportEntityNames = exportEntityNames;
            return this;
        }
    }
    
    @Override
    public ExportTraversalConfig newTravConfig() {
        return new ExportTraversalConfig();
    }

    @Override
    public ExportTraversalConfig getTravConfig() {
        return (ExportTraversalConfig) travConfig;
    }
    
    @Override
    public void reset() throws GeneralException {
        super.reset();
        if (isEntityTypeGrouping()) {
            entityTypeValues = new LinkedHashMap<>();
            // preset this order; any extras will go at the end
            for(String entityName : getTravConfig().getExportEntityNames()) {
                entityTypeValues.put(entityName, new LinkedList<GenericValue>());
            }
        }
    }

    public Writer getOutWriter() {
        return outWriter;
    }
    
    public PrintWriter getOutPrintWriter() {
        return outPrintWriter;
    }
    
    public void setOutWriter(Writer outWriter) {
        this.outWriter = outWriter;
        if (outWriter instanceof PrintWriter) {
            this.outPrintWriter = (PrintWriter) outWriter;
        } else {
            this.outPrintWriter = new PrintWriter(outWriter);
        }
    }
    
    public void flush(boolean flushWriter) throws GeneralException {
        if (isEntityTypeGrouping()) {
            for(List<GenericValue> list : entityTypeValues.values()) {
                for(GenericValue entityValue : list) {
                    writeEntityValue(entityValue);
                }
            }
        }
        if (flushWriter) {
            try {
                this.outPrintWriter.flush();
            } catch(Exception e) {
                throw new GeneralException(e);
            }
        }
    }

    @Override
    protected String getLogMsgPrefix() {
        return logPrefix;
    }

    @Override
    protected String getLogErrorPrefix() {
        return getLogMsgPrefix()+"Error exporting alternative links: ";
    }
    
    @Override
    public void visitCategory(GenericValue productCategory, TraversalState state)
            throws GeneralException {
        exportCategoryAltUrls(productCategory);
    }

    @Override
    public void visitProduct(GenericValue product, TraversalState state)
            throws GeneralException {
        exportProductAltUrls(product);
    }
    
    public void exportCategoryAltUrls(GenericValue productCategory) throws GeneralException {
        String productCategoryId = productCategory.getString("productCategoryId");
        List<GenericValue> productCategoryContentList = EntityQuery.use(getDelegator()).from("ProductCategoryContent")
                .where("productCategoryId", productCategoryId, "prodCatContentTypeId", "ALTERNATIVE_URL")
                .filterByDate(travConfig.isFilterByDate(), travConfig.getMoment())
                .orderBy("-fromDate").cache(isUseCache()).queryList();
        if (productCategoryContentList.size() > 0) {
            for(GenericValue productCategoryContent : productCategoryContentList) {
                contentExporter.traverseContentAndRelated(productCategoryContent.getString("contentId"));
                registerEntityValue(productCategoryContent);
            }
            getStats().categorySuccess++;
        }
    }
    
    public void exportProductAltUrls(GenericValue product) throws GeneralException {
        // NOTE: must check product itself here first
        boolean includeVariant = getTravConfig().isIncludeVariant();
        if (!includeVariant && "Y".equals(product.getString("isVariant"))) {
            return;
        }
        
        String productId = product.getString("productId");
        List<GenericValue> productContentList = EntityQuery.use(getDelegator()).from("ProductContent")
                .where("productId", productId, "productContentTypeId", "ALTERNATIVE_URL")
                .filterByDate(travConfig.isFilterByDate(), travConfig.getMoment())
                .orderBy("-fromDate").cache(isUseCache()).queryList();
        if (productContentList.size() > 0) {
            for(GenericValue productContent : productContentList) {
                contentExporter.traverseContentAndRelated(productContent.getString("contentId"));
                registerEntityValue(productContent);
            }
            getStats().productSuccess++;
        }
        
        if (getTravConfig().isDoChildProducts()) {
            if (includeVariant && "Y".equals(product.getString("isVirtual"))) {
                List<GenericValue> variantAssocList = EntityQuery.use(getDelegator()).from("ProductAssoc")
                        .where("productId", productId, "productAssocTypeId", "PRODUCT_VARIANT").filterByDate().cache(isUseCache()).queryList();
                for(GenericValue variantAssoc : variantAssocList) {
                    GenericValue variantProduct = variantAssoc.getRelatedOne("AssocProduct", isUseCache());
                    exportProductAltUrls(variantProduct);
                }
            }
        }
    }
    
    protected boolean isEntityTypeGrouping() {
        return getTravConfig().getRecordGrouping() == RecordGrouping.ENTITY_TYPE;
    }
    
    public void registerEntityValue(GenericValue entityValue) {
        if (isEntityTypeGrouping()) {
            List<GenericValue> entityList = entityTypeValues.get(entityValue.getEntityName());
            if (entityList == null) {
                entityList = new LinkedList<>();
                entityTypeValues.put(entityValue.getEntityName(), entityList);
            }
            entityList.add(entityValue);
        } else {
            writeEntityValue(entityValue);
        }
    }
    
    public void writeEntityValue(GenericValue entityValue) {
        entityValue.writeXmlText(getOutPrintWriter(), getTravConfig().getLinePrefix(), 
                entityCdataFields.get(entityValue.getEntityName())); // NOTE: 3rd parameter is a SCIPIO patch
    }

    protected class SeoContentExporter extends ContentTraverser {
        public SeoContentExporter(Delegator delegator, LocalDispatcher dispatcher, ContentTraverserConfig travConfig) {
            super(delegator, dispatcher, travConfig);
        }

        @Override
        protected String getLogMsgPrefix() {
            return SeoCatalogUrlExporter.logPrefix;
        }

        @Override
        public void visitEntity(GenericValue entityValue) throws GeneralException {
            registerEntityValue(entityValue);
        }
    }
    
    /**
     * Makes a map of sets of all the entity field names we want to always print out as CDATA blocks in XML output.
     * FIXME: copy-pasted this from CMS.
     */
    private static Map<String, Set<String>> makeEntityCdataFieldsMap() {
        Map<String, Set<String>> map = new HashMap<>();
        map.put("ElectronicText", new HashSet<>(Arrays.asList(new String[] { "textData" })));
        for(SpecDataResEntityInfo entityInfo : SpecDataResEntityInfo.getEntityInfoList()) {
            map.put(entityInfo.getEntityName(), new HashSet<>(Arrays.asList(new String[] { entityInfo.getDataFieldName() })));
        }
        return map;
    }
    
    /**
     * Loader for the <code>seo-urls.properties</code> "seourl.datafile.[configName].*" datafile configurations
     * and corresponding *Data.xml datafile writing helpers.
     */
    @SuppressWarnings("serial")
    public static class ExportDataFileConfig extends HashMap<String, Object> {
        private static final Map<String, ExportDataFileConfig> staticConfigs = Collections.unmodifiableMap(readDataFileConfigsFromProperties());
        
        private static final String configPropPrefix = "seourl.datafile.";
        
        private static final String DEFAULT_EXPORT_HEADER = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + lineSep() + "<entity-engine-xml>" + lineSep();
        private static final String DEFAULT_EXPORT_FOOTER = lineSep() + "</entity-engine-xml>" + lineSep();
        private static final Charset charset = Charset.forName("UTF-8");
        
        private final String configName;
        private final boolean configEnabled;
        
        public ExportDataFileConfig(String configName, Map<String, Object> map) {
            super(map);
            this.configName = configName;
            this.configEnabled = UtilMisc.booleanValueVersatile(this.get("configEnabled"), false);
        }

        public String getConfigName() {
            return configName;
        }

        public boolean isConfigEnabled() { // NOTE: those with true are excluded from staticConfigs for now
            return configEnabled;
        }
  
        public static ExportDataFileConfig getConfigByName(String configName) {
            return ExportDataFileConfig.staticConfigs.get(configName);
        }

        public static Collection<ExportDataFileConfig> getAllConfigs(String configName) {
            return ExportDataFileConfig.staticConfigs.values();
        }
        
        public static Collection<String> getAllConfigNames() {
            return ExportDataFileConfig.staticConfigs.keySet();
        }
        
        public static String getDefaultHeader() {
            return DEFAULT_EXPORT_HEADER;
        }
        
        public static String getDefaultFooter() {
            return DEFAULT_EXPORT_FOOTER;
        }
        
        /**
         * NOTE: This must be the same as PrintWriter uses, which is line.separator property,
         * due to GenericEntity.writeXmlText using it.
         */
        public static String lineSep() {
            return System.lineSeparator();
        }
        
        /**
         * FIXME: this should be forced to UTF-8 always, but PrintWriter may not currently
         * guarantee...
         */
        public static Charset getCharset() {
            return charset;
        } 
        
        /**
         * NOTE: this uses line.separator as we are forced by GenericValue.writeXmlText to use
         * that anyway.
         */
        public static final StringBuilder readCommentDelimitedTemplateHeader(String templateFile, String dataBeginMarker) throws IOException, URISyntaxException {
            StringBuilder header = new StringBuilder();
            String lineSep = ExportDataFileConfig.lineSep();
            BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(
                    new File(FlexibleLocation.resolveLocation(templateFile).toURI())), ExportDataFileConfig.getCharset()));  
            try {
                String line = null;  
                while ((line = br.readLine()) != null) {  
                    header.append(line);
                    header.append(lineSep);
                    if (line.contains(dataBeginMarker)) {
                        return header;
                    }
                } 
            } finally {
                br.close();
            }
            return null;
        }
        
        public static final Writer getOutFileWriter(String outFile) throws MalformedURLException, IOException, URISyntaxException {
            boolean append = false;
            return new OutputStreamWriter(new FileOutputStream(new File(FlexibleLocation.resolveLocation(outFile).toURI()), append), ExportDataFileConfig.getCharset());  
        }
        
        private static Map<String, ExportDataFileConfig> readDataFileConfigsFromProperties() {
            Map<String, ExportDataFileConfig> configs = new HashMap<>();
            try {
                ClassLoader loader = Thread.currentThread().getContextClassLoader();
                Enumeration<URL> resources = loader.getResources(DATAFILECONFIGS_RESOURCE + ".properties");
                while (resources.hasMoreElements()) {
                    URL propertyURL = resources.nextElement();
                    Debug.logInfo(logPrefix+"loading properties: " + propertyURL, module);
                    Properties props = UtilProperties.getProperties(propertyURL);
                    if (UtilValidate.isEmpty(props)) {
                        Debug.logError(logPrefix+"Unable to locate properties file " + propertyURL, module);
                    } else {
                        Map<String, Map<String, Object>> webSiteConfigs = new HashMap<>();
                        UtilProperties.extractPropertiesWithPrefixAndId(webSiteConfigs, props, configPropPrefix);
                        for(Map.Entry<String, Map<String, Object>> entry : webSiteConfigs.entrySet()) {
                            try {
                                ExportDataFileConfig config = new ExportDataFileConfig(entry.getKey(), entry.getValue());
                                if (config.isConfigEnabled()) {
                                    Debug.logInfo(logPrefix+"Read datafile config '" + entry.getKey() + "': " + config.toString(), module);
                                    configs.put(entry.getKey(), config);
                                } else {
                                    Debug.logInfo(logPrefix+"datafile config '" + entry.getKey() + "' is disabled; ignoring", module);
                                }
                            } catch(Exception e) {
                                Debug.logError(e, logPrefix+"Unable to read datafile config '" + entry.getKey() + "': " + e.getMessage(), module);
                            }
                        }
                    }
                }
                for(Map.Entry<String, ExportDataFileConfig> entry : configs.entrySet()) {
                    Debug.logInfo(logPrefix+"Found datafile config: " + entry.getKey(), module);
                }
            } catch (Exception e) {
                Debug.logError(e, logPrefix+"Could not load list of " + DATAFILECONFIGS_RESOURCE + ".properties", module);
            }
            return configs;
        }
    }
    
}
