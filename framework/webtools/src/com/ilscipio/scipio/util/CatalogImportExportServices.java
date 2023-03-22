package com.ilscipio.scipio.util;


import org.apache.commons.text.StringSubstitutor;
import org.apache.http.util.EntityUtils;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.ofbiz.base.util.*;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.common.preferences.PreferenceWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityFieldValue;
import org.ofbiz.entity.jdbc.DatabaseUtil;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelFieldType;
import org.ofbiz.entity.transaction.GenericTransactionException;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.*;
import org.apache.poi.ss.usermodel.*;

import javax.transaction.Transaction;
import java.io.*;
import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.util.*;
import java.util.concurrent.CopyOnWriteArrayList;

public class CatalogImportExportServices {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final Properties SCIPIO_IMPORT_PROPERTIES = UtilProperties.getMergedPropertiesFromAllComponents("ExcelImport");


    private static final String SCIPIO_IMPORT_PREFIX = "xlsx";

    /**
     * Reads an excel .xslx file according to the definition
     *
     * */
    public static Map<String, Object> excelI18nImport(DispatchContext dctx, Map<String, ? extends Object> context) throws IOException {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        int logLevel = Debug.getLevel(context.get("logLevel"), Debug.VERBOSE);
        Locale locale = (Locale) context.get("locale");
        ByteBuffer byteBuffer = (ByteBuffer) context.get("uploadedFile");
        String templateName = (String) context.get("templateName") != null ? context.get("templateName")+"." : "";
        String serviceMode = (String) context.get("serviceMode");
        Integer startRow = (Integer) context.get("startRow");
        Integer endRow = (Integer) context.get("endRow");
        String fileSize = (String) context.get("_uploadedFile_size");
        String fileName = (String) context.get("_uploadedFile_fileName");
        String contentType = (String) context.get("_uploadedFile_contentType");
        Map<String,String> categoryProperties = UtilProperties.getPropertiesWithPrefix(SCIPIO_IMPORT_PROPERTIES,SCIPIO_IMPORT_PREFIX+"."+templateName);
        Map<String,Integer> primaryIdMap = new HashMap<String,Integer>();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        List<String> errorList = new ArrayList<>();

        try(InputStream is = new ByteArrayInputStream(byteBuffer.array())) {
            //Create Workbook instance holding reference to .xlsx file
            XSSFWorkbook workbook = new XSSFWorkbook(is);
            boolean headerRead = false;
            String workbookName = "workbook"; // TODO

            Map<Integer,Map> headerInfo = new HashMap<Integer,Map>();
            if(workbook.getSheetAt(0) !=null){
                Sheet sheet = workbook.getSheetAt(0);
                int rowIndex = -1;
                for (Row row : sheet) {
                    rowIndex++;

                    //skip anything that follows the last rows
                    if(row.getRowNum() > sheet.getLastRowNum()){
                        break;
                    }

                    //skip empty rows
                    if ( row == null || row.getCell(0).getCellType().equals(CellType.STRING) && row.getCell(0).getStringCellValue().startsWith("#")){
                        //skip to first row with content that is not a comment row
                        continue;
                    }

                    if(!headerRead){
                        // Read headerInfo and compare against defined fields
                        headerRead= true;
                        for(Cell cell : row){
                            Map<String,String> headerInfoProps = new HashMap<>();
                            if(!cell.getCellType().equals(CellType.STRING)){
                                continue;
                            }
                            String attrName = cell.getStringCellValue();

                            String attrNameWithoutLocale = attrName;
                            String localeStr = null;
                            int delimIndex = attrName.lastIndexOf("-");
                            if (delimIndex > 0) {
                                attrNameWithoutLocale = attrName.substring(0, delimIndex);
                                localeStr = attrName.substring(delimIndex+1,attrName.length());
                                headerInfoProps.put("locale",localeStr);
                            }

                            headerInfoProps.put("name",attrNameWithoutLocale);

                            Map<String,String> fieldProperties = UtilProperties.getPropertiesWithPrefix(categoryProperties,attrNameWithoutLocale+".");
                            if(!fieldProperties.isEmpty()){
                                headerInfoProps.putAll(fieldProperties);
                                if(Boolean.parseBoolean(fieldProperties.get("primary"))){
                                    primaryIdMap.put(attrNameWithoutLocale,cell.getColumnIndex());
                                }
                            }

                            headerInfo.put(cell.getColumnIndex(),headerInfoProps);
                        }

                    }else{
                        Map<String,Object> serviceProps = new HashMap<String,Object>();
                        Map<String,Object> entityProps = new HashMap<String,Object>();

                        if ((UtilValidate.isNotEmpty(startRow) && row.getRowNum() < startRow)
                                || (UtilValidate.isNotEmpty(endRow) && row.getRowNum() > endRow)) {
                            continue;
                        }

                        int cellIndex = 0;
                        String cellAddr = null;
                        String entityName = null;
                        Map<String, Object> entityFields = null;
                        boolean beganTransaction = false;
                        Transaction parentTransaction = null;
                        Throwable transactionEx = null;
                        try {
                            if (TransactionUtil.isTransactionInPlace()) {
                                parentTransaction = TransactionUtil.suspend();
                            }
                            try { // Per-row transaction and atomic error handling
                                beganTransaction = TransactionUtil.begin();

                                //Handle cell information and commit
                                cellIndex = -1;
                                for (Cell cell : row) {
                                    cellIndex++;
                                    cellAddr = cell.getAddress().formatAsString();
                                    //skip empty cells
                                    if (cell == null || cell.getCellType() == CellType.BLANK || (cell.getCellType().equals(CellType.STRING) && UtilValidate.isEmpty(cell.getStringCellValue()))) {
                                        continue;
                                    }

                                    //skip primary key fields
                                    if (primaryIdMap.containsValue(cell.getColumnIndex())) {
                                        continue;
                                    }

                                    //reset per-row fields
                                    entityName = null;
                                    entityFields = new LinkedHashMap<>();

                                    //read value
                                    Object cellValue = null;
                                    switch (cell.getCellType()) {
                                        case STRING:
                                            cellValue = cell.getStringCellValue();
                                            break;
                                        case BLANK:
                                            cellValue = null;
                                            break;
                                        case NUMERIC:
                                            cellValue = new BigDecimal(cell.getNumericCellValue());
                                            break;
                                        case BOOLEAN:
                                            cellValue = cell.getBooleanCellValue();
                                            break;
                                        default:
                                            break;
                                    }

                                    if (headerInfo.get(cell.getColumnIndex()) != null) {
                                        Map<String, Object> serviceContext = new HashMap<>();
                                        serviceContext.put("cellValue", cellValue);
                                        serviceContext.put("userLogin", userLogin);

                                        primaryIdMap.forEach((key, location) -> {
                                            if (row.getCell(location).getCellType().equals(CellType.STRING)) {
                                                serviceContext.put(key, row.getCell(location).getStringCellValue());
                                            }
                                        });

                                        Map<String, String> headerInfoProps = headerInfo.get(cell.getColumnIndex());

                                        entityName = headerInfoProps.get("entity");
                                        Map<String, String> entityParameters = UtilProperties.getPropertiesWithPrefix(headerInfoProps, "entity.parameter.");
                                        Map<String, String> entityConfig = UtilProperties.getPropertiesWithPrefix(headerInfoProps, "entity.config.");
                                        String createService = headerInfoProps.get("create");
                                        String updateService = headerInfoProps.get("update");
                                        String localeStr = headerInfoProps.get("locale");
                                        String fieldName = headerInfoProps.get("name");
                                        // FlexibleStringExpander requires Locale
                                        serviceContext.put("locale", UtilMisc.parseLocale(localeStr));
                                        //serviceContext.put("locale", localeStr);

                                        serviceContext.put("today", UtilDateTime.getDayStart(UtilDateTime.nowTimestamp()));
                                        serviceContext.put("now", UtilDateTime.nowTimestamp());

                                        //fetch existing entity value
                                        if (UtilValidate.isNotEmpty(entityName)) {

                                            //enrich context with all column values
                                            headerInfo.forEach((index,hip) -> {
                                                Object value = row.getCell(index);
                                                entityProps.put((String) hip.get("name"),value);
                                            });

                                            serviceContext.forEach((key, value) -> {
                                                if (UtilValidate.isNotEmpty(value)) {
                                                    entityProps.put(key, value);
                                                }
                                            });

                                            Map<String, Object> targetEntityFields = entityFields; // final field kludge
                                            ModelEntity myLookupModel = delegator.getModelEntity(entityName);
                                            entityParameters.forEach((key, value) -> {
                                                if(myLookupModel!=null && myLookupModel.getField(key)!=null){
                                                    try {
                                                        String fieldType = myLookupModel.getField(key).getType();
                                                        ModelFieldType modelFieldType = delegator.getEntityFieldType(myLookupModel,fieldType);
                                                        Object substitutedValue = substituteVariables(value, entityProps);
                                                        Object typeCastObject = ObjectType.simpleTypeConvert(substitutedValue, modelFieldType.getJavaType(), null, locale);
                                                        targetEntityFields.put(key, typeCastObject);
                                                    }catch (Exception e){
                                                        Debug.logWarning("Error while typecasting field "+ key,module);
                                                    }
                                                }
                                            });

                                            String serviceName = updateService;
                                            String serviceType = "update";
                                            List<GenericValue> results = null;

                                            //if entityparameters are empty, skip the lookup as it would never return anything useful
                                            if (UtilValidate.isNotEmpty(entityFields)) {
                                                //If entity has a thruDate, select only current entries
                                                EntityCondition condition = EntityCondition.makeCondition(
                                                        EntityCondition.makeCondition(entityFields)
                                                );



                                                if (myLookupModel.getField("thruDate") != null) {
                                                    condition = EntityCondition.append(condition, EntityUtil.getFilterByDateExpr());
                                                }

                                                boolean hasLocale = false;
                                                EntityCondition conditionWithLocale = condition;
                                                //most of the entities have "locale" mapped as "localeString", so setting it here
                                                if (UtilValidate.isNotEmpty(localeStr) && myLookupModel.getField("localeString") != null) {
                                                    conditionWithLocale = EntityCondition.append(conditionWithLocale, EntityCondition.makeCondition("localeString", localeStr));
                                                    hasLocale = true;
                                                }
                                                // Still checking against "locale" just in case
                                                if (UtilValidate.isNotEmpty(localeStr) && myLookupModel.getField("locale") != null) {
                                                    conditionWithLocale = EntityCondition.append(conditionWithLocale, EntityCondition.makeCondition("locale", localeStr));
                                                    hasLocale = true;
                                                }

                                                serviceName = updateService;
                                                serviceType = "update";

                                                try {
                                                    results = delegator.findList(entityName, conditionWithLocale, null, null, null, false);
                                                    if (UtilValidate.isEmpty(results)) {
                                                        results = delegator.findList(entityName, condition, null, null, null, false);
                                                        if (!hasLocale || (hasLocale && UtilValidate.isEmpty(results))) {
                                                            serviceName = createService;
                                                            serviceType = "create";
                                                        }
                                                    }
                                                } catch (GenericEntityException e) {
                                                    throw new GenericEntityException("Could not find entity [" + entityName + "]" + e.getMessage(), e);
                                                }
                                            } else {
                                                serviceName = createService;
                                                serviceType = "create";
                                            }

                                            //Check if entry exists, if so, set it in context
                                            serviceContext.forEach((key, value) -> {
                                                if (UtilValidate.isNotEmpty(value)) {
                                                    serviceProps.put(key, value);
                                                }
                                            });

                                            if (UtilValidate.isNotEmpty(serviceName)) {
                                                Map<String, String> serviceParameters = UtilProperties.getPropertiesWithPrefix(headerInfoProps, serviceType + ".parameters.");

                                                if (UtilValidate.isNotEmpty(results)) {
                                                    GenericValue origEntry = results.get(0);
                                                    //serviceContext.putAll(origEntry.getAllFields());
                                                    origEntry.forEach((key, value) -> {
                                                        if (UtilValidate.isNotEmpty(value)) {
                                                            serviceProps.put(key, value);
                                                        }
                                                    });
                                                }

                                                serviceParameters.forEach((key, value) -> {
                                                    serviceContext.put(key, substituteVariables(value, serviceProps));
                                                });

                                                Map<String, Object> serviceFields = dispatcher.getDispatchContext().
                                                        makeValidContext(serviceName, ModelService.IN_PARAM, serviceContext);

                                                try {
                                                    List<String> missingServiceParams = new CopyOnWriteArrayList<>();
                                                    dispatcher.getModelService(serviceName).getInModelParamList().forEach(
                                                            s -> {
                                                                if (!s.optional && UtilValidate.isEmpty(serviceFields.get(s.getName()))) {
                                                                    missingServiceParams.add(s.getName());
                                                                }
                                                            }
                                                    );

                                                    if (logLevel == Debug.INFO || logLevel == Debug.VERBOSE) {
                                                        Debug.log(logLevel, workbookName + " [" + cellAddr + "]: Starting service [" +
                                                                serviceName + ", " + serviceMode + "]" + (logLevel == Debug.VERBOSE ? ": " + serviceFields : ""), module);
                                                    }
                                                    if (missingServiceParams.isEmpty()) {
                                                        if ("async".equals(serviceMode)) {
                                                            dispatcher.runAsync(serviceName, serviceFields, false);
                                                        } else if ("async-persist".equals(serviceMode)) {
                                                            // FIXME: Specify/eliminate retries on errors
                                                            dispatcher.runAsync(serviceName, serviceFields, true);
                                                        } else {
                                                            Map<String, Object> serviceResult = dispatcher.runSync(serviceName, serviceFields);

                                                            if (ServiceUtil.isSuccess(serviceResult)) {
                                                                Debug.logInfo(workbookName + " [" + cellAddr + "]: Imported field value: " + cellAddr, module);
                                                            } else {
                                                                throw new GenericServiceException("Could not import field [" + cellAddr +
                                                                        "] using service [" + serviceName + "]. Service returned with error: " + ServiceUtil.getErrorMessage(serviceResult));
                                                            }
                                                        }
                                                    } else {
                                                        throw new GenericServiceException("Could not run service [" + serviceName + "]: fields " + missingServiceParams +
                                                                " were missing from service invocation: " + serviceFields);
                                                    }
                                                } catch (GenericServiceException e) {
                                                    throw new GenericServiceException("Could not update from field value: "+cellAddr +" " + e.getMessage(), e);
                                                }
                                            } else {
                                                //Update the entity directly
                                                if (logLevel == Debug.INFO || logLevel == Debug.VERBOSE) {
                                                    Debug.log(logLevel, workbookName + " [" + cellAddr + "]: Updating entity [" +
                                                            entityName + "." + fieldName + "]", module);
                                                }
                                                try {
                                                    targetEntityFields.put(fieldName, cellValue);
                                                    GenericValue rec = delegator.makeValidValue(entityName, targetEntityFields, new GenericEntity.SetOptions().typeConvert(true));
                                                    rec.createOrStore();

                                                } catch (GenericEntityException e) {
                                                    throw new GenericEntityException("Could not create or store entity value for fieldName [" + fieldName + "]: " + e.getMessage(), e);
                                                }
                                            }
                                        }
                                    }
                                }

                            } catch (Throwable t) { // Per-row transaction and atomic error handling
                                transactionEx = t;
                                String msg = "Could not create or update entity [" + entityName + "] [" + entityFields + "]";
                                Debug.logWarning(workbookName + " [" + cellAddr + "]: " + msg + ": " + t.getMessage(), module);
                                errorList.add("[" + cellAddr + "]: " + msg + ": " + t.getMessage());
                            } finally { // finally required to ensure transaction rollback or commit happens (binary)
                                if (transactionEx != null) {
                                    try {
                                        if (TransactionUtil.isTransactionInPlace()) {
                                            TransactionUtil.rollback(beganTransaction, transactionEx.toString(), transactionEx);
                                        }
                                    } catch (GenericTransactionException e) {
                                        // NOTE: Normally this is considered a fatal error, but best to absorb it here for now
                                        Debug.logError(e, workbookName + " [" + cellAddr + "]: Unable to rollback transaction", module);
                                        errorList.add("[" + cellAddr + "]: Unable to rollback transaction: " + e.getMessage());
                                    }
                                } else {
                                    try {
                                        TransactionUtil.commit(beganTransaction);
                                    } catch (GenericTransactionException e) {
                                        // NOTE: Normally this is considered a fatal error, but best to absorb it here for now
                                        Debug.logError(e, workbookName + " [" + cellAddr + "]: Cannot commit transaction", module);
                                        errorList.add("[" + cellAddr + "]: Cannot commit transaction: " + e.getMessage());
                                    }
                                }
                            }
                        } catch (GenericTransactionException e) {
                            Debug.logError(e, workbookName + " [" +  cellAddr + "]: Cannot suspend transaction", module);
                            errorList.add("[" + cellAddr + "]: Cannot suspend transaction: " + e.getMessage());
                        } finally {
                            try {
                                if (parentTransaction != null) {
                                    TransactionUtil.resume(parentTransaction);
                                }
                            } catch (GenericTransactionException e) {
                                Debug.logError(e, workbookName + " [" +  cellAddr + "]: Cannot resume transaction", module);
                                errorList.add("[" + cellAddr + "]: Cannot resume transaction: " + e.getMessage());
                            }
                        }
                    }
                }
            }

        }catch (Exception e) {
            errorList.add("Unexpected exception: "+e.getMessage());
            return ServiceUtil.returnError(e.getMessage(), errorList);
        }

        if(!errorList.isEmpty()){
            return ServiceUtil.returnFailure(errorList);
        }
        return ServiceUtil.returnSuccess();
    }

    public static String substituteVariables(String originalValue, Map<String,Object> replaceProps){
        String str = FlexibleStringExpander.expandString(originalValue, replaceProps);
        //String str = StringSubstitutor.replace(originalValue, replaceProps);
        str=str.replaceAll("\\$\\{([^}]*?)\\}",""); //replace leftover variables
        if(str.trim().equals("null") || str.trim().length()==0){
            return null;
        }
        return str;
    }
}
