package com.ilscipio.scipio.util;

import org.apache.commons.text.StringSubstitutor;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.ofbiz.base.util.*;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.transaction.GenericTransactionException;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.*;
import org.apache.poi.ss.usermodel.*;

import javax.transaction.Transaction;
import java.io.*;
import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicBoolean;

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
        Locale locale = (Locale) context.get("locale");
        ByteBuffer byteBuffer = (ByteBuffer) context.get("uploadedFile");
        String templateName = (String) context.get("templateName") != null ? context.get("templateName")+"." : "";
        Boolean runAsync = (Boolean) context.get("runAsync");
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

            Map<Integer,Map> headerInfo = new HashMap<Integer,Map>();
            if(workbook.getSheetAt(0) !=null){
                Sheet sheet = workbook.getSheetAt(0);
                for (Row row : sheet) {

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


                        boolean beganTransaction = false;
                        Transaction parentTransaction = null;
                        try {
                            parentTransaction = TransactionUtil.suspend();
                            beganTransaction = TransactionUtil.begin();
                        } catch (GenericTransactionException e1) {
                            Debug.logError(e1, module);
                            errorList.add("Cannot resume transaction: "+e1.getMessage());
                        }


                        //Handle cell information and commit
                        for(Cell cell : row){
                            //skip empty cells
                            if (cell == null || cell.getCellType() == CellType.BLANK || (cell.getCellType().equals(CellType.STRING) && UtilValidate.isEmpty(cell.getStringCellValue()))) {
                                continue;
                            }

                            //skip primary key fields
                            if(primaryIdMap.containsValue(cell.getColumnIndex())){
                                continue;
                            }

                            //read value
                            Object cellValue = null;
                            switch(cell.getCellType()){
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

                            if(headerInfo.get(cell.getColumnIndex())!=null){
                                Map<String,Object> serviceContext = new HashMap<String,Object>();
                                serviceContext.put("cellValue",cellValue);
                                serviceContext.put("userLogin",userLogin);

                                primaryIdMap.forEach( (key,location)-> {
                                    if(row.getCell(location).getCellType().equals(CellType.STRING)){
                                        serviceContext.put(key,row.getCell(location).getStringCellValue());
                                    }
                                });

                                Map<String,String> headerInfoProps = headerInfo.get(cell.getColumnIndex());

                                String entityName = headerInfoProps.get("entity");
                                Map<String,String> entityParameters = UtilProperties.getPropertiesWithPrefix(headerInfoProps,"entity.parameter.");
                                Map<String,String> entityConfig = UtilProperties.getPropertiesWithPrefix(headerInfoProps,"entity.config.");
                                String createService = headerInfoProps.get("create");
                                String updateService = headerInfoProps.get("update");
                                String localeStr = headerInfoProps.get("locale");
                                String fieldName = headerInfoProps.get("name");
                                serviceContext.put("locale",localeStr);

                                serviceContext.put("today",UtilDateTime.getDayStart(UtilDateTime.nowTimestamp()));
                                serviceContext.put("now",UtilDateTime.nowTimestamp());

                                //fetch existing entity value
                                if(UtilValidate.isNotEmpty(entityName)){

                                    serviceContext.forEach((key,value)->{
                                        if(UtilValidate.isNotEmpty(value)){
                                            entityProps.put(key,value);
                                        }
                                    });

                                    entityParameters.forEach((key,value) -> {
                                        entityParameters.put(key,substituteVariables(value,entityProps));
                                    });

                                    String serviceName = updateService;
                                    String serviceType = "update";
                                    List<GenericValue> results = null;

                                    //if entityparameters are empty, skip the lookup as it would never return anything useful
                                    if(UtilValidate.isNotEmpty(entityParameters)) {
                                        //If entity has a thruDate, select only current entries
                                        EntityCondition condition = EntityCondition.makeCondition(
                                                EntityCondition.makeCondition(entityParameters)
                                        );

                                        ModelEntity myLookupModel = delegator.getModelEntity(entityName);

                                        if (myLookupModel.getField("thruDate") != null) {
                                            condition = condition.append(condition, EntityUtil.getFilterByDateExpr());
                                        }

                                        boolean hasLocale = false;
                                        EntityCondition conditionWithLocale = condition;
                                        //most of the entities have "locale" mapped as "localeString", so setting it here
                                        if (UtilValidate.isNotEmpty(localeStr) && myLookupModel.getField("localeString") != null) {
                                            conditionWithLocale = conditionWithLocale.append(conditionWithLocale, EntityCondition.makeCondition("localeString", localeStr));
                                            hasLocale = true;
                                        }
                                        // Still checking against "locale" just in case
                                        if (UtilValidate.isNotEmpty(localeStr) && myLookupModel.getField("locale") != null) {
                                            conditionWithLocale = conditionWithLocale.append(conditionWithLocale, EntityCondition.makeCondition("locale", localeStr));
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
                                        } catch (Exception e) {
                                            TransactionUtil.rollback(beganTransaction, "Error importing cell", e);
                                        }
                                    }else{
                                        serviceName = createService;
                                        serviceType = "create";
                                    }

                                    //Check if entry exists, if so, set it in context
                                    serviceContext.forEach((key, value) -> {
                                        if (UtilValidate.isNotEmpty(value)) {
                                            serviceProps.put(key, value);
                                        }
                                    });

                                    if(UtilValidate.isNotEmpty(serviceName)){
                                        Map<String,String> serviceParameters = UtilProperties.getPropertiesWithPrefix(headerInfoProps,serviceType+".parameters.");

                                        if(UtilValidate.isNotEmpty(results)) {
                                            GenericValue origEntry = results.get(0);
                                            //serviceContext.putAll(origEntry.getAllFields());
                                            origEntry.forEach((key,value)->{
                                                if(UtilValidate.isNotEmpty(value)){
                                                    serviceProps.put(key,value);
                                                }
                                            });
                                        }

                                        serviceParameters.forEach((key,value) -> {
                                            serviceContext.put(key,substituteVariables(value,serviceProps));
                                        });

                                        Map<String,Object> serviceFields = dispatcher.getDispatchContext().
                                                makeValidContext(serviceName,ModelService.IN_PARAM,serviceContext);

                                        try {
                                            AtomicBoolean isSafeToDispatch = new AtomicBoolean(true);
                                            dispatcher.getModelService(serviceName).getInModelParamList().forEach(
                                                    s -> {
                                                        if (!s.optional && UtilValidate.isEmpty(serviceFields.get(s.getName()))) {
                                                            isSafeToDispatch.set(false);
                                                        }
                                                    }
                                            );

                                            if (isSafeToDispatch.get()) {
                                                if(UtilValidate.isNotEmpty(runAsync) && runAsync){
                                                    dispatcher.runAsync(serviceName, serviceFields, true);
                                                }else{
                                                    Map<String, Object> serviceResult = dispatcher.runSync(serviceName, serviceFields);

                                                    if (ServiceUtil.isSuccess(serviceResult)) {
                                                        Debug.logInfo("Imported field value: " + cell.getAddress().formatAsString(), module);
                                                    }else{
                                                        errorList.add("Couldn't import field "+cell.getAddress().formatAsString()+". Service returned with error: "+serviceResult.get("errorMessageList"));
                                                    }
                                                }
                                            }else{
                                                errorList.add("Couldn't run service "+serviceName+" for cell "+cell.getAddress().formatAsString()+" as some info was missing from service: "+serviceFields);
                                            }
                                        } catch (ServiceValidationException ex){
                                            TransactionUtil.rollback(beganTransaction, "Error importing cell "+cell.getAddress().formatAsString(),	ex);
                                            errorList.add("ServiceValidationException: Couldn't update from field value: "+cell.getAddress().formatAsString()+"");
                                            Debug.logWarning("Couldn't update from field value: "+cell.getAddress().formatAsString(),module);
                                        } catch (GenericServiceException ex){
                                            TransactionUtil.rollback(beganTransaction, "Error importing cell "+cell.getAddress().formatAsString(),	ex);
                                            errorList.add("GenericServiceException: Couldn't update from field value: "+cell.getAddress().formatAsString());
                                            Debug.logWarning("Couldn't update from field value: "+cell.getAddress().formatAsString(),module);
                                        }
                                    }else{
                                        //Update the entity directly
                                        try{
                                            GenericValue origEntry = results.get(0);
                                            if(!origEntry.get(fieldName).equals(cellValue)){
                                                origEntry.set(fieldName,cellValue);
                                                origEntry.createOrStore();
                                            }
                                        } catch (GenericEntityException ex){
                                            TransactionUtil.rollback(beganTransaction, "Error importing cell "+cell.getAddress().formatAsString(),	ex);
                                            errorList.add("GenericEntityException: Couldn't create or store entity value for fieldName "+fieldName+" and cell: "+cell.getAddress().formatAsString());
                                            Debug.logWarning("Couldn't update from field value: "+cell.getAddress().formatAsString(),module);
                                        }
                                    }
                                }
                            }
                        }

                        try {
                            TransactionUtil.commit(beganTransaction);
                            if (parentTransaction != null) TransactionUtil.resume(parentTransaction);
                        } catch (GenericTransactionException e) {
                            Debug.logError(e, module);
                            errorList.add("Cannot resume transaction: "+e.getMessage());
                        }finally {
                            try {
                                if (parentTransaction != null) TransactionUtil.resume(parentTransaction);
                            } catch (GenericTransactionException e) {
                                errorList.add("Cannot resume transaction: "+e.getMessage());
                                Debug.logError(e, "Cannot resume transaction:"+e.getMessage(), module);
                            }
                        }

                    }
                }
            }

        }catch (Exception e) {
            errorList.add("An exception was thrown: "+e.getMessage());
            ServiceUtil.returnError(e.getMessage());
        }

        if(!errorList.isEmpty()){
            return ServiceUtil.returnFailure(errorList);
        }


        return ServiceUtil.returnSuccess();
    }

    public static String substituteVariables(String originalValue, Map<String,Object> replaceProps){
        String str = StringSubstitutor.replace(originalValue, replaceProps);
        str=str.replaceAll("\\$\\{([^}]*?)\\}",""); //replace leftover variables
        if(str.trim().equals("null") || str.trim().length()==0){
            return null;
        }
        return str;
    }
}
