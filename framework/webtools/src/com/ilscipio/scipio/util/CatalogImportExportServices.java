package com.ilscipio.scipio.util;

import org.apache.commons.text.StringSubstitutor;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.ofbiz.base.util.*;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.*;
import org.apache.poi.ss.usermodel.*;

import java.io.*;
import java.nio.ByteBuffer;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;

public class CatalogImportExportServices {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final Properties SCIPIO_IMPORT_PROPERTIES = UtilProperties.getProperties("ExcelImport");
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
        String fileSize = (String) context.get("_uploadedFile_size");
        String fileName = (String) context.get("_uploadedFile_fileName");
        String contentType = (String) context.get("_uploadedFile_contentType");
        Map<String,String> categoryProperties = UtilProperties.getPropertiesWithPrefix(SCIPIO_IMPORT_PROPERTIES,SCIPIO_IMPORT_PREFIX+"."+templateName);
        Map<String,Integer> primaryIdMap = new HashMap<String,Integer>();
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        try(InputStream is = new ByteArrayInputStream(byteBuffer.array())) {
            //Create Workbook instance holding reference to .xlsx file
            XSSFWorkbook workbook = new XSSFWorkbook(is);
            boolean headerRead = false;

            Map<Integer,Map> headerInfo = new HashMap<Integer,Map>();
            for (Sheet sheet : workbook ) {
                for (Row row : sheet) {
                    if ( row == null || row.getCell(0).getStringCellValue().startsWith("#")){
                        //skip to first row with content that is not a comment row
                        continue;
                    }

                    if(!headerRead){
                        // Read headerInfo and compare against defined fields
                        headerRead= true;
                        for(Cell cell : row){
                            Map<String,String> headerInfoProps = new HashMap<>();
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
                        Properties serviceProps = new Properties();
                        Properties entityProps = new Properties();

                        for(Cell cell : row){
                            String fieldValue = cell.getStringCellValue();

                            if(headerInfo.get(cell.getColumnIndex())!=null){
                                Map<String,Object> serviceContext = new HashMap<String,Object>();
                                serviceContext.put("value",fieldValue);
                                serviceContext.put("userLogin",userLogin);

                                primaryIdMap.forEach( (key,location)-> {
                                    serviceContext.put(key,row.getCell(location).getStringCellValue());
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

                                    //If entity has a thruDate, select only current entries
                                    EntityCondition condition = EntityCondition.makeCondition(
                                            EntityCondition.makeCondition(entityParameters)
                                    );

                                    ModelEntity myLookupModel = delegator.getModelEntity(entityName);

                                    if(myLookupModel.getField("thruDate") != null) {
                                        condition = condition.append(condition,EntityUtil.getFilterByDateExpr());
                                    }

                                    EntityCondition conditionWithLocale = condition;
                                    //most of the entities have "locale" mapped as "localeString", so setting it here
                                    if(UtilValidate.isNotEmpty(localeStr) && myLookupModel.getField("localeString")!= null){
                                        conditionWithLocale = conditionWithLocale.append(conditionWithLocale,EntityCondition.makeCondition("localeString",localeStr));
                                    }
                                    // Still checking against "locale" just in case
                                    if(UtilValidate.isNotEmpty(localeStr) && myLookupModel.getField("locale")!= null){
                                        conditionWithLocale = conditionWithLocale.append(conditionWithLocale,EntityCondition.makeCondition("locale",localeStr));
                                    }


                                    List<GenericValue> results = null;
                                    String serviceName = updateService;
                                    String serviceType = "update";
                                    try {
                                        results = delegator.findList(entityName, conditionWithLocale, null, null, null, false);
                                        if(UtilValidate.isEmpty(results)){
                                            serviceName = createService;
                                            serviceType = "create";
                                            results = delegator.findList(entityName, condition, null, null, null, false);
                                            if(results.isEmpty()){
                                                serviceName = null;
                                            }
                                        }
                                    }catch (Exception e){
                                    }

                                    //Check if entry exists, if so, set it in context
                                    serviceContext.forEach((key,value)->{
                                        if(UtilValidate.isNotEmpty(value)){
                                            serviceProps.put(key,value);
                                        }
                                    });

                                    if(UtilValidate.isNotEmpty(serviceName)){
                                        if(UtilValidate.isNotEmpty(results)) {
                                            GenericValue origEntry = results.get(0);
                                            //serviceContext.putAll(origEntry.getAllFields());
                                            origEntry.forEach((key,value)->{
                                                if(UtilValidate.isNotEmpty(value)){
                                                    serviceProps.put(key,value);
                                                }
                                            });
                                        }


                                        Map<String,String> serviceParameters = UtilProperties.getPropertiesWithPrefix(headerInfoProps,serviceType+".parameters.");

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
                                                Map<String, Object> serviceResult = dispatcher.runSync(serviceName, serviceFields, true);

                                                if (ServiceUtil.isSuccess(serviceResult)) {
                                                    Debug.logInfo("Imported field value: " + cell.getAddress().formatAsString(), module);
                                                }
                                            }
                                        } catch (ServiceValidationException ex){
                                            Debug.logWarning("Couldn't update from field value: "+cell.getAddress().formatAsString(),module);
                                        } catch (GenericServiceException ex){
                                            Debug.logWarning("Couldn't update from field value: "+cell.getAddress().formatAsString(),module);
                                        }
                                    }else{
                                        //Update the entity directly
                                        GenericValue origEntry = results.get(0);
                                        if(!origEntry.get(fieldName).equals(fieldValue)){
                                            origEntry.set(fieldName,fieldValue);
                                            origEntry.createOrStore();
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }



        }catch (Exception e) {
            ServiceUtil.returnError(e.getMessage());
        }


        return ServiceUtil.returnSuccess();
    }

    public static String substituteVariables(String originalValue, Properties replaceProps){
        String str = StringSubstitutor.replace(originalValue, replaceProps);
        str=str.replaceAll("\\$\\{([^}]*?)\\}",""); //replace leftover variables
        if(str.trim().equals("null") || str.trim().length()==0){
            return null;
        }
        return str;
    }
}
