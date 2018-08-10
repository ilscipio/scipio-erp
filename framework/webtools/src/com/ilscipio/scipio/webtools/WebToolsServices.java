/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package com.ilscipio.scipio.webtools;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.apache.commons.io.IOUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityComparisonOperator;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityJoinOperator;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelViewEntity;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.ce.util.PathUtil;

/**
 * WebTools Services
 */

public class WebToolsServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Generates an entity export based on a list of entity-names and optional fromDate. Creates a zip and stores in EntityExport
     * entity. Afterwards returns the generated downloadLink
     * @return String of generated filedownloadlink
     * */
    public static Map<String, Object> getEntityExport(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        List<String> entityNames = UtilGenerics.checkList(context.get("entityList"));
        EntityCondition entityFromCond = null;
        EntityCondition entityThruCond = null;
        EntityCondition entityDateCond = null;
        Delegator delegator = dctx.getDelegator();
        String entityFrom = (String) context.get("fromDate");
        String entityThru = (String) context.get("thruDate");
        String description = (String) context.get("description");
        List<File> srcFiles = new LinkedList<File>();
        Timestamp createdDate = UtilDateTime.nowTimestamp();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        
        if (UtilValidate.isNotEmpty(entityFrom)) {
            entityFromCond = EntityCondition.makeCondition("lastUpdatedTxStamp", EntityComparisonOperator.GREATER_THAN, entityFrom);
        }
        if (UtilValidate.isNotEmpty(entityThru)) {
            entityThruCond = EntityCondition.makeCondition("lastUpdatedTxStamp", EntityComparisonOperator.LESS_THAN, entityThru);
        }
        if (UtilValidate.isNotEmpty(entityFromCond) && UtilValidate.isNotEmpty(entityThruCond) ) {
            entityDateCond = EntityCondition.makeCondition(entityFromCond, EntityJoinOperator.AND, entityThruCond);
        } else if (UtilValidate.isNotEmpty(entityFromCond)) {
            entityDateCond = entityFromCond;
        } else if (UtilValidate.isNotEmpty(entityThruCond)) {
            entityDateCond = entityThruCond;
        }


        try {
            // Generate temporary export files
            for(String curEntityName : entityNames){
                long numberWritten = 0;
                ModelEntity me = delegator.getModelEntity(curEntityName);
                List<GenericValue> values = new LinkedList<GenericValue>();
                if (me instanceof ModelViewEntity) {
                    Debug.logInfo(curEntityName + " is a view entity. Skipping...",module);
                    continue;
                }
        
                boolean beganTx = TransactionUtil.begin();
                
                try {
                    
                    values = EntityQuery.use(delegator).from(curEntityName).where(entityDateCond).orderBy(me.getPkFieldNames()).queryList();
                } catch (Exception entityEx) {
                    Debug.logInfo("Error when looking up "+curEntityName + " "+entityEx,module);
                    continue;
                }
        
                
                // Iterate over all values
                if (values != null) {
                    File outFile = new File(PathUtil.getTempFileDir(), curEntityName +".xml");
                    PrintWriter writer = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outFile), "UTF-8")));
                    writer.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
                    writer.println("<entity-engine-xml>");

                   for(GenericValue value : values){
                        value.writeXmlText(writer, "");
                        numberWritten++;
                        if (numberWritten % 500 == 0) {
                            TransactionUtil.commit(beganTx);
                            beganTx = TransactionUtil.begin();
                            }
                        }
                   writer.println("</entity-engine-xml>");
                   writer.close();
                   srcFiles.add(outFile);
                   
                } else {
                    Debug.logInfo(curEntityName + " has no records, not writing file",module);
                }
                TransactionUtil.commit(beganTx);
               
            }
            
            // Zip Files
            String fileSuffix = new SimpleDateFormat("yyyyMMddHHmm").format(new Date());
            String fileName = "Entityexport_"+fileSuffix+".zip";
            File outZipFile = new File(PathUtil.getTempFileDir(), fileName);
            FileOutputStream fos = new FileOutputStream(outZipFile);
            ZipOutputStream zipOut = new ZipOutputStream(fos);
            for (File srcFile : srcFiles) {
                 FileInputStream fis = new FileInputStream(srcFile);
                 ZipEntry zipEntry = new ZipEntry(srcFile.getName());
                 zipOut.putNextEntry(zipEntry);
                 byte[] bytes = new byte[1024];
                 int length;
                 while((length = fis.read(bytes)) >= 0) {
                 zipOut.write(bytes, 0, length);
                 }
                 fis.close();
                 }
            
             zipOut.close();
             fos.close();
             
             //Upload to database
             GenericValue dataResource = delegator.makeValue("EntityExport");
             String seqId = delegator.getNextSeqId("EntityExport");
             dataResource.put("exportId", seqId);
             if(UtilValidate.isNotEmpty(description)){
                 dataResource.put("description", description);
             }else{
                 dataResource.put("description", "Generated on "+createdDate);
             }
             
             if(UtilValidate.isNotEmpty(userLogin)){
                 dataResource.put("createdBy", userLogin.getString("userLoginId"));                     
             }

             FileInputStream fileInputStream = new FileInputStream(outZipFile);
             byte[] zipFile = IOUtils.toByteArray(fileInputStream);
             dataResource.put("file", zipFile);
             dataResource.put("fileSize", new Long(zipFile.length));
             delegator.createOrStore(dataResource);
             
             //Return fileId
             result.put("exportId", dataResource.getPrimaryKey().getString("exportId"));
             
             
             //Cleanup 
             for(File srcFile : srcFiles){
                 srcFile.delete();
             }
             // outZipFile.delete();
        }catch(Exception e){
            Debug.logError("Error while generating Entity Export "+e, module);
            result = ServiceUtil.returnError("Error while generating Entity Export");
        }
        return result;
    }
    
}
