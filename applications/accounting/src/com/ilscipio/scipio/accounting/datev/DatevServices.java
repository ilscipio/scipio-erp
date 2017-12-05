package com.ilscipio.scipio.accounting.datev;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.apache.commons.csv.QuoteMode;
import org.apache.tika.mime.MediaType;
import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.common.util.TikaUtil;

public class DatevServices {

    public static String module = DatevServices.class.getName();

    /**
     * 
     * @param dctx
     * @param context
     * @return
     */
    public static Map<String, Object> importDatevTransactionEntries(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();

        // TODO: Implement import datev data from a csv
        String orgPartyId = (String) context.get("organizationPartyId");
        ByteBuffer fileBytes = (ByteBuffer) context.get("uploadedFile");
        
        String fileSize = (String) context.get("_uploadedFile_size");
        String fileName = (String) context.get("_uploadedFile_fileName");
        String contentType = (String) context.get("_uploadedFile_contentType");
        
        Debug.log("contentType =====> " + contentType);
        
        String encoding = System.getProperty("file.encoding");
        Debug.log("encoding ======> " + encoding);
//        TikaUtil.findCharset(, fileName);
        String csvString = Charset.forName("ISO-8859-1").decode(fileBytes).toString();
        final BufferedReader csvReader = new BufferedReader(new StringReader(csvString));
        CSVFormat fmt = CSVFormat.DEFAULT.withDelimiter(';').withQuote('"').withQuoteMode(QuoteMode.NON_NUMERIC);
        
        try {
            List<GenericValue> datevTransactionEntryDefinitions = EntityQuery.use(delegator).from("DatevTransactionEntryDefinition").queryList();            
            
            CSVParser parser = fmt.parse(csvReader);
            
            MediaType mediaType = TikaUtil.findMediaType(fileBytes, fileName);
            Debug.log("media type ====>" + mediaType.getType());

            for (final CSVRecord rec : parser.getRecords()) {
                Map<String, String> recordMap = rec.toMap();                
                for (Iterator<String> iter = rec.iterator(); iter.hasNext();) {
                    String value = iter.next();
                    Debug.log("Record [" + rec.getRecordNumber() + "]: <" + value + ">");
                }
            }
        } catch (IOException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
        
        
        return ServiceUtil.returnSuccess();
    }
    
    /**
     * 
     * @param dctx
     * @param context
     * @return
     */
    public static Map<String, Object> exportDatevTransactionEntries(DispatchContext dctx, Map<String, Object> context) {
        // TODO: Implement export datev data in csv format
        return ServiceUtil.returnSuccess();
    }

}
