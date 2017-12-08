package com.ilscipio.scipio.accounting.datev;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.Iterator;
import java.util.Map;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.apache.commons.csv.QuoteMode;
import org.apache.tika.mime.MediaType;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
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
//        if (Double.parseDouble(fileSize) <= 0) {
//            return ServiceUtil.returnError("Uploaded CSV file is empty");
//        }

        MediaType mediaType = TikaUtil.findMediaTypeSafe(fileBytes, fileName);
        if (mediaType != null) {
            Debug.log("media type ====>" + mediaType.getType());
        }

        Charset detectedCharset = TikaUtil.findCharsetSafe(fileBytes, fileName);
        if (UtilValidate.isEmpty(detectedCharset)) {
            String systemEncoding = System.getProperty("file.encoding");
            detectedCharset = Charset.forName(systemEncoding);
        }
        String csvString = detectedCharset.decode(fileBytes).toString();
        final BufferedReader csvReader = new BufferedReader(new StringReader(csvString));
        CSVFormat fmt = CSVFormat.DEFAULT.withDelimiter(';').withQuote('"').withQuoteMode(QuoteMode.NON_NUMERIC);

        try {
            CSVParser parser = fmt.parse(csvReader);

            DatevHelper datevHelper = new DatevHelper(delegator);

            long recordsNumber = parser.getRecordNumber();
            if (recordsNumber < 2) {
                return ServiceUtil.returnError("No records found.");
            } else {
                CSVRecord csvRecord = parser.getRecords().get(0);
                datevHelper.findHeader(csvRecord.toMap());
            }
            for (final CSVRecord rec : parser.getRecords()) {
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
