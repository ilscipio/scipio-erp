package com.ilscipio.scipio.accounting.external.datev;

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
import org.apache.tika.parser.txt.UniversalEncodingDetector;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.accounting.external.BaseOperationStats.NotificationLevel;
import com.ilscipio.scipio.accounting.external.BaseOperationStats.NotificationScope;
import com.ilscipio.scipio.common.util.TikaUtil;

import javolution.util.FastMap;

public class DatevServices {

    public final static String module = DatevServices.class.getName();
    public final static Character DEFAULT_DELIMITER = ';';
    public final static Character DEFAULT_QUOTE = '\"';

    /**
     * 
     * @param dctx
     * @param context
     * @return
     */
    public static Map<String, Object> importDatev(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();

        // Prepare result objects
        Map<String, Object> result = ServiceUtil.returnSuccess();

        // Get context params
        String orgPartyId = (String) context.get("orgPartyId");
        String topGlAccountId = (String) context.get("topGlAccountId");
        ByteBuffer fileBytes = (ByteBuffer) context.get("uploadedFile");
        String fileSize = (String) context.get("_uploadedFile_size");
        String fileName = (String) context.get("_uploadedFile_fileName");
        String contentType = (String) context.get("_uploadedFile_contentType");
        GenericValue dataCategory = (GenericValue) context.get("dataCategory");

        BufferedReader csvReader = null;
        DatevHelper datevHelper = null;
        String errorMessage = null;
        try {
            // Initialize helper
            datevHelper = new DatevHelper(delegator, orgPartyId, dataCategory);

            Character delimiter = DEFAULT_DELIMITER;
            if (UtilValidate.isNotEmpty(context.get("delimiter")))
                delimiter = ((String) context.get("delimiter")).charAt(0);
            Character quote = DEFAULT_QUOTE;
            if (UtilValidate.isNotEmpty(context.get("quote")))
                quote = ((String) context.get("quote")).charAt(0);
            Boolean hasMetaHeader = (Boolean) context.get("hasMetaHeader");
            Boolean hasHeader = (Boolean) context.get("hasHeader");

            if (Debug.isOn(Debug.VERBOSE)) {
                Debug.log("Content Type :" + contentType);
                Debug.log("File Name    :" + fileName);
                Debug.log("File Size    :" + fileSize);
            }

            double fileSizeConverted = UtilMisc.toDouble(fileSize);
            if (fileSizeConverted <= 0 && !fileBytes.hasRemaining()) {
                throw new DatevException("Uploaded CSV file is empty");
            }

            // Find media type
            MediaType mediaType = TikaUtil.findMediaTypeSafe(fileBytes, fileName);
            if (mediaType != null) {
                String[] splittedContentType = contentType.split("/");
                if (splittedContentType.length != 2) {
                    String notificationMessage = "File content type [" + contentType + "] is invalid";
                    datevHelper.addStat(notificationMessage, NotificationScope.GLOBAL, NotificationLevel.WARNING);
                    Debug.logWarning(notificationMessage, module);
                }

                String mediaTypeStr = mediaType.getType().concat("/").concat(mediaType.getSubtype());
                if (!contentType.equals("text/csv") && !mediaTypeStr.equals("text/csv")) {
                    ServiceUtil.returnError("File [" + fileName + "] is not a valid CSV file.");
                } else if (!contentType.equals(mediaTypeStr)) {
                    String notificationMessage = "File content type  [" + contentType + "] differs from the content type found by Tika [" + mediaType.getType() + "/"
                            + mediaType.getSubtype() + "]";
                    datevHelper.addStat(notificationMessage, NotificationScope.GLOBAL, NotificationLevel.WARNING);
                    Debug.logWarning(notificationMessage, module);
                }
            }
            fileBytes.rewind();

            // Find charset
            Charset detectedCharset = TikaUtil.findCharsetSafe(fileBytes, fileName, UniversalEncodingDetector.class, mediaType);
            if (UtilValidate.isEmpty(detectedCharset)) {
                String systemEncoding = System.getProperty("file.encoding");
                detectedCharset = Charset.forName(systemEncoding);
            }
            fileBytes.rewind();

            // Initialize CSV format
            fileBytes.rewind();
            csvReader = new BufferedReader(new StringReader(detectedCharset.decode(fileBytes).toString()));
            CSVFormat fmt = CSVFormat.newFormat(delimiter).withQuote(quote).withQuoteMode(QuoteMode.NON_NUMERIC);

            // Find out if CSV has a meta header so we can remove it and loop
            // only real records
            if (csvReader.markSupported())
                csvReader.mark(fileBytes.limit());
            String metaHeader = csvReader.readLine();
            try {
                Iterator<String> metaHeaderIter = CSVParser.parse(metaHeader, fmt).getRecords().get(0).iterator();
                if (!datevHelper.isMetaHeader(metaHeaderIter)) {
                    if (csvReader.markSupported()) {
                        csvReader.reset();
                    } else {
                        csvReader.close();
                        csvReader = new BufferedReader(new StringReader(detectedCharset.decode(fileBytes).toString()));
                    }
                }
            } catch (Exception e) {
                datevHelper.addStat("Failed parsing metadata header: " + e.getMessage(), NotificationScope.META_HEADER, NotificationLevel.IGNORE);
                Debug.logWarning(e, module);
            }

            // Find out if CSV has a header so we can remove it and loop only
            // real records
            String[] datevFieldNames = datevHelper.getFieldNames();
            fmt = fmt.withHeader(datevFieldNames);
            if (fmt.getHeader() != null && fmt.getHeader().length > 0) {
                fmt = fmt.withSkipHeaderRecord(true);
            } else {
                datevHelper.addStat("Header couldn't be found. CSV file parsed using column position according to DATEV specification.", NotificationScope.HEADER,
                        NotificationLevel.WARNING);
            }

            // Parse CSV
            CSVParser parser = fmt.parse(csvReader);
            List<CSVRecord> records = parser.getRecords();
            if (parser.getRecordNumber() <= 0) {
                throw new DatevException("No records found after CSV has been parsed.");
            } else {
                Iterator<CSVRecord> recordIter = records.iterator();
                for (int index = 0; recordIter.hasNext(); index++) {
                    final CSVRecord rec = recordIter.next();
                    if (Debug.isOn(Debug.VERBOSE)) {
                        Debug.logInfo(rec.toString(), module);
                    }
                    boolean allFieldValid = true;
                    Map<String, String> recordMap = FastMap.newInstance();
                    if (rec.isConsistent()) {
                        recordMap = rec.toMap();
                        for (String key : recordMap.keySet()) {
                            if (!datevHelper.validateField(key, recordMap.get(key))) {
                                allFieldValid = false;
                            }
                        }
                    } else {
                        // Assuming here the fields (columns) are in the order
                        // DATEV specification determines
                        Iterator<String> iter = rec.iterator();
                        for (int i = 0; iter.hasNext(); i++) {
                            String value = iter.next();
                            if (!datevHelper.validateField(i, value)) {
                                allFieldValid = false;
                            }
                            recordMap.put(datevFieldNames[i], value);
                        }
                    }
                    if (allFieldValid) {
                        try {
                            datevHelper.processRecord(index, recordMap);
                        } catch (DatevException e) {

                        }
                    } else {
                        datevHelper.addRecordStat("Record validation failed", NotificationLevel.ERROR, index, recordMap, false);
                    }
                }
            }

        } catch (DatevException e) {
            Debug.logError(e, module);
            errorMessage = e.getMessage();
        } catch (Exception e) {
            Debug.logError(e, module);
        } finally {
            if (UtilValidate.isNotEmpty(errorMessage)) {
                if (UtilValidate.isNotEmpty(datevHelper)) {
                    datevHelper.addStat(errorMessage, NotificationScope.GLOBAL, NotificationLevel.FATAL);
                } else {
                    result = ServiceUtil.returnError(errorMessage);
                }
            } else {
                if (UtilValidate.isNotEmpty(datevHelper)) {
                    result.put("operationStats", datevHelper.getStats());
                    if (!datevHelper.hasFatalNotification()) {
                        result.put("operationResults", datevHelper.getResults());
                    } else {
                        result = ServiceUtil.returnError("A fatal error ocurred while importing the CSV.");
                    }
                }
            }

            fileBytes.clear();
            try {
                csvReader.close();
            } catch (IOException e) {
                ;
            }

        }

        result.put("orgPartyId", orgPartyId);
        result.put("topGlAccountId", topGlAccountId);

        return result;
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
