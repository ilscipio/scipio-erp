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
import org.apache.tika.parser.txt.UniversalEncodingDetector;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.common.util.TikaUtil;

import javolution.util.FastList;

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

        String orgPartyId = (String) context.get("organizationPartyId");

        ByteBuffer fileBytes = (ByteBuffer) context.get("uploadedFile");
        String fileSize = (String) context.get("_uploadedFile_size");
        String fileName = (String) context.get("_uploadedFile_fileName");
        String contentType = (String) context.get("_uploadedFile_contentType");

        if (Debug.isOn(Debug.VERBOSE)) {
            Debug.log("Content Type :" + contentType);
            Debug.log("File Name    :" + fileName);
            Debug.log("File Size    :" + String.valueOf(fileSize));
        }

        double fileSizeConverted = UtilMisc.toDouble(fileSize);
        if (fileSizeConverted <= 0 && !fileBytes.hasRemaining()) {
            return ServiceUtil.returnError("Uploaded CSV file is empty");
        }

        // Find media type
        MediaType mediaType = TikaUtil.findMediaTypeSafe(fileBytes, fileName);
        if (mediaType != null) {
            String[] splittedContentType = contentType.split("/");
            if (splittedContentType.length != 2) {
                Debug.logWarning("File content type [" + contentType + "] is invalid", module);
            }

            String mediaTypeStr = mediaType.getType().concat("/").concat(mediaType.getSubtype());
            if (!contentType.equals("text/csv") && !mediaTypeStr.equals("text/csv")) {
                ServiceUtil.returnError("File [" + fileName + "] is not a valid CSV file.");
            } else if (!contentType.equals(mediaTypeStr)) {
                Debug.logWarning(
                        "File content type  [" + contentType + "] differs from the content type found by Tika [" + mediaType.getType() + "/" + mediaType.getSubtype() + "]",
                        module);
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

        // Parse CSV
        String csvString = detectedCharset.decode(fileBytes).toString();
        final BufferedReader csvReader = new BufferedReader(new StringReader(csvString));
        CSVFormat fmt = CSVFormat.DEFAULT.withDelimiter(';').withQuote('"').withQuoteMode(QuoteMode.NON_NUMERIC);

        try {
            DatevHelper datevHelper = new DatevHelper(delegator);
            fmt.withHeaderComments(datevHelper.getDatevMetadataTransactionFieldNames()).withHeader(datevHelper.getDatevTransactionFieldNames());
            // fmt.withHeader(datevHelper.getDatevTransactionFieldNames());
            CSVParser parser = fmt.parse(csvReader);
            long recordsNumber = parser.getRecordNumber();
            if (recordsNumber < 2) {
                return ServiceUtil.returnError("No records found.");
            } else {
                List<CSVRecord> records = parser.getRecords();
                List<CSVRecord> recordsWithoutHeaders = FastList.newInstance();
                recordsWithoutHeaders.addAll(records);
                for (int i = 0; i < 2; i++) {
                    CSVRecord csvRecord = records.get(i);
                    if (datevHelper.isHeader(csvRecord.toMap())) {
                        recordsWithoutHeaders.remove(csvRecord);
                    }
                }
                for (final CSVRecord rec : recordsWithoutHeaders) {
                    for (Iterator<String> iter = rec.iterator(); iter.hasNext();) {
                        String value = iter.next();
                        Debug.log("Record [" + rec.getRecordNumber() + "]: <" + value + ">");
                    }
                }
            }
        } catch (IOException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        } finally {
            try {
                csvReader.close();
            } catch (IOException e) {
                ;
            }
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
