package com.ilscipio.scipio.accounting.invoice;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.OutputStream;
import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

public class InvoiceEvents {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static String massDownloadInvoices(HttpServletRequest request, HttpServletResponse response) {
        Map<String, Object> params = UtilHttp.getCombinedMap(request);

        Delegator delegator = (Delegator) params.get("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) params.get("dispatcher");
        Locale locale = UtilHttp.getLocale(request);

        String screenLocation = "component://accounting/widget/AccountingPrintScreens.xml#InvoicePDF";

        String fromDateStr = (String) params.get("fromDate");
        String thruDateStr = (String) params.get("thruDate");
        String status = (String) params.get("status");
        String type = (String) params.get("type");
        String partyIdFrom = (String) params.get("partyIdFrom");
        String partyIdTo = (String) params.get("partyIdTo");
        String partyGroupId = (String) params.get("partyGroupId");

        Timestamp fromDate = UtilDateTime.nowTimestamp();
        Timestamp thruDate = null;
        if (UtilValidate.isNotEmpty(fromDateStr)) {
            try {
                fromDate = UtilDateTime.stringToTimeStamp(fromDateStr, "yyyy-MM-dd hh:mm:ss", UtilHttp.getTimeZone(request), locale);
            } catch (ParseException e) {
                Debug.logWarning(e.getMessage(), module);
                fromDate = UtilDateTime.nowTimestamp();
            }
        }
        if (UtilValidate.isNotEmpty(thruDateStr)) {
            thruDate = UtilDateTime.toTimestamp(thruDateStr);
        }
        if (UtilValidate.isNotEmpty(thruDate) && fromDate.after(thruDate)) {
            request.setAttribute("_ERROR_MESSAGE_", "Through date must be set ahead from date");
            return "error";
        }

        if ((UtilValidate.isEmpty(partyIdTo) && UtilValidate.isNotEmpty(type) && type.equals("PURCHASE_INVOICE"))) {
            partyIdTo = partyGroupId;
        }
        if (UtilValidate.isEmpty(partyIdTo) && UtilValidate.isEmpty(partyIdFrom)) {
            partyIdFrom = partyGroupId;
        }

        List<EntityCondition> invoiceConds = UtilMisc.newList();
        invoiceConds.add(EntityCondition.makeCondition("invoiceDate", EntityOperator.GREATER_THAN_EQUAL_TO, fromDate));
        if (UtilValidate.isNotEmpty(thruDate)) {
            invoiceConds.add(EntityCondition.makeCondition("invoiceDate", EntityOperator.LESS_THAN_EQUAL_TO, thruDate));
        }
        if (UtilValidate.isNotEmpty(partyIdFrom)) {
            invoiceConds.add(EntityCondition.makeCondition("partyIdFrom", EntityOperator.EQUALS, partyIdFrom));
        }
        if (UtilValidate.isNotEmpty(partyIdTo)) {
            invoiceConds.add(EntityCondition.makeCondition("partyId", EntityOperator.EQUALS, partyIdTo));
        }
        if (UtilValidate.isNotEmpty(type)) {
            invoiceConds.add(EntityCondition.makeCondition("invoiceTypeId", EntityOperator.EQUALS, type));
        }
        if (UtilValidate.isNotEmpty(status)) {
            invoiceConds.add(EntityCondition.makeCondition("statusId", EntityOperator.EQUALS, status));
        }

        try {
            List<GenericValue> invoices = delegator.findList("Invoice", EntityCondition.makeCondition(invoiceConds), UtilMisc.toSet("invoiceId"), null, null, false);
            Map<String, Object> ctx = UtilMisc.newMap();
            GenericValue userLogin = (GenericValue) params.get("userLogin");
            ctx.put("userLogin", userLogin);
            ctx.put("screenLocation", screenLocation);
//            ctx.put("filePath", PathUtil.getTempFileDir().getAbsolutePath());
            List<File> srcFiles = UtilMisc.newList();
            for (GenericValue invoice : invoices) {
                String invoiceId = invoice.getString("invoiceId");
                ctx.put("fileName", "invoice_" + invoiceId + "_");
                ctx.put("screenContext", UtilMisc.toMap("invoiceId", invoiceId, "locale", locale, "userLogin", userLogin));
                try {
                    Map<String, Object> createFileFromScreen = dispatcher.runSync("createFileFromScreen", ctx, false);
                    srcFiles.add((File) createFileFromScreen.get("fileOutput"));
                } catch (GenericServiceException e) {
                    Debug.logError(e, module);
                }
            }

            // Zip Files
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            String fileSuffix = new SimpleDateFormat("yyyyMMddHHmm").format(new Date());
            String fileName = "InvoiceExport_" + fileSuffix + ".zip";
//            File outZipFile = new File(PathUtil.getTempFileDir(), fileName);
//            FileOutputStream fos = new FileOutputStream(outZipFile);
            ZipOutputStream zipOut = new ZipOutputStream(bos);

            for (File srcFile : srcFiles) {
                FileInputStream fis = new FileInputStream(srcFile);
                ZipEntry zipEntry = new ZipEntry(srcFile.getName());
                zipOut.putNextEntry(zipEntry);
                byte[] bytes = new byte[1024];
                int length;
                while ((length = fis.read(bytes)) >= 0) {
                    zipOut.write(bytes, 0, length);
                }
                fis.close();
            }
            zipOut.close();
//            fos.close();
            bos.close();

            byte[] zipBytes = bos.toByteArray();
            ByteArrayInputStream bis = new ByteArrayInputStream(zipBytes);
            ZipInputStream zis = new ZipInputStream(bis);
            if (zis != null) {
//                UtilHttp.streamContentToBrowser(response, zis, zipBytes.length, "application/zip", fileName);
                response.setContentType("application/zip");
                if (fileName != null) {
                    UtilHttp.setContentDisposition(response, fileName);
                }
                OutputStream os = response.getOutputStream();
                os.write(zipBytes, 0, zipBytes.length);
                os.flush();
            }
            bis.close();
            zis.close();
        } catch (Exception e) {
            Debug.logError(e, module);
        }

        return "success";
    }

}
