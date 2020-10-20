package org.ofbiz.webapp.util;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.webapp.event.FileUploadProgressListener;

import javax.servlet.http.HttpServletRequest;
import java.io.File;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import static org.ofbiz.base.util.UtilGenerics.checkList;

/**
 * A version of {@link UtilHttp} for the webapp package (SCIPIO).
 * TODO: When UtilHttp dependencies fixed, these might get moved/delegated to UtilHttp where they have reflection workarounds for now.
 */
public abstract class UtilHttpWeb {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Reads to gets the last parsed multiPartMap.
     * NOTE: This method swallows upload errors, so use {@link #readMultiPartParameterMap(HttpServletRequest)} for those locations
     * that can handle them, otherwise reported errors may be incorrect.
     */
    public static Map<String, Object> getMultiPartParameterMap(HttpServletRequest request) { // SCIPIO: modified
        Map<String, Object> multiPartMap = UtilGenerics.cast(request.getAttribute("multiPartMap"));
        if (multiPartMap == null) {
            try {
                multiPartMap = readMultiPartParameterMap(request);
            } catch (IOException e) {
                Debug.logError(e, "Unable to read multi-part parameter map: " + e.getMessage(), module);
            }
            if (multiPartMap == null) {
                multiPartMap = Collections.emptyMap();
            }
            request.setAttribute("multiPartMap", multiPartMap);
        }
        return multiPartMap;
    }

    public static Map<String, Object> readMultiPartParameterMap(HttpServletRequest request) throws IOException { // SCIPIO: modified
        boolean isMultiPart = ServletFileUpload.isMultipartContent(request);
        Map<String, Object> multiPartMap = null;
        if (isMultiPart) {
            Delegator delegator = UtilGenerics.cast(request.getAttribute("delegator"));
            multiPartMap = new LinkedHashMap<String, Object>(); // SCIPIO: now LinkedHashMap
            // get the http upload configuration
            String maxSizeStr = EntityUtilProperties.getPropertyValue("general", "http.upload.max.size", "-1", delegator);
            long maxUploadSize = -1;
            try {
                maxUploadSize = Long.parseLong(maxSizeStr);
            } catch (NumberFormatException e) {
                Debug.logError(e, "Unable to obtain the max upload size from general.properties; using default -1", module);
                maxUploadSize = -1;
            }
            // get the http size threshold configuration - files bigger than this will be
            // temporarly stored on disk during upload
            String sizeThresholdStr = EntityUtilProperties.getPropertyValue("general", "http.upload.max.sizethreshold", "10240", delegator);
            int sizeThreshold = 10240; // 10K
            try {
                sizeThreshold = Integer.parseInt(sizeThresholdStr);
            } catch (NumberFormatException e) {
                Debug.logError(e, "Unable to obtain the threshold size from general.properties; using default 10K", module);
                sizeThreshold = -1;
            }
            // directory used to temporarily store files that are larger than the configured size threshold
            String tmpUploadRepository = EntityUtilProperties.getPropertyValue("general", "http.upload.tmprepository", "runtime/tmp", delegator);
            String encoding = request.getCharacterEncoding();
            // check for multipart content types which may have uploaded items

            ServletFileUpload upload = new ServletFileUpload(new DiskFileItemFactory(sizeThreshold, new File(tmpUploadRepository)));

            // create the progress listener and add it to the session
            FileUploadProgressListener listener = new FileUploadProgressListener();
            upload.setProgressListener(listener);
            request.getSession().setAttribute("uploadProgressListener", listener); // SCIPIO: request.getSession

            if (encoding != null) {
                upload.setHeaderEncoding(encoding);
            }
            upload.setSizeMax(maxUploadSize);

            List<FileItem> uploadedItems = null;
            try {
                uploadedItems = UtilGenerics.<FileItem>checkList(upload.parseRequest(request));
            } catch (FileUploadException e) {
                throw new IOException(e); // SCIPIO
            }
            if (uploadedItems != null) {
                for (FileItem item: uploadedItems) {
                    String fieldName = item.getFieldName();
                    //byte[] itemBytes = item.get();
                    /*
                    Debug.logInfo("Item Info [" + fieldName + "] : " + item.getName() + " / " + item.getSize() + " / " +
                            item.getContentType() + " FF: " + item.isFormField(), module);
                    */
                    if (item.isFormField() || item.getName() == null) {
                        if (multiPartMap.containsKey(fieldName)) {
                            Object mapValue = multiPartMap.get(fieldName);
                            if (mapValue instanceof List<?>) {
                                checkList(mapValue, Object.class).add(item.getString());
                            } else if (mapValue instanceof String) {
                                List<String> newList = new LinkedList<String>();
                                newList.add((String) mapValue);
                                newList.add(item.getString());
                                multiPartMap.put(fieldName, newList);
                            } else {
                                Debug.logWarning("Form field found [" + fieldName + "] which was not handled!", module);
                            }
                        } else {
                            if (encoding != null) {
                                try {
                                    multiPartMap.put(fieldName, item.getString(encoding));
                                } catch (java.io.UnsupportedEncodingException uee) {
                                    Debug.logError(uee, "Unsupported Encoding, using deafault", module);
                                    multiPartMap.put(fieldName, item.getString());
                                }
                            } else {
                                multiPartMap.put(fieldName, item.getString());
                            }
                        }
                    } else {
                        String fileName = item.getName();
                        if (fileName.indexOf('\\') > -1 || fileName.indexOf('/') > -1) {
                            // get just the file name IE and other browsers also pass in the local path
                            int lastIndex = fileName.lastIndexOf('\\');
                            if (lastIndex == -1) {
                                lastIndex = fileName.lastIndexOf('/');
                            }
                            if (lastIndex > -1) {
                                fileName = fileName.substring(lastIndex + 1);
                            }
                        }
                        multiPartMap.put(fieldName, ByteBuffer.wrap(item.get()));
                        multiPartMap.put("_" + fieldName + "_fileItem", item); // SCIPIO: Added 2020-10
                        multiPartMap.put("_" + fieldName + "_size", item.getSize());
                        multiPartMap.put("_" + fieldName + "_fileName", fileName);
                        multiPartMap.put("_" + fieldName + "_contentType", item.getContentType());
                    }
                }
            }
        }
        return multiPartMap;
    }
}
