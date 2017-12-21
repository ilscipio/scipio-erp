import java.nio.ByteBuffer

import org.apache.commons.fileupload.FileItem
import org.apache.commons.fileupload.FileUploadException
import org.apache.commons.fileupload.disk.DiskFileItemFactory
import org.apache.commons.fileupload.servlet.ServletFileUpload
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.FileUtil
import org.ofbiz.base.util.UtilGenerics
import org.ofbiz.base.util.UtilProperties
import org.ofbiz.content.content.UploadContentAndImage
import org.ofbiz.webapp.event.FileUploadProgressListener

import javolution.util.FastMap

final String module = "DatevUtil.groovy";

ServletFileUpload dfu = new ServletFileUpload(new DiskFileItemFactory(10240, FileUtil.getFile("runtime/tmp")));

// SCIPIO: patch - from ServiceEventHandler: create the progress listener and add it to the session
FileUploadProgressListener listener = new FileUploadProgressListener();
dfu.setProgressListener(listener);
request.getSession().setAttribute("uploadProgressListener", listener);

List<FileItem> lst = null;
try {
    lst = UtilGenerics.checkList(dfu.parseRequest(request));
} catch (FileUploadException e4) {
    request.setAttribute("_ERROR_MESSAGE_", e4.getMessage());
    Debug.logError("[UploadDatevCSV] " + e4.getMessage(), module);
    return "error";
}
//if (Debug.infoOn()) Debug.logInfo("[UploadContentAndImage]lst " + lst, module);

if (lst.size() == 0) {
    String errMsg = UtilProperties.getMessage(UploadContentAndImage.err_resource, "uploadContentAndImage.no_files_uploaded", locale);
    request.setAttribute("_ERROR_MESSAGE_", errMsg);
    Debug.logWarning("[UploadDatevCSV] No files uploaded", module);
    return "error";
}

//Map<String, Object> passedParams = FastMap.newInstance();
Map<String, Object> multiPartMap = FastMap.newInstance();
FileItem csvFileItem = null;
byte[] csvFileBytes = [];
for (int i = 0; i < lst.size(); i++) {
    csvFileItem = lst.get(i);
    String fieldName = csvFileItem.getFieldName();
    if (csvFileItem.isFormField()) {
        String fieldStr = csvFileItem.getString();
        multiPartMap.put(fieldName, fieldStr);
    } else {      
        String fileName = csvFileItem.getName();
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
        ByteBuffer bf = ByteBuffer.wrap(csvFileItem.get());        
        BufferedReader br = new BufferedReader(new InputStreamReader(csvFileItem.getInputStream()));        
//        String temp = null;
//        while((temp = br.readLine()) != null){            
//            Debug.log("csv line =====> " + temp);
//        }
//        br.close();
        multiPartMap.put("uploadedFile", bf);
        multiPartMap.put("_" + fieldName + "_size", Long.valueOf(csvFileItem.getSize()));
        multiPartMap.put("_" + fieldName + "_fileName", fileName);
        multiPartMap.put("_" + fieldName + "_contentType", csvFileItem.getContentType());
    }
}

context.multiPartMap=multiPartMap;