import java.nio.ByteBuffer

import javolution.util.FastList

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilProperties
import org.ofbiz.base.util.string.FlexibleStringExpander
import org.ofbiz.entity.GenericEntityException
import org.ofbiz.entity.GenericValue
import org.ofbiz.entity.util.EntityQuery
import org.ofbiz.entity.util.EntityUtil
import org.ofbiz.entity.util.EntityUtilProperties
import org.ofbiz.service.ServiceUtil

String module = "ProductCategoryContentServicesGroovy";

// UPLOADING STUFF
Map<String, Object> uploadProductCategoryImage() {
    Map result = ServiceUtil.returnSuccess();
    forLock = new Object();
    contentType = null;
    fileType = context.upload_file_type;
    ByteBuffer imageData = (ByteBuffer) context.get("uploadedFile");
    String clientFileName = context.get("_uploadedFile_fileName")
    if (clientFileName && fileType) {
        productCategoryId = context.productCategoryId;
        imageFilenameFormat = EntityUtilProperties.getPropertyValue("catalog", "image.filename.format", delegator);
        imageServerPath = FlexibleStringExpander.expandString(EntityUtilProperties.getPropertyValue("catalog", "image.server.path", delegator), context);
        imageUrlPrefix = FlexibleStringExpander.expandString(EntityUtilProperties.getPropertyValue("catalog", "image.url.prefix",delegator), context);
        imageServerPath = imageServerPath.endsWith("/") ? imageServerPath.substring(0, imageServerPath.length()-1) : imageServerPath;
        imageUrlPrefix = imageUrlPrefix.endsWith("/") ? imageUrlPrefix.substring(0, imageUrlPrefix.length()-1) : imageUrlPrefix;
        filenameExpander = FlexibleStringExpander.getInstance(imageFilenameFormat);
        String fileLocation = filenameExpander.expandString([location : "categories", type : fileType, id : context.productCategoryId]);
        String filePathPrefix = "";
        String filenameToUse = fileLocation;
        if (fileLocation.lastIndexOf("/") != -1) {
            filePathPrefix = fileLocation.substring(0, fileLocation.lastIndexOf("/") + 1); // adding 1 to include the trailing slash
            filenameToUse = fileLocation.substring(fileLocation.lastIndexOf("/") + 1);
        }
        int i1;
        if (contentType && (i1 = contentType.indexOf("boundary=")) != -1) {
            contentType = contentType.substring(i1 + 9);
            contentType = "--" + contentType;
        }

        // Get the file extension
        List<GenericValue> fileExtension = FastList.newInstance();
        try {
            fileExtension = EntityQuery.use(delegator).from("FileExtension").where("mimeTypeId", (String) context.get("_uploadedFile_contentType")).queryList();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue extension = EntityUtil.getFirst(fileExtension);
        if (extension != null) {
            filenameToUse += "." + extension.getString("fileExtensionId");
        }

        // Create the new directory
        defaultFileName = filenameToUse + "_temp";
        String targetDirectory = imageServerPath + "/" + filePathPrefix;
        try {
            File targetDir = new File(targetDirectory);
            if (!targetDir.exists()) {
                boolean created = targetDir.mkdirs();
                if (!created) {
                    String errMsg = UtilProperties.getMessage(resource, "ScaleImage.unable_to_create_target_directory", context.locale) + " - " + targetDirectory;
                    Debug.logFatal(errMsg, module);
                    return ServiceUtil.returnError(errMsg);
                }
            }
        } catch (NullPointerException e) {
            Debug.logError(e,module);
        }
        // Write the file
        File file;
        try {
            imagePath = imageServerPath + "/" + fileLocation + "." + extension.getString("fileExtensionId");
            file = new File(imagePath);
            RandomAccessFile out = new RandomAccessFile(file, "rw");
            out.write(imageData.array());
            out.close();
          
            // Store the corresponding ProductCategory entity
            if (clientFileName.lastIndexOf(".") > 0 && clientFileName.lastIndexOf(".") < clientFileName.length()) {
                filenameToUse += clientFileName.substring(clientFileName.lastIndexOf("."));
            } else {
                filenameToUse += ".jpg";
            }
            imageUrl = imageUrlPrefix + "/" + fileLocation + "." + extension.getString("fileExtensionId");
            productCategory = delegator.findOne("ProductCategory", ["productCategoryId" : productCategoryId], false);
            productCategory.set(fileType + "ImageUrl", imageUrl);
            productCategory.store();
        } catch (Exception e) {
            if (file && file.exists())
                file.delete();
            Debug.logError(e,module);
            return ServiceUtil.returnError(e.getMessage(), locale);
        }
    }
    return result;
}