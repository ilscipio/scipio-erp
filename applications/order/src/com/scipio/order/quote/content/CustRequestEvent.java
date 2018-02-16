package com.scipio.order.quote.content;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.nio.ByteBuffer;
import java.sql.Timestamp;
import java.util.Locale;
import java.util.TimeZone;

import javax.imageio.ImageIO;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.conversion.ConversionException;
import org.ofbiz.base.conversion.NumberConverters.StringToInteger;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.PropertyMessage;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;

import com.ilscipio.scipio.common.util.fileType.FileTypeException;
import com.ilscipio.scipio.common.util.fileType.FileTypeResolver;
import com.ilscipio.scipio.common.util.fileType.FileTypeUtil;

public class CustRequestEvent {

	private static final String module = CustRequestEvent.class.getName();

	public static String createCustRequestContent(HttpServletRequest request, HttpServletResponse response) {
		Delegator delegator = (Delegator) request.getAttribute("delegator");
		LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
		String result = "success";
		// GenericValue userLogin = (GenericValue) context.get("userLogin");
		Locale locale = (Locale) request.getAttribute("locale");
		TimeZone timeZone = (TimeZone) request.getAttribute("timeZone");

		ByteBuffer byteBuffer = (ByteBuffer) request.getAttribute("uploadedFile");
		String dataResourceTypeId = (String) request.getAttribute("dataResourceTypeId");
		String contentName = (String) request.getAttribute("contentName");

		String fileSize = (String) request.getAttribute("_uploadedFile_size");
		String fileName = (String) request.getAttribute("_uploadedFile_fileName");
		String contentType = (String) request.getAttribute("_uploadedFile_contentType");

		// USE SAME CREATED DATE FOR EVERYTHING RELATED
		Timestamp createdDate = UtilDateTime.nowTimestamp();

		StringToInteger std = new StringToInteger();
		Integer fileSizeConverted = 0;
		try {
			fileSizeConverted = std.convert(fileSize, locale, timeZone);
		} catch (ConversionException e) {
			Debug.logWarning("Can't store file size: " + e.getMessage(), module);
		}
		if (fileSizeConverted != byteBuffer.limit()) {
			Debug.logWarning("request header file size ===> " + fileSizeConverted
					+ " differs from received byte array size ==> " + byteBuffer.limit() + ". Byte array size prevails",
					module);
			fileSizeConverted = byteBuffer.limit();
		}

		try {
			GenericValue mimeType = null;
			if (UtilValidate.isNotEmpty(contentType)) {
				mimeType = delegator.findOne("MimeType", true, UtilMisc.toMap("mimeTypeId", contentType));
			}

			GenericValue dataResourceType = delegator.findOne("DataResourceType", true,
					UtilMisc.toMap("dataResourceTypeId", dataResourceTypeId));
			if (UtilValidate.isNotEmpty(dataResourceType) && dataResourceType.getBoolean("hasTable")) {
				FileTypeResolver fileTypeResolver = FileTypeResolver.getInstance(delegator, dataResourceTypeId);
				if (fileTypeResolver != null) {
					mimeType = fileTypeResolver.findMimeType(byteBuffer, fileName);
					if (mimeType != null) {
						GenericValue mediaDataResource;
						if (dataResourceTypeId.equals(FileTypeResolver.IMAGE_TYPE)) {
							mediaDataResource = delegator.makeValue("ImageDataResource");
							mediaDataResource.put("imageData", byteBuffer.array());
						} else if (dataResourceTypeId.equals(FileTypeResolver.AUDIO_TYPE)) {
							mediaDataResource = delegator.makeValue("AudioDataResource");
							mediaDataResource.put("audioData", byteBuffer.array());
						} else if (dataResourceTypeId.equals(FileTypeResolver.VIDEO_TYPE)) {
							mediaDataResource = delegator.makeValue("VideoDataResource");
							mediaDataResource.put("videoData", byteBuffer.array());
						} else if (dataResourceTypeId.equals(FileTypeResolver.DOCUMENT_TYPE)) {
							mediaDataResource = delegator.makeValue("DocumentDataResource");
							mediaDataResource.put("documentData", byteBuffer.array());
						} else {
							// TODO: REVIEW: I'm not sure we should cover this
							// case (2017-07-31: at least log it)
							Debug.logInfo("Could not determine media category for dataResourceTypeId '"
									+ dataResourceTypeId + "' and mimeTypeId '" + mimeType.getString("mimeTypeId")
									+ "'; storing as OtherDataResource", module);
							mediaDataResource = delegator.makeValue("OtherDataResource");
							mediaDataResource.put("dataResourceContent", byteBuffer.array());
						}

						GenericValue dataResource = delegator.makeValue("DataResource");
						dataResource.put("dataResourceTypeId", dataResourceTypeId);
						dataResource.put("dataResourceName", contentName); 
						dataResource.put("statusId", "CTNT_IN_PROGRESS");
						dataResource.put("mimeTypeId", mimeType.getString("mimeTypeId"));
						dataResource.put("isPublic", "N");
						dataResource.put("objectInfo", fileName);
						dataResource.put("createdDate", createdDate);
						if (dataResourceTypeId.equals(FileTypeResolver.IMAGE_TYPE)) {
							try {
								BufferedImage bugImg = ImageIO.read(new ByteArrayInputStream(byteBuffer.array()));
								dataResource.put("scpWidth", (long) bugImg.getWidth());
								dataResource.put("scpHeight", (long) bugImg.getHeight());
							} catch (Exception e) {
								Debug.logError(e, "Error uploading media file: Could not read/parse image file: "
										+ e.getMessage(), module);
								// return
								// ServiceUtil.returnError(UtilProperties.getMessage("ProductErrorUiLabels",
								// "ScaleImage.unable_to_parse", locale) + ": "
								// + e.getMessage());
								return "error";
							}
						}
						dataResource = delegator.createSetNextSeqId(dataResource);
						String dataResourceId = dataResource.getString("dataResourceId");
						// result.put("dataResourceId", dataResourceId);
						// result.put("dataResourceTypeId", dataResourceTypeId);

						GenericValue fileSizeDataResourceAttr = delegator.makeValue("DataResourceAttribute");
						fileSizeDataResourceAttr.put("dataResourceId", dataResource.get("dataResourceId"));
						fileSizeDataResourceAttr.put("attrName", FileTypeUtil.FILE_SIZE_ATTRIBUTE_NAME);
						fileSizeDataResourceAttr.put("attrValue", String.valueOf(fileSizeConverted));
						fileSizeDataResourceAttr.create();

						mediaDataResource.put("dataResourceId", dataResourceId);
						mediaDataResource.create();

						GenericValue content = delegator.makeValue("Content");
						content.put("contentTypeId", "SCP_MEDIA");
						content.put("contentName", contentName);
						content.put("dataResourceId", dataResourceId);
						content.put("createdDate", createdDate);
						content = delegator.createSetNextSeqId(content);
						String contentId = content.getString("contentId");
						// result.put("contentId", contentId);

						if (dataResourceTypeId.equals(FileTypeResolver.IMAGE_TYPE)) {
							// try {
							// Map<String, Object> resizeCtx =
							// dctx.makeValidContext("cmsRebuildMediaVariants",
							// ModelService.IN_PARAM, context);
							// resizeCtx.put("contentIdList",
							// UtilMisc.<String>toList(contentId));
							// resizeCtx.put("force", Boolean.TRUE);
							// resizeCtx.put("createdDate", createdDate);
							// Map<String, Object> resizeResult =
							// dispatcher.runSync("cmsRebuildMediaVariants",
							// resizeCtx);
							// if (!ServiceUtil.isSuccess(resizeResult)) {
							// return ServiceUtil.returnError("Error creating
							// resized images: " +
							// ServiceUtil.getErrorMessage(resizeResult));
							// }
							// } catch (GenericServiceException e) {
							// FormattedError err = errorFmt.format(e, "Error
							// creating resized images", null, context);
							// Debug.logError(err.getEx(), err.getLogMsg(),
							// module);
							// return err.returnError();
							// }
						}

					} else {
						throw new FileTypeException(PropertyMessage.make("CMSErrorUiLabels", "CmsUnsupportedFileType"));
					}
				} else {
					throw new FileTypeException(PropertyMessage.make("CMSErrorUiLabels", "CmsUnsupportedFileType"));
				}
			} else {
				// TODO: Handle this case or throw an error. In fact as
				// it is currently implemented all media (dataResources) handled
				// in here must have an associated entity
				throw new FileTypeException(PropertyMessage.make("CMSErrorUiLabels", "CmsUnsupportedFileType"));
			}
		} catch (Exception e) {
			// FormattedError err = errorFmt.format(e, "Error getting media
			// files", null, context);
			// if (!(e instanceof FileTypeException)) { // don't log, common
			// user input error
			// Debug.logError(err.getEx(), err.getLogMsg(), module);
			// }
			return "error";
		}
		return result;
	}

}
