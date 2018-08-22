package com.ilscipio.scipio.common.qrcode;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

import javax.imageio.ImageIO;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.qrcode.QRCodeServices;

import com.google.zxing.EncodeHintType;
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel;

import net.glxn.qrgen.core.image.ImageType;
import net.glxn.qrgen.javase.QRCode;

/**
 * Scipio-specific QRCode events.
 */
public abstract class QRCodeEvents {
    
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final String QR_LOGO_PATH = "component://base-theme/webapp/base/images/scipio-logo.png";
    
    protected QRCodeEvents() {
    }

    /**
     * QRCode serve event to match <code>@qrcode</code> scipio Freemarker macro.
     */
    public static String serveQRCodeImageForDirective(HttpServletRequest request, HttpServletResponse response) {
        String logoArg = request.getParameter("logo");

        String logo = QR_LOGO_PATH;
        Boolean useLogo = null; // NOTE: stock ofbiz generateQRCodeImage service default is true
        if ("true".equals(logoArg)) {
            useLogo = true;
        } else if ("false".equals(logoArg)) {
            useLogo = false;
        } else if (UtilValidate.isNotEmpty(logoArg)) {
            useLogo = true;
            // FIXME: security risk, cannot be specified over request - needs strict mapping system
            //logo = logoArg;
        }
        
        Boolean isVCard = UtilMisc.booleanValue(request.getParameter("isVCard"), false); // currently not implemented
        String message = request.getParameter("message");
        if(UtilValidate.isNotEmpty(message)){
            /* already done by tomcat
            try {
                message = URLDecoder.decode(message,"UTF-8");
            } catch (UnsupportedEncodingException e) {
                Debug.logError(e, "Cannot decode urlParam", module);
                request.setAttribute("_ERROR_MESSAGE_", "Missing or invalid text data for qrcode");
                return "error";
            }
            */
        } else {
            request.setAttribute("_ERROR_MESSAGE_", "Missing or invalid text data for qrcode");
            return "error";
        }
        if (isVCard){
            // Validate if this is a VCard
        }

        String ecLevel = request.getParameter("ecLevel");
        
        // Print response
        String format = request.getParameter("format");
        if (UtilValidate.isEmpty(format)) {
            format = QRCodeServices.QRCODE_DEFAULT_FORMAT;
        }
        String mimeType = "image/" + format;
        response.setContentType(mimeType);
        try {
            int width = UtilValidate.isNotEmpty(request.getParameter("width")) ? 
                    Integer.valueOf(request.getParameter("width")) : QRCodeServices.QRCODE_DEFAULT_WIDTH_INT;
            int height = UtilValidate.isNotEmpty(request.getParameter("height")) ? 
                    Integer.valueOf(request.getParameter("height")) : QRCodeServices.QRCODE_DEFAULT_HEIGHT_INT;

            QRCode qrCode = QRCode.from(message).withSize(width, height).to(ImageType.PNG);
            if (UtilValidate.isNotEmpty(ecLevel)) {
                qrCode.withHint(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.valueOf(ecLevel));
            } else if (QRCodeServices.QRCODE_DEFAULT_ECLEVEL != null) {
                qrCode.withHint(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.valueOf(QRCodeServices.QRCODE_DEFAULT_ECLEVEL));
            }
            File f = qrCode.file();
            if (!Boolean.FALSE.equals(useLogo)) {
                f = QRCodeUtil.drawLogo(ImageIO.read(qrCode.file()), format, logo);
            }
            BufferedImage bi = ImageIO.read(f);
            OutputStream out = response.getOutputStream();
            ImageIO.write(bi, format, out);
            out.flush();
        } catch (Exception e) {
            Debug.logError(e, module);
            request.setAttribute("_ERROR_MESSAGE_", "Could not generate qrcode");
            return "error";
        }
        return "success";
    }
    
}
