package com.ilscipio.scipio.common.qrcode;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

import javax.imageio.ImageIO;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;

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
        Boolean hasLogo = request.getParameter("hasLogo") != null ? Boolean.valueOf(request.getParameter("hasLogo")) : true;
        
        // FIXME: security risk
        //String logo = request.getParameter("logo") != null ? request.getParameter("logo") : QR_LOGO_PATH;
        String logo = QR_LOGO_PATH;
        
        Boolean isVCard = request.getParameter("isVCard") != null ? Boolean.valueOf(request.getParameter("isVCard")) : false; // currently not implemented
        String urlParam = request.getParameter("url");
        int width = request.getParameter("width") != null ? Integer.valueOf(request.getParameter("width")) : 250;
        int height = request.getParameter("height") != null ? Integer.valueOf(request.getParameter("height")) : 250;
        String text = null;
        if(UtilValidate.isNotEmpty(urlParam)){
            try {
                text = URLDecoder.decode(urlParam,"UTF-8");
            } catch (UnsupportedEncodingException e) {
                Debug.logError(e, "Cannot decode urlParam", module);
                request.setAttribute("_ERROR_MESSAGE_", "Missing or invalid text data for qrcode");
                return "error";
            }
        } else {
            request.setAttribute("_ERROR_MESSAGE_", "Missing or invalid text data for qrcode");
            return "error";
        }
        if(isVCard){
            // Validate if this is a VCard
        }

        // Print response
        response.setContentType("image/jpeg");
        QRCode qrCode = QRCode.from(text).withSize(width, height).to(ImageType.PNG);
        //qrCode.withColor(0x3d4a5d, 0xffffff);
        qrCode.withHint(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.H);
        File f = qrCode.file();
        if(hasLogo){
            f = QRCodeUtil.drawLogo(qrCode,logo);
        }
        try {
            BufferedImage bi = ImageIO.read(f);
            try (OutputStream out = response.getOutputStream()) {
                ImageIO.write(bi, "png", out);
            }
        } catch (IOException e) {
            Debug.logError(e, module);
        }

        return "success";
    }
    
}
