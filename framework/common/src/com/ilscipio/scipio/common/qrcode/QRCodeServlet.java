package com.ilscipio.scipio.common.qrcode;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URLDecoder;

import javax.imageio.ImageIO;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.UtilValidate;

import com.google.zxing.EncodeHintType;
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel;

import net.glxn.qrgen.core.image.ImageType;
import net.glxn.qrgen.javase.QRCode;

/**
 * SCIPIO qrcode servlet - NOT associated with stock qrcode at current time
 * (org.ofbiz.common.qrcode).
 */
public class QRCodeServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;
	private static final String QR_LOGO_PATH = "component://base-theme/webapp/base/images/scipio-logo.png";

	
	/** 
	 * QRCode Servlet
	 * Renders QRCode based on GET-Parameters; uses zxing & qrgen
	 * 
	 * @param text	String or url-encoded URL
	 * @param hasLogo Renders an overlay on top of the QRCode
     * @param logo location of logo image in component:// notation (defaults to base scipio erp logo)
	 * @param width QRCode width
	 * @param height QRCode height
	 * @param isVCard Currently not supported; renders vcard instead of simple string
	 *  
	 * */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
		Boolean hasLogo = request.getParameter("hasLogo") != null ? Boolean.valueOf(request.getParameter("hasLogo")) : true;
		String logo = request.getParameter("logo") != null ? request.getParameter("logo") : QR_LOGO_PATH;
		Boolean isVCard = request.getParameter("isVCard") != null ? Boolean.valueOf(request.getParameter("isVCard")) : false; // currently not implemented
		String urlParam = request.getParameter("url");
		int width = request.getParameter("width") != null ? Integer.valueOf(request.getParameter("width")) : 250;
		int height = request.getParameter("height") != null ? Integer.valueOf(request.getParameter("height")) : 250;
		String text = null;
		if(UtilValidate.isNotEmpty(urlParam)){
			text = URLDecoder.decode(urlParam,"UTF-8");		
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
		BufferedImage bi = ImageIO.read(f);
		OutputStream out = response.getOutputStream();
		ImageIO.write(bi, "png", out);
		out.close();
	}

}