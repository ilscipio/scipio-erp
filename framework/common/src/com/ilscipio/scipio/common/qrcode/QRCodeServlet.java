package com.ilscipio.scipio.common.qrcode;

import java.io.IOException;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * SCIPIO qrcode servlet - NOT associated with stock qrcode at current time
 * (org.ofbiz.common.qrcode).
 */
@Deprecated
public class QRCodeServlet extends HttpServlet {

    private static final long serialVersionUID = 1L;

    /**
    * QRCode Servlet
    * Renders QRCode based on GET-Parameters; uses zxing & qrgen
    *
    * @param text    String or url-encoded URL
    * @param hasLogo Renders an overlay on top of the QRCode
     * @param logo location of logo image in component:// notation (defaults to base scipio erp logo)
    * @param width QRCode width
    * @param height QRCode height
    * @param isVCard Currently not supported; renders vcard instead of simple string
    *
    * */
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        QRCodeEvents.serveQRCodeImageForDirective(request, response);
    }

}
