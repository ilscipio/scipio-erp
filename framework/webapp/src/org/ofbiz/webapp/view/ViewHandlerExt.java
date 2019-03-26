package org.ofbiz.webapp.view;

import java.io.Writer;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * SCIPIO: Extended view handler, with support for output to specific writer.
 * Added 2017-05-01.
 * <p>
 * DEV NOTE: This is required to avoid major compatibility breakage with custom renderers,
 * and some existing renderers are unable to implement custom writer.
 */
public interface ViewHandlerExt extends ViewHandler {

    /**
     * SCIPIO: Render the page to the specified writer.
     * Added 2017-05-01.
     *
     * @param name The name of the view.
     * @param page The source of the view; could be a page, url, etc depending on the type of handler.
     * @param info An info string attached to this view
     * @param request The HttpServletRequest object used when requesting this page.
     * @param response The HttpServletResponse object to be used to present the page.
     * @param writer the writer to print to (must NOT use response object)
     * @throws ViewHandlerException
     */
    public void render(String name, String page, String info, String contentType, String encoding, HttpServletRequest request, HttpServletResponse response, Writer writer) throws ViewHandlerException;
}
