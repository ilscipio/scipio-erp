package com.ilscipio.scipio.cms.control;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;

import javax.servlet.ServletContext;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.webapp.view.AbstractViewHandler;
import org.ofbiz.webapp.view.ViewHandlerException;
import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.content.CmsPageContext;

public class CmsViewHandler extends AbstractViewHandler {
    protected ServletContext servletContext = null;
    public static final String module = CmsViewHandler.class.getName();

    @Override
    public void init(ServletContext context) throws ViewHandlerException {
        this.servletContext = context;
    }

    @Override
    public void render(String name, String pagePath, String info,
            String contentType, String encoding, HttpServletRequest request,
            HttpServletResponse response) throws ViewHandlerException {

        Writer writer = null;
        try {
            if (this.servletContext != null) {
            	writer = response.getWriter();
            }else{
                ServletOutputStream ros = response.getOutputStream();
                writer = new OutputStreamWriter(ros, "UTF-8");
            }

            CmsPage page = (CmsPage) request.getAttribute("cmsPage");
            CmsPageContext cmsPageContext = (CmsPageContext)  request.getAttribute("cmsPageContext");
            String content = page.getFormattedContent(cmsPageContext);
            writer.write(content);
            writer.flush();

        } catch (IOException e) {
            throw new ViewHandlerException(
                    "Error in the response writer/output stream: "
                            + e.toString(), e);
        } catch (CmsException cmse) {
            throw new ViewHandlerException("Error in the CMS page rendering: "
                    + cmse.toString(), cmse);
        }
    }

}
