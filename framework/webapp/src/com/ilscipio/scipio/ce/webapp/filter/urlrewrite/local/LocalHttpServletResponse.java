package com.ilscipio.scipio.ce.webapp.filter.urlrewrite.local;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;

@SuppressWarnings("deprecation")
public class LocalHttpServletResponse implements HttpServletResponse {
    
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected final LocalServletContainer container;
    
    protected String charset;
    protected String contentType;

    protected Locale locale;
    protected int status = 200;
    
    protected Map<String, List<String>> headers = new HashMap<>();
    
    public LocalHttpServletResponse(LocalServletContainer container, String charset, String contentType,
            Locale locale) {
        this.container = container;
        this.charset = charset;
        this.contentType = contentType;
        this.locale = locale;
    }

    @Override
    public String getCharacterEncoding() {
        return charset;
    }

    @Override
    public String getContentType() {
        return contentType;
    }

    @Override
    public ServletOutputStream getOutputStream() throws IOException {
        throw new UnsupportedOperationException();
    }

    @Override
    public PrintWriter getWriter() throws IOException {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setCharacterEncoding(String charset) {
        this.charset = charset;
    }

    @Override
    public void setContentLength(int len) {
        Debug.logWarning("setContentLength called - unsupported - doing nothing", module);
    }

    @Override
    public void setContentLengthLong(long len) {
        Debug.logWarning("setContentLengthLong called - unsupported - doing nothing", module);
    }

    @Override
    public void setContentType(String type) {
        this.contentType = type;
    }

    @Override
    public void setBufferSize(int size) {
        Debug.logWarning("setBufferSize called - unsupported - doing nothing", module);
    }

    @Override
    public int getBufferSize() {
        Debug.logWarning("getBufferSize called - unsupported - returning 0", module);
        return 0;
    }

    @Override
    public void flushBuffer() throws IOException {
        Debug.logWarning("flushBuffer called - unsupported - doing nothing", module);
    }

    @Override
    public void resetBuffer() {
        Debug.logWarning("resetBuffer called - unsupported - doing nothing", module);
    }

    @Override
    public boolean isCommitted() {
        return false; // false will help this run in more cases...
    }

    @Override
    public void reset() {
        Debug.logWarning("reset called - unsupported - doing nothing", module);
    }

    @Override
    public void setLocale(Locale loc) {
        this.locale = loc;
    }

    @Override
    public Locale getLocale() {
        return locale;
    }

    @Override
    public void addCookie(Cookie cookie) {
        Debug.logWarning("addCookie called - unsupported - doing nothing", module);
    }

    @Override
    public boolean containsHeader(String name) {
        return (headers.get(name) != null && headers.get(name).size() > 0);
    }

    @Override
    public String encodeURL(String url) {
        return url;
    }

    @Override
    public String encodeRedirectURL(String url) {
        return url;
    }

    @Override
    public String encodeUrl(String url) {
        return url;
    }

    @Override
    public String encodeRedirectUrl(String url) {
        return url;
    }

    @Override
    public void sendError(int sc, String msg) throws IOException {
        Debug.logWarning("sendError called - unsupported - doing nothing", module);
    }

    @Override
    public void sendError(int sc) throws IOException {
        Debug.logWarning("sendError called - unsupported - doing nothing", module);
    }

    @Override
    public void sendRedirect(String location) throws IOException {
        Debug.logWarning("sendRedirect called - unsupported - doing nothing", module);
    }

    @Override
    public void setDateHeader(String name, long date) {
        setHeader(name, Long.toString(date));
    }

    @Override
    public void addDateHeader(String name, long date) {
        addHeader(name, Long.toString(date));
    }

    @Override
    public void setHeader(String name, String value) {
        headers.put(name, Arrays.asList(new String[] {value}));
    }

    @Override
    public void addHeader(String name, String value) {
        if (headers.get(name) != null) headers.get(name).add(value);
        else headers.put(name, Arrays.asList(new String[] {value}));
    }

    @Override
    public void setIntHeader(String name, int value) {
        setHeader(name, Integer.toString(value));
    }

    @Override
    public void addIntHeader(String name, int value) {
        addHeader(name, Integer.toString(value));
    }

    @Override
    public void setStatus(int sc) {
        this.status = sc;
    }

    @Override
    public void setStatus(int sc, String sm) {
       this.status = sc;
    }

    @Override
    public int getStatus() {
        return status;
    }

    @Override
    public String getHeader(String name) {
        if (headers.get(name).size() == 0) return null;
        return getHeaders(name).iterator().next();
    }

    @Override
    public Collection<String> getHeaders(String name) {
        if (headers.get(name) == null) Collections.emptyList();
        return Collections.unmodifiableList(headers.get(name));
    }

    @Override
    public Collection<String> getHeaderNames() {
        return Collections.unmodifiableSet(headers.keySet());
    }

}
