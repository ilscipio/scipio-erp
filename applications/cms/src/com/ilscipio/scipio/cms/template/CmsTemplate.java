package com.ilscipio.scipio.cms.template;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.ParameterizedType;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import javolution.util.FastList;
import javolution.util.FastMap;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.GroovyUtil;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilFormatOut;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.collections.ResourceBundleMapWrapper;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.widget.renderer.ScreenRenderer;
import org.ofbiz.widget.renderer.ScreenStringRenderer;
import org.ofbiz.widget.renderer.macro.MacroScreenRenderer;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.content.CmsPageContent;
import com.ilscipio.scipio.cms.content.CmsPageContext;
import com.ilscipio.scipio.cms.data.CmsDataObject;
import com.ilscipio.scipio.cms.util.CmsRecorderMap;

import freemarker.cache.TemplateLoader;
import freemarker.core.Environment;
import freemarker.ext.beans.BeansWrapper;
import freemarker.ext.servlet.HttpRequestHashModel;
import freemarker.ext.servlet.HttpSessionHashModel;
import freemarker.ext.servlet.ServletContextHashModel;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;
import freemarker.template.utility.DeepUnwrap;

public abstract class CmsTemplate extends CmsDataObject {

    private static final long serialVersionUID = 6906449955621642269L;
    public static final String module = CmsTemplate.class.getName();

    protected static Configuration fmConfig;
    protected static TemplateLoader fmTemplateLoader;
    /*protected static GroovyScriptEngine scriptEngine;*/

    protected List<CmsAttributeTemplate> attributeTemplates;
    protected Template fmTemplate;
    protected static ScreenStringRenderer screenStringRenderer;

    public CmsTemplate(GenericValue entity) {
        super(entity);
        try {
            screenStringRenderer= new MacroScreenRenderer(UtilProperties.getPropertyValue("widget", "screen.name"), UtilProperties.getPropertyValue("widget", "screen.screenrenderer"));
        } catch (TemplateException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public CmsTemplate(Map<String, ?> fields) {
        super(fields);
        try {
            screenStringRenderer= new MacroScreenRenderer(UtilProperties.getPropertyValue("widget", "screen.name"), UtilProperties.getPropertyValue("widget", "screen.screenrenderer"));
        } catch (TemplateException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public CmsTemplate(String primaryKey) {
        super(primaryKey);
        try {
            screenStringRenderer= new MacroScreenRenderer(UtilProperties.getPropertyValue("widget", "screen.name"), UtilProperties.getPropertyValue("widget", "screen.screenrenderer"));
        } catch (TemplateException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * Returns the template name.
     * 
     * @return name
     */
    public String getName() {
        return entity.getString("templateName");
    }

    /**
     * Sets the template name.
     * 
     */
    public void setName(String name) {
        entity.setString("templateName", name);
    }

    /**
     * Returns the template body.
     * 
     * @return freemarker template
     */
    public String getBody() {
        return entity.getString("templateBody");
    }

    /**
     * Sets the template body.
     * 
     * @return freemarker template
     */
    public void setBody(String body) {
        entity.setString("templateBody", body);
    }
    
    
    /**
     * Sets the template to inactive
     * 
     */
    public void setInactive(Boolean inactive) {
        entity.set("inactive", inactive);
    }
    
    
    /**
     * @return Boolean activity value
     */
    public Boolean getInactive() {
        return entity.getBoolean("inactive");
    }
    
    
    /**
     * Sets the user that added the template
     * 
     */
    public void setCreatedBy(String createdBy) {
        entity.setString("createdBy", createdBy);
    }
    
    
    /**
     * @return String createdBy value
     */
    public String getCreatedBy() {
        return entity.getString("createdBy");
    }

    /**
     * Sets id of website to which the template belongs.
     * 
     * @param website
     *            id
     */
    public void setWebSiteId(String webSiteId) {
        entity.setString("webSiteId", webSiteId);
    }

    /**
     * Returns id of website to which the template belongs.
     * 
     * @return website id
     */
    public String getWebSiteId() {
        return entity.getString("webSiteId");
    }

    /**
     * Returns the attribute templates of a page.
     * 
     * @return attribute templates
     */
    public List<CmsAttributeTemplate> getAttributeTemplates() {
        if (attributeTemplates == null) {
            attributeTemplates = new FastList<CmsAttributeTemplate>();
            try {
                List<GenericValue> ate = entity.getRelated(CmsAttributeTemplate.class.getSimpleName());
                for (GenericValue at : ate) {
                    attributeTemplates.add(new CmsAttributeTemplate(at));
                }
            } catch (GenericEntityException e) {
                throw new CmsException("Attribute templates could not be retrieved. Template: " + getName(), e, module);
            }
        }
        return attributeTemplates;
    }

    /**
     * Adds a new attribute template to the page template.
     * 
     * @param template
     */
    public void addAttributeTemplate(CmsAttributeTemplate template) {
        try {
            List<CmsAttributeTemplate> templates = getAttributeTemplates();
            template.setTemplate(this);
            template.store();
            templates.add(template);
        } catch (GenericEntityException e) {
            throw new CmsException("Could not add attribute template to page template. Template Name: " + getName(), e, module);
        }
    }

    /**
     * Merges the content with the template with consideration to the given
     * context information.
     * 
     * @param content
     * @param context
     * @return the rendered content
     */
    public String process(CmsPageContent content, CmsPageContext context) {
        //I think that both the directives can be cached
        content = setDirectives(content, context);
        StringWriter out = new StringWriter();
        String processorLocation = UtilProperties.getPropertyValue("cms.properties", "contentprocessor.script.location", "");
        try {
            //GroovyShell shell = new GroovyShell(processorContext);
            HttpServletRequest request = context.getRequest();
            HttpServletResponse response = context.getResponse();
            HttpSession session = request.getSession();
            Template template = getFreeMarkerTemplate();
            //The following was taken straight from ScreenRenderer.populateBasicContext
            MapStack mContext = MapStack.create();
            Set<String> sysVarNames;
            
            {
                // Use this while setting sys vars to auto-record added names
            	CmsRecorderMap<Object> mContextSysKeyRecorderMap = new CmsRecorderMap<Object>(mContext);
                Map<String, Object> mContextSys = mContextSysKeyRecorderMap;
                
                //GroovyShell shell = new GroovyShell(processorContext);
                mContextSys.put("pagecontext",context);
                
                // mContext.putAll(content); // Do NOT do this here - use mapstack levels to set priorities instead
                mContextSys.put("request", request);
                mContextSys.put("response", response);
                mContextSys.put("session", session);
                mContextSys.put("dispatcher", request.getAttribute("dispatcher"));
                mContextSys.put("delegator", request.getAttribute("delegator"));
                mContextSys.put("security", request.getAttribute("security"));
                mContextSys.put("locale", UtilHttp.getLocale(request)); 
                mContextSys.put("timeZone", UtilHttp.getTimeZone(request));
                mContextSys.put("userLogin", session.getAttribute("userLogin"));
                Set<String> attrNamesToSkip = UtilMisc.toSet("delegator", "dispatcher", "authz", "security", "webSiteId");
                Map<String, Object> parameterMap = UtilHttp.getCombinedMap(request, attrNamesToSkip);
                mContextSys.put("parameters", parameterMap);
                
    
                
                mContextSys.put("screens", new ScreenRenderer(out, mContext, screenStringRenderer));
                
                // make a reference for high level variables, a global context
                mContextSys.put("globalContext", mContext);
    
                // make sure the "nullField" object is in there for entity ops; note this is nullField and not null because as null causes problems in FreeMarker and such...
                mContextSys.put("nullField", GenericEntity.NULL_FIELD);
                mContextSys.put("StringUtil", StringUtil.INSTANCE);
    
                // these ones are FreeMarker specific and will only work in FTL templates, mainly here for backward compatibility
                BeansWrapper wrapper = BeansWrapper.getDefaultInstance();
                mContextSys.put("sessionAttributes", new HttpSessionHashModel(session, wrapper));
                mContextSys.put("requestAttributes", new HttpRequestHashModel(request, wrapper));
                mContextSys.put("requestParameters",  UtilHttp.getParameterMap(request));
                
                ServletContextHashModel ftlServletContext = (ServletContextHashModel) request.getAttribute("ftlServletContext");
                mContextSys.put("Application", ftlServletContext);
                mContextSys.put("Request", mContext.get("requestAttributes"));
               
                // setup message lists
                List<String> eventMessageList = UtilGenerics.toList(request.getAttribute("eventMessageList"));
                if (eventMessageList == null) eventMessageList = FastList.newInstance();
                List<String> errorMessageList = UtilGenerics.toList(request.getAttribute("errorMessageList"));
                if (errorMessageList == null) errorMessageList = FastList.newInstance();
    
                if (request.getAttribute("_EVENT_MESSAGE_") != null) {
                    eventMessageList.add(UtilFormatOut.replaceString((String) request.getAttribute("_EVENT_MESSAGE_"), "\n", "<br/>"));
                    request.removeAttribute("_EVENT_MESSAGE_");
                }
                List<String> msgList = UtilGenerics.toList(request.getAttribute("_EVENT_MESSAGE_LIST_"));
                if (msgList != null) {
                    eventMessageList.addAll(msgList);
                    request.removeAttribute("_EVENT_MESSAGE_LIST_");
                }
                if (request.getAttribute("_ERROR_MESSAGE_") != null) {
                    errorMessageList.add(UtilFormatOut.replaceString((String) request.getAttribute("_ERROR_MESSAGE_"), "\n", "<br/>"));
                    request.removeAttribute("_ERROR_MESSAGE_");
                }
                if (session.getAttribute("_ERROR_MESSAGE_") != null) {
                    errorMessageList.add(UtilFormatOut.replaceString((String) session.getAttribute("_ERROR_MESSAGE_"), "\n", "<br/>"));
                    session.removeAttribute("_ERROR_MESSAGE_");
                }
                msgList = UtilGenerics.toList(request.getAttribute("_ERROR_MESSAGE_LIST_"));
                if (msgList != null) {
                    errorMessageList.addAll(msgList);
                    request.removeAttribute("_ERROR_MESSAGE_LIST_");
                }
                mContextSys.put("eventMessageList", eventMessageList);
                mContextSys.put("errorMessageList", errorMessageList);
    
                if (request.getAttribute("serviceValidationException") != null) {
                    mContextSys.put("serviceValidationException", request.getAttribute("serviceValidationException"));
                    request.removeAttribute("serviceValidationException");
                }
    
                // if there was an error message, this is an error
                mContextSys.put("isError", errorMessageList.size() > 0 ? Boolean.TRUE : Boolean.FALSE);
                // if a parameter was passed saying this is an error, it is an error
                if ("true".equals(parameterMap.get("isError"))) {
                    mContextSys.put("isError", Boolean.TRUE);
                } 
                
                // Note down all the important system variable names that were recorded automatically...
                sysVarNames = new HashSet<String>(mContextSysKeyRecorderMap.getAddedNames());
                // ...and the few that weren't
                final List<String> otherSysVarNames = Arrays.asList(new String[] { "authz", "security", "webSiteId" });
                sysVarNames.addAll(otherSysVarNames);
               
                // Finished with mContextSys here
            }
            
            // Save system vars
            mContext.push();
            
            // At this point, we begin setting screen-level context data
            
            // Add uiLabelMaps (this is screen-/page-specific, may be overwritten, so after push)
            ResourceBundleMapWrapper uiLabelMap = UtilProperties.getResourceBundleMap("CMSErrorUiLabels", request.getLocale());
            mContext.put("uiLabelMap",uiLabelMap);
            
            putAllPageContent(mContext, content, sysVarNames);
          
            Map<String, CmsGroovyTemplate> groovyFiles = content.getPage().getTemplate().getGroovyTemplates();
            
            for (Entry<String, CmsGroovyTemplate> entry : groovyFiles.entrySet())
            {
                CmsGroovyTemplate gf = (CmsGroovyTemplate) entry.getValue();
                GenericValue groovyFile = gf.getEntity();
                if(groovyFile.get("templateLocation")!=null){
                    Debug.logInfo("Reading script from location "+groovyFile.get("templateLocation"), module);
                    GroovyUtil.runScriptAtLocation((String) groovyFile.get("templateLocation"),mContext);
                }
                
            }
            
            mContext.put("content", content);
            GroovyUtil.runScriptAtLocation(processorLocation, mContext);
            
            Map<String, Object> sourceInjectionContext = FastMap.newInstance();
            // (Not sure if necessary) Just in case there are CMS variable expressions that reference it (not sure), include the page content in the context
            sourceInjectionContext.putAll(content);
            // Then the most important part, the current context (possibly changed by the Groovy scripts)
            sourceInjectionContext.putAll(mContext);
            
            // Note: injectVariableContent currently injects/replaces content in-place (into first map)
            content = injectVariableContent(content, sourceInjectionContext);
            
            mContext.put("content", content);
            // Save all updated (injected) content back into context (making sure not to override important names)
            putAllPageContent(mContext, content, sysVarNames);

            template.process(mContext, out);
        } catch (Exception e) {
            throw new CmsException("Merging page content with template failed.", e, module);
        }
        return out.toString();
    }
    
    private static void putAllPageContent(Map<String, Object> intoMap, Map<String, Object> fromMap, 
            Set<String> namesToSkip) {
        for (Map.Entry<String, Object> entry : fromMap.entrySet()) {
            String key = entry.getKey();
            if (!namesToSkip.contains(key)) {
                intoMap.put(key, entry.getValue());
            }
        }
    }
    
    protected CmsPageContent injectVariableContent(CmsPageContent content, Map<String, ? extends Object> sourceContext) {
        // this pattern matches variables in the following format:
        // {{variableName}}
        // it also matches a dot notation variable name
        // {{aMap.keyRefenrencingAnotherMap.keyReferencingAString}}
        // the dot notation is limited to four levels
        Pattern variablePattern = Pattern.compile("\\{\\{([\\w\\.]+)\\}\\}");
        Matcher variableMatcher;
        for (CmsAttributeTemplate at : getAttributeTemplates()) {
            if(at.getType() != CmsAttributeTemplate.Type.BOOLEAN){
                String atc = (String) content.get(at.getName());
                // if there is no value set, we use the default values
                if (atc == null) {
                    if (at.getDefaultValue() != null && StringUtils.isNotBlank(at.getDefaultValue())) {
                        atc = at.getDefaultValue();
                    } else {
                        continue;
                    }
                }
                
                StringBuffer atcInjected = new StringBuffer();
                variableMatcher = variablePattern.matcher(atc);
                // go through the regex matches.
                while (variableMatcher.find()) {
                    Object result = sourceContext;
                    // split up dot notation variable name
                    StringTokenizer tokenizer = new StringTokenizer(variableMatcher.group(1), ".");
                    while (tokenizer.hasMoreTokens()) {
                        // we can cast here safely because result is either the
                        // initial content map
                        // or we only continued the loop below if result is indeed a
                        // map
                        result = ((Map) result).get(tokenizer.nextToken());
                        // if there is no variable with the name, return an html
                        // comment
                        if (result == null) {
                            result = "<!-- " + variableMatcher.group() + " is empty --!>";
                            break;
                            // if the retrieved value is a map, we continue to
                            // evaluate the dot notation path
                        } else if (result instanceof Map) {
                            continue;
                            // if the value is a string, this is what we will
                            // return. the remaining dot notation path will be
                            // ignored
                        } else if (result instanceof String) {
                            break;
                            // if the value is anything else, there is trouble in
                            // paradise. basically do nothing
                        } else {
                            result = null;
                            break;
                        }
                    }
                    // make sure that the result we use to replace the variable is a
                    // string
                    if (result instanceof String) {
                        variableMatcher.appendReplacement(atcInjected, (String) result);
                    } else {
                        // in every other case, replace the variable with a html
                        // comment
                        variableMatcher.appendReplacement(atcInjected, "<!-- " + variableMatcher.group() + " is not a string --!>");
                    }
                }
                // add the rest of the string we matched against after we went
                // through all regex matches
                variableMatcher.appendTail(atcInjected);
                content.put(at.getName(), atcInjected.toString());

            }else{
                try{
                    Boolean atc = (Boolean) content.get(at.getName());
                    content.put(at.getName(), atc);
                }catch(Exception e){
                    content.put(at.getName(), Boolean.FALSE);                
                }
            }
        }
        return content;
    }

    public CmsTemplate copy() {
        // copy the template itself
        CmsTemplate templateCopy = (CmsTemplate) super.copy();
        // copy attribute templates
        List<CmsAttributeTemplate> attributeTemplateEntries = getAttributeTemplates();
        for (CmsAttributeTemplate attributeTemplate : attributeTemplateEntries) {
            templateCopy.addAttributeTemplate((CmsAttributeTemplate) attributeTemplate.copy());
        }

        return templateCopy;
    }

    public Map<String, Object> getDescriptor() {
        List<CmsAttributeTemplate> attributeTemplates = getAttributeTemplates();
        Collections.<CmsAttributeTemplate> sort(attributeTemplates, new Comparator<CmsAttributeTemplate>() {
            @Override
            public int compare(CmsAttributeTemplate first, CmsAttributeTemplate second) {
                return first.getPosition().compareTo(second.getPosition());
            }

        });
        List<Map<String, ?>> atts = FastList.<Map<String, ?>> newInstance();
        for (CmsAttributeTemplate attributeTemplate : attributeTemplates) {
            atts.add(attributeTemplate.getDescriptor());
        }
        Map<String, Object> descriptor = FastMap.<String, Object> newInstance();
        descriptor.put("attributes", atts);
        descriptor.put("name", getName());
        return descriptor;
    }

    protected abstract CmsPageContent setDirectives(CmsPageContent content, CmsPageContext context);

    
    /**
     * Sets the standard Context variables dynamically
     * TODO: Check whether or not this is still being used - if it is, the returned object doesn't seem to be the correct one.
     * I think groovyContext must be added back to content somehow for it to work...
     * 
     * */
    public CmsPageContent setContextVariables(CmsPageContent content, CmsPageContext context) {        
        String processorLocation = UtilProperties.getPropertyValue("cms.properties", "contentprocessor.script.location", "");
        try {
            //GroovyShell shell = new GroovyShell(processorContext);
            Map<String, Object> groovyContext = FastMap.newInstance();
            HttpServletRequest request = context.getRequest();
            HttpServletResponse response = context.getResponse();
            groovyContext.put("request", request);
            groovyContext.put("response", response);
            HttpSession session = request.getSession();
            groovyContext.put("session", session);
            groovyContext.put("dispatcher", request.getAttribute("dispatcher"));
            groovyContext.put("delegator", request.getAttribute("delegator"));
            groovyContext.put("security", request.getAttribute("security"));
            groovyContext.put("locale", UtilHttp.getLocale(request));
            groovyContext.put("timeZone", UtilHttp.getTimeZone(request));
            groovyContext.put("userLogin", session.getAttribute("userLogin"));
            groovyContext.put("parameters", UtilHttp.getCombinedMap(request, UtilMisc.toSet("delegator", "dispatcher", "security", "locale", "timeZone", "userLogin")));
            // make sure the "nullField" object is in there for entity ops; note this is nullField and not null because as null causes problems in FreeMarker and such...
            groovyContext.put("nullField", GenericEntity.NULL_FIELD);
            groovyContext.put("StringUtil", StringUtil.INSTANCE);

            groovyContext.put("pagecontext",context);
            groovyContext.put("content",content);
            // This runs the groovy script at location xyz
            GroovyUtil.runScriptAtLocation(processorLocation,groovyContext);
        }catch (GeneralException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return content;
    }

    protected Reader getTemplateReader() {
        return new StringReader(getBody());
    }

    protected Template getFreeMarkerTemplate() {
        fmConfig = FreeMarkerWorker.getDefaultOfbizConfig();
        if (fmTemplate == null) {
            try {
                fmTemplate = new Template(getName(), getTemplateReader(), fmConfig);
            } catch (IOException e) {
                throw new CmsException("FreeMarker template could note be retrieved from database.", e, module);
            }
        }
        return fmTemplate;
    }

    /* This may no be necessary - the ofbiz engine should handle it...
      static synchronized protected GroovyScriptEngine getScriptEngine(String root) {
        if (scriptEngine == null) {
            try {
                scriptEngine = new GroovyScriptEngine(root);
            } catch (IOException e) {
                throw new CmsException("Could not initialize script engine. Root: " + root, e, module);
            }
        }
        return scriptEngine;
    }*/

    @SuppressWarnings("unchecked")
    static class CmsTemplateLoader<T extends CmsTemplate> implements TemplateLoader {

        Class<T> tClass;

        {
            if (getClass().getGenericSuperclass() instanceof ParameterizedType) {
                ParameterizedType paramType = (ParameterizedType) getClass().getGenericSuperclass();
                tClass = (Class<T>) paramType.getActualTypeArguments()[0];
            }
        }

        @Override
        public void closeTemplateSource(Object arg0) throws IOException {
        }

        @Override
        public Object findTemplateSource(String templateName) {
            T template = null;
            template = (T) findFirst(UtilMisc.toMap("templateName", templateName), tClass);
            return template;
        }

        @Override
        public long getLastModified(Object temp) {
            T template = (T) temp;
            return template.getLastModified().getTime();
        }

        @Override
        public Reader getReader(Object temp, String encoding) throws IOException {
            T template = (T) temp;
            return template.getTemplateReader();
        }

    }

    /**
     * A FreeMarker directive that inserts the content of an asset. The
     * directive is called with the import name of the asset. The asset is
     * evaluated as FreeMarker template with the content of the respective
     * variable as template model.
     * 
     * The method is called as follows: <@asset "assetImportName">
     * 
     */
    protected static class PageLinkDirective implements TemplateDirectiveModel {

        private CmsPageContext pageContext;

        public PageLinkDirective(CmsPageContext context) {
            super();
            pageContext = context;
        }

        /**
         * @see freemarker.template.TemplateDirectiveModel#execute(freemarker.core.Environment,
         *      java.util.Map, freemarker.template.TemplateModel[],
         *      freemarker.template.TemplateDirectiveBody)
         */
        @SuppressWarnings({ "rawtypes", "unchecked" })
        @Override
        public void execute(Environment env, Map paramsUntyped, TemplateModel[] loopVars, TemplateDirectiveBody body)
                throws TemplateException, IOException {
            Map<String, TemplateModel> params = (Map<String, TemplateModel>) paramsUntyped;
            Writer out = env.getOut();
            String output = null;
            String field = null;
            String value = null;
            // first, check if page name was given
            if (params.containsKey("page")) {
                field = "pageName";
                value = (String) DeepUnwrap.permissiveUnwrap(params.get("page"));
            } else if (params.containsKey("id")) {
                field = "pageId";
                value = (String) DeepUnwrap.permissiveUnwrap(params.get("id"));
            } else if (pageContext.isPreview()) {
                output = "<b>Link failed:</b> the name of linked page must be given as parameter \"page\"";
            } else {
                Debug.logError("Link failed: the name of linked page must be given as parameter \"page\"", module);
                throw new CmsException("Link failed: the name of linked page must be given as parameter \"page\"", module);
            }
            // the name parameter is there, so let's try to find the link
            if (value != null) {
                // get page name from directive
                try {
                    // we have to make sure the delegator is set so the
                    // RequestHandler can look up the WebSite
                    pageContext.getRequest().setAttribute("delegator", getDelegator());
                    // get link
                    CmsPage page = CmsDataObject.<CmsPage> findFirst(
                            UtilMisc.toMap(field, value, "webSiteId", pageContext.getWebSiteId()), CmsPage.class);

                    // render asset
                    output = getURL(pageContext.getRequest(), page);

                } catch (Exception e) {
                    // something went wrong, create an output if in preview mode
                    // or an exception if live
                    if (pageContext.isPreview()) {
                        output = "<b>Link failed:</b> please check page name. Error: " + e.getMessage();
                    } else {
                        Debug.logError(e, "Could not link to page. " + field + ": " + value, module);
                        throw new CmsException("Could not link to page. " + field + ": " + value, e, module);
                    }
                }
            }
            out.write(output);
        }

        /**
         * Builds the URL string for a page.
         * 
         * @param request
         *            the current request
         * @param page
         *            the page the link should point to
         * @return complete url with protocol scheme and port (if necessary)
         */
        protected static String getURL(HttpServletRequest request, CmsPage page) {
            // reconstruct the start of the URL
            StringBuffer url = new StringBuffer();
            String scheme = request.getScheme();
            int port = request.getServerPort();

            url.append(scheme);
            url.append("://");
            url.append(request.getServerName());
            if ((scheme.equals("http") && port != 80) || (scheme.equals("https") && port != 443)) {
                url.append(':');
                url.append(request.getServerPort());
            }
            url.append(request.getContextPath());
            url.append(page.getPath());

            return url.toString();
        }

    }

}