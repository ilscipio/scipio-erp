package org.ofbiz.webapp.renderer;

import java.io.IOException;

import org.ofbiz.base.util.GeneralException;

import freemarker.template.TemplateException;

/**
 * SCIPIO: Base sections renderer interface, so that the legacy ofbiz widget renderer "sections"
 * object can be replaced with compatible alternatives.
 */
public interface BasicSectionsRenderer {

    /** 
     * This is a lot like the ScreenRenderer class and returns an empty String so it can be used more easily with FreeMarker 
     * <p>
     * SCIPIO: supports asString bool, to render as string to result instead of default writer, logical default false
     * */
    String render(String sectionName, boolean asString) throws GeneralException, IOException, TemplateException;
    
    /** 
     * This is a lot like the ScreenRenderer class and returns an empty String so it can be used more easily with FreeMarker 
     */
    String render(String sectionName) throws GeneralException, IOException, TemplateException;
    
    /** 
     * SCIPIO: version which scopes by default by pushing context stack (shareScope FALSE).
     */
    String renderScoped(String sectionName, Boolean asString, Boolean shareScope) throws GeneralException, IOException, TemplateException;
    
    /** 
     * SCIPIO: version which scopes by default by pushing context stack (shareScope FALSE),
     * generic object/ftl-friendly version.
     */
    String renderScopedGen(String sectionName, Object asString, Object shareScope) throws GeneralException, IOException, TemplateException;
    
}
