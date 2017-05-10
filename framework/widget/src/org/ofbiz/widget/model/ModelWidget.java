/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.widget.model;

import java.io.Serializable;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilProperties;
import org.w3c.dom.Element;

/**
 * Widget Library - Widget model class. ModelWidget is a base class that is
 * extended by other widget model classes.
 */
@SuppressWarnings("serial")
public abstract class ModelWidget implements Serializable {

    public static final String module = ModelWidget.class.getName();
    /**
     * The parameter name used to control widget boundary comments. Currently
     * set to "widgetVerbose".
     */
    public static final String enableBoundaryCommentsParam = "widgetVerbose";

    private final String name;
    private final String systemId;
    private final int startColumn;
    private final int startLine;

    /**
     * Derived classes must call this constructor.
     * @param name The widget name
     */
    protected ModelWidget(String name) {
        this.name = name;
        this.systemId = "anonymous";
        this.startColumn = 0;
        this.startLine = 0;
    }

    /**
     * Derived classes must call this constructor.
     * <p>
     * SCIPIO: now supports explicit/custom name override; use empty string for explicit empty.
     * 
     * @param widgetElement The XML Element for the widget
     */
    protected ModelWidget(Element widgetElement, String name) {
        this.name = (name != null) ? name : widgetElement.getAttribute("name");
        // SCIPIO: workarounds for cases where userData not defined
        //this.systemId = (String) widgetElement.getUserData("systemId");
        //this.startColumn = ((Integer) widgetElement.getUserData("startColumn")).intValue();
        //this.startLine = ((Integer) widgetElement.getUserData("startLine")).intValue();
        String systemId = (String) widgetElement.getUserData("systemId");
        this.systemId = (systemId != null) ? systemId : "anonymous";
        Integer startColumn = (Integer) widgetElement.getUserData("startColumn");
        this.startColumn = (startColumn != null) ? startColumn.intValue() : 0;
        Integer startLine = (Integer) widgetElement.getUserData("startLine");
        this.startLine = (startLine != null) ? startLine.intValue() : 0;
    }
    
    /**
     * Derived classes must call this constructor.
     * @param widgetElement The XML Element for the widget
     */
    protected ModelWidget(Element widgetElement) {
        // SCIPIO: delegating:
        this(widgetElement, widgetElement.getAttribute("name"));
    }

    public abstract void accept(ModelWidgetVisitor visitor) throws Exception;

    /**
     * Returns the widget's name.
     * @return Widget's name
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the url as a string, from where this widget was defined.
     * @return url
     */
    public String getSystemId() {
        return systemId;
    }

    /**
     * Returns the column where this widget was defined, in it's containing xml file.
     * @return start column
     */
    public int getStartColumn() {
        return startColumn;
    }

    /**
     * Returns the line where this widget was defined, in it's containing xml file.
     * @return start line
     */
    public int getStartLine() {
        return startLine;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        ModelWidgetVisitor visitor = new XmlWidgetVisitor(sb);
        try {
            accept(visitor);
        } catch (Exception e) {
            Debug.logWarning(e, "Exception thrown in XmlWidgetVisitor: ", module);
        }
        return sb.toString();
    }

    /**
     * Returns the widget's name to be used in boundary comments. The default action
     * is to return the widget's name. Derived classes can override this method to
     * return a customized name.
     * @return Name to be used in boundary comments
     */
    public String getBoundaryCommentName() {
        return name;
    }

    /**
     * Returns <code>true</code> if widget boundary comments are enabled. Widget boundary comments are
     * enabled by setting <code>widget.verbose=true</code> in the <code>widget.properties</code> file.
     * The <code>true</code> setting can be overridden in <code>web.xml</code> or in the screen
     * rendering context. If <code>widget.verbose</code> is set to <code>false</code> in the
     * <code>widget.properties</code> file, then that setting will override all other settings and
     * disable all widget boundary comments.
     * 
     * @param context Optional context Map
     */
    public static boolean widgetBoundaryCommentsEnabled(Map<String, ? extends Object> context) {
        boolean result = "true".equals(UtilProperties.getPropertyValue("widget", "widget.verbose"));
        if (result && context != null) {
            String str = (String) context.get(enableBoundaryCommentsParam);
            if (str != null) {
                result = "true".equals(str);
            } else{
                Map<String, ? extends Object> parameters = UtilGenerics.checkMap(context.get("parameters"));
                if (parameters != null) {
                    str = (String) parameters.get(enableBoundaryCommentsParam);
                    if (str != null) {
                        result = "true".equals(str);
                    }
                }
            }
        }
        return result;
    }
    
    /**
     * SCIPIO: Returns the location of container of the widget.
     * May be a file location only, or a combination of location#name if it is a sub-widget contained in another.
     * <p>
     * NOTE: This is not guaranteed to be accurate for all widget types due to many merging
     * and reuse operations and memory instances. It is to help track down the sources of errors.
     */
    public abstract String getContainerLocation();
    
    /**
     * SCIPIO: Returns widget type, usually same as tag name 
     * 
     */
    public abstract String getWidgetType();
    
    /**
     * SCIPIO: Returns tag name, usually same as widget type 
     * (for post-construction logging, dynamic queries, targeted rendering).
     */
    public String getTagName() {
        return getWidgetType();
    }
    
    /**
     * SCIPIO: Returns location#name widget string.
     * May contain two sets of names to produce an absolute location, such as 
     * location#containername#name.
     * <p>
     * NOTE: This is not guaranteed to be accurate for all widget types due to many merging
     * and reuse operations and memory instances. It is to help track down the sources of errors.
     */
    public String getFullLocationAndName() {
        return getContainerLocation() + "#" + getName();
    }
    
    /**
     * SCIPIO: Returns suffix log message with location/id of widget (best-effort).
     */
    public String getLogWidgetLocationString() {
        return " (" + getWidgetType() +" widget: " + getFullLocationAndName() + ")";
    }
    
    /**
     * SCIPIO: For any ModelWidget that supports a <code>id="..."</code> attribute.
     */
    public interface IdAttrWidget {
        String getId();
    }
    
    /**
     * SCIPIO: For any ModelWidget that supports a flexible <code>name="..."</code> attribute.
     */
    public interface FlexibleNameAttrWidget {
        String getName(Map<String, Object> context);
    }

    /**
     * SCIPIO: For any ModelWidget that supports a flexible <code>id="..."</code> attribute.
     */
    public interface FlexibleIdAttrWidget { // no need yet: extends IdAttrWidget
        String getId(Map<String, Object> context);
    }
    
    /**
     * SCIPIO: Gets widget name, either flexible if available, or hardcoded if specified, or null.
     */
    public static String getName(ModelWidget widget, Map<String, Object> context) {
        if (widget instanceof FlexibleNameAttrWidget) {
            return ((FlexibleNameAttrWidget) widget).getName(context);
        } else {
            return widget.getName();
        }
    }
    
    /**
     * SCIPIO: Gets widget id, either flexible if available, or hardcoded if available, or null.
     */
    public static String getId(ModelWidget widget, Map<String, Object> context) {
        if (widget instanceof FlexibleIdAttrWidget) {
            return ((FlexibleIdAttrWidget) widget).getId(context);
        } else if (widget instanceof IdAttrWidget) {
            return ((IdAttrWidget) widget).getId();
        } else {
            return null;
        }
    }
    
}
