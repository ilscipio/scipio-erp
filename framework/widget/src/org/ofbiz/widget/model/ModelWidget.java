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
import java.util.Objects;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.w3c.dom.Element;

/**
 * Widget Library - Widget model class. ModelWidget is a base class that is
 * extended by other widget model classes.
 */
@SuppressWarnings("serial")
public abstract class ModelWidget implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    /**
     * The parameter name used to control widget boundary comments. Currently
     * set to "widgetVerbose".
     */
    public static final String enableBoundaryCommentsParam = "widgetVerbose";

    // SCIPIO: cached variable
    private static final boolean widgetVerboseGlobal = "true".equals(UtilProperties.getPropertyValue("widget", "widget.verbose"));

    /* SCIPIO: 2019-05-21: refactored into structure
    private final String name;
    private final String systemId;
    private final int startColumn;
    private final int startLine;
    */
    private final WidgetMetaInfo metaInfo;

    /**
     * Derived classes must call this constructor.
     * @param name The widget name, or null to read from "name" attribute (SCIPIO: improved behavior/checks)
     */
    protected ModelWidget(String name) {
        this.metaInfo = new WidgetMetaInfo(name); // SCIPIO: now delegating
    }

    /**
     * Derived classes must call this constructor.
     * <p>
     * SCIPIO: now supports explicit/custom name override; use empty string for explicit empty.
     * @param widgetElement The XML Element for the widget
     * @param name The widget name, or null to read from "name" attribute (SCIPIO: improved behavior/checks)
     */
    protected ModelWidget(Element widgetElement, String name) {
        this.metaInfo = new WidgetMetaInfo(widgetElement, name); // SCIPIO: now delegating
    }

    /**
     * Derived classes must call this constructor.
     * @param widgetElement The XML Element for the widget
     */
    protected ModelWidget(Element widgetElement) {
        this.metaInfo = new WidgetMetaInfo(widgetElement); // SCIPIO: now delegating
    }

    /**
     * SCIPIO: Construct from WidgetMetaInfo.
     * @param metaInfo The meta info for the widget
     */
    protected ModelWidget(WidgetMetaInfo metaInfo) {
        this.metaInfo = (metaInfo != null) ? metaInfo : WidgetMetaInfo.UNDEFINED;
    }

    public abstract void accept(ModelWidgetVisitor visitor) throws Exception;

    /**
     * Returns the widget's name.
     * @return Widget's name
     */
    public String getName() {
        return metaInfo.getName();
    } // SCIPIO: moved name to WidgetMetaInfo

    /**
     * SCIPIO: Returns the widget's original name, meaning its name without
     * any possible overrides.
     * Added 2019-02-01.
     * @return Widget's name
     */
    public String getOriginalName() { // final
        return metaInfo.getName();
    } // SCIPIO: moved name to WidgetMetaInfo

    /**
     * Returns the url as a string, from where this widget was defined.
     * @return url
     */
    public String getSystemId() {
        return metaInfo.getSystemId();
    } // SCIPIO: moved name to WidgetMetaInfo

    /**
     * Returns the column where this widget was defined, in it's containing xml file.
     * @return start column
     */
    public int getStartColumn() {
        return metaInfo.getStartColumn();
    } // SCIPIO: moved name to WidgetMetaInfo

    /**
     * Returns the line where this widget was defined, in it's containing xml file.
     * @return start line
     */
    public int getStartLine() {
        return metaInfo.getStartLine();
    } // SCIPIO: moved name to WidgetMetaInfo

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
        return metaInfo.getName();
    } // SCIPIO: moved name to WidgetMetaInfo

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
        // SCIPIO: cached
        //boolean result = "true".equals(UtilProperties.getPropertyValue("widget", "widget.verbose"));
        boolean result = widgetBoundaryCommentsEnabledGlobal(context);
        if (result && context != null) {
            // SCIPIO: support straight boolean
            //String str = (String) context.get(enableBoundaryCommentsParam);
            Object str = context.get(enableBoundaryCommentsParam);
            if (str != null) {
                result = UtilMisc.booleanValue(str, false); // SCIPIO: result = "true".equals(str);
            } else {
                /* SCIPIO: security: Do not do this, because it may read request parameters
                Map<String, ? extends Object> parameters = UtilGenerics.checkMap(context.get("parameters"));
                if (parameters != null) {
                    str = parameters.get(enableBoundaryCommentsParam); // SCIPIO: (String) parameters.get(enableBoundaryCommentsParam);
                    if (str != null) {
                        result = UtilMisc.booleanValue(str, false); // SCIPIO: //result = "true".equals(str);
                    }
                }*/
                Boolean enabled = widgetBoundaryCommentsEnabledRequestWebapp(context);
                if (enabled != null) {
                    result = enabled;
                }
            }
        }
        return result;
    }

    /**
     * SCIPIO: Returns <code>true</code> if widget boundary comments are enabled in the <code>widget.properties</code> 
     * file (only).
     */
    static boolean widgetBoundaryCommentsEnabledGlobal(Map<String, ? extends Object> context) {
        return widgetVerboseGlobal;
    }

    /**
     * SCIPIO: Returns <code>true</code> if widget boundary comments are enabled in request attributes or the webapp.
     */
    static Boolean widgetBoundaryCommentsEnabledRequestWebapp(Map<String, ? extends Object> context) {
        return widgetBoundaryCommentsEnabledRequestWebapp((HttpServletRequest) context.get("request"));
    }
    
    /**
     * SCIPIO: Returns <code>true</code> if widget boundary comments are enabled in request attributes or the webapp.
     */
    static Boolean widgetBoundaryCommentsEnabledRequestWebapp(HttpServletRequest request) {
        if (request == null) {
            return null;
        }
        Object enableBoundaryComments = request.getAttribute(ModelWidget.enableBoundaryCommentsParam);
        if (enableBoundaryComments == null) {
            enableBoundaryComments = request.getServletContext().getAttribute(ModelWidget.enableBoundaryCommentsParam);
        }
        return UtilMisc.booleanValue(enableBoundaryComments);
    }

    /**
     * SCIPIO: Prepares the widget boundary comments context field. Called by ScreenRenderer.
     * <p>
     * NOTE: security: this prevents {@link #widgetBoundaryCommentsEnabled} from ever reading request parameters.
     * In addition, optimizes access a little.
     * <p>
     * Always put widgetVerbose in context so that:
     * 1) easier access 
     * 2) it's already a boolean 
     * 3) request parameters will not be consulted by ModelWidget#widgetBoundaryCommentsEnabled (security, minor).
     * <p>
     * Added 2018-12-07.
     */
    public static void setWidgetBoundaryCommentsEnabledField(Map<String, Object> context, HttpServletRequest request) {
        // See #widgetBoundaryCommentsEnabled for default logic
        Object enableBoundaryComments = false;
        if (widgetBoundaryCommentsEnabledGlobal(context)) {
            if (context.get(enableBoundaryCommentsParam) == null) {
                enableBoundaryComments = widgetBoundaryCommentsEnabledRequestWebapp(request);
                if (enableBoundaryComments == null) {
                    enableBoundaryComments = true; 
                }
                context.put(ModelWidget.enableBoundaryCommentsParam, enableBoundaryComments); // force
            }
        } else {
            context.put(ModelWidget.enableBoundaryCommentsParam, enableBoundaryComments); // force disable
        }
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

    /**
     * SCIPIO: Returns meta (static) widget information.
     * NOTE: several of the constructor logic of ModelWidget is moved here.
     * Added 2019-05-21.
     */
    public WidgetMetaInfo getMetaInfo() {
        return metaInfo;
    }

    public static class WidgetMetaInfo implements Serializable {
        public static final String ANON_SYS = "anonymous";
        public static final WidgetMetaInfo UNDEFINED = new WidgetMetaInfo();

        private final String name;
        private final String systemId;
        private final int startColumn;
        private final int startLine;

        /**
         * SCIPIO: Fields constructor.
         */
        public WidgetMetaInfo(String name, String systemId, Integer startColumn, Integer startLine) {
            this.name = (name != null) ? name : ""; // SCIPIO: ensure non-null
            this.systemId = (systemId != null) ? systemId : ANON_SYS; // SCIPIO: ensure anonymous
            this.startColumn = (startColumn != null) ? startColumn : 0;
            this.startLine = (startLine != null) ? startLine : 0;
        }

        /**
         * SCIPIO: Default constructor - creates an instance having no name (empty string).
         * NOTE: the original ModelWidget class did not have default constructor for this.
         * NOTE: kept private even if subclassed because in practice almost everything needs a name.
         */
        private WidgetMetaInfo() {
            this((String) null);
        }

        /**
         * Derived classes must call this constructor.
         * @param name The widget name
         */
        public WidgetMetaInfo(String name) {
            this(name, null, null, null);
        }

        /**
         * XML constructor. SCIPIO: now supports explicit/custom name override; use empty string for explicit empty.
         * @param widgetElement The XML Element for the widget
         * @param name The widget name, or null to read from "name" attribute (SCIPIO: improved behavior/checks)
         */
        public WidgetMetaInfo(Element widgetElement, String name) {
            this((name != null) ? name : widgetElement.getAttribute("name"), (String) widgetElement.getUserData("systemId"),
                    (Integer) widgetElement.getUserData("startColumn"), (Integer) widgetElement.getUserData("startLine"));
        }

        public WidgetMetaInfo(Element widgetElement) {
            this(widgetElement, null); // SCIPIO: now delegating
        }

        /**
         * SCIPIO: Trivial copy constructor. NOTE: limited use because immutable.
         */
        protected WidgetMetaInfo(WidgetMetaInfo other) {
            this(other.getName(), other.getSystemId(), other.getStartColumn(), other.getStartLine());
        }

        /**
         * SCIPIO: Copy constructor with optional overrides (null signifies no override).
         */
        protected WidgetMetaInfo(WidgetMetaInfo other, String name, String systemId, Integer startColumn, Integer startLine) {
            this((name != null) ? name : other.getName(), (systemId != null) ? systemId : other.getSystemId(),
                    (startColumn != null) ? startColumn : other.getStartColumn(), (startLine != null) ? startLine : other.getStartLine());
        }

        public static WidgetMetaInfo make(String name, String systemId, Integer startColumn, Integer startLine) {
            WidgetMetaInfo metaInfo = new WidgetMetaInfo(name, systemId, startColumn, startLine);
            return metaInfo.equals(UNDEFINED) ? UNDEFINED : metaInfo;
        }
        public static WidgetMetaInfo make(String name) {
            WidgetMetaInfo metaInfo = new WidgetMetaInfo(name);
            return metaInfo.equals(UNDEFINED) ? UNDEFINED : metaInfo;
        }

        public static WidgetMetaInfo copy(WidgetMetaInfo other, String name, String systemId, Integer startColumn, Integer startLine) {
            return (other != null) ? other.copy(name, systemId, startColumn, startLine) : make(name, systemId, startColumn, startLine);
        }
        public static WidgetMetaInfo copy(WidgetMetaInfo other, String name) {
            return (other != null) ? other.copy(name) : make(name);
        }
        public static WidgetMetaInfo copy(WidgetMetaInfo other) {
            return (other != null) ? other.copy() : UNDEFINED;
        }

        public WidgetMetaInfo copy(String name, String systemId, Integer startColumn, Integer startLine) {
            WidgetMetaInfo metaInfo = copyAlways(name, systemId, startColumn, startLine);
            // SCIPIO: optimization: if same meta info, return the original; saves memory (NOTE: subclasses may alter this behavior)
            //return metaInfo;
            return metaInfo.equals(this) ? this : metaInfo;
        }
        public WidgetMetaInfo copy(String name) {
            WidgetMetaInfo metaInfo = copyAlways(name);
            // SCIPIO: optimization: if same meta info, return the original; saves memory (NOTE: subclasses may alter this behavior)
            //return metaInfo;
            return metaInfo.equals(this) ? this : metaInfo;
        }
        public WidgetMetaInfo copy() {
            // SCIPIO: optimization: if same meta info, return the original; saves memory (NOTE: subclasses may alter this behavior)
            //return this.copyAlways();
            return this;
        }

        protected WidgetMetaInfo copyAlways(String name, String systemId, Integer startColumn, Integer startLine) {
            return new WidgetMetaInfo(this, name, systemId, startColumn, startLine);
        }
        protected WidgetMetaInfo copyAlways(String name) {
            return copyAlways(name, null, null, null);
        }
        protected WidgetMetaInfo copyAlways() {
            return copyAlways(null, null, null, null);
        }

        public String getName() { return name; }
        public String getSystemId() { return systemId; }
        public int getStartColumn() { return startColumn; }
        public int getStartLine() { return startLine; }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof WidgetMetaInfo)) return false;
            WidgetMetaInfo that = (WidgetMetaInfo) o;
            return getStartColumn() == that.getStartColumn() &&
                    getStartLine() == that.getStartLine() &&
                    Objects.equals(getName(), that.getName()) &&
                    Objects.equals(getSystemId(), that.getSystemId());
        }

        @Override
        public int hashCode() {
            return Objects.hash(getName(), getSystemId(), getStartColumn(), getStartLine());
        }

        @Override
        public String toString() { // SCIPIO: NOTE: intended to be exception/log/public friendly
            return "{name=" + name +
                    ", systemId=" + systemId +
                    ", startColumn=" + startColumn +
                    ", startLine=" + startLine +
                    "}";
        }
    }
}
