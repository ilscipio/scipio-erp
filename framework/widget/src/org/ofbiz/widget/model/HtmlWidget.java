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

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.webapp.ftl.ExtendedWrapper;
import org.ofbiz.widget.renderer.ScreenRenderer;
import org.ofbiz.widget.renderer.ScreenStringRenderer;
import org.ofbiz.widget.renderer.html.HtmlWidgetRenderer;
import org.w3c.dom.Element;

import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;

/**
 * Widget Library - Screen model HTML class.
 */
@SuppressWarnings("serial")
public class HtmlWidget extends ModelScreenWidget {
    public static final String module = HtmlWidget.class.getName();

    private static final UtilCache<String, Template> specialTemplateCache = UtilCache.createUtilCache("widget.screen.template.ftl.general", 0, 0, false);
    protected static final Configuration specialConfig = FreeMarkerWorker.makeConfiguration(new ExtendedWrapper(FreeMarkerWorker.version, "html")); // SCIPIO: generalized, must pass "html"

    // SCIPIO: NOTE: 2016-10-17: Exceptionally, the Ofbiz ExtendedWrapper that was present here
    // was so generic and needed elsewhere, that it has been MOVED to:
    // org.ofbiz.webapp.ftl.ExtendedWrapper
    
    // End Static, begin class section

    private final List<ModelScreenWidget> subWidgets;

    public HtmlWidget(ModelScreen modelScreen, Element htmlElement) {
        super(modelScreen, htmlElement);
        List<? extends Element> childElementList = UtilXml.childElementList(htmlElement);
        if (childElementList.isEmpty()) {
            this.subWidgets = Collections.emptyList();
        } else {
            List<ModelScreenWidget> subWidgets = new ArrayList<ModelScreenWidget>(childElementList.size());
            for (Element childElement : childElementList) {
                if ("html-template".equals(childElement.getNodeName())) {
                    subWidgets.add(new HtmlTemplate(modelScreen, childElement));
                } else if ("html-template-decorator".equals(childElement.getNodeName())) {
                    subWidgets.add(new HtmlTemplateDecorator(modelScreen, childElement));
                } else {
                    throw new IllegalArgumentException("Tag not supported under the platform-specific -> html tag with name: "
                            + childElement.getNodeName());
                }
            }
            this.subWidgets = Collections.unmodifiableList(subWidgets);
        }
    }

    /**
     * SCIPIO: Gets the Freemarker config used by this class to render HTML templates.
     * <p>
     * It <em>may</em> need to be shared.
     */
    public static Configuration getFtlConfig() {
        return specialConfig;
    }
    
    public List<ModelScreenWidget> getSubWidgets() {
        return subWidgets;
    }

    @Override
    public void renderWidgetString(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException {
        for (ModelScreenWidget subWidget : subWidgets) {
            subWidget.renderWidgetString(writer, context, screenStringRenderer);
        }
    }

    public static void renderHtmlTemplate(Appendable writer, FlexibleStringExpander locationExdr, Map<String, Object> context) {
        String location = locationExdr.expandString(context);
        //Debug.logInfo("Rendering template at location [" + location + "] with context: \n" + context, module);

        if (UtilValidate.isEmpty(location)) {
            throw new IllegalArgumentException("Template location is empty");
        }

        if (location.endsWith(".ftl")) {
            try {
                Map<String, ? extends Object> parameters = UtilGenerics.checkMap(context.get("parameters"));
                boolean insertWidgetBoundaryComments = ModelWidget.widgetBoundaryCommentsEnabled(parameters);
                if (insertWidgetBoundaryComments) {
                    writer.append(HtmlWidgetRenderer.formatBoundaryComment("Begin", "Template", location));
                }

                //FreeMarkerWorker.renderTemplateAtLocation(location, context, writer);
                Template template = null;
                if (location.endsWith(".fo.ftl")) { // FOP can't render correctly escaped characters
                    template = FreeMarkerWorker.getTemplate(location);
                } else {
                    template = FreeMarkerWorker.getTemplate(location, specialTemplateCache, specialConfig);
                }
                FreeMarkerWorker.renderTemplate(template, context, writer);

                if (insertWidgetBoundaryComments) {
                    writer.append(HtmlWidgetRenderer.formatBoundaryComment("End", "Template", location));
                }
            } catch (IllegalArgumentException e) {
                String errMsg = "Error rendering included template at location [" + location + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                writeError(writer, errMsg);
            } catch (MalformedURLException e) {
                String errMsg = "Error rendering included template at location [" + location + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                writeError(writer, errMsg);
            } catch (TemplateException e) {
                String errMsg = "Error rendering included template at location [" + location + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                writeError(writer, errMsg);
            } catch (IOException e) {
                String errMsg = "Error rendering included template at location [" + location + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                writeError(writer, errMsg);
            }
        } else {
            throw new IllegalArgumentException("Rendering not yet supported for the template at location: " + location);
        }
    }

    // TODO: We can make this more fancy, but for now this is very functional
    public static void writeError(Appendable writer, String message) {
        try {
            writer.append(message);
        } catch (IOException e) {
        }
    }

    public static class HtmlTemplate extends ModelScreenWidget {
        protected final FlexibleStringExpander locationExdr; // SCIPIO: final added
        

        public HtmlTemplate(ModelScreen modelScreen, Element htmlTemplateElement) {
            super(modelScreen, htmlTemplateElement);
            this.locationExdr = FlexibleStringExpander.getInstance(htmlTemplateElement.getAttribute("location"));
        }

        public String getLocation(Map<String, Object> context) {
            return locationExdr.expandString(context);
        }

        @Override
        public void renderWidgetString(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) {
            renderHtmlTemplate(writer, this.locationExdr, context);
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public FlexibleStringExpander getLocationExdr() {
            return locationExdr;
        }
    }

    public static class HtmlTemplateDecorator extends ModelScreenWidget {
        protected final FlexibleStringExpander locationExdr; // SCIPIO: final added
        protected final Map<String, ModelScreenWidget> sectionMap;

        public HtmlTemplateDecorator(ModelScreen modelScreen, Element htmlTemplateDecoratorElement) {
            super(modelScreen, htmlTemplateDecoratorElement);
            this.locationExdr = FlexibleStringExpander.getInstance(htmlTemplateDecoratorElement.getAttribute("location"));

            Map<String, ModelScreenWidget> sectionMap = new HashMap<String, ModelScreenWidget>();
            List<? extends Element> htmlTemplateDecoratorSectionElementList = UtilXml.childElementList(htmlTemplateDecoratorElement, "html-template-decorator-section");
            for (Element htmlTemplateDecoratorSectionElement: htmlTemplateDecoratorSectionElementList) {
                String name = htmlTemplateDecoratorSectionElement.getAttribute("name");
                sectionMap.put(name, new HtmlTemplateDecoratorSection(modelScreen, htmlTemplateDecoratorSectionElement));
            }
            this.sectionMap = sectionMap;
        }

        @Override
        public void renderWidgetString(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) {
            // isolate the scope
            MapStack<String> contextMs;
            if (!(context instanceof MapStack<?>)) {
                contextMs = MapStack.create(context);
                context = contextMs;
            } else {
                contextMs = UtilGenerics.cast(context);
            }

            // create a standAloneStack, basically a "save point" for this SectionsRenderer, and make a new "screens" object just for it so it is isolated and doesn't follow the stack down
            MapStack<String> standAloneStack = contextMs.standAloneChildStack();
            standAloneStack.put("screens", new ScreenRenderer(writer, standAloneStack, screenStringRenderer));
            SectionsRenderer sections = new SectionsRenderer(this.sectionMap, standAloneStack, writer, screenStringRenderer);

            // put the sectionMap in the context, make sure it is in the sub-scope, ie after calling push on the MapStack
            contextMs.push();
            context.put("sections", sections);

            renderHtmlTemplate(writer, this.locationExdr, context);
            contextMs.pop();
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public FlexibleStringExpander getLocationExdr() {
            return locationExdr;
        }

        public Map<String, ModelScreenWidget> getSectionMap() {
            return sectionMap;
        }
    }

    public static class HtmlTemplateDecoratorSection extends ModelScreenWidget {
        protected final List<ModelScreenWidget> subWidgets; // SCIPIO: final added

        public HtmlTemplateDecoratorSection(ModelScreen modelScreen, Element htmlTemplateDecoratorSectionElement) {
            super(modelScreen, htmlTemplateDecoratorSectionElement);
            List<? extends Element> subElementList = UtilXml.childElementList(htmlTemplateDecoratorSectionElement);
            this.subWidgets = ModelScreenWidget.readSubWidgets(getModelScreen(), subElementList);
        }

        @Override
        public void renderWidgetString(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException {
            // render sub-widgets
            renderSubWidgetsString(this.subWidgets, writer, context, screenStringRenderer);
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public List<ModelScreenWidget> getSubWidgets() {
            return subWidgets;
        }
    }

    @Override
    public void accept(ModelWidgetVisitor visitor) throws Exception {
        visitor.visit(this);
    }
}
