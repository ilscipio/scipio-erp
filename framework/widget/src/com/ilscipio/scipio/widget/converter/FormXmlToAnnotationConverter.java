/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package com.ilscipio.scipio.widget.converter;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Converts Form XML definitions to Java annotation source code.
 *
 * <p>SCIPIO: 4.0.0: Added for XML-to-Annotation conversion support.</p>
 */
public class FormXmlToAnnotationConverter extends XmlToAnnotationConverter {

    private int scriptCounter = 0;

    public FormXmlToAnnotationConverter(String packageName, String className, String componentName,
                                        File outputDir, File scriptOutputDir) {
        super(packageName, className, componentName, outputDir, scriptOutputDir);
    }

    @Override
    protected String getImports() {
        return "import com.ilscipio.scipio.widget.def.form.*;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.screen.SetAction;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.screen.ServiceAction;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.screen.FieldMap;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.screen.EntityOneAction;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.screen.ScriptAction;" + NEWLINE +
               "import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;" + NEWLINE;
    }

    @Override
    public String convert(Document doc) {
        StringBuilder sb = new StringBuilder();

        sb.append(generateClassHeader());
        sb.append(NEWLINE);

        // SCIPIO: 4.0.0: Reset case-insensitive interface-name collision tracking for this class
        resetInterfaceNameTracking();

        Element root = doc.getDocumentElement();
        List<Element> formElements = childElementList(root, "form");

        for (Element formElement : formElements) {
            sb.append(convertForm(formElement));
            sb.append(NEWLINE);
        }

        // SCIPIO: 4.0.0: <grid> is a list form and may stand beside <form> in the same file;
        // without this the definition is dropped without a message.
        for (Element gridElement : childElementList(root, "grid")) {
            sb.append(convertForm(gridElement));
            sb.append(NEWLINE);
        }

        sb.append(generateClassFooter());

        return sb.toString();
    }

    /**
     * Converts a single form element to annotations.
     */
    protected String convertForm(Element formElement) {
        StringBuilder sb = new StringBuilder();
        String formName = getAttr(formElement, "name");

        // SCIPIO: 4.0.0: Skip forms with empty or missing names
        if (formName == null || formName.trim().isEmpty()) {
            System.err.println("Warning: Skipping form with empty name in " + sourceLocation);
            return "";
        }

        scriptCounter = 0;

        // Generate @Form annotation
        sb.append(indent(1)).append(generateFormAnnotation(formElement, formName));
        sb.append(NEWLINE);

        // Generate interface declaration
        // SCIPIO: 4.0.0: Disambiguate names that collide case-insensitively (Windows filesystem defect)
        sb.append(indent(1)).append("public interface ").append(resolveUniqueInterfaceName(toInterfaceName(formName))).append(" {}");
        sb.append(NEWLINE);

        return sb.toString();
    }

    /**
     * Generates the @Form annotation with all attributes.
     */
    protected String generateFormAnnotation(Element formElement, String formName) {
        StringBuilder sb = new StringBuilder();
        sb.append("@Form(").append(NEWLINE);

        List<String> attrs = new ArrayList<>();

        // Required attributes
        attrs.add(indent(2) + attrIfNotEmpty("name", formName));

        // SCIPIO: 4.0.0: Add location attribute for backward compatibility with XML references
        if (isNotEmpty(sourceLocation)) {
            attrs.add(indent(2) + attrIfNotEmpty("location", sourceLocation));
        }

        // Form type
        String type = getAttr(formElement, "type");
        if (!isNotEmpty(type) && "grid".equals(formElement.getTagName())) {
            type = "list";   // SCIPIO: 4.0.0: a grid carries no type attribute
        }
        if (isNotEmpty(type) && !"single".equals(type)) {
            attrs.add(indent(2) + "type = FormType." + type.toUpperCase());
        }

        // Basic attributes
        addAttrIfNotEmpty(attrs, formElement, "target", "target");
        addAttrIfNotEmpty(attrs, formElement, "target-window", "targetWindow");
        addAttrIfNotEmpty(attrs, formElement, "id", "id");
        addAttrIfNotEmpty(attrs, formElement, "style", "style");
        addAttrIfNotEmpty(attrs, formElement, "focus-field-name", "focusFieldName");
        addAttrIfNotEmpty(attrs, formElement, "title", "title");
        addAttrIfNotEmpty(attrs, formElement, "empty-form-data-message", "emptyFormDataMessage");
        addAttrIfNotEmpty(attrs, formElement, "tooltip", "tooltip");

        // List/multi form attributes
        addAttrIfNotEmpty(attrs, formElement, "list-name", "listName");
        addAttrIfNotEmpty(attrs, formElement, "list-entry-name", "listEntryName");
        addAttrIfNotEmpty(attrs, formElement, "default-map-name", "defaultMapName");
        addAttrIfNotEmpty(attrs, formElement, "default-entity-name", "defaultEntityName");
        addAttrIfNotEmpty(attrs, formElement, "default-service-name", "defaultServiceName");

        // Extends
        addAttrIfNotEmpty(attrs, formElement, "extends", "extendsForm");
        addAttrIfNotEmpty(attrs, formElement, "extends-resource", "extendsResource");

        // Pagination attributes
        addAttrIfNotEmpty(attrs, formElement, "paginate", "paginate");
        addAttrIfNotEmpty(attrs, formElement, "paginate-target", "paginateTarget");
        addAttrIfNotEmpty(attrs, formElement, "paginate-size-field", "paginateSizeField");
        addAttrIfNotEmpty(attrs, formElement, "paginate-index-field", "paginateIndexField");
        addAttrIfNotEmpty(attrs, formElement, "paginate-first-label", "paginateFirstLabel");
        addAttrIfNotEmpty(attrs, formElement, "paginate-previous-label", "paginatePreviousLabel");
        addAttrIfNotEmpty(attrs, formElement, "paginate-next-label", "paginateNextLabel");
        addAttrIfNotEmpty(attrs, formElement, "paginate-last-label", "paginateLastLabel");
        addAttrIfNotEmpty(attrs, formElement, "paginate-view-size-label", "paginateViewSizeLabel");
        addAttrIfNotEmpty(attrs, formElement, "paginate-style", "paginateStyle");
        addAttrIfNotEmpty(attrs, formElement, "paginate-target-anchor", "paginateTargetAnchor");
        addAttrIfNotEmpty(attrs, formElement, "override-list-size", "overrideListSize");
        addAttrIfNotEmpty(attrs, formElement, "item-index-separator", "itemIndexSeparator");

        // Style attributes
        addAttrIfNotEmpty(attrs, formElement, "header-row-style", "headerRowStyle");
        addAttrIfNotEmpty(attrs, formElement, "odd-row-style", "oddRowStyle");
        addAttrIfNotEmpty(attrs, formElement, "even-row-style", "evenRowStyle");
        addAttrIfNotEmpty(attrs, formElement, "default-table-style", "defaultTableStyle");
        addAttrIfNotEmpty(attrs, formElement, "default-title-style", "defaultTitleStyle");
        addAttrIfNotEmpty(attrs, formElement, "default-widget-style", "defaultWidgetStyle");
        addAttrIfNotEmpty(attrs, formElement, "default-tooltip-style", "defaultTooltipStyle");
        addAttrIfNotEmpty(attrs, formElement, "default-title-area-style", "defaultTitleAreaStyle");
        addAttrIfNotEmpty(attrs, formElement, "default-widget-area-style", "defaultWidgetAreaStyle");
        addAttrIfNotEmpty(attrs, formElement, "form-title-area-style", "formTitleAreaStyle");
        addAttrIfNotEmpty(attrs, formElement, "form-widget-area-style", "formWidgetAreaStyle");
        addAttrIfNotEmpty(attrs, formElement, "default-required-field-style", "defaultRequiredFieldStyle");
        addAttrIfNotEmpty(attrs, formElement, "sort-field-parameter-name", "sortFieldParameterName");
        addAttrIfNotEmpty(attrs, formElement, "default-sort-field-style", "defaultSortFieldStyle");
        addAttrIfNotEmpty(attrs, formElement, "default-sort-field-asc-style", "defaultSortFieldAscStyle");
        addAttrIfNotEmpty(attrs, formElement, "default-sort-field-desc-style", "defaultSortFieldDescStyle");

        // Boolean attributes
        addBoolAttr(attrs, formElement, "client-autocomplete-fields", "clientAutocompleteFields", true);
        addBoolAttr(attrs, formElement, "separate-columns", "separateColumns", false);
        addBoolAttr(attrs, formElement, "group-columns", "groupColumns", true);
        addBoolAttr(attrs, formElement, "hide-header", "hideHeader", false);
        addBoolAttr(attrs, formElement, "use-row-submit", "useRowSubmit", false);
        addBoolAttr(attrs, formElement, "default-combine-action-fields", "defaultCombineActionFields", true);

        // Integer attributes
        String viewSize = getAttr(formElement, "view-size");
        if (isNotEmpty(viewSize)) {
            try {
                int vs = Integer.parseInt(viewSize);
                if (vs != 0) {
                    attrs.add(indent(2) + "viewSize = " + vs);
                }
            } catch (NumberFormatException e) {
                // Ignore
            }
        }

        String positions = getAttr(formElement, "positions");
        if (isNotEmpty(positions)) {
            try {
                int pos = Integer.parseInt(positions);
                if (pos != 0) {
                    attrs.add(indent(2) + "positions = " + pos);
                }
            } catch (NumberFormatException e) {
                // Ignore
            }
        }

        String defaultPositionSpan = getAttr(formElement, "default-position-span");
        if (isNotEmpty(defaultPositionSpan)) {
            try {
                int dps = Integer.parseInt(defaultPositionSpan);
                if (dps != 0) {
                    attrs.add(indent(2) + "defaultPositionSpan = " + dps);
                }
            } catch (NumberFormatException e) {
                // Ignore
            }
        }

        // Other string attributes
        addAttrIfNotEmpty(attrs, formElement, "skip-start", "skipStart");
        addAttrIfNotEmpty(attrs, formElement, "skip-end", "skipEnd");
        addAttrIfNotEmpty(attrs, formElement, "use-request-parameters", "useRequestParameters");
        addAttrIfNotEmpty(attrs, formElement, "method", "method");
        addAttrIfNotEmpty(attrs, formElement, "attribs", "attribs");
        addAttrIfNotEmpty(attrs, formElement, "row-count", "rowCount");
        addAttrIfNotEmpty(attrs, formElement, "hide-header-when", "hideHeaderWhen");
        addAttrIfNotEmpty(attrs, formElement, "hide-table-when", "hideTableWhen");
        addAttrIfNotEmpty(attrs, formElement, "use-alternate-text-when", "useAlternateTextWhen");
        addAttrIfNotEmpty(attrs, formElement, "alternate-text", "alternateText");
        addAttrIfNotEmpty(attrs, formElement, "alternate-text-style", "alternateTextStyle");

        // SCIPIO: 4.0.0: URL mode (target-type) is NOT a valid @Form attribute
        // The target-type is part of the form submit behavior, not an annotation attribute
        // Removing this to avoid compilation errors - urlMode only valid on @HyperlinkField
        // String urlMode = getAttr(formElement, "target-type");

        // Process auto-fields-service
        List<Element> autoFieldsServiceElements = childElementList(formElement, "auto-fields-service");
        if (!autoFieldsServiceElements.isEmpty()) {
            StringBuilder afsBuilder = new StringBuilder();
            afsBuilder.append(indent(2)).append("autoFieldsService = {").append(NEWLINE);
            boolean first = true;
            for (Element afs : autoFieldsServiceElements) {
                if (!first) afsBuilder.append(",").append(NEWLINE);
                afsBuilder.append(indent(3)).append(generateAutoFieldsService(afs));
                first = false;
            }
            afsBuilder.append(NEWLINE).append(indent(2)).append("}");
            attrs.add(afsBuilder.toString());
        }

        // Process auto-fields-entity
        List<Element> autoFieldsEntityElements = childElementList(formElement, "auto-fields-entity");
        if (!autoFieldsEntityElements.isEmpty()) {
            StringBuilder afeBuilder = new StringBuilder();
            afeBuilder.append(indent(2)).append("autoFieldsEntity = {").append(NEWLINE);
            boolean first = true;
            for (Element afe : autoFieldsEntityElements) {
                if (!first) afeBuilder.append(",").append(NEWLINE);
                afeBuilder.append(indent(3)).append(generateAutoFieldsEntity(afe));
                first = false;
            }
            afeBuilder.append(NEWLINE).append(indent(2)).append("}");
            attrs.add(afeBuilder.toString());
        }

        // Process fields
        List<Element> fieldElements = childElementList(formElement, "field");
        if (!fieldElements.isEmpty()) {
            StringBuilder fieldsBuilder = new StringBuilder();
            fieldsBuilder.append(indent(2)).append("fields = {").append(NEWLINE);
            boolean first = true;
            for (Element field : fieldElements) {
                if (!first) fieldsBuilder.append(",").append(NEWLINE);
                fieldsBuilder.append(indent(3)).append(generateField(field, formName));
                first = false;
            }
            fieldsBuilder.append(NEWLINE).append(indent(2)).append("}");
            attrs.add(fieldsBuilder.toString());
        }

        // Process alt-target
        List<Element> altTargetElements = childElementList(formElement, "alt-target");
        if (!altTargetElements.isEmpty()) {
            StringBuilder atBuilder = new StringBuilder();
            atBuilder.append(indent(2)).append("altTargets = {").append(NEWLINE);
            boolean first = true;
            for (Element at : altTargetElements) {
                if (!first) atBuilder.append(",").append(NEWLINE);
                atBuilder.append(indent(3)).append(generateAltTarget(at));
                first = false;
            }
            atBuilder.append(NEWLINE).append(indent(2)).append("}");
            attrs.add(atBuilder.toString());
        }

        // Process actions
        Element actionsElement = firstChildElement(formElement, "actions");
        if (actionsElement != null) {
            String actionsCode = generateFormActions(actionsElement, formName);
            if (isNotEmpty(actionsCode)) {
                attrs.add(indent(2) + "actions = " + actionsCode);
            }
        }

        // Process row-actions
        Element rowActionsElement = firstChildElement(formElement, "row-actions");
        if (rowActionsElement != null) {
            String rowActionsCode = generateRowActions(rowActionsElement, formName);
            if (isNotEmpty(rowActionsCode)) {
                attrs.add(indent(2) + "rowActions = " + rowActionsCode);
            }
        }

        // Process sort-order
        Element sortOrderElement = firstChildElement(formElement, "sort-order");
        if (sortOrderElement != null) {
            String sortOrderCode = generateSortOrder(sortOrderElement);
            if (isNotEmpty(sortOrderCode)) {
                attrs.add(indent(2) + "sortOrder = " + sortOrderCode);
            }
        }

        // Process on-event-update-area
        List<Element> onEventUpdateAreaElements = childElementList(formElement, "on-event-update-area");
        if (!onEventUpdateAreaElements.isEmpty()) {
            StringBuilder oeuaBuilder = new StringBuilder();
            oeuaBuilder.append(indent(2)).append("onEventUpdateAreas = {").append(NEWLINE);
            boolean first = true;
            for (Element oeua : onEventUpdateAreaElements) {
                if (!first) oeuaBuilder.append(",").append(NEWLINE);
                oeuaBuilder.append(indent(3)).append(generateOnEventUpdateArea(oeua));
                first = false;
            }
            oeuaBuilder.append(NEWLINE).append(indent(2)).append("}");
            attrs.add(oeuaBuilder.toString());
        }

        // Join all attributes
        sb.append(String.join("," + NEWLINE, attrs));
        sb.append(NEWLINE).append(indent(1)).append(")");

        return sb.toString();
    }

    /**
     * Helper to add string attribute if not empty.
     */
    protected void addAttrIfNotEmpty(List<String> attrs, Element element, String xmlAttr, String annotationAttr) {
        String value = getAttr(element, xmlAttr);
        if (isNotEmpty(value)) {
            attrs.add(indent(2) + annotationAttr + " = " + toStringValue(value));
        }
    }

    /**
     * Helper to add boolean attribute if different from default.
     */
    protected void addBoolAttr(List<String> attrs, Element element, String xmlAttr, String annotationAttr, boolean defaultValue) {
        String value = getAttr(element, xmlAttr);
        if (isNotEmpty(value)) {
            boolean boolVal = "true".equalsIgnoreCase(value) || "Y".equalsIgnoreCase(value);
            if (boolVal != defaultValue) {
                attrs.add(indent(2) + annotationAttr + " = " + boolVal);
            }
        }
    }

    /**
     * Remaps a map-name value that collides with a UEL reserved word to an equivalent
     * nonexistent map name, so the generated mapName does not break FlexibleMapAccessor
     * at render time (e.g. XML idiom map-name="empty" -> mapName = "emptyMap").
     */
    protected static String remapReservedMapName(String mapName) {
        if ("empty".equals(mapName)) {
            return "emptyMap";
        }
        return mapName;
    }

    /**
     * Generates @AutoFieldsService annotation.
     */
    protected String generateAutoFieldsService(Element element) {
        String serviceName = getAttr(element, "service-name");
        // "empty" is a UEL reserved word; remap to an equivalent nonexistent map name
        String mapName = remapReservedMapName(getAttr(element, "map-name"));
        String defaultFieldType = getAttr(element, "default-field-type");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("serviceName", serviceName));
        if (isNotEmpty(mapName)) {
            attrs.add(attrIfNotEmpty("mapName", mapName));
        }
        if (isNotEmpty(defaultFieldType)) {
            String enumValue = defaultFieldType.replace("-", "_").toUpperCase();
            attrs.add("defaultFieldType = DefaultFieldType." + enumValue);
        }

        return "@AutoFieldsService(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates @AutoFieldsEntity annotation.
     */
    protected String generateAutoFieldsEntity(Element element) {
        String entityName = getAttr(element, "entity-name");
        // "empty" is a UEL reserved word; remap to an equivalent nonexistent map name
        String mapName = remapReservedMapName(getAttr(element, "map-name"));
        String defaultFieldType = getAttr(element, "default-field-type");

        List<String> attrs = new ArrayList<>();
        attrs.add(attrIfNotEmpty("entityName", entityName));
        if (isNotEmpty(mapName)) {
            attrs.add(attrIfNotEmpty("mapName", mapName));
        }
        if (isNotEmpty(defaultFieldType)) {
            String enumValue = defaultFieldType.replace("-", "_").toUpperCase();
            attrs.add("defaultFieldType = DefaultFieldType." + enumValue);
        }

        return "@AutoFieldsEntity(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates @FormField annotation for a field element.
     */
    protected String generateField(Element fieldElement, String formName) {
        StringBuilder sb = new StringBuilder();
        sb.append("@FormField(");

        List<String> attrs = new ArrayList<>();

        // Required name
        String fieldName = getAttr(fieldElement, "name");
        attrs.add(attrIfNotEmpty("name", fieldName));

        // Common field attributes
        // "empty" is a UEL reserved word; remap to an equivalent nonexistent map name
        String mapName = remapReservedMapName(getAttr(fieldElement, "map-name"));
        String entityName = getAttr(fieldElement, "entity-name");
        String fieldNameAttr = getAttr(fieldElement, "field-name");
        String serviceName = getAttr(fieldElement, "service-name");
        String attributeName = getAttr(fieldElement, "attribute-name");
        String entryName = getAttr(fieldElement, "entry-name");
        String parameterName = getAttr(fieldElement, "parameter-name");
        String title = getAttr(fieldElement, "title");
        String tooltip = getAttr(fieldElement, "tooltip");
        String useWhen = getAttr(fieldElement, "use-when");
        String ignoreWhen = getAttr(fieldElement, "ignore-when");
        String idName = getAttr(fieldElement, "id-name");
        String tabindex = getAttr(fieldElement, "tabindex");
        String position = getAttr(fieldElement, "position");
        String positionSpan = getAttr(fieldElement, "position-span");
        String titleStyle = getAttr(fieldElement, "title-style");
        String titleAreaStyle = getAttr(fieldElement, "title-area-style");
        String widgetStyle = getAttr(fieldElement, "widget-style");
        String widgetAreaStyle = getAttr(fieldElement, "widget-area-style");
        String tooltipStyle = getAttr(fieldElement, "tooltip-style");
        String requiredFieldStyle = getAttr(fieldElement, "required-field-style");
        String event = getAttr(fieldElement, "event");
        String action = getAttr(fieldElement, "action");
        String encodeOutput = getAttr(fieldElement, "encode-output");
        String requiredField = getAttr(fieldElement, "required-field");
        String sortField = getAttr(fieldElement, "sort-field");
        String redWhen = getAttr(fieldElement, "red-when");
        String separateColumn = getAttr(fieldElement, "separate-column");
        String combinePrevious = getAttr(fieldElement, "combine-previous");
        String disabled = getAttr(fieldElement, "disabled");

        if (isNotEmpty(mapName)) attrs.add(attrIfNotEmpty("mapName", mapName));
        if (isNotEmpty(entityName)) attrs.add(attrIfNotEmpty("entityName", entityName));
        if (isNotEmpty(fieldNameAttr)) attrs.add(attrIfNotEmpty("fieldName", fieldNameAttr));
        if (isNotEmpty(serviceName)) attrs.add(attrIfNotEmpty("serviceName", serviceName));
        if (isNotEmpty(attributeName)) attrs.add(attrIfNotEmpty("attributeName", attributeName));
        if (isNotEmpty(entryName)) attrs.add(attrIfNotEmpty("entryName", entryName));
        if (isNotEmpty(parameterName)) attrs.add(attrIfNotEmpty("parameterName", parameterName));
        if (isNotEmpty(title)) attrs.add(attrIfNotEmpty("title", title));
        if (isNotEmpty(tooltip)) attrs.add(attrIfNotEmpty("tooltip", tooltip));
        if (isNotEmpty(useWhen)) attrs.add(attrIfNotEmpty("useWhen", useWhen));
        if (isNotEmpty(ignoreWhen)) attrs.add(attrIfNotEmpty("ignoreWhen", ignoreWhen));
        if (isNotEmpty(idName)) attrs.add(attrIfNotEmpty("idName", idName));
        if (isNotEmpty(tabindex)) attrs.add(attrIfNotEmpty("tabindex", tabindex));
        if (isNotEmpty(titleStyle)) attrs.add(attrIfNotEmpty("titleStyle", titleStyle));
        if (isNotEmpty(titleAreaStyle)) attrs.add(attrIfNotEmpty("titleAreaStyle", titleAreaStyle));
        if (isNotEmpty(widgetStyle)) attrs.add(attrIfNotEmpty("widgetStyle", widgetStyle));
        if (isNotEmpty(widgetAreaStyle)) attrs.add(attrIfNotEmpty("widgetAreaStyle", widgetAreaStyle));
        if (isNotEmpty(tooltipStyle)) attrs.add(attrIfNotEmpty("tooltipStyle", tooltipStyle));
        if (isNotEmpty(requiredFieldStyle)) attrs.add(attrIfNotEmpty("requiredFieldStyle", requiredFieldStyle));
        if (isNotEmpty(event)) attrs.add(attrIfNotEmpty("event", event));
        if (isNotEmpty(action)) attrs.add(attrIfNotEmpty("action", action));
        if (isNotEmpty(redWhen) && !"by-name".equals(redWhen)) attrs.add(attrIfNotEmpty("redWhen", redWhen));

        // Integer attributes
        if (isNotEmpty(position)) {
            try {
                int pos = Integer.parseInt(position);
                if (pos != 1) {
                    attrs.add("position = " + pos);
                }
            } catch (NumberFormatException e) { /* ignore */ }
        }
        if (isNotEmpty(positionSpan)) {
            try {
                int ps = Integer.parseInt(positionSpan);
                if (ps != 0) {
                    attrs.add("positionSpan = " + ps);
                }
            } catch (NumberFormatException e) { /* ignore */ }
        }

        // Boolean attributes
        if ("false".equalsIgnoreCase(encodeOutput)) {
            attrs.add("encodeOutput = false");
        }
        if ("true".equalsIgnoreCase(requiredField)) {
            attrs.add("requiredField = true");
        }
        if ("true".equalsIgnoreCase(sortField)) {
            attrs.add("sortField = true");
        }
        if ("true".equalsIgnoreCase(separateColumn)) {
            attrs.add("separateColumn = true");
        }
        if ("true".equalsIgnoreCase(combinePrevious)) {
            attrs.add("combinePrevious = true");
        }
        if ("true".equalsIgnoreCase(disabled)) {
            attrs.add("disabled = true");
        }

        // Detect and generate field type
        String fieldTypeAnnotation = generateFieldType(fieldElement, formName);
        if (isNotEmpty(fieldTypeAnnotation)) {
            attrs.add(fieldTypeAnnotation);
        }

        sb.append(joinAttrs(attrs.toArray(new String[0])));
        sb.append(")");

        return sb.toString();
    }

    /**
     * Generates the field type annotation (text, hidden, display, dropdown, etc.).
     */
    protected String generateFieldType(Element fieldElement, String formName) {
        // Check for each field type child element
        Element textElement = firstChildElement(fieldElement, "text");
        if (textElement != null) {
            return generateTextField(textElement);
        }

        Element hiddenElement = firstChildElement(fieldElement, "hidden");
        if (hiddenElement != null) {
            return generateHiddenField(hiddenElement);
        }

        Element displayElement = firstChildElement(fieldElement, "display");
        if (displayElement != null) {
            return generateDisplayField(displayElement);
        }

        Element displayEntityElement = firstChildElement(fieldElement, "display-entity");
        if (displayEntityElement != null) {
            return generateDisplayEntityField(displayEntityElement);
        }

        Element dropDownElement = firstChildElement(fieldElement, "drop-down");
        if (dropDownElement != null) {
            return generateDropDownField(dropDownElement);
        }

        Element checkElement = firstChildElement(fieldElement, "check");
        if (checkElement != null) {
            return generateCheckField(checkElement);
        }

        Element radioElement = firstChildElement(fieldElement, "radio");
        if (radioElement != null) {
            return generateRadioField(radioElement);
        }

        Element dateTimeElement = firstChildElement(fieldElement, "date-time");
        if (dateTimeElement != null) {
            return generateDateTimeField(dateTimeElement);
        }

        Element textareaElement = firstChildElement(fieldElement, "textarea");
        if (textareaElement != null) {
            return generateTextareaField(textareaElement);
        }

        Element passwordElement = firstChildElement(fieldElement, "password");
        if (passwordElement != null) {
            return generatePasswordField(passwordElement);
        }

        Element submitElement = firstChildElement(fieldElement, "submit");
        if (submitElement != null) {
            return generateSubmitField(submitElement);
        }

        Element resetElement = firstChildElement(fieldElement, "reset");
        if (resetElement != null) {
            return generateResetField(resetElement);
        }

        Element hyperlinkElement = firstChildElement(fieldElement, "hyperlink");
        if (hyperlinkElement != null) {
            return generateHyperlinkField(hyperlinkElement);
        }

        Element lookupElement = firstChildElement(fieldElement, "lookup");
        if (lookupElement != null) {
            return generateLookupField(lookupElement);
        }

        Element fileElement = firstChildElement(fieldElement, "file");
        if (fileElement != null) {
            return generateFileField(fileElement);
        }

        Element imageElement = firstChildElement(fieldElement, "image");
        if (imageElement != null) {
            return generateImageField(imageElement);
        }

        Element ignoredElement = firstChildElement(fieldElement, "ignored");
        if (ignoredElement != null) {
            return "ignored = @IgnoredField";
        }

        Element textFindElement = firstChildElement(fieldElement, "text-find");
        if (textFindElement != null) {
            return generateTextFindField(textFindElement);
        }

        Element dateFindElement = firstChildElement(fieldElement, "date-find");
        if (dateFindElement != null) {
            return generateDateFindField(dateFindElement);
        }

        Element rangeFindElement = firstChildElement(fieldElement, "range-find");
        if (rangeFindElement != null) {
            return generateRangeFindField(rangeFindElement);
        }

        Element containerElement = firstChildElement(fieldElement, "container");
        if (containerElement != null) {
            return generateContainerField(containerElement);
        }

        Element includeScreenElement = firstChildElement(fieldElement, "include-screen");
        if (includeScreenElement != null) {
            return generateIncludeScreenField(includeScreenElement);
        }

        Element includeFormElement = firstChildElement(fieldElement, "include-form");
        if (includeFormElement != null) {
            return generateIncludeFormField(includeFormElement);
        }

        Element includeMenuElement = firstChildElement(fieldElement, "include-menu");
        if (includeMenuElement != null) {
            return generateIncludeMenuField(includeMenuElement);
        }

        Element includeGridElement = firstChildElement(fieldElement, "include-grid");
        if (includeGridElement != null) {
            return generateIncludeGridField(includeGridElement);
        }

        return "";
    }

    protected String generateTextField(Element element) {
        String size = getAttr(element, "size");
        String maxlength = getAttr(element, "maxlength");
        String defaultValue = getAttr(element, "default-value");
        String placeholder = getAttr(element, "placeholder");
        String mask = getAttr(element, "mask");
        String clientAutocomplete = getAttr(element, "client-autocomplete");
        String readonly = getAttr(element, "read-only");
        String disabled = getAttr(element, "disabled");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(size)) {
            try {
                int s = Integer.parseInt(size);
                if (s != 25) attrs.add("size = " + s);
            } catch (NumberFormatException e) { /* ignore */ }
        }
        if (isNotEmpty(maxlength)) {
            try {
                int ml = Integer.parseInt(maxlength);
                if (ml != 250) attrs.add("maxlength = " + ml);
            } catch (NumberFormatException e) { /* ignore */ }
        }
        if (isNotEmpty(defaultValue)) attrs.add(attrIfNotEmpty("defaultValue", defaultValue));
        if (isNotEmpty(placeholder)) attrs.add(attrIfNotEmpty("placeholder", placeholder));
        if (isNotEmpty(mask)) attrs.add(attrIfNotEmpty("mask", mask));
        if ("false".equalsIgnoreCase(clientAutocomplete)) attrs.add("clientAutocomplete = false");
        if ("true".equalsIgnoreCase(readonly)) attrs.add("readonly = true");
        if ("true".equalsIgnoreCase(disabled)) attrs.add("disabled = true");

        if (attrs.isEmpty()) {
            return "text = @TextField";
        }
        return "text = @TextField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateHiddenField(Element element) {
        String value = getAttr(element, "value");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(value)) attrs.add(attrIfNotEmpty("value", value));

        if (attrs.isEmpty()) {
            return "hidden = @HiddenField";
        }
        return "hidden = @HiddenField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateDisplayField(Element element) {
        String defaultValue = getAttr(element, "default-value");
        String description = getAttr(element, "description");
        String type = getAttr(element, "type");
        String alsoHidden = getAttr(element, "also-hidden");
        String imageLocation = getAttr(element, "image-location");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(defaultValue)) attrs.add(attrIfNotEmpty("defaultValue", defaultValue));
        if (isNotEmpty(description)) attrs.add(attrIfNotEmpty("description", description));
        if (isNotEmpty(type) && !"text".equals(type)) attrs.add(attrIfNotEmpty("type", type));
        if ("false".equalsIgnoreCase(alsoHidden)) attrs.add("alsoHidden = false");
        if (isNotEmpty(imageLocation)) attrs.add(attrIfNotEmpty("imageLocation", imageLocation));

        if (attrs.isEmpty()) {
            return "display = @DisplayField";
        }
        return "display = @DisplayField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateDisplayEntityField(Element element) {
        String entityName = getAttr(element, "entity-name");
        String keyFieldName = getAttr(element, "key-field-name");
        String description = getAttr(element, "description");
        String alsoHidden = getAttr(element, "also-hidden");
        String useCache = getAttr(element, "cache");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(entityName)) attrs.add(attrIfNotEmpty("entityName", entityName));
        if (isNotEmpty(keyFieldName)) attrs.add(attrIfNotEmpty("keyFieldName", keyFieldName));
        if (isNotEmpty(description)) attrs.add(attrIfNotEmpty("description", description));
        if ("false".equalsIgnoreCase(alsoHidden)) attrs.add("alsoHidden = false");
        if ("true".equalsIgnoreCase(useCache)) attrs.add("cache = true");

        // Handle sub-hyperlink
        Element subHyperlink = firstChildElement(element, "sub-hyperlink");
        if (subHyperlink != null) {
            attrs.add("subHyperlink = " + generateSubHyperlink(subHyperlink));
        }

        if (attrs.isEmpty()) {
            return "displayEntity = @DisplayEntityField";
        }
        return "displayEntity = @DisplayEntityField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateDropDownField(Element element) {
        String allowEmpty = getAttr(element, "allow-empty");
        String allowMulti = getAttr(element, "allow-multiple");
        String current = getAttr(element, "current");
        String currentDescription = getAttr(element, "current-description");
        String size = getAttr(element, "size");
        String otherFieldSize = getAttr(element, "other-field-size");
        String textSize = getAttr(element, "text-size");

        List<String> attrs = new ArrayList<>();
        if ("true".equalsIgnoreCase(allowEmpty)) attrs.add("allowEmpty = true");
        if ("true".equalsIgnoreCase(allowMulti)) attrs.add("allowMulti = true");
        if (isNotEmpty(current) && !"first-in-list".equals(current)) attrs.add(attrIfNotEmpty("current", current));
        if (isNotEmpty(currentDescription)) attrs.add(attrIfNotEmpty("currentDescription", currentDescription));
        if (isNotEmpty(size)) {
            try {
                int s = Integer.parseInt(size);
                if (s != 1) attrs.add("size = " + s);
            } catch (NumberFormatException e) { /* ignore */ }
        }
        if (isNotEmpty(otherFieldSize)) {
            try {
                int ofs = Integer.parseInt(otherFieldSize);
                if (ofs != 0) attrs.add("otherFieldSize = " + ofs);
            } catch (NumberFormatException e) { /* ignore */ }
        }
        if (isNotEmpty(textSize)) {
            try {
                int ts = Integer.parseInt(textSize);
                if (ts != 0) attrs.add("textSize = " + ts);
            } catch (NumberFormatException e) { /* ignore */ }
        }

        // Process options and entity-options
        List<String> options = generateDropDownOptions(element);
        if (!options.isEmpty()) {
            attrs.add("options = {" + String.join(", ", options) + "}");
        }

        Element entityOptions = firstChildElement(element, "entity-options");
        if (entityOptions != null) {
            attrs.add("entityOptions = " + generateEntityOptions(entityOptions));
        }

        Element listOptions = firstChildElement(element, "list-options");
        if (listOptions != null) {
            attrs.add("listOptions = " + generateListOptions(listOptions));
        }

        if (attrs.isEmpty()) {
            return "dropDown = @DropDownField";
        }
        return "dropDown = @DropDownField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected List<String> generateDropDownOptions(Element dropDownElement) {
        List<String> options = new ArrayList<>();
        for (Element option : childElementList(dropDownElement, "option")) {
            String key = getAttr(option, "key");
            String description = getAttr(option, "description");
            List<String> optAttrs = new ArrayList<>();
            if (isNotEmpty(key)) optAttrs.add(attrIfNotEmpty("key", key));
            if (isNotEmpty(description)) optAttrs.add(attrIfNotEmpty("description", description));
            options.add("@Option(" + joinAttrs(optAttrs.toArray(new String[0])) + ")");
        }
        return options;
    }

    protected String generateEntityOptions(Element element) {
        String entityName = getAttr(element, "entity-name");
        String description = getAttr(element, "description");
        String keyFieldName = getAttr(element, "key-field-name");
        String filterByDate = getAttr(element, "filter-by-date");
        String useCache = getAttr(element, "cache");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(entityName)) attrs.add(attrIfNotEmpty("entityName", entityName));
        if (isNotEmpty(description)) attrs.add(attrIfNotEmpty("description", description));
        if (isNotEmpty(keyFieldName)) attrs.add(attrIfNotEmpty("keyFieldName", keyFieldName));
        if (isNotEmpty(filterByDate)) attrs.add(attrIfNotEmpty("filterByDate", filterByDate));
        if ("true".equalsIgnoreCase(useCache)) attrs.add("cache = true");

        // Handle entity-constraint
        List<Element> constraints = childElementList(element, "entity-constraint");
        if (!constraints.isEmpty()) {
            StringBuilder ecBuilder = new StringBuilder();
            ecBuilder.append("constraints = {");
            boolean first = true;
            for (Element ec : constraints) {
                if (!first) ecBuilder.append(", ");
                ecBuilder.append(generateEntityConstraint(ec));
                first = false;
            }
            ecBuilder.append("}");
            attrs.add(ecBuilder.toString());
        }

        // Handle entity-order-by
        List<Element> orderBys = childElementList(element, "entity-order-by");
        if (!orderBys.isEmpty()) {
            StringBuilder obBuilder = new StringBuilder();
            obBuilder.append("orderBy = {");
            boolean first = true;
            for (Element ob : orderBys) {
                if (!first) obBuilder.append(", ");
                String fieldName = getAttr(ob, "field-name");
                obBuilder.append("@EntityOrderBy(").append(attrIfNotEmpty("fieldName", fieldName)).append(")");
                first = false;
            }
            obBuilder.append("}");
            attrs.add(obBuilder.toString());
        }

        return "@EntityOptions(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateEntityConstraint(Element element) {
        String name = getAttr(element, "name");
        String value = getAttr(element, "value");
        String envName = getAttr(element, "env-name");
        String operator = getAttr(element, "operator");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(name)) attrs.add(attrIfNotEmpty("name", name));
        if (isNotEmpty(value)) attrs.add(attrIfNotEmpty("value", value));
        if (isNotEmpty(envName)) attrs.add(attrIfNotEmpty("envName", envName));
        if (isNotEmpty(operator) && !"equals".equals(operator)) attrs.add(attrIfNotEmpty("operator", operator));

        return "@EntityConstraint(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateListOptions(Element element) {
        String listName = getAttr(element, "list-name");
        String listEntryName = getAttr(element, "list-entry-name");
        String keyName = getAttr(element, "key-name");
        String description = getAttr(element, "description");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(listName)) attrs.add(attrIfNotEmpty("listName", listName));
        if (isNotEmpty(listEntryName)) attrs.add(attrIfNotEmpty("listEntryName", listEntryName));
        if (isNotEmpty(keyName)) attrs.add(attrIfNotEmpty("keyName", keyName));
        if (isNotEmpty(description)) attrs.add(attrIfNotEmpty("description", description));

        return "@ListOptions(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateCheckField(Element element) {
        String allChecked = getAttr(element, "all-checked");
        String noCurrentSelectedKey = getAttr(element, "no-current-selected-key");

        List<String> attrs = new ArrayList<>();
        if ("true".equalsIgnoreCase(allChecked)) attrs.add("allChecked = true");
        if (isNotEmpty(noCurrentSelectedKey)) attrs.add(attrIfNotEmpty("noCurrentSelectedKey", noCurrentSelectedKey));

        // Process options
        List<String> options = generateDropDownOptions(element);
        if (!options.isEmpty()) {
            attrs.add("options = {" + String.join(", ", options) + "}");
        }

        Element entityOptions = firstChildElement(element, "entity-options");
        if (entityOptions != null) {
            attrs.add("entityOptions = " + generateEntityOptions(entityOptions));
        }

        if (attrs.isEmpty()) {
            return "check = @CheckField";
        }
        return "check = @CheckField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateRadioField(Element element) {
        String noCurrentSelectedKey = getAttr(element, "no-current-selected-key");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(noCurrentSelectedKey)) attrs.add(attrIfNotEmpty("noCurrentSelectedKey", noCurrentSelectedKey));

        // Process options
        List<String> options = generateDropDownOptions(element);
        if (!options.isEmpty()) {
            attrs.add("options = {" + String.join(", ", options) + "}");
        }

        Element entityOptions = firstChildElement(element, "entity-options");
        if (entityOptions != null) {
            attrs.add("entityOptions = " + generateEntityOptions(entityOptions));
        }

        if (attrs.isEmpty()) {
            return "radio = @RadioField";
        }
        return "radio = @RadioField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateDateTimeField(Element element) {
        String defaultValue = getAttr(element, "default-value");
        String type = getAttr(element, "type");
        String inputMethod = getAttr(element, "input-method");
        String clock = getAttr(element, "clock");
        String mask = getAttr(element, "mask");
        String step = getAttr(element, "step");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(defaultValue)) attrs.add(attrIfNotEmpty("defaultValue", defaultValue));
        if (isNotEmpty(type) && !"timestamp".equals(type)) attrs.add(attrIfNotEmpty("type", type));
        if (isNotEmpty(inputMethod) && !"time-dropdown".equals(inputMethod)) attrs.add(attrIfNotEmpty("inputMethod", inputMethod));
        if (isNotEmpty(clock) && !"12".equals(clock)) attrs.add(attrIfNotEmpty("clock", clock));
        if (isNotEmpty(mask)) attrs.add(attrIfNotEmpty("mask", mask));
        if (isNotEmpty(step)) {
            try {
                int s = Integer.parseInt(step);
                if (s != 1) attrs.add("step = " + s);
            } catch (NumberFormatException e) { /* ignore */ }
        }

        if (attrs.isEmpty()) {
            return "dateTime = @DateTimeField";
        }
        return "dateTime = @DateTimeField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateTextareaField(Element element) {
        String cols = getAttr(element, "cols");
        String rows = getAttr(element, "rows");
        String defaultValue = getAttr(element, "default-value");
        String readOnly = getAttr(element, "read-only");
        String maxlength = getAttr(element, "maxlength");
        String visualEditorEnable = getAttr(element, "visual-editor-enable");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(cols)) {
            try {
                int c = Integer.parseInt(cols);
                if (c != 60) attrs.add("cols = " + c);
            } catch (NumberFormatException e) { /* ignore */ }
        }
        if (isNotEmpty(rows)) {
            try {
                int r = Integer.parseInt(rows);
                if (r != 6) attrs.add("rows = " + r);
            } catch (NumberFormatException e) { /* ignore */ }
        }
        if (isNotEmpty(defaultValue)) attrs.add(attrIfNotEmpty("defaultValue", defaultValue));
        if ("true".equalsIgnoreCase(readOnly)) attrs.add("readonly = true");
        if (isNotEmpty(maxlength)) {
            try {
                int ml = Integer.parseInt(maxlength);
                if (ml != 0) attrs.add("maxlength = " + ml);
            } catch (NumberFormatException e) { /* ignore */ }
        }
        if ("true".equalsIgnoreCase(visualEditorEnable)) attrs.add("visualEditorEnable = true");

        if (attrs.isEmpty()) {
            return "textarea = @TextareaField";
        }
        return "textarea = @TextareaField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generatePasswordField(Element element) {
        String size = getAttr(element, "size");
        String maxlength = getAttr(element, "maxlength");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(size)) {
            try {
                int s = Integer.parseInt(size);
                if (s != 25) attrs.add("size = " + s);
            } catch (NumberFormatException e) { /* ignore */ }
        }
        if (isNotEmpty(maxlength)) {
            try {
                int ml = Integer.parseInt(maxlength);
                if (ml != 250) attrs.add("maxlength = " + ml);
            } catch (NumberFormatException e) { /* ignore */ }
        }

        if (attrs.isEmpty()) {
            return "password = @PasswordField";
        }
        return "password = @PasswordField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateSubmitField(Element element) {
        String buttonType = getAttr(element, "button-type");
        String confirmationMessage = getAttr(element, "confirmation-message");
        String imageLocation = getAttr(element, "image-location");
        String backgroundColor = getAttr(element, "background-color");
        String requestConfirmation = getAttr(element, "request-confirmation");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(buttonType) && !"button".equals(buttonType)) attrs.add(attrIfNotEmpty("buttonType", buttonType));
        if (isNotEmpty(confirmationMessage)) attrs.add(attrIfNotEmpty("confirmationMessage", confirmationMessage));
        if (isNotEmpty(imageLocation)) attrs.add(attrIfNotEmpty("imageLocation", imageLocation));
        if (isNotEmpty(backgroundColor)) attrs.add(attrIfNotEmpty("backgroundColor", backgroundColor));
        if ("true".equalsIgnoreCase(requestConfirmation)) attrs.add("requestConfirmation = true");

        if (attrs.isEmpty()) {
            return "submit = @SubmitField";
        }
        return "submit = @SubmitField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateResetField(Element element) {
        // Reset field typically has no special attributes
        return "reset = @ResetField";
    }

    protected String generateHyperlinkField(Element element) {
        String target = getAttr(element, "target");
        String targetType = getAttr(element, "target-type");
        String description = getAttr(element, "description");
        String alsoHidden = getAttr(element, "also-hidden");
        String linkStyle = getAttr(element, "link-style");
        String linkType = getAttr(element, "link-type");
        String targetWindow = getAttr(element, "target-window");
        String useWhen = getAttr(element, "use-when");
        String confirmationMessage = getAttr(element, "confirmation-message");
        String requestConfirmation = getAttr(element, "request-confirmation");
        String urlMode = getAttr(element, "url-mode");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(target)) attrs.add(attrIfNotEmpty("target", target));
        if (isNotEmpty(targetType) && !"intra-app".equals(targetType)) {
            String enumValue = targetType.replace("-", "_").toUpperCase();
            attrs.add("urlMode = UrlMode." + enumValue);
        }
        if (isNotEmpty(description)) attrs.add(attrIfNotEmpty("description", description));
        if ("false".equalsIgnoreCase(alsoHidden)) attrs.add("alsoHidden = false");
        if (isNotEmpty(linkStyle)) attrs.add(attrIfNotEmpty("linkStyle", linkStyle));
        if (isNotEmpty(linkType)) attrs.add(attrIfNotEmpty("linkType", linkType));
        if (isNotEmpty(targetWindow)) attrs.add(attrIfNotEmpty("targetWindow", targetWindow));
        if (isNotEmpty(useWhen)) attrs.add(attrIfNotEmpty("useWhen", useWhen));
        if (isNotEmpty(confirmationMessage)) attrs.add(attrIfNotEmpty("confirmationMessage", confirmationMessage));
        // SCIPIO: 4.0.0: HyperlinkField.requestConfirmation is String, not boolean
        if ("true".equalsIgnoreCase(requestConfirmation)) attrs.add("requestConfirmation = \"true\"");
        if (isNotEmpty(urlMode) && !"intra-app".equals(urlMode)) attrs.add(attrIfNotEmpty("urlMode", urlMode));

        // Handle parameters
        List<String> params = generateHyperlinkParameters(element);
        if (!params.isEmpty()) {
            attrs.add("parameters = {" + String.join(", ", params) + "}");
        }

        if (attrs.isEmpty()) {
            return "hyperlink = @HyperlinkField";
        }
        return "hyperlink = @HyperlinkField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected List<String> generateHyperlinkParameters(Element hyperlinkElement) {
        List<String> params = new ArrayList<>();
        for (Element param : childElementList(hyperlinkElement, "parameter")) {
            String paramName = getAttr(param, "param-name");
            String value = getAttr(param, "value");
            String fromField = getAttr(param, "from-field");
            List<String> paramAttrs = new ArrayList<>();
            if (isNotEmpty(paramName)) paramAttrs.add(attrIfNotEmpty("paramName", paramName));
            if (isNotEmpty(value)) paramAttrs.add(attrIfNotEmpty("value", value));
            if (isNotEmpty(fromField)) paramAttrs.add(attrIfNotEmpty("fromField", fromField));
            params.add("@ParameterDef(" + joinAttrs(paramAttrs.toArray(new String[0])) + ")");
        }
        return params;
    }

    protected String generateSubHyperlink(Element element) {
        String target = getAttr(element, "target");
        String description = getAttr(element, "description");
        String linkStyle = getAttr(element, "link-style");
        String linkType = getAttr(element, "link-type");
        String useWhen = getAttr(element, "use-when");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(target)) attrs.add(attrIfNotEmpty("target", target));
        if (isNotEmpty(description)) attrs.add(attrIfNotEmpty("description", description));
        if (isNotEmpty(linkStyle)) attrs.add(attrIfNotEmpty("linkStyle", linkStyle));
        if (isNotEmpty(linkType)) attrs.add(attrIfNotEmpty("linkType", linkType));
        if (isNotEmpty(useWhen)) attrs.add(attrIfNotEmpty("useWhen", useWhen));

        // Handle parameters
        List<String> params = generateHyperlinkParameters(element);
        if (!params.isEmpty()) {
            attrs.add("parameters = {" + String.join(", ", params) + "}");
        }

        return "@SubHyperlink(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateLookupField(Element element) {
        String targetFormName = getAttr(element, "target-form-name");
        String descriptionFieldName = getAttr(element, "description-field-name");
        String size = getAttr(element, "size");
        String maxlength = getAttr(element, "maxlength");
        String defaultValue = getAttr(element, "default-value");
        String presentation = getAttr(element, "presentation");
        String initiallyCollapsed = getAttr(element, "initially-collapsed");
        String showDescription = getAttr(element, "show-description");
        String fadeBackground = getAttr(element, "fade-background");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(targetFormName)) attrs.add(attrIfNotEmpty("targetFormName", targetFormName));
        if (isNotEmpty(descriptionFieldName)) attrs.add(attrIfNotEmpty("descriptionFieldName", descriptionFieldName));
        if (isNotEmpty(size)) {
            try {
                int s = Integer.parseInt(size);
                if (s != 25) attrs.add("size = " + s);
            } catch (NumberFormatException e) { /* ignore */ }
        }
        if (isNotEmpty(maxlength)) {
            try {
                int ml = Integer.parseInt(maxlength);
                if (ml != 250) attrs.add("maxlength = " + ml);
            } catch (NumberFormatException e) { /* ignore */ }
        }
        if (isNotEmpty(defaultValue)) attrs.add(attrIfNotEmpty("defaultValue", defaultValue));
        if (isNotEmpty(presentation) && !"layer".equals(presentation)) attrs.add(attrIfNotEmpty("presentation", presentation));
        if ("true".equalsIgnoreCase(initiallyCollapsed)) attrs.add("initiallyCollapsed = true");
        if ("true".equalsIgnoreCase(showDescription)) attrs.add("showDescription = true");
        if (isNotEmpty(fadeBackground)) attrs.add(attrIfNotEmpty("fadeBackground", fadeBackground));

        if (attrs.isEmpty()) {
            return "lookup = @LookupField";
        }
        return "lookup = @LookupField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateFileField(Element element) {
        String size = getAttr(element, "size");
        String maxlength = getAttr(element, "maxlength");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(size)) {
            try {
                int s = Integer.parseInt(size);
                if (s != 25) attrs.add("size = " + s);
            } catch (NumberFormatException e) { /* ignore */ }
        }
        if (isNotEmpty(maxlength)) {
            try {
                int ml = Integer.parseInt(maxlength);
                if (ml != 250) attrs.add("maxlength = " + ml);
            } catch (NumberFormatException e) { /* ignore */ }
        }

        if (attrs.isEmpty()) {
            return "file = @FileField";
        }
        return "file = @FileField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateImageField(Element element) {
        String defaultValue = getAttr(element, "default-value");
        String value = getAttr(element, "value");
        String description = getAttr(element, "description");
        String style = getAttr(element, "style");
        String border = getAttr(element, "border");
        String width = getAttr(element, "width");
        String height = getAttr(element, "height");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(defaultValue)) attrs.add(attrIfNotEmpty("defaultValue", defaultValue));
        if (isNotEmpty(value)) attrs.add(attrIfNotEmpty("value", value));
        if (isNotEmpty(description)) attrs.add(attrIfNotEmpty("description", description));
        if (isNotEmpty(style)) attrs.add(attrIfNotEmpty("style", style));
        if (isNotEmpty(border)) attrs.add(attrIfNotEmpty("border", border));
        if (isNotEmpty(width)) attrs.add(attrIfNotEmpty("width", width));
        if (isNotEmpty(height)) attrs.add(attrIfNotEmpty("height", height));

        if (attrs.isEmpty()) {
            return "image = @ImageField";
        }
        return "image = @ImageField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateTextFindField(Element element) {
        String size = getAttr(element, "size");
        String maxlength = getAttr(element, "maxlength");
        String defaultValue = getAttr(element, "default-value");
        String defaultOption = getAttr(element, "default-option");
        String ignoreCase = getAttr(element, "ignore-case");
        String hideOptions = getAttr(element, "hide-options");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(size)) {
            try {
                int s = Integer.parseInt(size);
                if (s != 25) attrs.add("size = " + s);
            } catch (NumberFormatException e) { /* ignore */ }
        }
        if (isNotEmpty(maxlength)) {
            try {
                int ml = Integer.parseInt(maxlength);
                if (ml != 250) attrs.add("maxlength = " + ml);
            } catch (NumberFormatException e) { /* ignore */ }
        }
        if (isNotEmpty(defaultValue)) attrs.add(attrIfNotEmpty("defaultValue", defaultValue));
        if (isNotEmpty(defaultOption) && !"contains".equals(defaultOption)) attrs.add(attrIfNotEmpty("defaultOption", defaultOption));
        if ("false".equalsIgnoreCase(ignoreCase)) attrs.add("ignoreCase = false");
        if ("true".equalsIgnoreCase(hideOptions)) attrs.add("hideOptions = \"true\""); // SCIPIO: 4.0.0: String attribute

        if (attrs.isEmpty()) {
            return "textFind = @TextFindField";
        }
        return "textFind = @TextFindField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateDateFindField(Element element) {
        String type = getAttr(element, "type");
        String defaultValue = getAttr(element, "default-value");
        String defaultOptionFrom = getAttr(element, "default-option-from");
        String defaultOptionThru = getAttr(element, "default-option-thru");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(type) && !"timestamp".equals(type)) attrs.add(attrIfNotEmpty("type", type));
        if (isNotEmpty(defaultValue)) attrs.add(attrIfNotEmpty("defaultValue", defaultValue));
        if (isNotEmpty(defaultOptionFrom) && !"greaterThanEqualTo".equals(defaultOptionFrom)) attrs.add(attrIfNotEmpty("defaultOptionFrom", defaultOptionFrom));
        if (isNotEmpty(defaultOptionThru) && !"lessThanEqualTo".equals(defaultOptionThru)) attrs.add(attrIfNotEmpty("defaultOptionThru", defaultOptionThru));

        if (attrs.isEmpty()) {
            return "dateFind = @DateFindField";
        }
        return "dateFind = @DateFindField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateRangeFindField(Element element) {
        String size = getAttr(element, "size");
        String maxlength = getAttr(element, "maxlength");
        String defaultValue = getAttr(element, "default-value");
        String defaultOptionFrom = getAttr(element, "default-option-from");
        String defaultOptionThru = getAttr(element, "default-option-thru");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(size)) {
            try {
                int s = Integer.parseInt(size);
                if (s != 6) attrs.add("size = " + s);
            } catch (NumberFormatException e) { /* ignore */ }
        }
        if (isNotEmpty(maxlength)) {
            try {
                int ml = Integer.parseInt(maxlength);
                if (ml != 20) attrs.add("maxlength = " + ml);
            } catch (NumberFormatException e) { /* ignore */ }
        }
        if (isNotEmpty(defaultValue)) attrs.add(attrIfNotEmpty("defaultValue", defaultValue));
        if (isNotEmpty(defaultOptionFrom) && !"greaterThanEqualTo".equals(defaultOptionFrom)) attrs.add(attrIfNotEmpty("defaultOptionFrom", defaultOptionFrom));
        if (isNotEmpty(defaultOptionThru) && !"lessThanEqualTo".equals(defaultOptionThru)) attrs.add(attrIfNotEmpty("defaultOptionThru", defaultOptionThru));

        if (attrs.isEmpty()) {
            return "rangeFind = @RangeFindField";
        }
        return "rangeFind = @RangeFindField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateContainerField(Element element) {
        String id = getAttr(element, "id");
        String style = getAttr(element, "style");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(id)) attrs.add(attrIfNotEmpty("id", id));
        if (isNotEmpty(style)) attrs.add(attrIfNotEmpty("style", style));

        if (attrs.isEmpty()) {
            return "container = @ContainerField";
        }
        return "container = @ContainerField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateIncludeScreenField(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(name)) attrs.add(attrIfNotEmpty("name", name));
        if (isNotEmpty(location)) attrs.add(attrIfNotEmpty("location", location));

        return "includeScreen = @IncludeScreenField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateIncludeFormField(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(name)) attrs.add(attrIfNotEmpty("name", name));
        if (isNotEmpty(location)) attrs.add(attrIfNotEmpty("location", location));

        return "includeForm = @IncludeFormField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateIncludeMenuField(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(name)) attrs.add(attrIfNotEmpty("name", name));
        if (isNotEmpty(location)) attrs.add(attrIfNotEmpty("location", location));

        return "includeMenu = @IncludeMenuField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateIncludeGridField(Element element) {
        String name = getAttr(element, "name");
        String location = getAttr(element, "location");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(name)) attrs.add(attrIfNotEmpty("name", name));
        if (isNotEmpty(location)) attrs.add(attrIfNotEmpty("location", location));

        return "includeGrid = @IncludeGridField(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates @AltTarget annotation.
     */
    protected String generateAltTarget(Element element) {
        String useWhen = getAttr(element, "use-when");
        String target = getAttr(element, "target");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(useWhen)) attrs.add(attrIfNotEmpty("useWhen", useWhen));
        if (isNotEmpty(target)) attrs.add(attrIfNotEmpty("target", target));

        return "@AltTarget(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates @FormActions annotation.
     */
    protected String generateFormActions(Element actionsElement, String formName) {
        // Group actions by type
        List<String> setActions = new ArrayList<>();
        List<String> serviceActions = new ArrayList<>();
        List<String> scriptActions = new ArrayList<>();
        List<String> entityOneActions = new ArrayList<>();
        List<String> entityConditionActions = new ArrayList<>();
        List<String> propertyToFieldActions = new ArrayList<>();

        for (Element child : childElementList(actionsElement)) {
            String tagName = child.getNodeName();
            String annotation = generateActionAnnotation(child, tagName, formName);
            if (isNotEmpty(annotation) && !annotation.startsWith("// TODO:")) {
                switch (tagName) {
                    case "set":
                        setActions.add(annotation);
                        break;
                    case "service":
                        serviceActions.add(annotation);
                        break;
                    case "script":
                        scriptActions.add(annotation);
                        break;
                    case "entity-one":
                        entityOneActions.add(annotation);
                        break;
                    case "entity-condition":
                        entityConditionActions.add(annotation);
                        break;
                    case "property-to-field":
                        propertyToFieldActions.add(annotation);
                        break;
                }
            }
        }

        // Build @FormActions with type-specific arrays
        List<String> formActionAttrs = new ArrayList<>();
        if (!setActions.isEmpty()) {
            formActionAttrs.add("set = {" + String.join(", ", setActions) + "}");
        }
        if (!serviceActions.isEmpty()) {
            formActionAttrs.add("service = {" + String.join(", ", serviceActions) + "}");
        }
        if (!scriptActions.isEmpty()) {
            formActionAttrs.add("script = {" + String.join(", ", scriptActions) + "}");
        }
        if (!entityOneActions.isEmpty()) {
            formActionAttrs.add("entityOne = {" + String.join(", ", entityOneActions) + "}");
        }
        if (!entityConditionActions.isEmpty()) {
            formActionAttrs.add("entityCondition = {" + String.join(", ", entityConditionActions) + "}");
        }
        if (!propertyToFieldActions.isEmpty()) {
            formActionAttrs.add("propertyToField = {" + String.join(", ", propertyToFieldActions) + "}");
        }

        if (formActionAttrs.isEmpty()) {
            return "";
        }

        return "@FormActions(" + String.join(", ", formActionAttrs) + ")";
    }

    /**
     * Generates @RowActions annotation.
     */
    protected String generateRowActions(Element rowActionsElement, String formName) {
        // Group actions by type
        List<String> setActions = new ArrayList<>();
        List<String> serviceActions = new ArrayList<>();
        List<String> scriptActions = new ArrayList<>();
        List<String> entityOneActions = new ArrayList<>();
        List<String> entityConditionActions = new ArrayList<>();
        List<String> propertyToFieldActions = new ArrayList<>();

        for (Element child : childElementList(rowActionsElement)) {
            String tagName = child.getNodeName();
            String annotation = generateActionAnnotation(child, tagName, formName);
            if (isNotEmpty(annotation) && !annotation.startsWith("// TODO:")) {
                switch (tagName) {
                    case "set":
                        setActions.add(annotation);
                        break;
                    case "service":
                        serviceActions.add(annotation);
                        break;
                    case "script":
                        scriptActions.add(annotation);
                        break;
                    case "entity-one":
                        entityOneActions.add(annotation);
                        break;
                    case "entity-condition":
                        entityConditionActions.add(annotation);
                        break;
                    case "property-to-field":
                        propertyToFieldActions.add(annotation);
                        break;
                }
            }
        }

        // Build @RowActions with type-specific arrays
        List<String> rowActionAttrs = new ArrayList<>();
        if (!setActions.isEmpty()) {
            rowActionAttrs.add("set = {" + String.join(", ", setActions) + "}");
        }
        if (!serviceActions.isEmpty()) {
            rowActionAttrs.add("service = {" + String.join(", ", serviceActions) + "}");
        }
        if (!scriptActions.isEmpty()) {
            rowActionAttrs.add("script = {" + String.join(", ", scriptActions) + "}");
        }
        if (!entityOneActions.isEmpty()) {
            rowActionAttrs.add("entityOne = {" + String.join(", ", entityOneActions) + "}");
        }
        if (!entityConditionActions.isEmpty()) {
            rowActionAttrs.add("entityCondition = {" + String.join(", ", entityConditionActions) + "}");
        }
        if (!propertyToFieldActions.isEmpty()) {
            rowActionAttrs.add("propertyToField = {" + String.join(", ", propertyToFieldActions) + "}");
        }

        if (rowActionAttrs.isEmpty()) {
            return "";
        }

        return "@RowActions(" + String.join(", ", rowActionAttrs) + ")";
    }

    /**
     * Generates a single action annotation.
     */
    protected String generateActionAnnotation(Element element, String tagName, String formName) {
        switch (tagName) {
            case "set":
                return generateSetAction(element);
            case "script":
                return generateScriptAction(element, formName);
            case "entity-one":
                return generateEntityOneAction(element);
            case "entity-and":
                return generateEntityAndAction(element);
            case "service":
                return generateServiceAction(element);
            default:
                return "// TODO: Unsupported action: " + tagName;
        }
    }

    protected String generateSetAction(Element element) {
        String field = getAttr(element, "field");
        String value = getAttr(element, "value");
        String fromField = getAttr(element, "from-field");
        String type = getAttr(element, "type");
        String defaultValue = getAttr(element, "default-value");
        String global = getAttr(element, "global");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(field)) attrs.add(attrIfNotEmpty("field", field));
        if (isNotEmpty(value)) attrs.add(attrIfNotEmpty("value", value));
        if (isNotEmpty(fromField)) attrs.add(attrIfNotEmpty("fromField", fromField));
        if (isNotEmpty(type)) attrs.add(attrIfNotEmpty("type", type));
        if (isNotEmpty(defaultValue)) attrs.add(attrIfNotEmpty("defaultValue", defaultValue));
        if ("true".equalsIgnoreCase(global)) attrs.add("global = true");

        return "@SetAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateScriptAction(Element element, String formName) {
        String location = getAttr(element, "location");
        String lang = getAttr(element, "lang", "groovy");

        // Check for inline script (CDATA)
        String code = element.getTextContent();
        if (isNotEmpty(code) && code.trim().length() > 0) {
            scriptCounter++;
            location = extractScript(formName, scriptCounter, lang, code.trim());
        }

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(location)) attrs.add(attrIfNotEmpty("location", location));
        if (!"groovy".equals(lang)) attrs.add(attrIfNotEmpty("lang", lang));

        return "@ScriptAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateEntityOneAction(Element element) {
        String entityName = getAttr(element, "entity-name");
        String valueField = getAttr(element, "value-field");
        String useCache = getAttr(element, "use-cache");
        String autoFieldMap = getAttr(element, "auto-field-map");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(entityName)) attrs.add(attrIfNotEmpty("entityName", entityName));
        if (isNotEmpty(valueField)) attrs.add(attrIfNotEmpty("valueField", valueField));
        if ("true".equalsIgnoreCase(useCache)) attrs.add("useCache = true");
        if ("false".equalsIgnoreCase(autoFieldMap)) attrs.add("autoFieldMap = false");

        return "@EntityOneAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateEntityAndAction(Element element) {
        String entityName = getAttr(element, "entity-name");
        String list = getAttr(element, "list");
        String useCache = getAttr(element, "use-cache");
        String filterByDate = getAttr(element, "filter-by-date");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(entityName)) attrs.add(attrIfNotEmpty("entityName", entityName));
        if (isNotEmpty(list)) attrs.add(attrIfNotEmpty("list", list));
        if ("true".equalsIgnoreCase(useCache)) attrs.add("useCache = true");
        if (isNotEmpty(filterByDate)) attrs.add(attrIfNotEmpty("filterByDate", filterByDate));

        return "@EntityAndAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    protected String generateServiceAction(Element element) {
        String serviceName = getAttr(element, "service-name");
        // Support both result-map and result-map-name (form uses result-map)
        String resultMapName = getAttr(element, "result-map");
        if (isEmpty(resultMapName)) {
            resultMapName = getAttr(element, "result-map-name");
        }
        String resultMapList = getAttr(element, "result-map-list");
        if (isEmpty(resultMapList)) {
            resultMapList = getAttr(element, "result-map-list-name");
        }
        String resultMapField = getAttr(element, "result-map-field");
        String autoFieldMap = getAttr(element, "auto-field-map");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(serviceName)) attrs.add(attrIfNotEmpty("serviceName", serviceName));
        if (isNotEmpty(resultMapName)) attrs.add(attrIfNotEmpty("resultMapName", resultMapName));
        if (isNotEmpty(resultMapList)) attrs.add(attrIfNotEmpty("resultMapList", resultMapList));
        if (isNotEmpty(resultMapField)) attrs.add(attrIfNotEmpty("resultMapField", resultMapField));
        if ("false".equalsIgnoreCase(autoFieldMap)) attrs.add("autoFieldMap = false");

        // Process field-map child elements
        List<Element> fieldMapElements = childElementList(element, "field-map");
        if (!fieldMapElements.isEmpty()) {
            List<String> fieldMaps = new ArrayList<>();
            for (Element fieldMapElement : fieldMapElements) {
                String fieldName = getAttr(fieldMapElement, "field-name");
                String fromField = getAttr(fieldMapElement, "from-field");
                if (isEmpty(fromField)) {
                    fromField = getAttr(fieldMapElement, "env-name");
                }
                String value = getAttr(fieldMapElement, "value");

                List<String> fmAttrs = new ArrayList<>();
                if (isNotEmpty(fieldName)) fmAttrs.add(attrIfNotEmpty("fieldName", fieldName));
                if (isNotEmpty(fromField)) fmAttrs.add(attrIfNotEmpty("fromField", fromField));
                if (isNotEmpty(value)) fmAttrs.add(attrIfNotEmpty("value", value));

                if (!fmAttrs.isEmpty()) {
                    fieldMaps.add("@FieldMap(" + joinAttrs(fmAttrs.toArray(new String[0])) + ")");
                }
            }
            if (!fieldMaps.isEmpty()) {
                attrs.add("fieldMaps = {" + String.join(", ", fieldMaps) + "}");
            }
        }

        return "@ServiceAction(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Generates @SortOrder annotation.
     */
    protected String generateSortOrder(Element sortOrderElement) {
        List<String> sortFields = new ArrayList<>();
        List<String> lastFields = new ArrayList<>();

        for (Element sortField : childElementList(sortOrderElement, "sort-field")) {
            String name = getAttr(sortField, "name");
            String position = getAttr(sortField, "position");
            List<String> attrs = new ArrayList<>();
            if (isNotEmpty(name)) attrs.add(attrIfNotEmpty("name", name));
            if (isNotEmpty(position)) {
                try {
                    int pos = Integer.parseInt(position);
                    if (pos != 1) attrs.add("position = " + pos);
                } catch (NumberFormatException e) { /* ignore */ }
            }
            sortFields.add("@SortField(" + joinAttrs(attrs.toArray(new String[0])) + ")");
        }

        for (Element lastField : childElementList(sortOrderElement, "last-field")) {
            String name = getAttr(lastField, "name");
            String position = getAttr(lastField, "position");
            List<String> attrs = new ArrayList<>();
            if (isNotEmpty(name)) attrs.add(attrIfNotEmpty("name", name));
            if (isNotEmpty(position)) {
                try {
                    int pos = Integer.parseInt(position);
                    if (pos != 1) attrs.add("position = " + pos);
                } catch (NumberFormatException e) { /* ignore */ }
            }
            lastFields.add("@LastField(" + joinAttrs(attrs.toArray(new String[0])) + ")");
        }

        StringBuilder sb = new StringBuilder();
        sb.append("@SortOrder(");

        List<String> parts = new ArrayList<>();
        if (!sortFields.isEmpty()) {
            parts.add("sortFields = {" + String.join(", ", sortFields) + "}");
        }
        if (!lastFields.isEmpty()) {
            parts.add("lastFields = {" + String.join(", ", lastFields) + "}");
        }

        sb.append(String.join(", ", parts));
        sb.append(")");

        return sb.toString();
    }

    /**
     * Generates @OnEventUpdateArea annotation.
     */
    protected String generateOnEventUpdateArea(Element element) {
        String eventType = getAttr(element, "event-type");
        String areaId = getAttr(element, "area-id");
        String areaTarget = getAttr(element, "area-target");

        List<String> attrs = new ArrayList<>();
        if (isNotEmpty(eventType)) attrs.add(attrIfNotEmpty("eventType", eventType));
        if (isNotEmpty(areaId)) attrs.add(attrIfNotEmpty("areaId", areaId));
        if (isNotEmpty(areaTarget)) attrs.add(attrIfNotEmpty("areaTarget", areaTarget));

        return "@OnEventUpdateArea(" + joinAttrs(attrs.toArray(new String[0])) + ")";
    }

    /**
     * Converts a form name to a valid Java interface name.
     */
    protected String toInterfaceName(String formName) {
        String result = formName.replaceAll("[^a-zA-Z0-9_]", "_");
        if (result.length() > 0 && Character.isDigit(result.charAt(0))) {
            result = "_" + result;
        }
        return result;
    }
}
