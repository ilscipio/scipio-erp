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
package org.ofbiz.widget.renderer;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.util.EntityListIterator;
import org.ofbiz.widget.WidgetWorker;
import org.ofbiz.widget.model.AbstractModelAction;
import org.ofbiz.widget.model.FieldInfo;
import org.ofbiz.widget.model.ModelForm;
import org.ofbiz.widget.model.ModelForm.FieldGroup;
import org.ofbiz.widget.model.ModelForm.FieldGroupBase;
import org.ofbiz.widget.model.ModelFormField;
import org.ofbiz.widget.model.ModelGrid;

/**
 * A form rendering engine.
 * 
 */
public class FormRenderer {

    /*
     * ----------------------------------------------------------------------- *
     *                     DEVELOPERS PLEASE READ
     * ----------------------------------------------------------------------- *
     * 
     * An instance of this class is created by each thread for each form that
     * is rendered. If you need to keep track of things while rendering, then
     * this is the place to do it. In other words, feel free to modify this
     * object's state (except for the final fields of course).
     * 
     */

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static String getCurrentContainerId(ModelForm modelForm, Map<String, Object> context) {
        Locale locale = UtilMisc.ensureLocale(context.get("locale"));
        String retVal = FlexibleStringExpander.expandString(modelForm.getContainerId(), context, locale);
        Integer itemIndex = (Integer) context.get("itemIndex");
        if (itemIndex != null && "list".equals(modelForm.getType())) {
            return retVal + modelForm.getItemIndexSeparator() + itemIndex.intValue();
        }
        return retVal;
    }

    public static String getCurrentFormName(ModelForm modelForm, Map<String, Object> context) {
        Integer itemIndex = (Integer) context.get("itemIndex");
        String formName = (String) context.get("formName");
        if (UtilValidate.isEmpty(formName)) {
            formName = modelForm.getName();
        }
        if (itemIndex != null && "list".equals(modelForm.getType())) {
            return formName + modelForm.getItemIndexSeparator() + itemIndex.intValue();
        } else {
            return formName;
        }
    }

    public static String getFocusFieldName(ModelForm modelForm, Map<String, Object> context) {
        String focusFieldName = (String) context.get(modelForm.getName().concat(".focusFieldName"));
        if (focusFieldName == null) {
            return "";
        }
        return focusFieldName;
    }

    private final ModelForm modelForm;
    private final FormStringRenderer formStringRenderer;
    private String focusFieldName;

    public FormRenderer(ModelForm modelForm, FormStringRenderer formStringRenderer) {
        this.modelForm = modelForm;
        this.formStringRenderer = formStringRenderer;
        this.focusFieldName = modelForm.getFocusFieldName();
    }

    private Collection<List<ModelFormField>> getFieldListsByPosition(List<ModelFormField> modelFormFieldList) {
        Map<Integer, List<ModelFormField>> fieldsByPosition = new TreeMap<Integer, List<ModelFormField>>();
        for (ModelFormField modelFormField : modelFormFieldList) {
            Integer position = Integer.valueOf(modelFormField.getPosition());
            List<ModelFormField> fieldListByPosition = fieldsByPosition.get(position);
            if (fieldListByPosition == null) {
                fieldListByPosition = new LinkedList<ModelFormField>();
                fieldsByPosition.put(position, fieldListByPosition);
            }
            fieldListByPosition.add(modelFormField);
        }
        return fieldsByPosition.values();
    }

    public String getFocusFieldName() {
        return focusFieldName;
    }

    private List<ModelFormField> getHiddenIgnoredFields(Map<String, Object> context, Set<String> alreadyRendered,
            List<ModelFormField> fieldList, int position) {
        /*
         * Method does not reference internal state - should be moved to another class.
         */
        List<ModelFormField> hiddenIgnoredFieldList = new LinkedList<ModelFormField>();
        for (ModelFormField modelFormField : fieldList) {
            // with position == -1 then gets all the hidden fields
            if (position != -1 && modelFormField.getPosition() != position) {
                continue;
            }
            FieldInfo fieldInfo = modelFormField.getFieldInfo();

            // render hidden/ignored field widget
            switch (fieldInfo.getFieldType()) {
            case FieldInfo.HIDDEN:
            case FieldInfo.IGNORED:
                if (modelFormField.shouldUse(context)) {
                    hiddenIgnoredFieldList.add(modelFormField);
                    if (alreadyRendered != null)
                        alreadyRendered.add(modelFormField.getName());
                }
                break;

            case FieldInfo.DISPLAY:
            case FieldInfo.DISPLAY_ENTITY:
                ModelFormField.DisplayField displayField = (ModelFormField.DisplayField) fieldInfo;
                if (displayField.getAlsoHidden() && modelFormField.shouldUse(context)) {
                    hiddenIgnoredFieldList.add(modelFormField);
                    // don't add to already rendered here, or the display won't ger rendered: if (alreadyRendered != null) alreadyRendered.add(modelFormField.getName());
                }
                break;

            case FieldInfo.HYPERLINK:
                ModelFormField.HyperlinkField hyperlinkField = (ModelFormField.HyperlinkField) fieldInfo;
                if (hyperlinkField.getAlsoHidden() && modelFormField.shouldUse(context)) {
                    hiddenIgnoredFieldList.add(modelFormField);
                    // don't add to already rendered here, or the hyperlink won't ger rendered: if (alreadyRendered != null) alreadyRendered.add(modelFormField.getName());
                }
                break;
            }
        }
        return hiddenIgnoredFieldList;
    }

    private List<FieldGroupBase> getInbetweenList(FieldGroup startFieldGroup, FieldGroup endFieldGroup) {
        List<FieldGroupBase> inbetweenList = new ArrayList<FieldGroupBase>();
        boolean firstFound = false;
        String startFieldGroupId = null;
        String endFieldGroupId = null;
        if (endFieldGroup != null) {
            endFieldGroupId = endFieldGroup.getId();
        }
        if (startFieldGroup == null) {
            firstFound = true;
        } else {
            startFieldGroupId = startFieldGroup.getId();
        }
        Iterator<FieldGroupBase> iter = modelForm.getFieldGroupList().iterator();
        while (iter.hasNext()) {
            FieldGroupBase obj = iter.next();
            if (obj instanceof ModelForm.Banner) {
                if (firstFound)
                    inbetweenList.add(obj);
            } else {
                FieldGroup fieldGroup = (FieldGroup) obj;
                String fieldGroupId = fieldGroup.getId();
                if (!firstFound) {
                    if (fieldGroupId.equals(startFieldGroupId)) {
                        firstFound = true;
                        continue;
                    }
                }
                if (firstFound) {
                    if (fieldGroupId.equals(endFieldGroupId)) {
                        break;
                    } else {
                        inbetweenList.add(fieldGroup);
                    }
                }
            }
        }
        return inbetweenList;
    }

    /**
     * Renders this form to a writer, as defined with the
     * FormStringRenderer implementation.
     *
     * @param writer The Writer that the form text will be written to
     * @param context Map containing the form context; the following are
     *   reserved words in this context: parameters (Map), isError (Boolean),
     *   itemIndex (Integer, for lists only, otherwise null), bshInterpreter,
     *   formName (String, optional alternate name for form, defaults to the
     *   value of the name attribute)
     */
    public void render(Appendable writer, Map<String, Object> context)
            throws Exception {
        //  increment the paginator, only for list and multi forms
        if (modelForm instanceof ModelGrid) {
            WidgetWorker.incrementPaginatorNumber(context);
        }

        // Populate the viewSize and viewIndex so they are available for use during form actions
        context.put("viewIndex", Paginator.getViewIndex(modelForm, context));
        context.put("viewSize", Paginator.getViewSize(modelForm, context));

        modelForm.runFormActions(context);

        // if this is a list form, don't use Request Parameters
        if (modelForm instanceof ModelGrid) {
            context.put("useRequestParameters", Boolean.FALSE);
        }

        
        // find the highest position number to get the max positions used
        // SCIPIO: use explicit if set, and also take position-span into account here
        Integer positions = modelForm.getPositions();
        if (positions == null || positions < 1) {
            positions = 1;
            for (ModelFormField modelFormField : modelForm.getFieldList()) {
                int curPos = modelFormField.getPosition();
                
                Integer positionSpan = modelFormField.getPositionSpan();
                if (positionSpan == null) {
                    positionSpan = modelForm.getDefaultPositionSpan();
                }
                if (positionSpan != null && positionSpan > 0) {
                    curPos += (positionSpan - 1);
                }
                
                if (curPos > positions) {
                    positions = curPos;
                }
                
                FieldInfo currentFieldInfo = modelFormField.getFieldInfo();
                if (currentFieldInfo == null) {
                    throw new IllegalArgumentException(
                            "Error rendering form, a field has no FieldInfo, ie no sub-element for the type of field for field named: "
                                    + modelFormField.getName());
                }
            }
        }

        if ("single".equals(modelForm.getType())) {
            this.renderSingleFormString(writer, context, positions);
        } else if ("list".equals(modelForm.getType())) {
            this.renderListFormString(writer, context, positions);
        } else if ("multi".equals(modelForm.getType())) {
            this.renderMultiFormString(writer, context, positions);
        } else if ("upload".equals(modelForm.getType())) {
            this.renderSingleFormString(writer, context, positions);
        } else {
            if (UtilValidate.isEmpty(modelForm.getType())) {
                throw new IllegalArgumentException("The form 'type' tag is missing or empty on the form with the name "
                        + modelForm.getName());
            } else {
                throw new IllegalArgumentException("The form type " + modelForm.getType()
                        + " is not supported for form with name " + modelForm.getName());
            }
        }
    }

    /**
     * SCIPIO: Gets the real/accurate number of inner form field cells, by running through them in advance.
     */
    private int getInnerFormFieldCellCount(ModelForm modelForm, List<ModelFormField> innerFormFields) {
        int innerFormFieldsCells = 0;
        if (modelForm.getGroupColumns()) {
            if (innerFormFields != null && innerFormFields.size() > 0) {
                Iterator<ModelFormField> innerFormFieldsIt = innerFormFields.iterator();
                String lastFieldName = "";
                while (innerFormFieldsIt.hasNext()) {
                    ModelFormField modelFormField = innerFormFieldsIt.next();
                    if (modelForm.getSeparateColumns() || modelFormField.getSeparateColumn()) {
                        // 2018-03-02: we must not count successive fields having the same name (different use-when conditions)
                        if (!modelFormField.getName().equals(lastFieldName)) {
                            innerFormFieldsCells++;
                        }
                    }
                    lastFieldName = modelFormField.getName();
                }
                if (innerFormFieldsCells < 1) {
                    innerFormFieldsCells = 1; // minimum one
                }
            }
        } else {
            innerFormFieldsCells = getBasicFormFieldCellCount(modelForm, innerFormFields);
        }
        return innerFormFieldsCells;
    }
    
    /**
     * SCIPIO: Gets the basic number of cells, discarding duplicates, assuming each unique
     * field name corresponds to one cell. 
     */
    private int getBasicFormFieldCellCount(ModelForm modelForm, List<ModelFormField> formFields) {
        int innerFormFieldsCells = 0;
        if (formFields == null) return innerFormFieldsCells;
        
        // 2018-03-02: we must not count successive fields having the same name (different use-when conditions)
        //innerFormFieldsCells = innerFormFields.size();
        String lastFieldName = "";
        Iterator<ModelFormField> innerFormFieldsIt = formFields.iterator();
        while (innerFormFieldsIt.hasNext()) {
            ModelFormField modelFormField = innerFormFieldsIt.next();
            if (!modelFormField.getName().equals(lastFieldName)) {
                innerFormFieldsCells++;
            }
            lastFieldName = modelFormField.getName();
        }
        
        return innerFormFieldsCells;
    }
    
    private int renderHeaderRow(Appendable writer, Map<String, Object> context)
            throws IOException {
        int maxNumOfColumns = 0;

        // SCIPIO: factored this out; I don't recall in which cases this could be false, but keeping for safety.
        boolean isListOrMultiForm = ("list".equals(modelForm.getType()) || "multi".equals(modelForm.getType()));
        
        // We will render one title/column for all the fields with the same name
        // in this model: we can have more fields with the same name when use-when
        // conditions are used or when a form is extended or when the fields are
        // automatically retrieved by a service or entity definition.
        List<ModelFormField> tempFieldList = new LinkedList<ModelFormField>();
        tempFieldList.addAll(modelForm.getFieldList());    
        // SCIPIO: NOTE: this addAll added by us
        if (isListOrMultiForm && !modelForm.getUseMasterSubmitField()) {
            tempFieldList.addAll(modelForm.getMultiSubmitFields());
        }
        for (int j = 0; j < tempFieldList.size(); j++) {
            ModelFormField modelFormField = tempFieldList.get(j);
            for (int i = j + 1; i < tempFieldList.size(); i++) {
                ModelFormField curField = tempFieldList.get(i);
                if (curField.getName() != null && curField.getName().equals(modelFormField.getName())) {
                    tempFieldList.remove(i--);
                }
            }
        }

        // ===========================
        // Preprocessing
        // ===========================
        // We get a sorted (by position, ascending) set of lists;
        // each list contains all the fields with that position.
        Collection<List<ModelFormField>> fieldListsByPosition = this.getFieldListsByPosition(tempFieldList);
        List<Map<String, List<ModelFormField>>> fieldRowsByPosition = new LinkedList<Map<String, List<ModelFormField>>>(); // this list will contain maps, each one containing the list of fields for a position
        for (List<ModelFormField> mainFieldList : fieldListsByPosition) {
            int numOfColumns = 0;
            List<ModelFormField> innerDisplayHyperlinkFieldsBegin = new LinkedList<ModelFormField>();
            List<ModelFormField> innerFormFields = new LinkedList<ModelFormField>();
            List<ModelFormField> innerDisplayHyperlinkFieldsEnd = new LinkedList<ModelFormField>();

            // render title for each field, except hidden & ignored, etc

            // start by rendering all display and hyperlink fields, until we
            //get to a field that should go into the form cell, then render
            //the form cell with all non-display and non-hyperlink fields, then
            //do a start after the first form input field and
            //render all display and hyperlink fields after the form

            // prepare the two lists of display and hyperlink fields
            // the fields in the first list will be rendered as columns before the
            // combined column for the input fields; the fields in the second list
            // will be rendered as columns after it
            boolean inputFieldFound = false;
            for (ModelFormField modelFormField : mainFieldList) {
                FieldInfo fieldInfo = modelFormField.getFieldInfo();
               

                // if the field's title is explicitly set to "" (title="") then
                // the header is not created for it; this is useful for position list
                // where one line can be rendered with more than one row, and we
                // only want to display the title header for the main row
                String modelFormFieldTitle = modelFormField.getTitle(context);
                if ("".equals(modelFormFieldTitle)) {
                    continue;
                }
                // don't do any header for hidden or ignored fields
                if (fieldInfo.getFieldType() == FieldInfo.HIDDEN
                        || fieldInfo.getFieldType() == FieldInfo.IGNORED) {
                    continue;
                }

                // SCIPIO: added SUBMIT here
                if (fieldInfo.getFieldType() != FieldInfo.DISPLAY
                        && fieldInfo.getFieldType() != FieldInfo.DISPLAY_ENTITY
                        && fieldInfo.getFieldType() != FieldInfo.HYPERLINK  
                        && fieldInfo.getFieldType() != FieldInfo.SUBMIT) {
                    inputFieldFound = true;
                    continue;
                }

                // separate into two lists the display/hyperlink fields found before and after the first input fields
                if (!inputFieldFound) {
                    innerDisplayHyperlinkFieldsBegin.add(modelFormField);
                } else {
                    innerDisplayHyperlinkFieldsEnd.add(modelFormField);
                }

            }

            // prepare the combined title for the column that will contain the form/input fields
            for (ModelFormField modelFormField : mainFieldList) {
                FieldInfo fieldInfo = modelFormField.getFieldInfo();

                // don't do any header for hidden or ignored fields
                if (fieldInfo.getFieldType() == FieldInfo.HIDDEN
                        || fieldInfo.getFieldType() == FieldInfo.IGNORED) {
                    continue;
                }

                // skip all of the display/hyperlink fields
                // SCIPIO: added SUBMIT here
                if (fieldInfo.getFieldType() == FieldInfo.DISPLAY
                        || fieldInfo.getFieldType() == FieldInfo.DISPLAY_ENTITY
                        || fieldInfo.getFieldType() == FieldInfo.HYPERLINK
                        || fieldInfo.getFieldType() == FieldInfo.SUBMIT) {
                    continue;
                }

                innerFormFields.add(modelFormField);
            }

            // SCIPIO: get real/accurate count of inner field cells
            int innerFormFieldsCells = getInnerFormFieldCellCount(modelForm, innerFormFields);
            int innerDisplayHyperlinkFieldsBeginCells = innerDisplayHyperlinkFieldsBegin.size();
            int innerDisplayHyperlinkFieldsEndCells = innerDisplayHyperlinkFieldsEnd.size();
            
            // SCIPIO: Add an extra column to hold a checkbox or radio button depending on the type of form.
            if (innerFormFieldsCells > 0 && modelForm.getUseRowSubmit()) {
                ModelFormField headerItem = modelForm.getRowSubmitHeaderSelectField();
                if (headerItem != null) {
                    innerDisplayHyperlinkFieldsEnd.add(headerItem);
                    mainFieldList.add(headerItem);
                }
            }
            
            if (UtilValidate.isNotEmpty(innerDisplayHyperlinkFieldsBegin))
                numOfColumns += innerDisplayHyperlinkFieldsBeginCells;
            if (UtilValidate.isNotEmpty(innerDisplayHyperlinkFieldsEnd))
                numOfColumns += innerDisplayHyperlinkFieldsEndCells;
            // SCIPIO: this is not enough. we must exclude any grouped columns.
            //if (UtilValidate.isNotEmpty(innerFormFields))
            //    numOfColumns += innerFormFields.size();
            numOfColumns += innerFormFieldsCells;

            if (maxNumOfColumns < numOfColumns) {
                maxNumOfColumns = numOfColumns;
            }
            
            Map<String, List<ModelFormField>> fieldRow = UtilMisc.toMap("displayBefore", innerDisplayHyperlinkFieldsBegin,
                    "inputFields", innerFormFields, "displayAfter", innerDisplayHyperlinkFieldsEnd, "mainFieldList",
                    mainFieldList);
            fieldRowsByPosition.add(fieldRow);
        }
        // ===========================
        // Rendering
        // ===========================
        for (Map<String, List<ModelFormField>> listsMap : fieldRowsByPosition) {
            List<ModelFormField> innerDisplayHyperlinkFieldsBegin = listsMap.get("displayBefore");
            List<ModelFormField> innerFormFields = listsMap.get("inputFields");
            List<ModelFormField> innerDisplayHyperlinkFieldsEnd = listsMap.get("displayAfter");
            List<ModelFormField> mainFieldList = listsMap.get("mainFieldList");

            // SCIPIO: NEW BLOCK: get real/accurate count of inner field cells
            int innerFormFieldsCells = getInnerFormFieldCellCount(modelForm, innerFormFields);
            int innerDisplayHyperlinkFieldsBeginCells = innerDisplayHyperlinkFieldsBegin.size();
            int innerDisplayHyperlinkFieldsEndCells = innerDisplayHyperlinkFieldsEnd.size();
            
            int numOfCells = innerDisplayHyperlinkFieldsBeginCells + innerDisplayHyperlinkFieldsEndCells
                    + innerFormFieldsCells; //+ (innerFormFields.size() > 0 ? 1 : 0);
            int numOfColumnsToSpan = maxNumOfColumns - numOfCells + 1;
            if (numOfColumnsToSpan < 1) {
                numOfColumnsToSpan = 1;
            }
            
            if (numOfCells > 0) {
                formStringRenderer.renderFormatHeaderRowOpen(writer, context, modelForm);

                if (modelForm.getGroupColumns()) {
                    Iterator<ModelFormField> innerDisplayHyperlinkFieldsBeginIt = innerDisplayHyperlinkFieldsBegin.iterator();
                    while (innerDisplayHyperlinkFieldsBeginIt.hasNext()) {
                        ModelFormField modelFormField = innerDisplayHyperlinkFieldsBeginIt.next();
                        // span columns only if this is the last column in the row (not just in this first list)
                        if (innerDisplayHyperlinkFieldsBeginIt.hasNext() || numOfCells > innerDisplayHyperlinkFieldsBeginCells) {
                            formStringRenderer.renderFormatHeaderRowCellOpen(writer, context, modelForm, modelFormField, 1);
                        } else {
                            formStringRenderer.renderFormatHeaderRowCellOpen(writer, context, modelForm, modelFormField,
                                    numOfColumnsToSpan);
                        }
                        formStringRenderer.renderFieldTitle(writer, context, modelFormField);
                        formStringRenderer.renderFormatHeaderRowCellClose(writer, context, modelForm, modelFormField);
                    }
                    if (innerFormFields.size() > 0) {
                        // SCIPIO: we have to pre-parse this part due to modelFormField.getSeparateColumn()
                        boolean hasSepColumns = false;
                        for(ModelFormField modelFormField : innerFormFields) {
                            if (modelForm.getSeparateColumns() || modelFormField.getSeparateColumn()) {
                                hasSepColumns = true;
                                break;
                            }
                        }
                        
                        // SCIPIO: FIXME?: the renderFormatHeaderRow(Form)CellOpen calls here always pass position span 1,
                        // which only works assuming innerDisplayHyperlinkFieldsEnd is non-empty (usually true) 

                        // SCIPIO: rearranged this completely. it's the modelFormField.getSeparateColumn() option that complicates this.
                        if (hasSepColumns) {
                            boolean cellOpen = false;
                            boolean firstInCell = true;
                            Iterator<ModelFormField> innerFormFieldsIt = innerFormFields.iterator();
                            ModelFormField modelFormField = null;
                            while (innerFormFieldsIt.hasNext()) {
                                modelFormField = innerFormFieldsIt.next();
                                boolean fieldHasSepColumn = (modelForm.getSeparateColumns() || modelFormField.getSeparateColumn());
                                if (cellOpen && fieldHasSepColumn) {
                                    formStringRenderer.renderFormatHeaderRowCellClose(writer, context, modelForm, modelFormField);
                                    cellOpen = false;
                                }
                                if (fieldHasSepColumn || !cellOpen) {
                                    formStringRenderer.renderFormatHeaderRowCellOpen(writer, context, modelForm, modelFormField, 1); // positionSpan hardcoded...
                                    cellOpen = true;
                                    firstInCell = true;
                                }
                                if (!fieldHasSepColumn && !firstInCell) {
                                    formStringRenderer.renderFormatHeaderRowFormCellTitleSeparator(writer, context, modelForm,
                                            modelFormField, false); // TODO: can't really do isLast properly (!innerFormFieldsIt.hasNext() not good enough)
                                }

                                // render title (unless this is a submit or a reset field)
                                formStringRenderer.renderFieldTitle(writer, context, modelFormField);
                                firstInCell = false;
                                
                                if (fieldHasSepColumn) {
                                    formStringRenderer.renderFormatHeaderRowCellClose(writer, context, modelForm, modelFormField);
                                    cellOpen = false;
                                }
                            }
                            if (cellOpen) {
                                formStringRenderer.renderFormatHeaderRowCellClose(writer, context, modelForm, modelFormField);
                                cellOpen = false;
                            }
                        } else { // SCIPIO: simple case. here renderFormatHeaderRowFormCellOpen is used (but otherwise would produce same as previous).
                            formStringRenderer.renderFormatHeaderRowFormCellOpen(writer, context, modelForm); // TODO: manage colspan
                            boolean firstInCell = true;
                            for(ModelFormField modelFormField : innerFormFields) {
                                if (!firstInCell) {
                                    formStringRenderer.renderFormatHeaderRowFormCellTitleSeparator(writer, context, modelForm,
                                            modelFormField, false);
                                } 
                                // render title (unless this is a submit or a reset field)
                                formStringRenderer.renderFieldTitle(writer, context, modelFormField);
                                firstInCell = false;
                            }
                            formStringRenderer.renderFormatHeaderRowFormCellClose(writer, context, modelForm);
                        }
                    }
                    Iterator<ModelFormField> innerDisplayHyperlinkFieldsEndIt = innerDisplayHyperlinkFieldsEnd.iterator();
                    while (innerDisplayHyperlinkFieldsEndIt.hasNext()) {
                        ModelFormField modelFormField = innerDisplayHyperlinkFieldsEndIt.next();
                        // span columns only if this is the last column in the row (not just in this first list)
                        // SCIPIO: bad check
                        //if (innerDisplayHyperlinkFieldsEndIt.hasNext() || numOfCells > innerDisplayHyperlinkFieldsEndCells) {
                        if (innerDisplayHyperlinkFieldsEndIt.hasNext()) {
                            formStringRenderer.renderFormatHeaderRowCellOpen(writer, context, modelForm, modelFormField, 1);
                        } else {
                            formStringRenderer.renderFormatHeaderRowCellOpen(writer, context, modelForm, modelFormField,
                                    numOfColumnsToSpan);
                        }
                        formStringRenderer.renderFieldTitle(writer, context, modelFormField);
                        formStringRenderer.renderFormatHeaderRowCellClose(writer, context, modelForm, modelFormField);
                    }
                } else {
                    Iterator<ModelFormField> mainFieldListIter = mainFieldList.iterator();
                    while (mainFieldListIter.hasNext()) {
                        ModelFormField modelFormField = mainFieldListIter.next();

                        // don't do any header for hidden or ignored fields
                        FieldInfo fieldInfo = modelFormField.getFieldInfo();
                        if (fieldInfo.getFieldType() == FieldInfo.HIDDEN
                                || fieldInfo.getFieldType() == FieldInfo.IGNORED) {
                            continue;
                        }

                        // span columns only if this is the last column in the row (not just in this first list)
                        if (mainFieldListIter.hasNext() || numOfCells > mainFieldList.size()) {
                            formStringRenderer.renderFormatHeaderRowCellOpen(writer, context, modelForm, modelFormField, 1);
                        } else {
                            formStringRenderer.renderFormatHeaderRowCellOpen(writer, context, modelForm, modelFormField,
                                    numOfColumnsToSpan);
                        }
                        formStringRenderer.renderFieldTitle(writer, context, modelFormField);
                        formStringRenderer.renderFormatHeaderRowCellClose(writer, context, modelForm, modelFormField);
                    }
                }

                formStringRenderer.renderFormatHeaderRowClose(writer, context, modelForm);
            }
        }

        return maxNumOfColumns;
    }

    private void renderHiddenIgnoredFields(Appendable writer, Map<String, Object> context, FormStringRenderer formStringRenderer,
            List<ModelFormField> fieldList) throws IOException {
        for (ModelFormField modelFormField : fieldList) {
            FieldInfo fieldInfo = modelFormField.getFieldInfo();

            // render hidden/ignored field widget
            switch (fieldInfo.getFieldType()) {
            case FieldInfo.HIDDEN:
            case FieldInfo.IGNORED:
                modelFormField.renderFieldString(writer, context, formStringRenderer);
                break;

            case FieldInfo.DISPLAY:
            case FieldInfo.DISPLAY_ENTITY:
            case FieldInfo.HYPERLINK:
                formStringRenderer.renderHiddenField(writer, context, modelFormField, modelFormField.getEntry(context));
                break;
            }
        }
    }

    // The fields in the three lists, usually created in the preprocessing phase
    // of the renderItemRows method are rendered: this will create a visual representation
    // of one row (corresponding to one position).
    private void renderItemRow(Appendable writer, Map<String, Object> localContext, FormStringRenderer formStringRenderer,
            boolean formPerItem, List<ModelFormField> hiddenIgnoredFieldList,
            List<ModelFormField> innerDisplayHyperlinkFieldsBegin, List<ModelFormField> innerFormFields,
            List<ModelFormField> innerDisplayHyperlinkFieldsEnd, List<ModelFormField> mainFieldList, int position,
            int numOfColumns, boolean isListOrMultiForm) throws IOException {
        
        boolean renderedHiddenFields = false;
        // SCIPIO: NEW BLOCK: get real/accurate count of inner field cells
        int innerFormFieldsCells = getInnerFormFieldCellCount(modelForm, innerFormFields);
        int innerDisplayHyperlinkFieldsBeginCells = innerDisplayHyperlinkFieldsBegin.size();
        int innerDisplayHyperlinkFieldsEndCells = innerDisplayHyperlinkFieldsEnd.size();
                
        // SCIPIO: Add an extra column to hold a radio for form lists that use an specific row for submit buttons. This radio will determine which row must be submitted.
        if (innerFormFieldsCells > 0 && modelForm.getUseRowSubmit()) {
            ModelFormField item = modelForm.getRowSubmitSelectField();
            if (item != null) {
                innerDisplayHyperlinkFieldsEnd.add(item);
                mainFieldList.add(item);
            }
        }
        
        int numOfCells = innerDisplayHyperlinkFieldsBeginCells + innerDisplayHyperlinkFieldsEndCells
                + innerFormFieldsCells; // + (innerFormFields.size() > 0 ? 1 : 0);
        int numOfColumnsToSpan = numOfColumns - numOfCells + 1;
        if (numOfColumnsToSpan < 1) {
            numOfColumnsToSpan = 1;
        }

        // render row formatting open
        formStringRenderer.renderFormatItemRowOpen(writer, localContext, modelForm);
        Iterator<ModelFormField> innerDisplayHyperlinkFieldsBeginIter = innerDisplayHyperlinkFieldsBegin.iterator();
        Map<String, Integer> fieldCount = new HashMap<String, Integer>();
        while (innerDisplayHyperlinkFieldsBeginIter.hasNext()) {
            ModelFormField modelFormField = innerDisplayHyperlinkFieldsBeginIter.next();
            if (fieldCount.containsKey(modelFormField.getName())) {
                fieldCount.put(modelFormField.getName(), fieldCount.get(modelFormField.getName()) + 1);
            } else {
                fieldCount.put(modelFormField.getName(), 1);
            }
        }
        
        if (modelForm.getGroupColumns()) {
            // do the first part of display and hyperlink fields
            Iterator<ModelFormField> innerDisplayHyperlinkFieldIter = innerDisplayHyperlinkFieldsBegin.iterator();
            while (innerDisplayHyperlinkFieldIter.hasNext()) {
                boolean cellOpen = false;
                ModelFormField modelFormField = innerDisplayHyperlinkFieldIter.next();
                // span columns only if this is the last column in the row (not just in this first list)
                if (fieldCount.get(modelFormField.getName()) < 2) {
                    if ((innerDisplayHyperlinkFieldIter.hasNext() || numOfCells > innerDisplayHyperlinkFieldsBeginCells)) {
                        formStringRenderer.renderFormatItemRowCellOpen(writer, localContext, modelForm, modelFormField, 1);
                    } else {
                        formStringRenderer.renderFormatItemRowCellOpen(writer, localContext, modelForm, modelFormField,
                                numOfColumnsToSpan);
                    }
                    cellOpen = true;
                }
                if (!isListOrMultiForm || modelFormField.shouldUse(localContext)) {
                    if ((fieldCount.get(modelFormField.getName()) > 1)) {
                        if ((innerDisplayHyperlinkFieldIter.hasNext() || numOfCells > innerDisplayHyperlinkFieldsBeginCells)) {
                            formStringRenderer.renderFormatItemRowCellOpen(writer, localContext, modelForm, modelFormField, 1);
                        } else {
                            formStringRenderer.renderFormatItemRowCellOpen(writer, localContext, modelForm, modelFormField,
                                    numOfColumnsToSpan);
                        }
                        cellOpen = true;
                    }
                    modelFormField.renderFieldString(writer, localContext, formStringRenderer);
                }
                if (cellOpen) {
                    formStringRenderer.renderFormatItemRowCellClose(writer, localContext, modelForm, modelFormField);
                }
            }

            // The form cell is rendered only if there is at least an input field
            if (innerFormFields.size() > 0) {
                // SCIPIO: we have to pre-parse this part due to modelFormField.getSeparateColumn()
                boolean hasSepColumns = false;
                for(ModelFormField modelFormField : innerFormFields) {
                    if (modelForm.getSeparateColumns() || modelFormField.getSeparateColumn()) {
                        hasSepColumns = true;
                        break;
                    }
                }
                
                // SCIPIO: only support this if no separate columns at all.
                if (!hasSepColumns) {
                    // render the "form" cell                
                    formStringRenderer.renderFormatItemRowFormCellOpen(writer, localContext, modelForm); // TODO: colspan
                }
                if (formPerItem) {
                    // SCIPIO: special flags
                    localContext.put("renderForm_formScope", "item");
                    localContext.put("renderForm_formSpread", hasSepColumns ? "multi-cell" : "single-cell");
                    formStringRenderer.renderFormOpen(writer, localContext, modelForm);
                    localContext.remove("renderForm_formScope");
                    localContext.remove("renderForm_formSpread");
                }

                if (hasSepColumns) {
                    boolean cellOpen = false;
                    ListIterator<ModelFormField> innerFormFieldsIt = innerFormFields.listIterator(); // SCIPIO: now listIterator
                    ModelFormField modelFormField = null;
                    while (innerFormFieldsIt.hasNext()) {
                        modelFormField = innerFormFieldsIt.next();
                        boolean fieldHasSepColumn = (modelForm.getSeparateColumns() || modelFormField.getSeparateColumn());
                        if (cellOpen && fieldHasSepColumn) {
                            formStringRenderer.renderFormatItemRowCellClose(writer, localContext, modelForm, modelFormField);
                            cellOpen = false;
                        }
                        if (fieldHasSepColumn || !cellOpen) {
                            formStringRenderer.renderFormatItemRowCellOpen(writer, localContext, modelForm, modelFormField, 1); // positionSpan hardcoded
                            cellOpen = true;
                            // SCIPIO: make an effort to put the hidden fields inside the first cell, so a little less invalid html...
                            if (!renderedHiddenFields) {
                                // do all of the hidden fields...
                                this.renderHiddenIgnoredFields(writer, localContext, formStringRenderer, hiddenIgnoredFieldList);
                                renderedHiddenFields = true;
                            }
                        }
                        // render field widget
                        // SCIPIO: 2018-03-02: factored out for possible need to reuse
                        renderItemRowCellFields(writer, localContext, formStringRenderer, modelFormField, innerFormFieldsIt, !isListOrMultiForm);
                        
                        if (fieldHasSepColumn) {
                            formStringRenderer.renderFormatItemRowCellClose(writer, localContext, modelForm, modelFormField);
                            cellOpen = false;
                        }
                    }
                    if (!renderedHiddenFields) { // should already have been done, but call again just in case
                        // do all of the hidden fields...
                        this.renderHiddenIgnoredFields(writer, localContext, formStringRenderer, hiddenIgnoredFieldList);
                        renderedHiddenFields = true;
                    }
                    if (cellOpen) {
                        formStringRenderer.renderFormatItemRowCellClose(writer, localContext, modelForm, modelFormField);
                        cellOpen = false;
                    }
                } else { // simple case
                    // do all of the hidden fields...
                    this.renderHiddenIgnoredFields(writer, localContext, formStringRenderer, hiddenIgnoredFieldList);
                    renderedHiddenFields = true;
                    for(ModelFormField modelFormField : innerFormFields) {
                        if (!isListOrMultiForm || modelFormField.shouldUse(localContext)) {
                            modelFormField.renderFieldString(writer, localContext, formStringRenderer);
                        }
                    }
                }

                if (formPerItem) {
                    formStringRenderer.renderFormClose(writer, localContext, modelForm);
                }
                if (!hasSepColumns) {
                    formStringRenderer.renderFormatItemRowFormCellClose(writer, localContext, modelForm);
                }
            }

            // render the rest of the display/hyperlink fields
            innerDisplayHyperlinkFieldIter = innerDisplayHyperlinkFieldsEnd.iterator();
            while (innerDisplayHyperlinkFieldIter.hasNext()) {
                ModelFormField modelFormField = innerDisplayHyperlinkFieldIter.next();
                // span columns only if this is the last column in the row
                if (innerDisplayHyperlinkFieldIter.hasNext()) {
                    formStringRenderer.renderFormatItemRowCellOpen(writer, localContext, modelForm, modelFormField, 1);
                } else {
                    formStringRenderer.renderFormatItemRowCellOpen(writer, localContext, modelForm, modelFormField,
                            numOfColumnsToSpan);
                }
                
                // SCIPIO: 2016-11-02: if there were no editable "middle" fields to render, there's still the case where
                // we have only hidden fields with a submit button (such as delete action).
                // in that case we just insert them into the next cell.
                if (!renderedHiddenFields) {
                    this.renderHiddenIgnoredFields(writer, localContext, formStringRenderer, hiddenIgnoredFieldList);
                    renderedHiddenFields = true;
                }
                
                if (!isListOrMultiForm || modelFormField.shouldUse(localContext)) {
                    modelFormField.renderFieldString(writer, localContext, formStringRenderer);
                }
                formStringRenderer.renderFormatItemRowCellClose(writer, localContext, modelForm, modelFormField);
            }
        } else {
            // do all of the hidden fields...
            this.renderHiddenIgnoredFields(writer, localContext, formStringRenderer, hiddenIgnoredFieldList);
            renderedHiddenFields = true;

            Iterator<ModelFormField> mainFieldIter = mainFieldList.iterator();
            while (mainFieldIter.hasNext()) {
                ModelFormField modelFormField = mainFieldIter.next();

                // don't do any header for hidden or ignored fields inside this loop
                FieldInfo fieldInfo = modelFormField.getFieldInfo();
                if (fieldInfo.getFieldType() == FieldInfo.HIDDEN
                        || fieldInfo.getFieldType() == FieldInfo.IGNORED) {
                    continue;
                }

                // span columns only if this is the last column in the row
                if (mainFieldIter.hasNext()) {
                    formStringRenderer.renderFormatItemRowCellOpen(writer, localContext, modelForm, modelFormField, 1);
                } else {
                    formStringRenderer.renderFormatItemRowCellOpen(writer, localContext, modelForm, modelFormField,
                            numOfColumnsToSpan);
                }
                if (!isListOrMultiForm || modelFormField.shouldUse(localContext)) {
                    modelFormField.renderFieldString(writer, localContext, formStringRenderer);
                }
                formStringRenderer.renderFormatItemRowCellClose(writer, localContext, modelForm, modelFormField);
            }
        }

        // render row formatting close
        formStringRenderer.renderFormatItemRowClose(writer, localContext, modelForm);
    }

    /**
     * SCIPIO: Renders the given modelFormField (within the open cell) if it passes shouldUse condition;
     * in addition, if the following fields have the same name, they are also rendered and
     * consumed from the formFieldsIt list iterator.
     * <p>
     * This is needed because we could have several fields of same name
     * in a row with different use-when expressions. In such case, we must render 
     * exactly one cell for all of them (at most one, but one is always required also).
     * We must process the fields having same name as a "batch" here.
     * <p>
     * Added 2018-03-02.
     */
    private void renderItemRowCellFields(Appendable writer, Map<String, Object> localContext, FormStringRenderer formStringRenderer, 
            ModelFormField modelFormField, ListIterator<ModelFormField> formFieldsIt, boolean forceUse) throws IOException {
        List<ModelFormField> sameNameFields = null;
        String modelFormFieldName = modelFormField.getName();
        while(formFieldsIt.hasNext()) {
            ModelFormField nextField = formFieldsIt.next();
            if (modelFormFieldName.equals(nextField.getName())) {
                if (sameNameFields == null) sameNameFields = new ArrayList<>();
                sameNameFields.add(nextField);
            } else {
                formFieldsIt.previous();
                break;
            }
        }

        if (forceUse || modelFormField.shouldUse(localContext)) {
            modelFormField.renderFieldString(writer, localContext, formStringRenderer);
        }
        if (sameNameFields != null) {
            for(ModelFormField sameNameField : sameNameFields) {
                if (forceUse || sameNameField.shouldUse(localContext)) {
                    sameNameField.renderFieldString(writer, localContext, formStringRenderer);
                }
            }
        }
    }
    
    
    /**
     * SCIPIO: callbacks for important render item rows events.
     */
    private interface RenderItemRowsEventHandler {
        void notifyHasList() throws IOException;
        void notifyHasResult() throws IOException;
        void notifyHasDisplayResult() throws IOException;
        int getNumOfColumns();
    }
    
    private void renderItemRows(Appendable writer, Map<String, Object> context, FormStringRenderer formStringRenderer,
            boolean formPerItem, int numOfColumns, RenderItemRowsEventHandler listFormHandler) throws IOException {
        String lookupName = modelForm.getListName();
        if (UtilValidate.isEmpty(lookupName)) {
            Debug.logError("No value for list or iterator name found.", module);
            return;
        }
        Object obj = context.get(lookupName);
        if (obj == null) {
            if (Debug.verboseOn())
                Debug.logVerbose("No object for list or iterator name [" + lookupName + "] found, so not rendering rows.", module);
            return;
        }
        // if list is empty, do not render rows
        Iterator<?> iter = null;
        if (obj instanceof Iterator<?>) {
            iter = (Iterator<?>) obj;
        } else if (obj instanceof List<?>) {
            iter = ((List<?>) obj).listIterator();
        }

        // set low and high index
        Paginator.getListLimits(modelForm, context, obj);

        int listSize = ((Integer) context.get("listSize")).intValue();
        int lowIndex = ((Integer) context.get("lowIndex")).intValue();
        int highIndex = ((Integer) context.get("highIndex")).intValue();

        // we're passed a subset of the list, so use (0, viewSize) range
        if (modelForm.isOverridenListSize()) {
            lowIndex = 0;
            highIndex = ((Integer) context.get("viewSize")).intValue();
        }

        // SCIPIO: factored this out; I don't recall in which cases this could be false, but keeping for safety.
        boolean isListOrMultiForm = ("list".equals(modelForm.getType()) || "multi".equals(modelForm.getType()));
        
        if (iter != null) {
            
            listFormHandler.notifyHasList();
            
            // render item rows
            int itemIndex = -1;
            Object item = null;
            context.put("wholeFormContext", context);
            Map<String, Object> previousItem = new HashMap<String, Object>();
            while ((item = safeNext(iter)) != null) {
                
                listFormHandler.notifyHasResult();
                
                itemIndex++;
                if (itemIndex >= highIndex) {
                    break;
                }

                // TODO: this is a bad design, for EntityListIterators we should skip to the lowIndex and go from there, MUCH more efficient...
                if (itemIndex < lowIndex) {
                    continue;
                }

                listFormHandler.notifyHasDisplayResult();
                
                // reset/remove the BshInterpreter now as well as later because chances are there is an interpreter at this level of the stack too
                this.resetBshInterpreter(context);

                Map<String, Object> itemMap = UtilGenerics.checkMap(item);
                MapStack<String> localContext = MapStack.create(context);
                if (UtilValidate.isNotEmpty(modelForm.getListEntryName())) {
                    localContext.put(modelForm.getListEntryName(), item);
                } else {
                    if (itemMap instanceof GenericEntity) {
                        // Rendering code might try to modify the GenericEntity instance,
                        // so we make a copy of it.
                        Map<String, Object> genericEntityClone = UtilGenerics.cast(((GenericEntity) itemMap).clone());
                        localContext.push(genericEntityClone);
                    } else {
                        localContext.push(itemMap);
                    }
                }

                // reset/remove the BshInterpreter now as well as later because chances are there is an interpreter at this level of the stack too
                this.resetBshInterpreter(localContext);
                localContext.push();
                localContext.put("previousItem", previousItem);
                previousItem = new HashMap<String, Object>();
                previousItem.putAll(itemMap);

                AbstractModelAction.runSubActions(modelForm.getRowActions(), localContext);

                localContext.put("itemIndex", Integer.valueOf(itemIndex - lowIndex));
                if (UtilValidate.isNotEmpty(context.get("renderFormSeqNumber"))) {
                    localContext.put("formUniqueId", "_" + context.get("renderFormSeqNumber"));
                }

                this.resetBshInterpreter(localContext);

                if (Debug.verboseOn())
                    Debug.logVerbose("In form got another row, context is: " + localContext, module);

                // Check to see if there is a field, same name and same use-when (could come from extended form)
                List<ModelFormField> tempFieldList = new LinkedList<ModelFormField>();
                tempFieldList.addAll(modelForm.getFieldList());
                for (int j = 0; j < tempFieldList.size(); j++) {
                    ModelFormField modelFormField = tempFieldList.get(j);
                    if (!modelFormField.isUseWhenEmpty()) {
                        boolean shouldUse1 = modelFormField.shouldUse(localContext);
                        for (int i = j + 1; i < tempFieldList.size(); i++) {
                            ModelFormField curField = tempFieldList.get(i);
                            if (curField.getName() != null && curField.getName().equals(modelFormField.getName())) {
                                boolean shouldUse2 = curField.shouldUse(localContext);
                                if (shouldUse1 == shouldUse2) {
                                    tempFieldList.remove(i--);
                                }
                            } else {
                                continue;
                            }
                        }
                    }
                }

                // Each single item is rendered in one or more rows if its fields have
                // different "position" attributes. All the fields with the same position
                // are rendered in the same row.
                // The default position is 1, and represents the main row:
                // it contains the fields that are in the list header (columns).
                // The positions lower than 1 are rendered in rows before the main one;
                // positions higher than 1 are rendered after the main one.

                // We get a sorted (by position, ascending) set of lists;
                // each list contains all the fields with that position.
                Collection<List<ModelFormField>> fieldListsByPosition = this.getFieldListsByPosition(tempFieldList);
                //List hiddenIgnoredFieldList = getHiddenIgnoredFields(localContext, null, tempFieldList);
                for (List<ModelFormField> fieldListByPosition : fieldListsByPosition) {
                    // For each position (the subset of fields with the same position attribute)
                    // we have two phases: preprocessing and rendering

                    List<ModelFormField> innerDisplayHyperlinkFieldsBegin = new LinkedList<ModelFormField>();
                    List<ModelFormField> innerFormFields = new LinkedList<ModelFormField>();
                    List<ModelFormField> innerDisplayHyperlinkFieldsEnd = new LinkedList<ModelFormField>();

                    // Preprocessing:
                    // all the form fields are evaluated and the ones that will
                    // appear in the form are put into three separate lists:
                    // - hyperlink fields that will appear at the beginning of the row
                    // - fields of other types
                    // - hyperlink fields that will appear at the end of the row
                    Iterator<ModelFormField> innerDisplayHyperlinkFieldIter = fieldListByPosition.iterator();
                    int currentPosition = 1;
                    while (innerDisplayHyperlinkFieldIter.hasNext()) {
                        ModelFormField modelFormField = innerDisplayHyperlinkFieldIter.next();
                        FieldInfo fieldInfo = modelFormField.getFieldInfo();

                        // don't do any header for hidden or ignored fields
                        if (fieldInfo.getFieldType() == FieldInfo.HIDDEN
                                || fieldInfo.getFieldType() == FieldInfo.IGNORED) {
                            continue;
                        }

                        if (fieldInfo.getFieldType() != FieldInfo.DISPLAY
                                && fieldInfo.getFieldType() != FieldInfo.DISPLAY_ENTITY
                                && fieldInfo.getFieldType() != FieldInfo.HYPERLINK) {
                            // okay, now do the form cell
                            break;
                        }

                        // if this is a list or multi form don't skip here because we don't want to skip the table cell, will skip the actual field later
                        if (!isListOrMultiForm && !modelFormField.shouldUse(localContext)) {
                            continue;
                        }
                        innerDisplayHyperlinkFieldsBegin.add(modelFormField);
                        currentPosition = modelFormField.getPosition();
                    }
                    Iterator<ModelFormField> innerFormFieldIter = fieldListByPosition.iterator();
                    while (innerFormFieldIter.hasNext()) {
                        ModelFormField modelFormField = innerFormFieldIter.next();
                        FieldInfo fieldInfo = modelFormField.getFieldInfo();

                        // don't do any header for hidden or ignored fields
                        if (fieldInfo.getFieldType() == FieldInfo.HIDDEN
                                || fieldInfo.getFieldType() == FieldInfo.IGNORED) {
                            continue;
                        }

                        // skip all of the display/hyperlink fields
                        // SCIPIO: added SUBMIT here
                        if (fieldInfo.getFieldType() == FieldInfo.DISPLAY
                                || fieldInfo.getFieldType() == FieldInfo.DISPLAY_ENTITY
                                || fieldInfo.getFieldType() == FieldInfo.HYPERLINK
                                || fieldInfo.getFieldType() == FieldInfo.SUBMIT) {
                            continue;
                        }

                        // if this is a list or multi form don't skip here because we don't want to skip the table cell, will skip the actual field later
                        if (!isListOrMultiForm && !modelFormField.shouldUse(localContext)) {
                            continue;
                        }
                        innerFormFields.add(modelFormField);
                        currentPosition = modelFormField.getPosition();
                    }
                    while (innerDisplayHyperlinkFieldIter.hasNext()) {
                        ModelFormField modelFormField = innerDisplayHyperlinkFieldIter.next();
                        FieldInfo fieldInfo = modelFormField.getFieldInfo();

                        // don't do any header for hidden or ignored fields
                        if (fieldInfo.getFieldType() == FieldInfo.HIDDEN
                                || fieldInfo.getFieldType() == FieldInfo.IGNORED) {
                            continue;
                        }

                        // skip all non-display and non-hyperlink fields
                        // SCIPIO: added SUBMIT here
                        if (fieldInfo.getFieldType() != FieldInfo.DISPLAY
                                && fieldInfo.getFieldType() != FieldInfo.DISPLAY_ENTITY
                                && fieldInfo.getFieldType() != FieldInfo.HYPERLINK
                                && fieldInfo.getFieldType() != FieldInfo.SUBMIT) {
                            continue;
                        }

                        // if this is a list or multi form don't skip here because we don't want to skip the table cell, will skip the actual field later
                        if (!isListOrMultiForm && !modelFormField.shouldUse(localContext)) {
                            continue;
                        }
                        innerDisplayHyperlinkFieldsEnd.add(modelFormField);
                        currentPosition = modelFormField.getPosition();
                    }
                          
                    // SCIPIO: Adding submit buttons if use-row-submit flag in the form definition is set to false
                    if (isListOrMultiForm && !modelForm.getUseMasterSubmitField()) {
                        Iterator<ModelFormField> submitFields = modelForm.getMultiSubmitFields().iterator();
                        while (submitFields.hasNext()) {
                            ModelFormField submitField = submitFields.next();
                            if (submitField != null && submitField.shouldUse(context)) {
                                innerDisplayHyperlinkFieldsEnd.add(submitField);
                                fieldListByPosition.add(submitField);
                            }
                        }
                    }
                    
                    List<ModelFormField> hiddenIgnoredFieldList = getHiddenIgnoredFields(localContext, null, tempFieldList,
                            currentPosition);
                    
                    
                    // Rendering:
                    // the fields in the three lists created in the preprocessing phase
                    // are now rendered: this will create a visual representation
                    // of one row (for the current position).
                    if (innerDisplayHyperlinkFieldsBegin.size() > 0 || innerFormFields.size() > 0
                            || innerDisplayHyperlinkFieldsEnd.size() > 0) {
                        
                        numOfColumns = listFormHandler.getNumOfColumns();
                                
                        this.renderItemRow(writer, localContext, formStringRenderer, formPerItem, hiddenIgnoredFieldList,
                                innerDisplayHyperlinkFieldsBegin, innerFormFields, innerDisplayHyperlinkFieldsEnd,
                                fieldListByPosition, currentPosition, numOfColumns, isListOrMultiForm);
                    }
                } // iteration on positions
            } // iteration on items

            // reduce the highIndex if number of items falls short
            if ((itemIndex + 1) < highIndex) {
                highIndex = itemIndex + 1;
                // if list size is overridden, use full listSize
                context.put("highIndex", Integer.valueOf(modelForm.isOverridenListSize() ? listSize : highIndex));
            }
            context.put("actualPageSize", Integer.valueOf(highIndex - lowIndex));

            if (iter instanceof EntityListIterator) {
                try {
                    ((EntityListIterator) iter).close();
                } catch (GenericEntityException e) {
                    Debug.logError(e, "Error closing list form render EntityListIterator: " + e.toString(), module);
                }
            }
        }
    }

    /**
     * SCIPIO: Helper object to handle renderer the table wrappers, headers, etc.
     */
    private class RenderListFormHandler implements RenderItemRowsEventHandler {
        
        private Appendable writer;
        private Map<String, Object> context;
        
        private int numOfColumns = 0;
        
        private boolean wrapperOpened = false;
        private boolean headerRendered = false;
        private boolean footerRendered = false;
        private boolean alternateTextRendered = false;
        private boolean wrapperClosed = false;
        private boolean submitFormRendered = false;
        
        private boolean hasList = false;
        private boolean hasResult = false;
        private boolean hasDisplayResult = false;
        
        public RenderListFormHandler(Appendable writer, Map<String, Object> context) {
            super();
            this.writer = writer;
            this.context = context;
        }
        
        public void renderInit() throws IOException {
            context.put("formHasList", hasList);
            context.put("formHasListResult", hasResult);   
            context.put("formHasDisplayResult", hasDisplayResult);
        }

        @Override
        public void notifyHasList() throws IOException {
            hasList = true;
            context.put("formHasList", hasList);
        }
        
        @Override
        public void notifyHasResult() throws IOException {
            hasResult = true;
            context.put("formHasListResult", hasResult);
        }
        
        @Override
        public void notifyHasDisplayResult() throws IOException {
            hasDisplayResult = true;
            context.put("formHasDisplayResult", hasDisplayResult);
            
            // MUST render the table wrapper if results are going to be displayed
            renderTableOpen(true, false);
        }
        
        public void renderTableOpen(boolean wrapperRequired, boolean headerRequired) throws IOException {
            renderTableWrapperOpen(wrapperRequired);
            renderTableHeader(headerRequired);
        }
        
        @Override
        public int getNumOfColumns() {
            return numOfColumns;
        }
        
        public void renderTableClose() throws IOException {
            renderTableWrapperOpen(false);
            renderTableHeader(false);
            renderAlternateText(false);
            renderTableWrapperClose();
        }

        public void renderFinalize() throws IOException {
            context.remove("formHasList");
            context.remove("formHasListResult"); 
            context.remove("formHasDisplayResult"); 
        }
        
        private void renderTableWrapperOpen(boolean required) throws IOException {
            if (!wrapperOpened) {
                if (required || !modelForm.isHideTableWhen(context)) {   
                    formStringRenderer.renderFormatListWrapperOpen(writer, context, modelForm);
                    wrapperOpened = true;
                }
            }
        }
        
        private void renderTableHeader(boolean required) throws IOException {
            if (wrapperOpened && !headerRendered) {
                if (required ||
                    (!modelForm.isHideHeaderWhen(context) && !modelForm.getHideHeader(context))) {   
                    numOfColumns = renderHeaderRow(writer, context);
                    headerRendered = true;
                }
            }  
        }
        
        private void renderAlternateText(boolean required) throws IOException {
            if (!alternateTextRendered) {
                if (required || modelForm.isUseAlternateTextWhen(context)) {
                    // note: numColumns may be zero if no header printed...
                    formStringRenderer.renderAlternateText(writer, context, modelForm, wrapperOpened, headerRendered, numOfColumns);
                }
                alternateTextRendered = true;
            }
        }
        
        private void renderTableWrapperClose() throws IOException {
            if (wrapperOpened && !wrapperClosed) {
                // render formatting wrapper close
                formStringRenderer.renderFormatListWrapperClose(writer, context, modelForm);
                this.wrapperClosed = true;
            }
        }

        /**
         * SCIPIO: Renders the submit button in the tfoot
         * <p>
         * never causes a table to appear on its own.
         */
        public void renderTableFooter() throws IOException {
            if (!footerRendered && wrapperOpened && hasDisplayResult && !wrapperClosed && UtilValidate.isNotEmpty(modelForm.getMultiSubmitFields())) {
                Iterator<ModelFormField> submitFields = modelForm.getMultiSubmitFields().iterator();
                formStringRenderer.renderFormatFooterRowOpen(writer, context, modelForm);
                
                // gather the submit fields that should actually render
                List<ModelFormField> includedFields = new ArrayList<>(modelForm.getMultiSubmitFields().size());
                while (submitFields.hasNext()) {
                    ModelFormField submitField = submitFields.next();
                    if (submitField != null && submitField.shouldUse(context)) {
                        includedFields.add(submitField);
                    }
                }

                if (includedFields.isEmpty()) {
                    formStringRenderer.renderFormatItemRowCellOpen(writer, context, modelForm, null, numOfColumns);
                    formStringRenderer.renderFormatItemRowCellClose(writer, context, modelForm, null);
                } else {
                    int i = 0;
                    submitFields = includedFields.iterator();
                    while (submitFields.hasNext()) {
                        ModelFormField submitField = submitFields.next();
                        int positionSpan = (submitFields.hasNext()) ? 1 : (numOfColumns - i);
                        formStringRenderer.renderFormatItemRowCellOpen(writer, context, modelForm, submitField, positionSpan);
                        submitField.renderFieldString(writer, context, formStringRenderer);
                        formStringRenderer.renderFormatItemRowCellClose(writer, context, modelForm, submitField);
                        i++;
                    }
                }
                formStringRenderer.renderFormatFooterRowClose(writer, context, modelForm);
                footerRendered = true;
            }
        }
        
        public void renderSubmitForm() throws IOException {
            if (!submitFormRendered && wrapperOpened && wrapperClosed && hasDisplayResult) {
                // in addition, if row-submit, we should have rendered a foot
                if (footerRendered || !modelForm.getUseRowSubmit()) {
                    // SCIPIO: Renders (if not already) a hidden form at the end of the list of results that will be used to submit the values once an action gets triggered.       
                    formStringRenderer.renderSubmitForm(writer, context, modelForm);
                    submitFormRendered = true;
                }
            }
        }
    }
    
    private void renderListFormString(Appendable writer, Map<String, Object> context,
            int positions) throws IOException {
        RenderListFormHandler listFormHandler = new RenderListFormHandler(writer, context);
        listFormHandler.renderInit();
        
        // render list/tabular type forms

        // prepare the items iterator and compute the pagination parameters
        Paginator.preparePager(modelForm, context);

        int numOfColumns = listFormHandler.getNumOfColumns();
        
        // ===== render the item rows =====
        final boolean formPerItem = true; // SCIPIO: NOTE: the orig stock value was true.
        // TODO: set formPerItem = false when figure out alternative, because it produces invalid HTML
        this.renderItemRows(writer, context, formStringRenderer, formPerItem, numOfColumns, listFormHandler);
        
        if (modelForm.getUseMasterSubmitField())
            listFormHandler.renderTableFooter();
        
        listFormHandler.renderTableClose();
        
        // SCIPIO: Renders (if not already) a hidden form at the end of the list of results that will be used to submit the values once an action gets triggered.       
        listFormHandler.renderSubmitForm();
        
        // SCIPIO: 2017-04-21: new
        formStringRenderer.renderFormPageScripts(writer, context, modelForm);
       
        listFormHandler.renderFinalize();
    }

    private void renderMultiFormString(Appendable writer, Map<String, Object> context, 
            int positions) throws IOException {
        RenderListFormHandler listFormHandler = new RenderListFormHandler(writer, context);
        listFormHandler.renderInit();
        
        if (!modelForm.getSkipStart()) {
            formStringRenderer.renderFormOpen(writer, context, modelForm);
        }

        // prepare the items iterator and compute the pagination parameters
        Paginator.preparePager(modelForm, context);

        int numOfColumns = listFormHandler.getNumOfColumns();
        
        // ===== render the item rows =====
        final boolean formPerItem = false; // SCIPIO: NOTE: this was always false even in stock ofbiz
        this.renderItemRows(writer, context, formStringRenderer, formPerItem, numOfColumns, listFormHandler);
        
        if (modelForm.getUseMasterSubmitField())        
            listFormHandler.renderTableFooter(); 

        listFormHandler.renderTableClose();

        if (!modelForm.getSkipEnd()) {
            formStringRenderer.renderMultiFormClose(writer, context, modelForm);
        }
        
        // SCIPIO: Renders (if not already) a hidden form at the end of the list of results that will be used to submit the values once an action gets triggered.   
        // NOTE: this must be OUTSIDE the multi form close because it has its own <form> element
        listFormHandler.renderSubmitForm();
        
        // SCIPIO: 2017-04-21: new
        formStringRenderer.renderFormPageScripts(writer, context, modelForm);

        listFormHandler.renderFinalize();
    }

    private void renderSingleFormString(Appendable writer, Map<String, Object> context, 
            int positions) throws IOException {
        List<ModelFormField> tempFieldList = new LinkedList<ModelFormField>();
        tempFieldList.addAll(modelForm.getFieldList());

        // Check to see if there is a field, same name and same use-when (could come from extended form)
        for (int j = 0; j < tempFieldList.size(); j++) {
            ModelFormField modelFormField = tempFieldList.get(j);
            if (modelForm.getUseWhenFields().contains(modelFormField.getName())) {
                boolean shouldUse1 = modelFormField.shouldUse(context);
                for (int i = j + 1; i < tempFieldList.size(); i++) {
                    ModelFormField curField = tempFieldList.get(i);
                    if (curField.getName() != null && curField.getName().equals(modelFormField.getName())) {
                        boolean shouldUse2 = curField.shouldUse(context);
                        if (shouldUse1 == shouldUse2) {
                            tempFieldList.remove(i--);
                        }
                    } else {
                        continue;
                    }
                }
            }
        }

        Set<String> alreadyRendered = new TreeSet<String>();
        FieldGroup lastFieldGroup = null;
        // render form open
        if (!modelForm.getSkipStart())
            formStringRenderer.renderFormOpen(writer, context, modelForm);

        // render all hidden & ignored fields
        List<ModelFormField> hiddenIgnoredFieldList = this.getHiddenIgnoredFields(context, alreadyRendered, tempFieldList, -1);
        this.renderHiddenIgnoredFields(writer, context, formStringRenderer, hiddenIgnoredFieldList);

        // render formatting wrapper open
        // This should be covered by fieldGroup.renderStartString
        //formStringRenderer.renderFormatSingleWrapperOpen(writer, context, this);

        // render each field row, except hidden & ignored rows
        Iterator<ModelFormField> fieldIter = tempFieldList.iterator();
        ModelFormField lastFormField = null;
        ModelFormField currentFormField = null;
        ModelFormField nextFormField = null;
        if (fieldIter.hasNext()) {
            currentFormField = fieldIter.next();
        }
        if (fieldIter.hasNext()) {
            nextFormField = fieldIter.next();
        }

        FieldGroup currentFieldGroup = null;
        String currentFieldGroupName = null;
        String lastFieldGroupName = null;
        if (currentFormField != null) {
            currentFieldGroup = (FieldGroup) modelForm.getFieldGroupMap().get(currentFormField.getFieldName());
            if (currentFieldGroup == null) {
                currentFieldGroup = modelForm.getDefaultFieldGroup();
            }
            if (currentFieldGroup != null) {
                currentFieldGroupName = currentFieldGroup.getId();
            }
        }

        Integer lastPositionInRow = null;
        RenderRowFieldEntrySequencer rowFieldEntries = null;
        
        boolean isFirstPass = true;
        boolean haveRenderedOpenFieldRow = false;
        while (currentFormField != null) {
            // do the check/get next stuff at the beginning so we can still use the continue stuff easily
            // don't do it on the first pass though...
            if (isFirstPass) {
                isFirstPass = false;
                List<FieldGroupBase> inbetweenList = getInbetweenList(lastFieldGroup, currentFieldGroup);
                for (FieldGroupBase obj : inbetweenList) {
                    if (obj instanceof ModelForm.Banner) {
                        ((ModelForm.Banner) obj).renderString(writer, context, formStringRenderer);
                    }
                }
                if (currentFieldGroup != null && (lastFieldGroup == null || !lastFieldGroupName.equals(currentFieldGroupName))) {
                    currentFieldGroup.renderStartString(writer, context, formStringRenderer);
                    lastFieldGroup = currentFieldGroup;
                }
            } else {
                if (fieldIter.hasNext()) {
                    // at least two loops left
                    lastFormField = currentFormField;
                    currentFormField = nextFormField;
                    nextFormField = fieldIter.next();
                } else if (nextFormField != null) {
                    // okay, just one loop left
                    lastFormField = currentFormField;
                    currentFormField = nextFormField;
                    nextFormField = null;
                } else {
                    // at the end...
                    lastFormField = currentFormField;
                    currentFormField = null;
                    // nextFormField is already null
                    break;
                }
                currentFieldGroup = null;
                if (currentFormField != null) {
                    currentFieldGroup = (FieldGroup) modelForm.getFieldGroupMap().get(currentFormField.getName());
                }
                if (currentFieldGroup == null) {
                    currentFieldGroup = modelForm.getDefaultFieldGroup();
                }
                currentFieldGroupName = currentFieldGroup.getId();

                if (lastFieldGroup != null) {
                    lastFieldGroupName = lastFieldGroup.getId();
                    if (!lastFieldGroupName.equals(currentFieldGroupName)) {
                        if (haveRenderedOpenFieldRow) {
                            rowFieldEntries.processRowEnd(writer, context, positions);
                            formStringRenderer.renderFormatFieldRowClose(writer, context, modelForm);
                            haveRenderedOpenFieldRow = false;
                        }
                        lastFieldGroup.renderEndString(writer, context, formStringRenderer);

                        List<FieldGroupBase> inbetweenList = getInbetweenList(lastFieldGroup, currentFieldGroup);
                        for (FieldGroupBase obj : inbetweenList) {
                            if (obj instanceof ModelForm.Banner) {
                                ((ModelForm.Banner) obj).renderString(writer, context, formStringRenderer);
                            }
                        }
                    }
                }

                if (lastFieldGroup == null || !lastFieldGroupName.equals(currentFieldGroupName)) {
                    currentFieldGroup.renderStartString(writer, context, formStringRenderer);
                    lastFieldGroup = currentFieldGroup;
                }
            }

            FieldInfo fieldInfo = currentFormField.getFieldInfo();
            if (fieldInfo.getFieldType() == FieldInfo.HIDDEN
                    || fieldInfo.getFieldType() == FieldInfo.IGNORED) {
                continue;
            }
            if (alreadyRendered.contains(currentFormField.getName())) {
                continue;
            }
            //Debug.logInfo("In single form evaluating use-when for field " + currentFormField.getName() + ": " + currentFormField.getUseWhen(), module);
            if (!currentFormField.shouldUse(context)) {
                if (UtilValidate.isNotEmpty(lastFormField)) {
                    currentFormField = lastFormField;
                }
                continue;
            }
            alreadyRendered.add(currentFormField.getName());
            if (focusFieldName.isEmpty()) {
                if (fieldInfo.getFieldType() != FieldInfo.DISPLAY && fieldInfo.getFieldType() != FieldInfo.HIDDEN
                        && fieldInfo.getFieldType() != FieldInfo.DISPLAY_ENTITY
                        && fieldInfo.getFieldType() != FieldInfo.IGNORED
                        && fieldInfo.getFieldType() != FieldInfo.IMAGE) {
                    focusFieldName = currentFormField.getName();
                    context.put(modelForm.getName().concat(".focusFieldName"), focusFieldName);
                }
            }

            boolean stayingOnRow = false;
            if (lastFormField != null) {
                if (currentFormField.isCombinePrevious(context, lastFormField)) {
                    // staying on same row
                    stayingOnRow = true;
                }
                else if (lastFormField.getPosition() >= currentFormField.getPosition()) {
                    // moving to next row
                    stayingOnRow = false;
                } else {
                    // staying on same row
                    stayingOnRow = true;
                }
            }

            Integer nextPositionInRow = null;
            // SCIPIO: support a specific position span. note: the value we pass to macro is one less.
            Integer fieldPositionSpan = currentFormField.getPositionSpan();
            int positionSpan;
            if (fieldPositionSpan != null && fieldPositionSpan > 0) {
                positionSpan = fieldPositionSpan - 1;
            }
            else {
                if (nextFormField != null) {
                    if (nextFormField.getPosition() > currentFormField.getPosition()) {
                        positionSpan = nextFormField.getPosition() - currentFormField.getPosition() - 1;
                        nextPositionInRow = Integer.valueOf(nextFormField.getPosition());
                    } else {
                        positionSpan = positions - currentFormField.getPosition();
                    }
                }
                else {
                    positionSpan = positions - currentFormField.getPosition();
                }
                if (fieldPositionSpan == null) {
                    Integer defaultPositionSpan = modelForm.getDefaultPositionSpan();
                    if (defaultPositionSpan != null && defaultPositionSpan > 0) {
                        // form default shouldn't permit breaking grid, less strong 
                        defaultPositionSpan = defaultPositionSpan - 1;
                        if (defaultPositionSpan < positionSpan) {
                            positionSpan = defaultPositionSpan;
                        }
                    }
                }
            }

            // SCIPIO: pass these (and unset below)
            context.put("formFieldRender_positions", positions);
            
            if (stayingOnRow) {
                // no spacer cell, might add later though...
                //formStringRenderer.renderFormatFieldRowSpacerCell(writer, context, currentFormField);
            } else {
                if (haveRenderedOpenFieldRow) {
                    rowFieldEntries.processRowEnd(writer, context, positions);
                    // render row formatting close
                    formStringRenderer.renderFormatFieldRowClose(writer, context, modelForm);
                    haveRenderedOpenFieldRow = false;
                }

                // render row formatting open
                formStringRenderer.renderFormatFieldRowOpen(writer, context, modelForm);
                haveRenderedOpenFieldRow = true;
                lastPositionInRow = null;
                rowFieldEntries = new RenderRowFieldEntrySequencer();
            }

            //
            // It must be a row open before rendering a field. If not, open it
            //
            if (!haveRenderedOpenFieldRow) {
                formStringRenderer.renderFormatFieldRowOpen(writer, context, modelForm);
                haveRenderedOpenFieldRow = true;
                lastPositionInRow = null;
                rowFieldEntries = new RenderRowFieldEntrySequencer();
            }
            
            context.remove("formFieldRender_positions");
            
            // SCIPIO: don't force render form field entry here. allow to accumulate them for row and render all at once at row close.
            // This allows delayed render so more info available and fixes ofbiz bug where nextFormField was
            // sometimes a field that was not going to be rendered, giving invalid positions.
            // render form field
            //new RenderFieldEntry(...).render(...);
            rowFieldEntries.acceptFieldEntry(new RenderFieldEntry(currentFormField, positionSpan, nextPositionInRow, lastPositionInRow));
            
            lastPositionInRow = currentFormField.getPosition();
        }
        // render row formatting close after the end if needed
        if (haveRenderedOpenFieldRow) {
            rowFieldEntries.processRowEnd(writer, context, positions);
            formStringRenderer.renderFormatFieldRowClose(writer, context, modelForm);
        }

        if (lastFieldGroup != null) {
            lastFieldGroup.renderEndString(writer, context, formStringRenderer);
        }
        // render formatting wrapper close
        // should be handled by renderEndString
        //formStringRenderer.renderFormatSingleWrapperClose(writer, context, this);

        // render form close
        if (!modelForm.getSkipEnd())
            formStringRenderer.renderFormClose(writer, context, modelForm);
        
        // SCIPIO: 2017-04-21: new
        formStringRenderer.renderFormPageScripts(writer, context, modelForm);
    }

    /**
     * SCIPIO: Factored out field entry render code
     */
    private class RenderFieldEntry {
        private final ModelFormField formField;
        // change in plans: recalculate these at end and ignore ones in loop
        //private final int positionSpan;
        //private final Integer nextPositionInRow;
        //private final Integer lastPositionInRow;
        
        public RenderFieldEntry(ModelFormField currentFormField,
                int positionSpan, Integer nextPositionInRow, Integer lastPositionInRow) {
            super();
            this.formField = currentFormField;
            //this.positionSpan = positionSpan;
            //this.nextPositionInRow = nextPositionInRow;
            //this.lastPositionInRow = lastPositionInRow;
        }
        
        public ModelFormField getFormField() {
            return formField;
        }
        
        public void render(Appendable writer, Map<String, Object> context, 
                int positions, int positionSpan, Integer nextPositionInRow, Integer lastPositionInRow) throws IOException {
            this.render(writer, context, positions, positionSpan, nextPositionInRow, lastPositionInRow, true, true, true);
        }
        
        public void render(Appendable writer, Map<String, Object> context, 
                int positions, int positionSpan, Integer nextPositionInRow, Integer lastPositionInRow,
                boolean openWrappers, boolean renderBody, boolean closeWrappers) throws IOException {
            FieldInfo fieldInfo = formField.getFieldInfo();
            
            context.put("formFieldRender_positions", positions);
            context.put("formFieldRender_position", formField.getPosition());
            context.put("formFieldRender_positionSpan", positionSpan);
            context.put("formFieldRender_nextPositionInRow", nextPositionInRow);
            context.put("formFieldRender_lastPositionInRow", lastPositionInRow);
            
            if (openWrappers) {
                // render title formatting open
                formStringRenderer.renderFormatFieldRowTitleCellOpen(writer, context, formField);
    
                // render title (unless this is a submit or a reset field)
                if (fieldInfo.getFieldType() != FieldInfo.SUBMIT
                        && fieldInfo.getFieldType() != FieldInfo.RESET) {
                    formStringRenderer.renderFieldTitle(writer, context, formField);
                } else {
                    // SCIPIO: 2017-01-13: new role parameter
                    context.put("renderFormatEmptySpace_role", "field-title");
                    formStringRenderer.renderFormatEmptySpace(writer, context, modelForm);
                    context.remove("renderFormatEmptySpace_role");
                }
    
                // render title formatting close
                formStringRenderer.renderFormatFieldRowTitleCellClose(writer, context, formField);
    
                // render separator
                formStringRenderer.renderFormatFieldRowSpacerCell(writer, context, formField);
    
                // render widget formatting open
                formStringRenderer.renderFormatFieldRowWidgetCellOpen(writer, context, formField, positions, positionSpan,
                        nextPositionInRow);
            }

            if (renderBody) {
                // render widget
                formField.renderFieldString(writer, context, formStringRenderer);
            }

            if (closeWrappers) {
                // render widget formatting close
                formStringRenderer.renderFormatFieldRowWidgetCellClose(writer, context, formField, positions, positionSpan,
                        nextPositionInRow);
            }
            
            context.remove("formFieldRender_positions");
            context.remove("formFieldRender_position");
            context.remove("formFieldRender_positionSpan");
            context.remove("formFieldRender_nextPositionInRow");
            context.remove("formFieldRender_lastPositionInRow");
        }
        
    }
    
    /**
     * SCIPIO: renders accumulated field entries all at once (for delayed render).
     */
    private class RenderRowFieldEntrySequencer {
        private List<RenderFieldEntry> fieldEntries = new ArrayList<RenderFieldEntry>();
        
        /**
         * Accepts a new field entry for rendering. Currently simply accumulates.
         */
        public void acceptFieldEntry(RenderFieldEntry fieldEntry) {
            // simply accumulate
            fieldEntries.add(fieldEntry);
        }
        
        /**
         * Processes row close. Currently renders all accumulated entries.
         */
        public void processRowEnd(Appendable writer, Map<String, Object> context, 
                int positions) throws IOException {
            // note: the "last" and "next" form fields here may differ from the ones in the main loop
            // these are for calculating positions only
            
            Integer lastPositionInRow = null;
            for(int i = 0; i < fieldEntries.size(); i++) {
                RenderFieldEntry fieldEntry = fieldEntries.get(i);
                ModelFormField currentFormField = fieldEntry.getFormField();
                
                ModelFormField nextFormField = null;
                List<RenderFieldEntry> combinedFields = new ArrayList<RenderFieldEntry>();
                // find out if any fields are supposed to be body-combined with this one
                // and get next non-combining field
                int j = i + 1;
                RenderFieldEntry prevEntry = fieldEntry;
                while(nextFormField == null && j < fieldEntries.size()) {
                    RenderFieldEntry candidateEntry = fieldEntries.get(j);
                    if (candidateEntry.getFormField().isCombinePrevious(context, prevEntry.getFormField())) {
                        combinedFields.add(candidateEntry);
                    }
                    else {
                        nextFormField = candidateEntry.getFormField();
                    }
                    prevEntry = candidateEntry;
                    j++;
                }
                    
                Integer nextPositionInRow = null;
                // SCIPIO: support a specific position span. note: the value we pass to macro is one less.
                Integer fieldPositionSpan = currentFormField.getPositionSpan();
                int positionSpan;
                if (fieldPositionSpan != null && fieldPositionSpan > 0) {
                    positionSpan = fieldPositionSpan - 1;
                }
                else {
                    if (nextFormField != null) {
                        if (nextFormField.getPosition() > currentFormField.getPosition()) {
                            positionSpan = nextFormField.getPosition() - currentFormField.getPosition() - 1;
                            nextPositionInRow = Integer.valueOf(nextFormField.getPosition());
                        } else {
                            positionSpan = positions - currentFormField.getPosition();
                        }
                    }
                    else {
                        positionSpan = positions - currentFormField.getPosition();
                    }
                    if (fieldPositionSpan == null) {
                        Integer defaultPositionSpan = modelForm.getDefaultPositionSpan();
                        if (defaultPositionSpan != null && defaultPositionSpan > 0) {
                            // form default shouldn't permit breaking grid, is weaker than field setting
                            defaultPositionSpan = defaultPositionSpan - 1;
                            if (defaultPositionSpan < positionSpan) {
                                positionSpan = defaultPositionSpan;
                            }
                        }
                    }
                }
                
                if (combinedFields.size() <= 0) {
                    fieldEntry.render(writer, context, positions, positionSpan, nextPositionInRow, lastPositionInRow);
                }
                else {
                    fieldEntry.render(writer, context, positions, positionSpan, nextPositionInRow, lastPositionInRow, true, true, false);
                    for(RenderFieldEntry combinedField : combinedFields) {
                        combinedField.render(writer, context, positions, positionSpan, nextPositionInRow, lastPositionInRow, false, true, false);
                    }
                    fieldEntry.render(writer, context, positions, positionSpan, nextPositionInRow, lastPositionInRow, false, false, true);
                }
                
                lastPositionInRow = currentFormField.getPosition();
                // skip the combined ones
                if (combinedFields.size() > 0) {
                    i += combinedFields.size();
                }
            }
        }
    }
    
    private void resetBshInterpreter(Map<String, Object> context) {
        context.remove("bshInterpreter");
    }

    private static <X> X safeNext(Iterator<X> iterator) {
        try {
            return iterator.next();
        } catch (NoSuchElementException e) {
            return null;
        }
    }
}
