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
package org.ofbiz.widget.model;

import java.util.List;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.entity.model.ModelReader;
import org.ofbiz.service.DispatchContext;
import org.w3c.dom.Element;

/**
 * Models the &lt;form&gt; element.
 *
 * @see <code>widget-form.xsd</code>
 */
@SuppressWarnings("serial")
public class ModelSingleForm extends ModelForm {

    /*
     * ----------------------------------------------------------------------- *
     *                     DEVELOPERS PLEASE READ
     * ----------------------------------------------------------------------- *
     *
     * This model is intended to be a read-only data structure that represents
     * an XML element. Outside of object construction, the class should not
     * have any behaviors. All behavior should be contained in model visitors.
     *
     * Instances of this class will be shared by multiple threads - therefore
     * it is immutable. DO NOT CHANGE THE OBJECT'S STATE AT RUN TIME!
     *
     */

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /** XML Constructor */
    public ModelSingleForm(Element formElement, String formLocation, ModelReader entityModelReader,
            DispatchContext dispatchContext) {
        super(formElement, formLocation, entityModelReader, dispatchContext, "single");
    }

    @Override
    public void accept(ModelWidgetVisitor visitor) throws Exception {
        visitor.visit(this);
    }

    protected ModelForm getParentModel(Element formElement, ModelReader entityModelReader, DispatchContext dispatchContext) {
        ModelForm parent = null;
        String parentResource = formElement.getAttribute("extends-resource");
        String parentForm = formElement.getAttribute("extends");
        if (!parentForm.isEmpty()) {
            // check if we have a resource name
            if (!parentResource.isEmpty()) {
                try {
                    parent = FormFactory.getFormFromLocation(parentResource, parentForm, entityModelReader, dispatchContext);
                } catch (Exception e) {
                    Debug.logError(e, "Failed to load parent form definition '" + parentForm + "' at resource '" + parentResource
                            + "'", module);
                }
            } else if (!parentForm.equals(formElement.getAttribute("name"))) {
                // try to find a form definition in the same file
                Element rootElement = formElement.getOwnerDocument().getDocumentElement();
                // SCIPIO: Forward-compatibility with grid
                //List<? extends Element> formElements = UtilXml.childElementList(rootElement, "form");
                List<? extends Element> formElements = UtilXml.childElementList(rootElement);
                //Uncomment below to add support for abstract forms
                //formElements.addAll(UtilXml.childElementList(rootElement, "abstract-form"));
                for (Element parentElement : formElements) {
                    if (!("grid".equals(parentElement.getTagName()) || "form".equals(parentElement.getTagName()))) { // SCIPIO
                        continue;
                    }
                    if (parentElement.getAttribute("name").equals(parentForm)) {
                        parent = FormFactory.createModelForm(parentElement, entityModelReader, dispatchContext, parentResource,
                                parentForm);
                        break;
                    }
                }
                if (parent == null) {
                    Debug.logError("Failed to find parent form definition '" + parentForm + "' in same document.", module);
                }
            } else {
                Debug.logError("Recursive form definition found for '" + formElement.getAttribute("name") + ".'", module);
            }
        }
        return parent;
    }

    @Override
    public String getWidgetType() { // SCIPIO
        return "form";
    }
}
