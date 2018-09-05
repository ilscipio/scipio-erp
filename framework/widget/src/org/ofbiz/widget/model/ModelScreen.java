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
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.widget.renderer.ScreenRenderException;
import org.ofbiz.widget.renderer.ScreenStringRenderer;
import org.ofbiz.widget.renderer.WidgetRenderTargetExpr;
import org.ofbiz.widget.renderer.WidgetRenderTargetExpr.WidgetRenderTargetState;
import org.w3c.dom.Element;

/**
 * Widget Library - Screen model class
 */
@SuppressWarnings("serial")
public class ModelScreen extends ModelWidget implements ModelScreens.ScreenEntry {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    static final Set<String> validScreenElementTagNames = Collections.unmodifiableSet(
            UtilMisc.toSet("screen")); // SCIPIO: new, for future use
    
    private final String sourceLocation;
    private final FlexibleStringExpander transactionTimeoutExdr;
    // SCIPIO: generalized
    //private final Map<String, ModelScreen> modelScreenMap;
    private final ModelScreenGroup modelScreenGroup;
    private final boolean useTransaction;
    private final boolean useCache;
    private final ModelScreenWidget.Section section;

    /** XML Constructor */
    public ModelScreen(Element screenElement, ModelScreenGroup modelScreenGroup, String sourceLocation) {
        super(screenElement);
        this.sourceLocation = sourceLocation;
        this.transactionTimeoutExdr = FlexibleStringExpander.getInstance(screenElement.getAttribute("transaction-timeout"));
        this.modelScreenGroup = modelScreenGroup;
        this.useTransaction = "true".equals(screenElement.getAttribute("use-transaction"));
        this.useCache = "true".equals(screenElement.getAttribute("use-cache"));

        // read in the section, which will read all sub-widgets too
        Element sectionElement = UtilXml.firstChildElement(screenElement, "section");
        if (sectionElement == null) {
            // SCIPIO: we support actions or widgets block shorthand
            // NOTE: the XSD may not support widgets block shorthand for now, 
            // because unlike actions shorthand, widgets shorthand usually ends up counterproductive...
            sectionElement = UtilXml.firstChildElement(screenElement, "actions");
            if (sectionElement == null) {
                sectionElement = UtilXml.firstChildElement(screenElement, "widgets");
                if (sectionElement == null) {
                    throw new IllegalArgumentException("No section (or actions/widgets shorthand) found for the screen definition with name: " + getName());
                }
            }
        } 
        this.section = new ModelScreenWidget.Section(this, sectionElement, true);
    }

    /**
     * SCIPIO: Returns true if this screen only contains actions as content, or in other words,
     * if this screen represents a collection of reusable actions.
     * <p>
     * Note the best way to express this in screen files is to omit the top sections and use
     * only the actions block.
     */
    public boolean isActionsOnly() {
        return section.isActionsOnly();
    }
    
    /**
     * SCIPIO: Returns true if the named element is a screen (top-level) element or any element
     * that can stand in for it.
     */
    public static boolean isScreenElement(Element element) {
        return validScreenElementTagNames.contains(element.getTagName());
    }
    
    @Override
    public void accept(ModelWidgetVisitor visitor) throws Exception {
        visitor.visit(this);
    }

    public String getTransactionTimeout() {
        return transactionTimeoutExdr.getOriginal();
    }

    /**
     * Returns model screen map.
     * <p>
     * SCIPIO: return value changed to ModelScreens, which implements Map<String, ModelScreen> and should work
     * with all existing code.
     */
    public ModelScreens getModelScreenMap() {
        // SCIPIO: get from parent group
        //return modelScreenMap;
        return modelScreenGroup.getModelScreens();
    }
    
    /**
     * SCIPIO: getModelScreenGroup (new).
     */
    public ModelScreenGroup getModelScreenGroup() {
        return modelScreenGroup;
    }

    public boolean getUseTransaction() {
        return useTransaction;
    }

    public boolean getUseCache() {
        return useCache;
    }

    public ModelScreenWidget.Section getSection() {
        return section;
    }

    public String getSourceLocation() {
        return sourceLocation;
    }

    /**
     * Renders this screen to a String, i.e. in a text format, as defined with the
     * ScreenStringRenderer implementation.
     * SCIPIO: 2017-05-09: RENAMED to renderScreenStringCore.
     *
     * @param writer The Writer that the screen text will be written to
     * @param context Map containing the screen context; the following are
     *   reserved words in this context:
     *    - parameters (contains any special initial parameters coming in)
     *    - userLogin (if a user is logged in)
     *    - autoUserLogin (if a user is automatically logged in, ie no password has been entered)
     *    - formStringRenderer
     *    - request, response, session, application (special case, only in HTML contexts, etc)
     *    - delegator, dispatcher, security
     *    - null (represents a null field value for entity operations)
     *    - sections (used for decorators to reference the sections to be decorated and render them)
     * @param screenStringRenderer An implementation of the ScreenStringRenderer
     *   interface that is responsible for the actual text generation for
     *   different screen elements; implementing your own makes it possible to
     *   use the same screen definitions for many types of screen UIs
     */
    protected void renderScreenStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws ScreenRenderException {
        // make sure the "nullField" object is in there for entity ops
        context.put("nullField", GenericEntity.NULL_FIELD);

        // wrap the whole screen rendering in a transaction, should improve performance in querying and such
        Map<String, String> parameters = UtilGenerics.cast(context.get("parameters"));
        boolean beganTransaction = false;
        int transactionTimeout = -1;
        if (parameters != null) {
            String transactionTimeoutPar = parameters.get("TRANSACTION_TIMEOUT");
            if (transactionTimeoutPar != null) {
                try {
                    transactionTimeout = Integer.parseInt(transactionTimeoutPar);
                } catch (NumberFormatException nfe) {
                    String msg = "TRANSACTION_TIMEOUT parameter for screen [" + this.sourceLocation + "#" + getName() + "] is invalid and it will be ignored: " + nfe.toString();
                    Debug.logWarning(msg, module);
                }
            }
        }

        if (transactionTimeout < 0 && !transactionTimeoutExdr.isEmpty()) {
            // no TRANSACTION_TIMEOUT parameter, check screen attribute
            String transactionTimeoutStr = transactionTimeoutExdr.expandString(context);
            if (UtilValidate.isNotEmpty(transactionTimeoutStr)) {
                try {
                    transactionTimeout = Integer.parseInt(transactionTimeoutStr);
                } catch (NumberFormatException e) {
                    Debug.logWarning(e, "Could not parse transaction-timeout value, original=[" + transactionTimeoutExdr + "], expanded=[" + transactionTimeoutStr + "]", module);
                }
            }
        }

        try {
            // If transaction timeout is not present (i.e. is equal to -1), the default transaction timeout is used
            // If transaction timeout is present, use it to start the transaction
            // If transaction timeout is set to zero, no transaction is started
            if (useTransaction) {
                if (transactionTimeout < 0) {
                    beganTransaction = TransactionUtil.begin();
                }
                if (transactionTimeout > 0) {
                    beganTransaction = TransactionUtil.begin(transactionTimeout);
                }
            }

            // render the screen, starting with the top-level section
            this.section.renderWidgetString(writer, context, screenStringRenderer);
            TransactionUtil.commit(beganTransaction);
        // SCIPIO: 2018-09-04: TODO: REVIEW: this is from upstream, but I believe it's at least
        // half an error because it bypasses the rollback...
        //} catch (RuntimeException e) {
        //    throw e;
        } catch (Exception e) {
            String errMsg = "Error rendering screen [" + this.sourceLocation + "#" + getName() + "]: " + e.toString();
            Debug.logError(errMsg + ". Rolling back transaction.", module);
            try {
                // only rollback the transaction if we started one...
                TransactionUtil.rollback(beganTransaction, errMsg, e);
            } catch (GenericEntityException e2) {
                Debug.logError(e2, "Could not rollback transaction: " + e2.toString(), module);
            }

            // throw nested exception, don't need to log details here: Debug.logError(e, errMsg, module);

            // after rolling back, rethrow the exception
            // SCIPIO: WORKAROUND: only wrap if it's not already a ScreenRenderException, to avoid huge log overload
            // (even with this it is still heavy)
            if (e instanceof ScreenRenderException) {
                throw ((ScreenRenderException) e);
            } else {
                throw new ScreenRenderException(errMsg, e);
            }
        }
    }

    /**
     * SCIPIO: New wrapper around *Core method for targeted rendering.
     */
    public final void renderScreenString(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws ScreenRenderException {
        // SCIPIO: targeted rendering applicability check.
        WidgetRenderTargetState renderTargetState = WidgetRenderTargetExpr.getRenderTargetState(context);
        WidgetRenderTargetState.ExecutionInfo execInfo;
        try {
            execInfo = renderTargetState.handleShouldExecute(this, writer, context, screenStringRenderer);
        } catch (IOException e) {
            throw new ScreenRenderException(e);
        }
        if (!execInfo.shouldExecute()) {
            return;
        }
        try {
            renderScreenStringCore(execInfo.getWriterForElementRender(), context, screenStringRenderer);
        } finally {
            try {
                execInfo.handleFinished(context); // SCIPIO: return logic
            } catch (IOException e) {
                throw new ScreenRenderException(e);
            }
        }
    }
    
    public LocalDispatcher getDispatcher(Map<String, Object> context) {
        LocalDispatcher dispatcher = (LocalDispatcher) context.get("dispatcher");
        return dispatcher;
    }

    public Delegator getDelegator(Map<String, Object> context) {
        Delegator delegator = (Delegator) context.get("delegator");
        return delegator;
    }

    @Override
    public List<ModelScreen> getScreenList() { // SCIPIO: new
        return UtilMisc.toList(this);
    }
 
    @Override
    public String getContainerLocation() { // SCIPIO: new
        return sourceLocation;
    }
    
    @Override
    public String getWidgetType() { // SCIPIO: new
        return "screen";
    }
}


