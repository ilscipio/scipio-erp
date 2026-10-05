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
package org.ofbiz.widget.renderer.ftl;

import java.io.IOException;
import java.io.Writer;
import java.util.Map;

import org.ofbiz.webapp.renderer.RenderWriter;
import org.ofbiz.widget.model.ftl.ModelFtlWidget;
import org.ofbiz.widget.model.ftl.ModelVirtualSectionFtlWidget;
import org.ofbiz.widget.renderer.WidgetRenderTargetExpr;
import org.ofbiz.widget.renderer.WidgetRenderTargetExpr.WidgetRenderTargetState;

import com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;

/**
 * <code>@virtualSection</code> implementation - defines a rendering section without any actual markup
 * (similar to <code>@fields</code>).
 * FIXME: we have no choice to put this in widget package due to dependencies...
 */
public class VirtualSectionDirective implements TemplateDirectiveModel {

    public VirtualSectionDirective() {
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    @Override
    public void execute(Environment env, Map params, TemplateModel[] loopVars, TemplateDirectiveBody body)
            throws TemplateException, IOException {
        this.executeTyped(env, params, loopVars, body);
    }

    protected void executeTyped(Environment env, Map<String, TemplateModel> params, TemplateModel[] loopVars, TemplateDirectiveBody body)
            throws TemplateException, IOException {
        Writer writer = env.getOut();

        // NOTE: this can only work if we already had a RenderWriter.
        // if not, don't even bother trying.
        if (writer instanceof RenderWriter) {
            Map<String, Object> context = ContextFtlUtil.getContext(env);
            WidgetRenderTargetState renderTargetState = WidgetRenderTargetExpr.getRenderTargetState(context);
            if (renderTargetState.isEnabled()) {
                String name = TransformUtil.getStringNonEscapingArg(params, "name");
                String containsExpr = TransformUtil.getStringNonEscapingArg(params, "contains");
                String location = "unknown-location"; // FIXME
                ModelFtlWidget widget = new ModelVirtualSectionFtlWidget(name, location, containsExpr);

                WidgetRenderTargetState.ExecutionInfo execInfo = renderTargetState.handleShouldExecute(widget, writer, context, null);
                if (!execInfo.shouldExecute()) {
                    return;
                }
                try {
                    if (body != null) {
                        body.render((Writer) execInfo.getWriterForElementRender());
                    }
                } finally {
                    execInfo.handleFinished(context); // SCIPIO: return logic
                }
                return;
            }
        }

        body.render(writer);
    }

}
