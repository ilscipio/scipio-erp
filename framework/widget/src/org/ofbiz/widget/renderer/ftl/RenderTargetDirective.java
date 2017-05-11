package org.ofbiz.widget.renderer.ftl;

import java.io.IOException;
import java.io.Writer;
import java.util.Map;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.webapp.renderer.RenderWriter;
import org.ofbiz.widget.renderer.RenderTargetExpr;
import org.ofbiz.widget.renderer.RenderTargetExpr.ModelFtlWidget;
import org.ofbiz.widget.renderer.RenderTargetExpr.RenderTargetState;

import com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateHashModel;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateScalarModel;

/**
 * <code>@varSection</code> implementation.
 */
public class RenderTargetDirective implements TemplateDirectiveModel {

    public RenderTargetDirective() {
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
            RenderTargetState renderTargetState = RenderTargetExpr.getRenderTargetState(context);
            if (renderTargetState.isEnabled()) {
                String dirType = TransformUtil.getStringArg(params, "dirType");
                TemplateHashModel dirArgs = (TemplateHashModel) params.get("dirArgs");
                
                TemplateScalarModel idModel = (TemplateScalarModel) dirArgs.get("id");
                if (idModel != null) {
                    String id = LangFtlUtil.getAsStringNonEscaping(idModel);
                    if ("section".equals(dirType)) {
                        TemplateScalarModel containerIdModel = (TemplateScalarModel) dirArgs.get("containerId");
                        if (containerIdModel != null) {
                            String containerId = LangFtlUtil.getAsStringNonEscaping(containerIdModel);
                            if (UtilValidate.isNotEmpty(containerId)) {
                                id = containerId;
                            }
                        }
                    }
                    
                    // ONLY ID SUPPORTED
                    if (UtilValidate.isNotEmpty(id)) {
                        String location = "unknown-location"; // FIXME
                        ModelFtlWidget widget = new ModelFtlWidget("", dirType, location, id);
                        
                        RenderTargetState.ExecutionInfo execInfo = renderTargetState.handleShouldExecute(widget, writer, context, null);
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
            }
        }
        
        body.render(writer);
    }

}
