package com.ilscipio.scipio.ce.webapp.ftl.context;

import java.io.IOException;
import java.util.Map;

import com.ilscipio.scipio.ce.webapp.ftl.context.MultiVarMethod.ClearVarsMethod;
import com.ilscipio.scipio.ce.webapp.ftl.context.MultiVarMethod.CommonVarMaps;
import com.ilscipio.scipio.ce.webapp.ftl.context.MultiVarMethod.ExtractVarsMethod;
import com.ilscipio.scipio.ce.webapp.ftl.context.MultiVarMethod.SetVarsMethod;

import freemarker.core.Environment;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;

/**
 * <code>@varSection</code> implementation.
 */
public class VarSectionDirective implements TemplateDirectiveModel {

    public VarSectionDirective() {
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    @Override
    public void execute(Environment env, Map params, TemplateModel[] loopVars, TemplateDirectiveBody body)
            throws TemplateException, IOException {
        this.executeTyped(env, params, loopVars, body);
    }

    protected void executeTyped(Environment env, Map<String, TemplateModel> params, TemplateModel[] loopVars, TemplateDirectiveBody body)
            throws TemplateException, IOException {
        CommonVarMaps<Map<String, Object>> varMaps = CommonVarMaps.getRawMaps(params);
        boolean restoreValues = TransformUtil.getBooleanArg(params, "restoreValues", true);
        boolean clearValues = TransformUtil.getBooleanArg(params, "clearValues", false);

        CommonVarMaps<Boolean> skipSet = new CommonVarMaps<>(
                TransformUtil.getBooleanArg(params, "skipSetCtxVars", false),
                TransformUtil.getBooleanArg(params, "skipSetGlobalCtxVars", false),
                TransformUtil.getBooleanArg(params, "skipSetReqAttribs", false));

        CommonVarMaps<Map<String, Object>> origValues = null;
        if (restoreValues && !clearValues) {
            origValues = ExtractVarsMethod.extractVars(CommonVarMaps.getRawSequences(varMaps), true, env);
        }

        SetVarsMethod.setVars(varMaps, skipSet, env);

        if (body != null) {
            body.render(env.getOut());
        }

        if (clearValues) {
            ClearVarsMethod.clearVars(CommonVarMaps.getRawSequences(varMaps), env);
        } else if (restoreValues) {
            SetVarsMethod.setVars(origValues, env);
        }
    }

}
