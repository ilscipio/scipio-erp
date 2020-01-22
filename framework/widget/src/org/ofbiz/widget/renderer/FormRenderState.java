package org.ofbiz.widget.renderer;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.collections.CompositeReadOnlyMap;
import org.ofbiz.widget.model.ModelForm;
import org.ofbiz.widget.model.ModelFormField;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * SCIPIO: a state passed around in context used to record info about the form render.
 * TODO: This is currently only partially used, may be improved in future
 * <p>
 * NOTE: {@link FormRenderer} is always recreated so it may not always carry over the information, and this parallels MenuRenderState.
 * <p>
 * NOTE: this probably didn't need to be serializable, but is just in case
 */
@SuppressWarnings("serial")
public class FormRenderState extends CompositeReadOnlyMap<String, Object> implements Serializable {

    public static final String CONTEXT_KEY = "currentFormRenderState";

    // TODO: this requires a ModelForm/info stack to correctly represent
    //private List<ModelForm> formStack;

    // NOTE: these are also stored in the map, may change the redundancy later
    private int currentDepth;

    private transient FormFieldState formFieldState;

    protected FormRenderState(Map<String, Object> context, ModelForm modelForm) {
        //this.formStack = UtilMisc.toList(modelForm);
        setCurrentDepth(1);
        this.formFieldState = null;
    }

    protected Object setArg(String key, Object value) {
        if ("currentDepth".equals(key)) {
            this.setCurrentDepth((Integer) value);
            return this.get(key);
        } else {
            return setInternal(key, value);
        }
    }

    protected void setArgs(Map<? extends String, ?> args) {
        for(Entry<? extends String, ?> entry : args.entrySet()) {
            this.setArg(entry.getKey(), entry.getValue());
        }
    }

    protected Object setInternal(String key, Object value) {
        return internalMap.put(key, value);
    }

    public static FormRenderState create(Map<String, Object> context, ModelForm modelForm) {
        return new FormRenderState(context, modelForm);
    }

    public int getCurrentDepth() {
        return currentDepth;
    }

    protected void setCurrentDepth(Integer currentDepth) {
        if (currentDepth == null) {
            this.currentDepth = 1;
        } else {
            this.currentDepth = currentDepth;
        }
        setInternal("currentDepth", this.currentDepth);
    }

    //public ModelForm getModelForm() {
    //    return modelForm;
    //}

    public void increaseCurrentDepth() {
        this.currentDepth++;
        setInternal("currentDepth", this.currentDepth);
    }

    public void decreaseCurrentDepth() {
        this.currentDepth--;
        setInternal("currentDepth", this.currentDepth);
    }

    //public FormFieldState getFormFieldState() {
    //    return formFieldState;
    //}

    //public void setFormFieldState(FormFieldState formFieldState) {
    //    this.formFieldState = formFieldState;
    //    this.put("itemState", formFieldState);
    //}

    // context helper methods

    public static FormRenderState push(Map<String, Object> context, ModelForm modelForm) {
        FormRenderState renderState = FormRenderState.retrieve(context);
        if (renderState == null) {
            renderState = FormRenderState.createAndStore(context, modelForm);
        } else {
            renderState.increaseCurrentDepth();
        }
        return renderState;
    }

    public void pop(Map<String, Object> context) {
        if (getCurrentDepth() <= 1) {
            FormRenderState.remove(context);
        } else {
            decreaseCurrentDepth();
        }
    }

    public static FormRenderState createAndStore(Map<String, Object> context, ModelForm modelForm) {
        FormRenderState renderState = create(context, modelForm);
        store(context, renderState);
        return renderState;
    }

    public static void store(Map<String, Object> context, FormRenderState renderState) {
        context.put(CONTEXT_KEY, renderState);
    }

    public static boolean hasRenderState(Map<String, Object> context) {
        return (retrieve(context) != null);
    }

    public static FormRenderState retrieve(Map<String, Object> context) {
        return (FormRenderState) context.get(CONTEXT_KEY);
    }

    public static FormRenderState remove(Map<String, Object> context) {
        return (FormRenderState) context.remove(CONTEXT_KEY);
    }

    @Override
    public Object put(String key, Object value) {
        return setArg(key, value);
    }

    @Override
    public void putAll(Map<? extends String, ? extends Object> m) {
        setArgs(m);
    }

    public static class FormFieldState extends CompositeReadOnlyMap<String, Object> implements Serializable {

        protected FormFieldState() {
        }

        public static FormFieldState fromCurrent(ModelFormField modelFormField, Map<String, Object> context) {
            return fromCurrent(modelFormField, context, FormRenderState.retrieve(context));
        }

        public static FormFieldState fromCurrent(ModelFormField modelFormField, Map<String, Object> context,
                FormRenderState renderState) {
            return new FormFieldState();
        }
    }

}