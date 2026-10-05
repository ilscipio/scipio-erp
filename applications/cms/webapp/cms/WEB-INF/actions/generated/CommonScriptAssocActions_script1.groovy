import org.ofbiz.base.util.*;
                import org.ofbiz.entity.*;
                import org.ofbiz.entity.condition.*;
                import com.ilscipio.scipio.cms.template.CmsScriptTemplate;

                final String module = "CommonEditRenderTemplateActions.groovy";

                standaloneScriptTemplates = [];
                slaveScriptTemplates = [];
                allScriptTemplates = [];
                try {
                    for (st in CmsScriptTemplate.getWorker().findAll(delegator, (EntityCondition) null, ["templateName"], false)) {
                        if (st.isStandalone()) {
                            standaloneScriptTemplates.add(st);
                        } else {
                            slaveScriptTemplates.add(st);
                        }
                    }
                    allScriptTemplates.addAll(standaloneScriptTemplates);
                    allScriptTemplates.addAll(slaveScriptTemplates);
                } catch (Exception e) {
                    Debug.logError(e, "Cms: Could not read script templates", module);
                }
                context.standaloneScriptTemplates = standaloneScriptTemplates;
                context.slaveScriptTemplates = slaveScriptTemplates;
                context.allScriptTemplates = allScriptTemplates;