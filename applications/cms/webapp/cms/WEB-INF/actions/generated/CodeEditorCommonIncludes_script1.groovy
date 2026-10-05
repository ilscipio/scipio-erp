import org.ofbiz.base.util.*;
            
                ctx = globalContext;
                
                // Script languages which can currently be executed from stored bodies
                // script names should be a subset of: CmsScriptTemplate.ScriptLang.getNames()
                // This is limited by the Ofbiz script utils/API, which mostly expect file locations rather than bodies.
                ctx.supportedScriptBodyLangs = ["groovy"];
                ctx.defaultScriptBodyLang = "groovy";
                
                // Script language names we currently accept for template locations
                // FIXME: for now, we always required "auto" - auto-determine language from location, to simplify our code; 
                //     later we should allow override, because the auto-resolve algorithm is weak (see CmsScriptTemplate.ScriptExecutor)
                //     In theory we should allow: "groovy", "simple-method", "screen-actions", "auto"
                ctx.supportedScriptLocationLangs = ["auto"];
                ctx.defaultScriptLocationLang = "auto";
            
                // map of internal CMS script lang names to CodeMirror lang modes
                ctx.scriptLangEditorModeMap = [
                    "groovy" : "groovy",
                    "screen-actions" : "xml",
                    "simple-method" : "xml",
                    // FIXME: what is sane default/fallback/none mode? "clike"? I am putting "text" so that nothing highlights for these, but it's not a real mode name.
                    "auto" : "text",
                    "none" : "text",
                    "default" : "text" // default is for anything that doesn't map into the above
                ];
                
                ctx.indentWithTabs = UtilProperties.getPropertyAsBoolean("cms", "cms.editor.indentWithTabs", false);