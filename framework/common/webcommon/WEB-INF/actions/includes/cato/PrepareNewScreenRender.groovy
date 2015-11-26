/**
 * Cato: Prepares request and widget context for new render.
 * <p>
 * Invoked by the screen renderer upon start of view/screen rendering.
 * <p>
 * This is essential to:
 * - clean up request vars across servlet forwards
 */

import org.ofbiz.base.util.template.FreeMarkerWorker;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateScalarModel;
import org.ofbiz.base.util.*;
import com.ilscipio.cato.webapp.ftl.CommonFtlUtil;

final module = "PrepareNewScreenRender.groovy"

Debug.logInfo("Cato: Preparing new templating API render; clearing request-scope templating vars", module);

// note: env param will usually be null here, but just in case...
CommonFtlUtil.resetRequestVars(context.request, context, FreeMarkerWorker.getCurrentEnvironment());
