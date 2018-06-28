package com.ilscipio.scipio.ce.webapp.ftl;

import org.ofbiz.base.util.template.FreeMarkerWorker;

import freemarker.core.Environment;
import freemarker.template.TemplateModelException;

/**
 * SCIPIO: Common Freemarker utility methods.
 * <p>
 * Most methods should go into specific util classes, unless generic or generic and so common enough
 * that could use a facade here.
 * <p>
 * These utilities and the ones in specific util classes not belonging to any theme
 * or styling framework implementation should follow similar rules as the utilities defined in:
 * <code>component://common/webcommon/includes/scipio/lib/utilities.ftl</code>.
 * They should not contain any theme- or styling-framework-specific code.
 * Any such code should go in a specific theme- or styling-framework package.
 * <p>
 * However, not all of these methods have the same contextual information
 * available, so platform-specific (html, fo, etc.) code may be acceptable for some
 * or where clear variants must be provided as parameters or overloads.
 * <p>
 * In general, Freemarker utilities here and in specific classes come in two
 * flavors: context-dependent and context-independent. 
 * <p>
 * Context-dependent means they will fetch
 * the environment, variables and state from static methods provided by Freemarker using
 * the current thread state. These are roughly equivalent to functions defined in *.ftl files.
 * <em>DEV NOTE</em>: Make sure to identify these in docs. Few to none are needed
 * when transforms are used instead of FTL #function defs in *.ftl files. Failure to identify
 * will lead to unintended global states sneaking in.
 * <p>
 * Context-independent methods will simply work with given parameters.
 * <p>
 * Most methods are between: they operate only on given parameters (no access to global state) but accept
 * context parameters.
 * <p>
 * Note this class and specific utility classes are not meant to be a java facade equivalents 
 * of <code>utilities.ftl</code>.
 * <em>DEV NOTE:</em> I don't think it's worth maintaining a java facade for the
 * utility classes. <code>utilities.ftl</code> largely provides that for FTL templates and macros
 * and we can simply provide FTL functions or transforms for everything here, there.
 * However java method alternatives should still be provided for accessing values from languages and contexts
 * where no Freemarker environment is available, in cases where Scipio Freemarker additions extend
 * outside of Freemarker (e.g. <code>set/getRequestVar</code>). Those are not facades but simply
 * ensure correctness and calls without Freemarker environment classes needed. See
 * {@link com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil#pushRequestStack} for example.
 */
public abstract class CommonFtlUtil {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    protected CommonFtlUtil() {
    }
    
    public static Environment getCurrentEnvironment() throws TemplateModelException {
        Environment env = FreeMarkerWorker.getCurrentEnvironment();
        if (env == null) {
            throw new TemplateModelException("Missing Freemarker environment");
        }
        return env;
    }
    
}
