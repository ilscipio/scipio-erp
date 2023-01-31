package com.ilscipio.scipio.webtools;

import com.ilscipio.scipio.ce.webapp.control.def.Controller;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.ControlResponse;
import com.ilscipio.scipio.ce.webapp.control.def.View;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Java-based controller definitions emulating controller.xml.
 *
 * <p>Shows example usage and can be used as-is.</p>
 *
 * <p>NOTE: This single-class *Controller pattern does not need to be followed strictly,
 * but for component with several webapps it lowers the need to specify the controller.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support example.</p>
 */
@Controller(controller = "webtools")
public class WebtoolsController {

    /*
     * Event responses.
     * NOTE: For performance and reuse these can be left static here, because it internally caches the XML-emulating
     * class, but this is not strictly necessary. However, it is very useful for defining views themselves using
     * the @View annotation.
     */

    protected static final ControlResponse.ViewLast VIEW_MAIN_LAST = ControlResponse.viewLast("main");

    /**
     * View definition and control response for events.
     *
     * <p>Here the annotation is equivalent to {@code <view/>} definition, and the ControlResponse instance
     * can be returned by events. NOTE: The ControlResponse instance does not need to be unique.</p>
     */
    @View(name = "maincopy", type = "screen", page = "component://webtools/widget/CommonScreens.xml#main")
    protected static final ControlResponse.View VIEW_MAINCOPY = ControlResponse.view("maincopy");

    @Request(uri = "testViewLast")
    public static Object testViewLast(HttpServletRequest request, HttpServletResponse response) {
        return VIEW_MAIN_LAST;
        // Also acceptable depending on style
        //return ControlResponse.viewLast("main");
    }

    @Request(uri = "testViewMain")
    public static Object testViewMain(HttpServletRequest request, HttpServletResponse response) {
        return ControlResponse.view("main");
    }

    @Request(uri = "testViewMainCopy")
    public static Object testViewMainCopy(HttpServletRequest request, HttpServletResponse response) {
        return VIEW_MAINCOPY;
    }

}
