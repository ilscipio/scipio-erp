package com.ilscipio.scipio.webtools;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.UtilValidate;

public abstract class TargetedRenderingTestEvents {

    protected TargetedRenderingTestEvents() {
    }

    public static String testEvent(HttpServletRequest request, HttpServletResponse response) {
        String eventResult = request.getParameter("testEventResult");
        if ("exception".equals(eventResult)) {
            throw new IllegalArgumentException("Throwing exception from testEvent upon request");
        } else if (UtilValidate.isNotEmpty(eventResult)) {
            return eventResult;
        }
        return "success";
    }
    
}
