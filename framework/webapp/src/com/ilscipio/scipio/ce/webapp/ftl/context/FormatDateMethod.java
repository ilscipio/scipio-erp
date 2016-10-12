/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package com.ilscipio.scipio.ce.webapp.ftl.context;

import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilFormatOut;

import com.ilscipio.scipio.ce.webapp.ftl.CommonFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateBooleanModel;
import freemarker.template.TemplateDateModel;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: FormatDateMethod - Freemarker Method for formatting dates
 */
public class FormatDateMethod implements TemplateMethodModelEx {

    public static final String module = FormatDateMethod.class.getName();

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @SuppressWarnings("unchecked")
    @Override
    public Object exec(List args) throws TemplateModelException {
        if (args == null || args.size() < 1 || args.size() > 5)
            throw new TemplateModelException("Invalid number of arguments");
        if (!(args.get(0) instanceof TemplateDateModel))
            throw new TemplateModelException("First argument is not a Date object/model");
        TemplateDateModel dateModel = ((TemplateDateModel) args.get(0));
        String dateTimeFormat = (args.size() > 1) ? LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) args.get(1))) : null;
        TemplateModel specLocaleModel = (args.size() > 2) ? ((TemplateModel) args.get(2)) : null;
        TemplateModel specTimeZoneModel = (args.size() > 3) ? ((TemplateModel) args.get(3)) : null;
        String dateType = (args.size() > 4) ? LangFtlUtil.getAsStringNonEscaping(((TemplateScalarModel) args.get(4))) : null;
        
        Environment env = CommonFtlUtil.getCurrentEnvironment();
        
        Date date = dateModel.getAsDate();
        
        if (dateTimeFormat != null && dateTimeFormat.isEmpty()) {
            dateTimeFormat = null;
        }
        
        Locale locale;
        if (specLocaleModel == null || specLocaleModel instanceof TemplateBooleanModel) {
            if (specLocaleModel == null || ((TemplateBooleanModel) specLocaleModel).getAsBoolean()) {
                locale = LangFtlUtil.getLocale(env.getVariable("locale"));
            } else {
                locale = null;
            }
        } else {
            locale = LangFtlUtil.getLocale(specLocaleModel);
        }
        
        TimeZone timeZone;
        if (specTimeZoneModel == null || specTimeZoneModel instanceof TemplateBooleanModel) {
            if (specTimeZoneModel == null || ((TemplateBooleanModel) specTimeZoneModel).getAsBoolean()) {
                timeZone = LangFtlUtil.getTimeZone(env.getVariable("timeZone"));
            } else {
                timeZone = null;
            }
        } else {
            timeZone = LangFtlUtil.getTimeZone(specTimeZoneModel);
        }
        
        // NOTE: 2016-10-12: CANNOT pass null locale or timeZone because it causes crash.
        // warn when missing.
        if (locale == null) {
            locale = Locale.getDefault();
            Debug.logWarning("Scipio: formatDate(Time) transform received null/empty locale; using"
                    + " system default", module);
        }
        if (timeZone == null) {
            timeZone = TimeZone.getDefault();
            Debug.logWarning("Scipio: formatDate(Time) transform received null/empty time zone; using"
                    + " system default", module);
        }
        
        Object res;
        if (dateType == null || dateType.isEmpty() || "date".equals(dateType)) { // "date" is default
            res = UtilFormatOut.formatDate(date, dateTimeFormat, locale, timeZone);
        } else if ("time".equals(dateType)) {
            res = UtilFormatOut.formatTime(date, dateTimeFormat, locale, timeZone);
        } else {
            res = UtilFormatOut.formatDateTime(date, dateTimeFormat, locale, timeZone);
        }
        return res;
    }

}
