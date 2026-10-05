/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
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

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
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

        // NOTE: 2016-10-12: CANNOT pass null locale or timeZone because it causes crash.
        // warn when missing.

        Locale locale;
        if (specLocaleModel == null || specLocaleModel instanceof TemplateBooleanModel) {
            if (specLocaleModel == null || ((TemplateBooleanModel) specLocaleModel).getAsBoolean()) {
                locale = LangFtlUtil.getLocale(TransformUtil.getFtlContextGlobalVar("locale", env));
                if (locale == null) {
                    locale = Locale.getDefault();
                    Debug.logWarning("Scipio: formatDate(Time): locale empty (from context); using system default", module);
                }
            } else {
                locale = Locale.getDefault();
                Debug.logWarning("Scipio: formatDate(Time): locale false (from caller); using system default", module);
            }
        } else {
            locale = LangFtlUtil.getLocale(specLocaleModel);
            if (locale == null) {
                locale = Locale.getDefault();
                Debug.logWarning("Scipio: formatDate(Time): locale empty (from caller); using system default", module);
            }
        }

        TimeZone timeZone;
        if (specTimeZoneModel == null || specTimeZoneModel instanceof TemplateBooleanModel) {
            if (specTimeZoneModel == null || ((TemplateBooleanModel) specTimeZoneModel).getAsBoolean()) {
                timeZone = LangFtlUtil.getTimeZone(TransformUtil.getFtlContextGlobalVar("timeZone", env));
                if (timeZone == null) {
                    timeZone = TimeZone.getDefault();
                    Debug.logWarning("Scipio: formatDate(Time): timeZone empty (from context); using system default", module);
                }
            } else {
                timeZone = TimeZone.getDefault();
                Debug.logWarning("Scipio: formatDate(Time): timeZone false (from caller); using system default", module);
            }
        } else {
            timeZone = LangFtlUtil.getTimeZone(specTimeZoneModel);
            if (timeZone == null) {
                timeZone = TimeZone.getDefault();
                Debug.logWarning("Scipio: formatDate(Time): timeZone empty (from caller); using"
                        + " system default", module);
            }
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
