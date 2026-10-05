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
package org.ofbiz.webapp.ftl;

import java.io.IOException;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilFormatOut;

import com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;

/**
 * AmountDirective - Freemarker Transform for amounts (?)
 * <p>
 * SCIPIO: 2019-02-05: Reimplemented as TemplateDirectiveModel (was previously: OfbizAmountTransform)
 */
public class AmountDirective implements TemplateDirectiveModel {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String SPELLED_OUT_FORMAT = "spelled-out";

    @Override
    public void execute(Environment env, @SuppressWarnings("rawtypes") Map args, TemplateModel[] loopVars, TemplateDirectiveBody body)
            throws TemplateException, IOException {
        Double amount = TransformUtil.getDoubleArg(args, "amount");
        if (amount == null) {
            throw new TemplateException("Missing or invalid amount", env);
        }
        Locale locale = TransformUtil.getOfbizLocaleArgOrCurrent(args, "locale", env);
        String format = TransformUtil.getStringNonEscapingArg(args, "format");

        if (Debug.verboseOn()) {
            Debug.logVerbose("Formatting amount: [amount=" + amount + ", format=" + format
                    + ", locale=" + locale + "]", module);
        }
        String formattedAmount;
        try {
            if (AmountDirective.SPELLED_OUT_FORMAT.equals(format)) {
                formattedAmount = UtilFormatOut.formatSpelledOutAmount(amount.doubleValue(), locale);
            } else {
                formattedAmount = UtilFormatOut.formatAmount(amount, locale);
            }
        } catch (Exception e) {
            throw new TemplateException(e, env);
        }
        env.getOut().write(formattedAmount);
    }
}
