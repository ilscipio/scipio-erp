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
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilFormatOut;
import org.ofbiz.base.util.UtilNumber;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.util.EntityUtilProperties;

import com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * CurrencyDirective - Freemarker Transform for content links
 * <p>
 * SCIPIO: 2019-02-05: Reimplemented as TemplateDirectiveModel; previously named: OfbizCurrencyTransform.java.
 */
public class CurrencyDirective implements TemplateDirectiveModel {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final RoundingMode ROUNDING_MODE = UtilNumber.getRoundingMode("general", "currency.rounding.mode", null);

    @Override
    public void execute(Environment env, @SuppressWarnings("rawtypes") Map args, TemplateModel[] loopVars, TemplateDirectiveBody body)
            throws TemplateException, IOException {
        HttpServletRequest request = ContextFtlUtil.getRequest(env);
        Delegator delegator = ContextFtlUtil.getDelegator(request, env);

        BigDecimal amount = TransformUtil.getBigDecimalArg(args, "amount");
        if (amount == null) {
            throw new TemplateException("Missing or invalid currency amount", env);
        }
        String isoCode = TransformUtil.getStringNonEscapingArg(args, "isoCode"); // SCIPIO: Renamed var (not param name)
        Locale locale = TransformUtil.getOfbizLocaleArgOrCurrent(args, "locale", env);
        Integer rounding = getRounding(args, "rounding", delegator, amount);

        // SCIPIO: This is practically always an error... but we fallback for backward-compat
        if (UtilValidate.isEmpty(isoCode)) {
            // SCIPIO: As first fallback, check context for a currencyUomId...
            try {
                isoCode = LangFtlUtil.getAsStringNonEscaping((TemplateScalarModel) env.getVariable("currencyUomId"));
            } catch (Exception e) {
                Debug.logError("Could not get currencyUomId as String from environment", module);
            }
            if (UtilValidate.isNotEmpty(isoCode)) {
                Debug.logWarning("Currency directive called without an isoCode= parameter (amount: " + amount
                    + ") (usually an error); using currencyUomId found in context instead (" + isoCode + ")", module);
            } else {
                isoCode = UtilProperties.getPropertyValue("general", "currency.uom.id.default", "USD");
                Debug.logWarning("Currency directive called without an isoCode= parameter (amount: " + amount
                        + ") (usually an error); system default will be used", module);
            }
        } else if ("none".equals(isoCode)) {
            // SCIPIO: Explicit none requested (to bypass the messages, if somehow intentional)
            isoCode = null;
        }

        if (Debug.verboseOn()) {
            Debug.logVerbose("Formatting currency: [amount=" + amount + ", isoCode=" + isoCode
                    + ", locale=" + locale + "]", module);
        }

        if (ROUNDING_MODE != null) { // SCIPIO
            amount = amount.setScale(rounding, ROUNDING_MODE);
        }

        env.getOut().write(UtilFormatOut.formatCurrency(amount, isoCode, locale, rounding)); // SCIPIO: Let IOException propagate
    }

    private static Integer getRounding(Map<?, ?> args, String argName, Delegator delegator, BigDecimal amount) throws TemplateModelException {
        // SCIPIO: Refactored
        // check the rounding -- DEFAULT is 10 to not round for display, only use this when necessary
        // rounding should be handled by the code, however some times the numbers are coming from
        // someplace else (i.e. an integration)
        Integer rounding = TransformUtil.getIntegerArg(args, argName);
        String scaleEnabled = EntityUtilProperties.getPropertyValue("general", "currency.scale.enabled", "N", delegator);
        if (rounding == null) {
            rounding = EntityUtilProperties.getPropertyAsInteger("general", "currency.rounding.default", 10, delegator);
        }
        if ("Y".equals(scaleEnabled)) {
            if (amount.stripTrailingZeros().scale() <= 0) {
                rounding = 0;
            }
        }
        return rounding;
    }
}
