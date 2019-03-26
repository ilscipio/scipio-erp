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
