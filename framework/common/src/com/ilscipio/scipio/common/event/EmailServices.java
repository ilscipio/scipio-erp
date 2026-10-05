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
package com.ilscipio.scipio.common.event;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://common/script/org/ofbiz/common/email/EmailServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class EmailServices {

    private static final String MODULE = EmailServices.class.getName();


    /**
     * Send Mail from Email Template Setting
     */
    public static Map<String, Object> sendMailFromTemplateSetting(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> getEmail = null;
        Map<String, Object> emailParams = null;
        if (UtilValidate.isEmpty(context.get("sendTo"))) {
            if (UtilValidate.isEmpty(context.get("partyIdTo"))) {
                Debug.logError("PartyId or SendTo should be specified!", MODULE);
                {
                    String errorMsg = UtilProperties.getMessage("CommonUiLabels", "CommonEmailShouldBeSpecified", locale);
                    error_list.add(errorMsg);
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("partyIdTo"))) {
            if (UtilValidate.isEmpty(context.get("sendTo"))) {
                getEmail.put("partyId", context.get("partyIdTo"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("getPartyEmail", getEmail);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    context.put("sendTo", serviceResult.get("emailAddress"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling getPartyEmail: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(context.get("sendTo"))) {
                    Debug.logInfo("PartyId: " + context.get("partyIdTo") + " has no valid email address, not sending email", MODULE);
                    return result;
                }
            }
        }
        GenericValue emailTemplateSetting = null;
        try {
            emailTemplateSetting = EntityQuery.use(delegator)
                    .from("EmailTemplateSetting")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying EmailTemplateSetting: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(emailTemplateSetting)) {
            emailParams.put("bodyScreenUri", emailTemplateSetting.get("bodyScreenLocation"));
            emailParams.put("xslfoAttachScreenLocation", emailTemplateSetting.get("xslfoAttachScreenLocation"));
            emailParams.put("partyId", context.get("partyIdTo"));
            if (UtilValidate.isNotEmpty(emailTemplateSetting.get("fromAddress"))) {
                emailParams.put("sendFrom", emailTemplateSetting.get("fromAddress"));
            } else {
                Object emailParams_sendFrom = UtilProperties.getMessage("general", "defaultFromEmailAddress", locale);
            }
            emailParams.put("sendCc", emailTemplateSetting.get("ccAddress"));
            emailParams.put("sendBcc", emailTemplateSetting.get("bccAddress"));
            emailParams.put("subject", emailTemplateSetting.get("subject"));
            emailParams.put("contentType", emailTemplateSetting.get("contentType"));
            if (UtilValidate.isNotEmpty(context.get("custRequestId"))) {
                emailParams.put("bodyParameters.custRequestId", context.get("custRequestId"));
            }
            // set-service-fields from "parameters" to "emailParams" for service "sendMailFromScreen"
            emailParams.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("sendMailFromScreen", emailParams);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                result.put("messageWrapper", serviceResult.get("messageWrapper"));
                result.put("body", serviceResult.get("body"));
                result.put("communicationEventId", serviceResult.get("communicationEventId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling sendMailFromScreen: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            Debug.logError("sendMailFromTemplateSetting service could not find the emailTemplateSettingId: " + context.get("emailTemplateSettingId"), MODULE);
        }

        return result;
    }

}
