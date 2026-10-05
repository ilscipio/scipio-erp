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
package org.ofbiz.widget.renderer;

import java.util.Locale;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelReader;

/**
 * Util for working with Help Text
 */
public final class UtilHelpText {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private UtilHelpText () {}

    /**
     * Find the help text associated with an entity field.
     *
     * @param entityName the entity name
     * @param fieldName the field name
     * @param delegator the delegator
     * @param locale the locale
     * @return the help text, or the resource propertyName if no help text exists
     */
    public static String getEntityFieldDescription(final String entityName, final String fieldName, final Delegator delegator, final Locale locale) {

        if (UtilValidate.isEmpty(entityName)) {
            // Debug.logWarning("entityName [" + entityName + "] is empty", module);
            return "";
        }
        if (UtilValidate.isEmpty(fieldName)) {
            Debug.logWarning("fieldName [" + fieldName + "] is empty", module);
            return "";
        }
        ModelReader reader = delegator.getModelReader();
        ModelEntity entity = null;
        try {
            if (!reader.getEntityNames().contains(entityName)) {
                Debug.logWarning("couldn't find entityName [" + entityName + "]", module);
                return "";
            }
            entity = reader.getModelEntity(entityName);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error getting help text for entity=" + entityName + " field " + fieldName, module);
            return "";
        }
        String entityResourceName = entity.getDefaultResourceName();
        String messageId = "FieldDescription." + entityName + "." + fieldName;
        String fieldDescription = UtilProperties.getMessage(entityResourceName, messageId, locale, true); // SCIPIO: optional=true: return null if missing
        //if (fieldDescription.equals(messageId)) {
        if (UtilValidate.isEmpty(fieldDescription)) {
            messageId = "FieldDescription." + fieldName;
            if (Debug.verboseOn()) {
                Debug.logVerbose("No help text found in [" + entityResourceName + "] with key [" + messageId + "], Trying with: " + messageId, module);
            }
            fieldDescription = UtilProperties.getMessage(entityResourceName, messageId, locale, true); // SCIPIO: optional=true: return null if missing
            if (UtilValidate.isEmpty(fieldDescription)) {
            //if (fieldDescription.equals(messageId)) {
                if (Debug.verboseOn()) {
                    Debug.logVerbose("No help text found in [" + entityResourceName + "] with key [" + messageId + "]", module);
                }
                return "";
            }
        }
        return fieldDescription;
    }
}
