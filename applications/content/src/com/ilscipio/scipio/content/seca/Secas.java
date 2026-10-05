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
package com.ilscipio.scipio.content.seca;

import com.ilscipio.scipio.service.def.seca.*;

/**
 * Auto-generated annotation-based service ECA definitions.
 *
 * <p>Generated from secas.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Secas {

    /**
     * SECA for service createFile on event in-validate.
     */
    @Seca(
        service = "createFile",
        event = "in-validate",
        condition = "empty(dataResource)",
        actions = {
            @SecaAction(
                service = "createDataResource",
                mode = "sync"
            )
        }
    )
    public interface CreateFileinvalidateSeca1 {}

    /**
     * SECA for service createSurveyQuestion on event commit.
     */
    @Seca(
        service = "createSurveyQuestion",
        event = "commit",
        condition = "!empty(surveyId)",
        actions = {
            @SecaAction(
                service = "createSurveyQuestionAppl",
                mode = "sync"
            )
        }
    )
    public interface CreateSurveyQuestioncommitSeca2 {}

    /**
     * SECA for service createDataResource on event commit.
     */
    @Seca(
        service = "createDataResource",
        event = "commit",
        condition = "!empty(partyId) && !empty(roleTypeId)",
        actions = {
            @SecaAction(
                service = "createDataResourceRole",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateDataResourcecommitSeca3 {}

    /**
     * SECA for service createDataResource on event commit.
     */
    @Seca(
        service = "createDataResource",
        event = "commit",
        condition = "!empty(partyId) && empty(roleTypeId)",
        assignments = {
            @SecaSet(fieldName = "roleTypeId", value = "OWNER")
        },
        actions = {
            @SecaAction(
                service = "createDataResourceRole",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateDataResourcecommitSeca4 {}

    /**
     * SECA for service createDataResource on event commit.
     */
    @Seca(
        service = "createDataResource",
        event = "commit",
        condition = "empty(partyId) && empty(roleTypeId) && !empty(userLogin) && !empty(userLogin.partyId)",
        assignments = {
            @SecaSet(fieldName = "partyId", envName = "${userLogin.partyId}"),
            @SecaSet(fieldName = "roleTypeId", value = "OWNER")
        },
        actions = {
            @SecaAction(
                service = "createDataResourceRole",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateDataResourcecommitSeca5 {}

    /**
     * SECA for service createOtherDataResource on event in-validate.
     */
    @Seca(
        service = "createOtherDataResource",
        event = "in-validate",
        condition = "empty(dataResourceId)",
        assignments = {
            @SecaSet(fieldName = "dataResourceTypeId", value = "OTHER_OBJECT")
        },
        actions = {
            @SecaAction(
                service = "createDataResource",
                mode = "sync"
            )
        }
    )
    public interface CreateOtherDataResourceinvalidateSeca6 {}

    /**
     * SECA for service createDataResourceRole on event invoke.
     */
    @Seca(
        service = "createDataResourceRole",
        event = "invoke",
        actions = {
            @SecaAction(
                service = "ensurePartyRole",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateDataResourceRoleinvokeSeca7 {}

    /**
     * SECA for service createDataText on event invoke.
     */
    @Seca(
        service = "createDataText",
        event = "invoke",
        condition = "dataResourceTypeId == 'ELECTRONIC_TEXT'",
        assignments = {
            @SecaSet(fieldName = "dataResourceTypeId", value = "ELECTRONIC_TEXT")
        },
        actions = {
            @SecaAction(
                service = "createElectronicText",
                mode = "sync"
            )
        }
    )
    public interface CreateDataTextinvokeSeca8 {}

    /**
     * SECA for service createDataText on event invoke.
     */
    @Seca(
        service = "createDataText",
        event = "invoke",
        condition = "empty(dataResourceTypeId) && !empty(textData)",
        assignments = {
            @SecaSet(fieldName = "dataResourceTypeId", value = "ELECTRONIC_TEXT")
        },
        actions = {
            @SecaAction(
                service = "createElectronicText",
                mode = "sync"
            )
        }
    )
    public interface CreateDataTextinvokeSeca9 {}

    /**
     * SECA for service createDataText on event invoke.
     */
    @Seca(
        service = "createDataText",
        event = "invoke",
        condition = "dataResourceTypeId == 'SHORT_TEXT' && !empty(textData)",
        assignments = {
            @SecaSet(fieldName = "dataResourceTypeId", value = "SHORT_TEXT")
        },
        actions = {
            @SecaAction(
                service = "createDataResource",
                mode = "sync"
            )
        }
    )
    public interface CreateDataTextinvokeSeca10 {}

    /**
     * SECA for service createDataText on event invoke.
     */
    @Seca(
        service = "createDataText",
        event = "invoke",
        condition = "empty(dataResourceTypeId) && empty(textData)",
        assignments = {
            @SecaSet(fieldName = "dataResourceTypeId", value = "SHORT_TEXT")
        },
        actions = {
            @SecaAction(
                service = "createDataResource",
                mode = "sync"
            )
        }
    )
    public interface CreateDataTextinvokeSeca11 {}

    /**
     * SECA for service createDataText on event invoke.
     */
    @Seca(
        service = "createDataText",
        event = "invoke",
        condition = "dataResourceTypeId != 'ELECTRONIC_TEXT' && !empty(dataResourceTypeId) && empty(textData)",
        actions = {
            @SecaAction(
                service = "createDataResource",
                mode = "sync"
            )
        }
    )
    public interface CreateDataTextinvokeSeca12 {}

    /**
     * SECA for service updateDataText on event invoke.
     */
    @Seca(
        service = "updateDataText",
        event = "invoke",
        condition = "dataResourceTypeId == 'ELECTRONIC_TEXT'",
        actions = {
            @SecaAction(
                service = "updateDataResource",
                mode = "sync"
            ),
            @SecaAction(
                service = "updateElectronicText",
                mode = "sync"
            )
        }
    )
    public interface UpdateDataTextinvokeSeca13 {}

    /**
     * SECA for service updateDataText on event invoke.
     */
    @Seca(
        service = "updateDataText",
        event = "invoke",
        condition = "dataResourceTypeId != 'ELECTRONIC_TEXT'",
        actions = {
            @SecaAction(
                service = "updateDataResource",
                mode = "sync"
            )
        }
    )
    public interface UpdateDataTextinvokeSeca14 {}

    /**
     * SECA for service createElectronicText on event invoke.
     */
    @Seca(
        service = "createElectronicText",
        event = "invoke",
        condition = "empty(dataResourceId)",
        assignments = {
            @SecaSet(fieldName = "dataResourceTypeId", value = "ELECTRONIC_TEXT")
        },
        actions = {
            @SecaAction(
                service = "createDataResource",
                mode = "sync"
            )
        }
    )
    public interface CreateElectronicTextinvokeSeca15 {}

    /**
     * SECA for service createContent on event commit.
     */
    @Seca(
        service = "createContent",
        event = "commit",
        condition = "!empty(partyId) && !empty(roleTypeId)",
        actions = {
            @SecaAction(
                service = "createContentRole",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateContentcommitSeca16 {}

    /**
     * SECA for service createContent on event commit.
     */
    @Seca(
        service = "createContent",
        event = "commit",
        condition = "!empty(partyId) && empty(roleTypeId)",
        assignments = {
            @SecaSet(fieldName = "roleTypeId", value = "OWNER")
        },
        actions = {
            @SecaAction(
                service = "createContentRole",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateContentcommitSeca17 {}

    /**
     * SECA for service createContent on event commit.
     */
    @Seca(
        service = "createContent",
        event = "commit",
        condition = "empty(partyId) && empty(roleTypeId) && !empty(userLogin) && !empty(userLogin.partyId)",
        assignments = {
            @SecaSet(fieldName = "partyId", envName = "${userLogin.partyId}"),
            @SecaSet(fieldName = "roleTypeId", value = "OWNER")
        },
        actions = {
            @SecaAction(
                service = "createContentRole",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateContentcommitSeca18 {}

    /**
     * SECA for service createContent on event commit.
     */
    @Seca(
        service = "createContent",
        event = "commit",
        condition = "!empty(contentAssocTypeId) && !empty(contentIdTo)",
        actions = {
            @SecaAction(
                service = "createContentAssoc",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateContentcommitSeca19 {}

    /**
     * SECA for service createContent on event commit.
     */
    @Seca(
        service = "createContent",
        event = "commit",
        condition = "!empty(contentAssocTypeId) && !empty(contentIdFrom)",
        actions = {
            @SecaAction(
                service = "createContentAssoc",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateContentcommitSeca20 {}

    /**
     * SECA for service createContent on event commit.
     */
    @Seca(
        service = "createContent",
        event = "commit",
        condition = "!empty(contentPurposeTypeId)",
        actions = {
            @SecaAction(
                service = "createContentPurpose",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateContentcommitSeca21 {}

    /**
     * SECA for service createContentRole on event invoke.
     */
    @Seca(
        service = "createContentRole",
        event = "invoke",
        actions = {
            @SecaAction(
                service = "ensurePartyRole",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateContentRoleinvokeSeca22 {}

    /**
     * SECA for service updateContent on event commit.
     */
    @Seca(
        service = "updateContent",
        event = "commit",
        condition = "!empty(contentAssocTypeId) && !empty(contentIdTo) && !empty(fromDate)",
        actions = {
            @SecaAction(
                service = "updateContentAssoc",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface UpdateContentcommitSeca23 {}

    /**
     * SECA for service updateContent on event commit.
     */
    @Seca(
        service = "updateContent",
        event = "commit",
        condition = "!empty(contentAssocTypeId) && !empty(contentIdFrom) && !empty(fromDate)",
        actions = {
            @SecaAction(
                service = "updateContentAssoc",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface UpdateContentcommitSeca24 {}

    /**
     * SECA for service updateContent on event commit.
     */
    @Seca(
        service = "updateContent",
        event = "commit",
        condition = "!empty(contentPurposeTypeId)",
        actions = {
            @SecaAction(
                service = "updateSingleContentPurpose",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface UpdateContentcommitSeca25 {}

    /**
     * SECA for service createContentAssoc on event in-validate.
     */
    @Seca(
        service = "createContentAssoc",
        event = "in-validate",
        actions = {
            @SecaAction(
                service = "checkContentAssocIds",
                mode = "sync"
            )
        }
    )
    public interface CreateContentAssocinvalidateSeca26 {}

    /**
     * SECA for service updateContentAssoc on event in-validate.
     */
    @Seca(
        service = "updateContentAssoc",
        event = "in-validate",
        actions = {
            @SecaAction(
                service = "checkContentAssocIds",
                mode = "sync"
            )
        }
    )
    public interface UpdateContentAssocinvalidateSeca27 {}

    /**
     * SECA for service createContent on event commit.
     */
    @Seca(
        service = "createContent",
        event = "commit",
        condition = "!empty(contentId)",
        actions = {
            @SecaAction(
                service = "createContentAlternativeUrl",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateContentcommitSeca28 {}

}
