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
package com.ilscipio.scipio.setup.widget;

import com.ilscipio.scipio.widget.def.form.*;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class SetupForms {

    @Form(
        name = "EditOrganization",
        location = "component://setup/widget/SetupForms.xml",
        target = "${target}",
        id = "NewOrganization",
        extendsForm = "NewOrganization",
        extendsResource = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        fields = {
            @FormField(name = "partyId", useWhen = "party==null", text = @TextField),
            @FormField(name = "partyId", useWhen = "party!=null", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "scpSubmitSetupStep", hidden = @HiddenField(value = "${setupStep}")),
            @FormField(name = "orgPartyId", useWhen = "${not empty context.orgPartyId}", hidden = @HiddenField(value = "${orgPartyId}")),
            @FormField(name = "orgProductStoreId", hidden = @HiddenField(value = "${productStoreId}"))
        }
    )
    public interface EditOrganization {}

    @Form(
        name = "ViewOrganization",
        location = "component://setup/widget/SetupForms.xml",
        extendsForm = "ViewOrganization",
        extendsResource = "component://commonext/widget/ofbizsetup/SetupForms.xml"
    )
    public interface ViewOrganization {}

    @Form(
        name = "NewCustomer",
        location = "component://setup/widget/SetupForms.xml",
        extendsForm = "NewCustomer",
        extendsResource = "component://commonext/widget/ofbizsetup/SetupForms.xml"
    )
    public interface NewCustomer {}

    @Form(
        name = "EditUser",
        location = "component://setup/widget/SetupForms.xml",
        target = "${target}",
        extendsForm = "NewCustomer",
        extendsResource = "component://commonext/widget/ofbizsetup/SetupForms.xml"
    )
    public interface EditUser {}

    @Form(
        name = "EditCustomer",
        location = "component://setup/widget/SetupForms.xml",
        extendsForm = "EditCustomer",
        extendsResource = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        fields = {
            @FormField(name = "scpSubmitSetupStep", hidden = @HiddenField(value = "${setupStep}")),
            @FormField(name = "orgPartyId", hidden = @HiddenField(value = "${orgPartyId}")),
            @FormField(name = "orgProductStoreId", hidden = @HiddenField(value = "${productStoreId}"))
        }
    )
    public interface EditCustomer {}

    @Form(
        name = "EditFacility",
        location = "component://setup/widget/SetupForms.xml",
        target = "${target}",
        extendsForm = "EditFacility",
        extendsResource = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        fields = {
            @FormField(name = "scpSubmitSetupStep", hidden = @HiddenField(value = "${setupStep}")),
            @FormField(name = "orgPartyId", hidden = @HiddenField(value = "${orgPartyId}")),
            @FormField(name = "orgProductStoreId", hidden = @HiddenField(value = "${productStoreId}"))
        },
        altTargets = {
            @AltTarget(useWhen = "facility==null", target = "${target}")
        }
    )
    public interface EditFacility {}

    @Form(
        name = "EditProductStore",
        location = "component://setup/widget/SetupForms.xml",
        target = "${target}",
        extendsForm = "EditProductStore",
        extendsResource = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        fields = {
            @FormField(name = "scpSubmitSetupStep", hidden = @HiddenField(value = "${setupStep}")),
            @FormField(name = "orgPartyId", hidden = @HiddenField(value = "${orgPartyId}")),
            @FormField(name = "orgProductStoreId", useWhen = "${not empty context.productStoreId}", hidden = @HiddenField(value = "${productStoreId}"))
        },
        altTargets = {
            @AltTarget(useWhen = "productStore==null", target = "${target}")
        }
    )
    public interface EditProductStore {}

    @Form(
        name = "EditWebSite",
        location = "component://setup/widget/SetupForms.xml",
        target = "${target}",
        extendsForm = "EditWebSite",
        extendsResource = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        fields = {
            @FormField(name = "scpSubmitSetupStep", hidden = @HiddenField(value = "${setupStep}")),
            @FormField(name = "orgPartyId", hidden = @HiddenField(value = "${orgPartyId}")),
            @FormField(name = "orgProductStoreId", hidden = @HiddenField(value = "${productStoreId}"))
        }
    )
    public interface EditWebSite {}

    @Form(
        name = "EditProdCatalog",
        location = "component://setup/widget/SetupForms.xml",
        target = "${target}",
        extendsForm = "EditProdCatalog",
        extendsResource = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        fields = {
            @FormField(name = "scpSubmitSetupStep", hidden = @HiddenField(value = "${setupStep}")),
            @FormField(name = "orgPartyId", hidden = @HiddenField(value = "${orgPartyId}")),
            @FormField(name = "orgProductStoreId", hidden = @HiddenField(value = "${productStoreId}"))
        }
    )
    public interface EditProdCatalog {}

    @Form(
        name = "EditProductCategory",
        location = "component://setup/widget/SetupForms.xml",
        extendsForm = "EditProductCategory",
        extendsResource = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        fields = {
            @FormField(name = "scpSubmitSetupStep", hidden = @HiddenField(value = "${setupStep}")),
            @FormField(name = "orgPartyId", hidden = @HiddenField(value = "${orgPartyId}")),
            @FormField(name = "orgProductStoreId", hidden = @HiddenField(value = "${productStoreId}"))
        },
        altTargets = {
            @AltTarget(useWhen = "productCategory==null", target = "createProductCategory")
        }
    )
    public interface EditProductCategory {}

    @Form(
        name = "EditProduct",
        location = "component://setup/widget/SetupForms.xml",
        extendsForm = "EditProduct",
        extendsResource = "component://commonext/widget/ofbizsetup/SetupForms.xml",
        fields = {
            @FormField(name = "scpSubmitSetupStep", hidden = @HiddenField(value = "${setupStep}")),
            @FormField(name = "orgPartyId", hidden = @HiddenField(value = "${orgPartyId}")),
            @FormField(name = "orgProductStoreId", hidden = @HiddenField(value = "${productStoreId}"))
        }
    )
    public interface EditProduct {}

    @Form(
        name = "ListProduct",
        location = "component://setup/widget/SetupForms.xml",
        extendsForm = "ListProduct",
        extendsResource = "component://commonext/widget/ofbizsetup/SetupForms.xml"
    )
    public interface ListProduct {}

    @Form(
        name = "ListOrganizations",
        location = "component://setup/widget/SetupForms.xml",
        extendsForm = "ListOrganizations",
        extendsResource = "component://commonext/widget/ofbizsetup/SetupForms.xml"
    )
    public interface ListOrganizations {}

}
