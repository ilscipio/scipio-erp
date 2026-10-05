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
package com.ilscipio.scipio.workeffort.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class TimesheetServices {

    /**
     * Creates Timesheet
     */
    @Service(
        name = "createTimesheet",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml",
        invoke = "createTimesheet",
        description = "Creates Timesheet",
        defaultEntityName = "Timesheet",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "CREATE")
    )
    public interface CreateTimesheet {}

    /**
     * Updates the Timesheet status back to in process to be able to correct errors
     */
    @Service(
        name = "updateTimesheetToInProcess",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml",
        invoke = "updateTimesheetToInProcess",
        description = "Updates the Timesheet status back to in process to be able to correct errors",
        defaultEntityName = "Timesheet",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface UpdateTimesheetToInProcess {}

    /**
     * Updates Timesheet
     */
    @Service(
        name = "updateTimesheet",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml",
        invoke = "updateTimesheet",
        description = "Updates Timesheet",
        defaultEntityName = "Timesheet",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateTimesheet {}

    /**
     * Deletes Timesheet
     */
    @Service(
        name = "deleteTimesheet",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml",
        invoke = "deleteTimesheet",
        description = "Deletes Timesheet",
        defaultEntityName = "Timesheet",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteTimesheet {}

    /**
     * Creates Timesheet for multiple Parties in a single shot
     */
    @Service(
        name = "createTimesheets",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml",
        invoke = "createTimesheets",
        description = "Creates Timesheet for multiple Parties in a single shot",
        auth = "true",
        attributes = {
            @Attribute(name = "partyIdList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "clientPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "comments", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateTimesheets {}

    /**
     * Creates Timesheet for this week if no required date specified.
     */
    @Service(
        name = "createTimesheetForThisWeek",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml",
        invoke = "createTimesheetForThisWeek",
        description = "Creates Timesheet for this week if no required date specified.",
        defaultEntityName = "Timesheet",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"fromDate", "thruDate"})
        },
        attributes = {
            @Attribute(name = "requiredDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface CreateTimesheetForThisWeek {}

    /**
     * Add Timesheet to Invoice
     */
    @Service(
        name = "addTimesheetToInvoice",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml",
        invoke = "addTimesheetToInvoice",
        description = "Add Timesheet to Invoice",
        defaultEntityName = "Timesheet",
        auth = "true",
        attributes = {
            @Attribute(name = "timesheetId", type = "String", mode = "IN"),
            @Attribute(name = "invoiceId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "CREATE")
    )
    public interface AddTimesheetToInvoice {}

    /**
     * Add Timesheet to Invoice
     */
    @Service(
        name = "addTimesheetToNewInvoice",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml",
        invoke = "addTimesheetToInvoice",
        description = "Add Timesheet to Invoice",
        defaultEntityName = "Timesheet",
        auth = "true",
        attributes = {
            @Attribute(name = "timesheetId", type = "String", mode = "IN"),
            @Attribute(name = "partyIdFrom", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "invoiceId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "CREATE")
    )
    public interface AddTimesheetToNewInvoice {}

    /**
     * Add WorkEffort Time to existing Invoice, with the option to combine all timeentries with the same rateType into one invoiceItem 
     */
    @Service(
        name = "addWorkEffortTimeToInvoice",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml",
        invoke = "addWorkEffortTimeToInvoice",
        description = "Add WorkEffort Time to existing Invoice, with the option to combine all timeentries with the same rateType into one invoiceItem ",
        defaultEntityName = "Timesheet",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "invoiceId", type = "String", mode = "IN"),
            @Attribute(name = "combineInvoiceItem", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "CREATE")
    )
    public interface AddWorkEffortTimeToInvoice {}

    /**
     * Add WorkEffort Time to a new Invoice with the option to combine all time entries with the same rateType into one invoiceItem
     */
    @Service(
        name = "addWorkEffortTimeToNewInvoice",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml",
        invoke = "addWorkEffortTimeToInvoice",
        description = "Add WorkEffort Time to a new Invoice with the option to combine all time entries with the same rateType into one invoiceItem",
        defaultEntityName = "Timesheet",
        auth = "true",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "partyIdFrom", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "invoiceId", type = "String", mode = "OUT"),
            @Attribute(name = "combineInvoiceItem", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "CREATE")
    )
    public interface AddWorkEffortTimeToNewInvoice {}

    /**
     * Creates TimesheetRole
     */
    @Service(
        name = "createTimesheetRole",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml",
        invoke = "createTimesheetRole",
        description = "Creates TimesheetRole",
        defaultEntityName = "TimesheetRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "CREATE")
    )
    public interface CreateTimesheetRole {}

    /**
     * Deletes TimesheetRole
     */
    @Service(
        name = "deleteTimesheetRole",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml",
        invoke = "deleteTimesheetRole",
        description = "Deletes TimesheetRole",
        defaultEntityName = "TimesheetRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "workEffortGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteTimesheetRole {}

    /**
     * Creates TimeEntry
     */
    @Service(
        name = "createTimeEntry",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml",
        invoke = "createTimeEntry",
        description = "Creates TimeEntry",
        defaultEntityName = "TimeEntry",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateTimeEntry {}

    /**
     * Updates TimeEntry
     */
    @Service(
        name = "updateTimeEntry",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml",
        invoke = "updateTimeEntry",
        description = "Updates TimeEntry",
        defaultEntityName = "TimeEntry",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateTimeEntry {}

    /**
     * Deletes TimeEntry
     */
    @Service(
        name = "deleteTimeEntry",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml",
        invoke = "deleteTimeEntry",
        description = "Deletes TimeEntry",
        defaultEntityName = "TimeEntry",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteTimeEntry {}

    /**
     * Deletes TimeEntry
     */
    @Service(
        name = "unlinkInvoiceFromTimeEntry",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml",
        invoke = "unlinkInvoiceFromTimeEntry",
        description = "Deletes TimeEntry",
        defaultEntityName = "TimeEntry",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "INOUT")
        }
    )
    public interface UnlinkInvoiceFromTimeEntry {}

    /**
     * Creates TimeEntry
     */
    @Service(
        name = "getTimeEntryRate",
        engine = "simple",
        location = "component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml",
        invoke = "getTimeEntryRate",
        description = "Creates TimeEntry",
        defaultEntityName = "TimeEntry",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "rateAmount", type = "BigDecimal", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "workEffortManagerPermission", mainAction = "VIEW")
    )
    public interface GetTimeEntryRate {}

}
