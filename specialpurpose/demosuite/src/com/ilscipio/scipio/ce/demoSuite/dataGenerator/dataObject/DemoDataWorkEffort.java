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
package com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject;

import java.sql.Timestamp;

public class DemoDataWorkEffort implements AbstractDataObject {

    private String id;
    private String name;
    private String type;
    private String status;
    private Timestamp createdDate;

    private Timestamp estimatedStart;
    private Timestamp estimatedCompletion;
    private Timestamp actualStart;
    private Timestamp actualCompletion;

    private String partyStatus;
    
    private String assetStatus;
    private String fixedAsset;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Timestamp getEstimatedStart() {
        return estimatedStart;
    }

    public void setEstimatedStart(Timestamp estimatedStart) {
        this.estimatedStart = estimatedStart;
    }

    public Timestamp getEstimatedCompletion() {
        return estimatedCompletion;
    }

    public void setEstimatedCompletion(Timestamp estimatedCompletion) {
        this.estimatedCompletion = estimatedCompletion;
    }

    public Timestamp getActualStart() {
        return actualStart;
    }

    public void setActualStart(Timestamp actualStart) {
        this.actualStart = actualStart;
    }

    public Timestamp getActualCompletion() {
        return actualCompletion;
    }

    public void setActualCompletion(Timestamp actualCompletion) {
        this.actualCompletion = actualCompletion;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getPartyStatus() {
        return partyStatus;
    }

    public void setPartyStatus(String partyStatus) {
        this.partyStatus = partyStatus;
    }

    public String getAssetStatus() {
        return assetStatus;
    }

    public void setAssetStatus(String assetStatus) {
        this.assetStatus = assetStatus;
    }

    public Timestamp getCreatedDate() {
        return createdDate;
    }

    public void setCreatedDate(Timestamp createdDate) {
        this.createdDate = createdDate;
    }

    public String getFixedAsset() {
        return fixedAsset;
    }

    public void setFixedAsset(String fixedAsset) {
        this.fixedAsset = fixedAsset;
    }

}
