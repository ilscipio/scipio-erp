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
package com.ilscipio.scipio.entity.def;

import com.ilscipio.scipio.ce.base.component.ComponentReflectInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilTimer;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.model.ModelReader;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Extend entity annotation reader - collects @ExtendEntity annotations for processing.
 *
 * <p>This reader collects the extension information which is then applied to existing
 * entities after all entities have been loaded.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@SuppressWarnings("serial")
public class ExtendEntityAnnotationReader implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected final ComponentReflectInfo reflectInfo;
    protected final ModelReader modelReader;

    public ExtendEntityAnnotationReader(ComponentReflectInfo reflectInfo, ModelReader modelReader) {
        this.reflectInfo = reflectInfo;
        this.modelReader = modelReader;
    }

    /**
     * Reads all @ExtendEntity annotated classes and returns extension info objects.
     */
    public List<ExtendEntityInfo> getExtendEntityInfos() {
        UtilTimer utilTimer = new UtilTimer();
        utilTimer.timerString("Before start of extend-entity loop in entity annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "]");

        List<ExtendEntityInfo> extendEntityInfos = new ArrayList<>();
        int entityCount = 0;

        for (Class<?> entityClass : reflectInfo.getReflectQuery().getAnnotatedClasses(ExtendEntity.class)) {
            ExtendEntity extendEntityDef = entityClass.getAnnotation(ExtendEntity.class);
            if (extendEntityDef == null) {
                continue;
            }

            try {
                ExtendEntityInfo info = createExtendEntityInfo(extendEntityDef, entityClass);
                if (info != null) {
                    extendEntityInfos.add(info);
                    entityCount++;
                }
            } catch (Exception e) {
                Debug.logError(e, "Error creating extend-entity info from annotation for entity: " +
                        extendEntityDef.name() + " in class " + entityClass.getName(), module);
            }
        }

        utilTimer.timerString("Finished extend-entity annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "] - Total Extend-Entities: " + entityCount + " FINISHED");
        Debug.logInfo("Loaded [" + entityCount + "] Extend-Entities from annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "]", module);

        return extendEntityInfos;
    }

    /**
     * Creates an ExtendEntityInfo from the annotation.
     */
    protected ExtendEntityInfo createExtendEntityInfo(ExtendEntity extendEntityDef, Class<?> entityClass) {
        return new ExtendEntityInfo(extendEntityDef, entityClass);
    }

    /**
     * Data class holding @ExtendEntity annotation info for later application.
     */
    public static class ExtendEntityInfo implements Serializable {
        private final ExtendEntity annotation;
        private final Class<?> annotatedClass;

        public ExtendEntityInfo(ExtendEntity annotation, Class<?> annotatedClass) {
            this.annotation = annotation;
            this.annotatedClass = annotatedClass;
        }

        public String getEntityName() {
            return annotation.name();
        }

        public ExtendEntity getAnnotation() {
            return annotation;
        }

        public Class<?> getAnnotatedClass() {
            return annotatedClass;
        }

        public String getDefaultResourceName() {
            return annotation.defaultResourceName();
        }

        public String getDependentOn() {
            return annotation.dependentOn();
        }

        public int getSequenceBankSize() {
            return annotation.sequenceBankSize();
        }

        public String getEnableLock() {
            return annotation.enableLock();
        }

        public String getNoAutoStamp() {
            return annotation.noAutoStamp();
        }

        public String getNeverCache() {
            return annotation.neverCache();
        }

        public String getAutoClearCache() {
            return annotation.autoClearCache();
        }

        public Field[] getFields() {
            return annotation.fields();
        }

        public Relation[] getRelations() {
            return annotation.relations();
        }

        public Index[] getIndexes() {
            return annotation.indexes();
        }

        public String getLocation() {
            return "class://" + annotatedClass.getName();
        }
    }
}
