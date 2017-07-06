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
package org.ofbiz.content.image;

import java.util.Map;

import org.ofbiz.service.DispatchContext;

/**
 * SCIPIO: Content/generic image services.
 * Added 2017-07-04.
 * <p->
 * Derived from:
 * <ul>
 * <li>{@link org.ofbiz.product.image.ScaleImage}</li>
 * <li>{@link org.ofbiz.product.imagemanagement.ImageManagementServices#scaleImageMangementInAllSize}</li>
 * <li>(potentially more, please update list as needed to track for maintenance purposes)</li>
 * </ul>
 */
public abstract class ContentImageServices {

    public static final String module = ContentImageServices.class.getName();

    protected ContentImageServices() {
    }

    public static Map<String, Object> imageFileScaleInAllSizeCore(DispatchContext dctx, Map<String, ?> context) {
        return ContentImageWorker.imageFileScaleInAllSizeCore(dctx.getDelegator(), context);
    }
    
    public static Map<String, Object> imageDbScaleInAllSizeCore(DispatchContext dctx, Map<String, ?> context) {
        return ContentImageWorker.imageDbScaleInAllSizeCore(dctx.getDelegator(), context);
    }
      
}
