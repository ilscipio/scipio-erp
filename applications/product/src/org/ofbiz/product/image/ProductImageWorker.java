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
package org.ofbiz.product.image;

import org.ofbiz.content.image.ContentImageWorker;

/**
 * SCIPIO: Image utilities for product (specifically) image handling.
 * Added 2017-07-04.
 * 
 * @see org.ofbiz.content.image.ContentImageWorker
 * @see org.ofbiz.product.image.ScaleImage
 */
public abstract class ProductImageWorker {

    protected ProductImageWorker() {
    }

    /**
     * SCIPIO: Returns the full path to the ImageProperties.xml file to use for product image size definitions.
     * Uses the one from product component if available; otherwise falls back on the generic one under content component.
     * Added 2017-07-04.
     */
    public static String getProductImagePropertiesFullPath() {
        String path = ContentImageWorker.getImagePropertiesFullPath("/applications/product/config/ImageProperties.xml");
        if (new java.io.File(path).exists()) {
            return path;
        } else {
            return ContentImageWorker.getContentImagePropertiesFullPath();
        }
    }
}
