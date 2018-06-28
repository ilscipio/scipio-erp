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
package org.ofbiz.base.component;

import org.ofbiz.base.config.GenericConfigException;

/**
 * ComponentException
 *
 */
@SuppressWarnings("serial")
public class ComponentException extends GenericConfigException {

    public ComponentException() {
        super();
    }

    public ComponentException(String str) {
        super(str);
    }

    public ComponentException(Throwable nested) {
        super(nested);
    }

    public ComponentException(String str, Throwable nested) {
        super(str, nested);
    }
    
    /**
     * SCIPIO: Exception specifically to indicate a component was not found, so callers can handle.
     * WARN/FIXME: currently (2017-08-03), not enough code will be using this for it to be reliable everywhere, so 
     * cannot rely on methods throwing the appropriate subclass at this time.
     * Added 2017-08-03.
     */
    public static class ComponentNotFoundException extends ComponentException {

        protected ComponentNotFoundException() {
            super();
        }

        protected ComponentNotFoundException(String str, Throwable nested) {
            super(str, nested);
        }

        protected ComponentNotFoundException(String str) {
            super(str);
        }

        protected ComponentNotFoundException(Throwable nested) {
            super(nested);
        }

    }
    
}
