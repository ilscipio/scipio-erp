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

package org.ofbiz.base.util;

import org.ofbiz.base.crypto.HashCrypt;

public class CredentialUtil {

    public static boolean checkPassword(String oldPassword, boolean useEncryption, String currentPassword) {
        boolean passwordMatches = false;
        if (oldPassword != null) {
            if (useEncryption) {
                passwordMatches = HashCrypt.comparePassword(oldPassword, HashCrypt.getPasswordEncryptHashType(), currentPassword);
            } else {
                passwordMatches = oldPassword.equals(currentPassword);
            }
        }
        if (!passwordMatches && "true".equals(UtilProperties.getPropertyValue("security", "password.accept.encrypted.and.plain"))) {
            passwordMatches = currentPassword.equals(oldPassword);
        }
        return passwordMatches;
    }
}
