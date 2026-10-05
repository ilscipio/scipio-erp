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


import org.apache.commons.io.FileUtils
import org.ofbiz.base.util.FileUtil
import org.ofbiz.base.util.UtilProperties
import org.ofbiz.testtools.GroovyScriptTestCase

class FileUtilTests extends GroovyScriptTestCase {

    /**
     * Test FileUtil zipFileStream and unzipFileToFolder methods, using README.adoc
     */
    void testZipReadme() {
        String zipFilePath = UtilProperties.getPropertyValue("general", "http.upload.tmprepository", "runtime/tmp")
        // SCIPIO
        //String zipName = 'README.adoc.zip'
        //String fileName = 'README.adoc'
        String zipName = 'README.md.zip'
        String fileName = 'README.md'
        File originalReadme = new File(fileName)

        //validate zipStream from README.adoc is not null
        def zipStream = FileUtil.zipFileStream(originalReadme.newInputStream(), fileName)
        assert zipStream

        //ensure no zip already exists
        File readmeZipped = new File(zipFilePath, zipName)
        if (readmeZipped.exists()) readmeZipped.delete()

        //write it down into tmp folder
        OutputStream out = new FileOutputStream(readmeZipped)
        byte[] buf = new byte[8192]
        int len
        while ((len = zipStream.read(buf)) > 0) {
            out.write(buf, 0, len)
        }
        out.close()
        zipStream.close()

        //ensure no README.adoc exist in tmp folder
        File readme = new File(zipFilePath, fileName)
        if (readme.exists()) readme.delete()

        //validate unzip and compare the two files
        FileUtil.unzipFileToFolder(readmeZipped, zipFilePath)

        assert FileUtils.contentEquals(originalReadme, new File(zipFilePath, fileName))
    }
}
