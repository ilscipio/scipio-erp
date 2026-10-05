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
package com.redfin.sitemapgenerator;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.xml.XMLConstants;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;

/** Validates sitemaps and sitemap indexes
 * 
 * @author Dan Fabulich
 *
 */
public class SitemapValidator {
	
	//TODO support gzip
	//TODO confirm < 10MB
	//TODO confirm single host
	//TODO confirm correct host
	//TODO confirm UTF-8
	
	//TODO support Mobile/Geo/Video/Code/News (sitemap.xsd doesn't support them)
		//TODO confirm mobile restrictions: no non-mobile urls
		//TODO confirm news restrictions: 3 days, 1000 URLs
		//TODO video restrictions: title, player_loc/content_loc, no non-video urls
		//IMO news should have no non-news urls, geo should have no non-geo urls, code should have no non-code urls
	
	private static Schema sitemapSchema, sitemapIndexSchema;
	
	private synchronized static void lazyLoad() {
		if (sitemapSchema != null)  return;
		SchemaFactory factory =
			SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
		try {
			sitemapSchema = lazyLoad(factory, "sitemap.xsd");
			sitemapIndexSchema = lazyLoad(factory, "siteindex.xsd");
		} catch (Exception e) {
			throw new RuntimeException("BUG", e);
		}
	}

	private synchronized static Schema lazyLoad(SchemaFactory factory, String resource) throws IOException, SAXException {
		InputStream stream = null;

		try {
			stream = SitemapValidator.class.getResourceAsStream(resource);
			if (stream == null) throw new RuntimeException("BUG Couldn't load " + resource);
			StreamSource source = new StreamSource(stream);
			return factory.newSchema(source);
		} finally {
			if(stream != null) {
				stream.close();
			}
		}

	}
	
	/** Validates an ordinary web sitemap file (NOT a Google-specific sitemap) */
	public static void validateWebSitemap(File sitemap) throws SAXException {
		lazyLoad();
		validateXml(sitemap, sitemapSchema);
	}
	
	/** Validates a sitemap index file  */
	public static void validateSitemapIndex(File sitemap) throws SAXException {
		lazyLoad();
		validateXml(sitemap, sitemapIndexSchema);
	}

	private static void validateXml(File sitemap, Schema schema) throws SAXException {
		try {
			Validator validator = schema.newValidator();
			FileReader reader = null;
			try {
				reader = new FileReader(sitemap);
				SAXSource source = new SAXSource(new InputSource(reader));
				validator.validate(source);
			} catch (IOException e) {
				throw new RuntimeException(e);
			} finally {
				if(reader != null) {
					reader.close();
				}
			}
		} catch (IOException ex) {
			throw new RuntimeException("Unable to close stream.", ex);
		}

	}

}
