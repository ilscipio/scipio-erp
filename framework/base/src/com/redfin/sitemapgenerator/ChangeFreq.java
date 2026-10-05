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

/**
 * How frequently the page is likely to change. This value provides
 * general information to search engines and may not correlate exactly
 * to how often they crawl the page. The value {@link #ALWAYS} should be used to
 * describe documents that change each time they are accessed. The value
 * {@link #NEVER} should be used to describe archived URLs.
 * 
 * <p>Please note that the
 * value of this tag is considered a <em>hint</em> and not a command. Even though
 * search engine crawlers may consider this information when making
 * decisions, they may crawl pages marked {@link #HOURLY} less frequently than
 * that, and they may crawl pages marked {@link #YEARLY} more frequently than
 * that. Crawlers may periodically crawl pages marked {@link #NEVER} so that
 * they can handle unexpected changes to those pages.</p>
 */
public enum ChangeFreq {
	ALWAYS, HOURLY, DAILY, WEEKLY, MONTHLY, YEARLY, NEVER;
	String lowerCase;
	private ChangeFreq() {
		lowerCase = this.name().toLowerCase();
	}
	
	@Override
	public String toString() {
		return lowerCase;
	}
}
