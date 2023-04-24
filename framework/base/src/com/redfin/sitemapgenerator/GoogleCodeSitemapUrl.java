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

package com.redfin.sitemapgenerator;

import java.net.MalformedURLException;
import java.net.URL;

/**
 * One configurable Google Code Search URL.  To configure, use {@link Options}
 * @author Dan Fabulich
 * @see Options
 * @see <a href="http://www.google.com/support/webmasters/bin/answer.py?answer=75224">Creating Code Search Sitemaps</a>
 */
public class GoogleCodeSitemapUrl extends WebSitemapUrl {

	/** The type of code represented by this URL
	 * 
	 * @author Dan Fabulich
	 * @see <a href="http://www.google.com/support/webmasters/bin/answer.py?answer=75252">Supported languages</a>
	 */
	public enum FileType { 
		/** A special value meaning that the URL is a compressed archive containing code.
		 * @see @see <a href="http://www.google.com/support/webmasters/bin/answer.py?answer=75259">Supported archive suffixes</a>
		 */
		ARCHIVE("Archive"),
		ADA("Ada"),
		APPLESCRIPT("AppleScript"),
		ASP("ASP"),
		ASSEMBLY("Assembly"),
		AUTOCONF("Autoconf"),
		AUTOMAKE("Automake"),
		AWK("Awk"),
		BASIC("Basic/Visual Basic"),
		BAT("batch file"),
		C("C"),
		CPP("C++"),
		CSHARP("C#"),
		COBOL("COBOL"),
		COLDFUSION("ColdFusion"),
		CONFIGURE("configure script"),
		CPUDESCGCC("GCC machine description"),
		CPUDESCLCC("LCC machine description"),
		CPUDESCMONO("Mono machine description"),
		CPUDESCVCODE("VCODE machine description"),
		CSS("CSS"),
		CSV("CSV"),
		D("D"),
		EIFFEL("Eiffel"),
		ERLANG("Erlang"),
		FORTRAN("Fortran"),
		HASKELL("Haskell"),
		HTML("HTML"),
		JAVA("Java"),
		JAVASCRIPT("JavaScript"),
		JSP("JSP"),
		LEX("Lex"),
		LIMBO("Limbo"),
		LISP("Lisp"),
		LUA("Lua"),
		M4("m4"),
		MAKEFILE("Makefile"),
		MATHEMATICA("Mathematica"),
		MATLAB("Matlab"),
		MODULA2("Modula-2"),
		MODULA3("Modula-3"),
		OBJECTIVEC("Objective C"),
		OCAML("OCaml"),
		PASCAL("Pascal/Delphi"),
		PATCH("diff file"),
		PERL("Perl"),
		PHP("PHP"),
		POD("Plain Old Documentation"),
		MESSAGECATALOG("message catalog"),
		POSTSCRIPT("PostScript"),
		PROLOG("Prolog"),
		PYTHON("Python"),
		R("R"),
		REBOL("REBOL"),
		RUBY("Ruby"),
		SCHEME("Scheme"),
		SHELL("Shell"),
		SGML("SGML"),
		SMALLTALK("Smalltalk"),
		SQL("SQL"),
		SML("Standard ML"),
		SVG("SVG"),
		TCL("Tcl"),
		TEX("TeX/LaTeX"),
		TEXINFO("Texinfo"),
		TROFF("Troff"),
		VHDL("VHDL"),
		VIM("Vim script"),
		XML("XML"),
		XPM("XPM"),
		XSLT("XSLT"),
		XSUB("XSUB"),
		XUL("XUL"),
		YACC("Yacc");
		
		private final String longName;
		FileType(String longName) {
			this.longName = longName;
		}
		
		/** The pretty name for this filetype */
		public String getLongName() {
			return longName;
		}
		
		@Override
		public String toString() {
			if (this == CPP) return "c++";
			if (this == CSHARP) return "c#";
			return this.name().toLowerCase();
		};
	}
	
	/** The license of the code represented by this URL
	 * 
	 * @author Dan Fabulich
	 * @see <a href="http://www.google.com/support/webmasters/bin/answer.py?answer=75256">Supported licenses</a> 
	 */
	public enum License {
		ALADDIN("Aladdin Public License"),
		ARTISTIC("Artistic License"),
		APACHE("Apache License"),
		APPLE("Apple Public Source License"),
		BSD("BSD License"),
		CPL("Common Public License"),
		GPL("GNU General Public License"),
		LGPL("GNU Lesser General Public License"),
		DISCLAIMER("Historical Permission Notice and Disclaimer"),
		IBM("IBM Public License"),
		LUCENT("Lucent Public License"),
		MIT("MIT License"),
		MOZILLA("Mozilla Public License"),
		NASA("NASA Open Source Agreement"),
		PYTHON("Python Software Foundation License"),
		QPL("Q Public License"),
		SLEEPYCAT("Sleepycat License"),
		ZOPE("Zope Public License");
		
		private final String longName;
		License(String longName) {
			this.longName = longName;
		}
		
		/** The pretty name for this license */
		public String getLongName() {
			return longName;
		}
		
		@Override
		public String toString() {
			return this.name().toLowerCase();
		};
	}
	
	private final String fileType;
	private final String license;
	private final String fileName;
	private final URL packageUrl;
	private final String packageMap;

	/** Options to configure Google Code Search URLs */
	public static class Options extends AbstractSitemapUrlOptions<GoogleCodeSitemapUrl, Options> {
		private String fileType;
		private String license;
		private String fileName;
		private URL packageUrl;
		private String packageMap;

		/** Specifies an url and a filetype (both mandatory in Google Code Search) */
		public Options(String url, FileType fileType) throws MalformedURLException {
			super(url, GoogleCodeSitemapUrl.class);
			this.fileType = fileType.toString();
		}
		
		/** Specifies an url and a filetype (both mandatory in Google Code Search) */
		public Options(URL url, FileType fileType) {
			super(url, GoogleCodeSitemapUrl.class);
			this.fileType = fileType.toString();
		}
		
		/** Specifies an url and a filetype (both mandatory in Google Code Search) */
		public Options(String url, String fileType) throws MalformedURLException {
			super(url, GoogleCodeSitemapUrl.class);
			this.fileType = fileType;
		}
		
		/** Specifies an url and a filetype (both mandatory in Google Code Search) */
		public Options(URL url, String fileType) {
			super(url, GoogleCodeSitemapUrl.class);
			this.fileType = fileType;
		}
		
		/** Specifies code license */
		public Options license(License license) {
			this.license = license.toString();
			return this;
		}
		/**
		 * Specifies code license; when the value is not one of the recognized
		 * licenses, this will cause Google to index the item as
		 * "unknown license".
		 */
		public Options license(String license) {
			this.license = license;
			return this;
		}
		/**
		 * The name of the actual file; this is useful if the URL ends in
		 * something like download.php?id=1234 instead of the actual filename.
		 * The name can contain any character except "/". If the file is an
		 * {@link FileType#ARCHIVE} file, it will be indexed only if it has one of the supported
		 * archive suffixes.
		 * 
		 * @see <a href="http://www.google.com/support/webmasters/bin/answer.py?answer=75259">Supported archive suffixes</a>
		 */
		public Options fileName(String fileName) {
			this.fileName = fileName;
			return this;
		}

		/**
		 * The URL truncated at the top-level directory for the package; this
		 * tells Google which files belong together. For use only when the
		 * filetype is not {@link FileType#ARCHIVE}. For example, the file
		 * http://path/Foo/1.23/bar/file.c could have the package URL
		 * http://path/Foo/1.23. All files in a package should have the same
		 * packageurl.
		 */
		public Options packageUrl(URL packageUrl) {
			this.packageUrl = packageUrl;
			return this;
		}
		/**
		 * The URL truncated at the top-level directory for the package; this
		 * tells Google which files belong together. For use only when the
		 * filetype is not {@link FileType#ARCHIVE}. For example, the file
		 * http://path/Foo/1.23/bar/file.c could have the package URL
		 * http://path/Foo/1.23. All files in a package should have the same
		 * packageurl.
		 */
		public Options packageUrl(String packageUrl) throws MalformedURLException {
			this.packageUrl = new URL(packageUrl);
			return this;
		}
		/**
		 * The name of the packagemap file inside an {@link FileType#ARCHIVE};
		 * just like a Sitemap is a list of files on a web site, a packagemap is
		 * a list of files in a package. Case-sensitive. For use only when
		 * filetype is {@link FileType#ARCHIVE}.
		 */
		public Options packageMap(String packageMap) {
			if (!FileType.ARCHIVE.toString().equals(fileType)) {
				throw new IllegalArgumentException("You can only specify a packageMap when the fileType is 'archive'");
			}
			this.packageMap = packageMap;
			return this;
		}
		
	}

	/** Specifies an url and a filetype (both mandatory in Google Code Search) */
	public GoogleCodeSitemapUrl(URL url, FileType fileType) {
		this(new Options(url, fileType));
	}
	
	/** Specifies an url and a filetype (both mandatory in Google Code Search) */
	public GoogleCodeSitemapUrl(String url, FileType fileType) throws MalformedURLException {
		this(new Options(url, fileType));
	}
	
	/** Specifies an url and a filetype (both mandatory in Google Code Search) */
	public GoogleCodeSitemapUrl(URL url, String fileType) {
		this(new Options(url, fileType));
	}
	
	/** Specifies an url and a filetype (both mandatory in Google Code Search) */
	public GoogleCodeSitemapUrl(String url, String fileType) throws MalformedURLException {
		this(new Options(url, fileType));
	}

	public GoogleCodeSitemapUrl(Options options) {
		super(options);
		fileType = options.fileType;
		license = options.license;
		fileName = options.fileName;
		packageUrl = options.packageUrl;
		packageMap = options.packageMap;
	}

	/** Retrieves the {@link Options#fileType} */
	public String getFileType() {
		return fileType;
	}

	/** Retrieves the {@link Options#license} */
	public String getLicense() {
		return license;
	}

	/** Retrieves the {@link Options#fileName} */
	public String getFileName() {
		return fileName;
	}

	/** Retrieves the {@link Options#packageUrl} */
	public URL getPackageUrl() {
		return packageUrl;
	}

	/** Retrieves the {@link Options#packageMap} */
	public String getPackageMap() {
		return packageMap;
	}

	

}
