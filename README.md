```
**************************************************************************

   _____    _____   _   _____    _    ____      _____   _____    _____
  / ____|  / ____| | | |  __ \  | |  / __ \    |  ___| |  __ \  |  __ \
 | (___   | |      | | | |__) | | | | |  | |   | |___  | |__) | | |__) |
  \___ \  | |      | | |  ___/  | | | |  | |   |  ___| |  _  /  |  ___/
  ____) | | |____  | | | |      | | | |__| |   | |___  | | \ \  | |
 |_____/   \_____| |_| |_|      |_|  \____/    |_____| |_|  \_\ |_|

                    https://www.scipioerp.com

**************************************************************************
```
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-blue.svg?style=flat)](http://makeapullrequest.com) [![Maintainability](https://api.codeclimate.com/v1/badges/0193ee026d92287b5db0/maintainability)](https://codeclimate.com/github/ilscipio/scipio-erp/maintainability) [![Read the Docs](https://img.shields.io/readthedocs/pip.svg)](https://www.scipioerp.com/community/developer/installation-configuration/)

# SCIPIO ERP - Community Edition
![Scipio ERP is a toolkit for the development of modern business applications.](https://www.scipioerp.com/wp-content/uploads/2023/02/multichannel4-480x415.png)

* [Website](https://www.scipioerp.com)
* [Demo](https://www.scipioerp.com/demo/)
* [Developer Docs](https://www.scipioerp.com/community/developer/installation-configuration/)
* [User Docs](https://www.scipioerp.com/community/end-user/applications/)

## What is Scipio ERP
Scipio ERP is an Open Source Business Application Toolkit based on Java 11+ and a built-in
Tomcat application server. We provide standard applications, functions (services)
and a well thought-out datamodel, so that you can create modern web applications.

Our templating toolkit simplifies the creation of modern UIs and is compatible with most
modern HTML frameworks.

[Technologies](https://www.scipioerp.com/products/technologies/)

### TL;DR
* Simplifies the creation of business or ECommerce applications
* Highly modular, extendable and customizable
* Bundles a long list of working applications
* Supports most modern HTML frameworks
* Supports Caching & Clustering
* Can be rolled out internationally

## New in 4.0
* **Annotations instead of XML.** Screens, forms, menus, trees, services, entities, ECA rules and
  controllers are Java annotations (`@Screen`, `@Request`, `@Service`, `@Entity`, ...). The IDE completes
  them and the compiler checks them. A converter migrates existing XML components.
* **Agent access built in.** Every application answers the Model Context Protocol at
  `POST /<webapp>/mcp` with curated tools, bearer tokens, permissions and an audit log.
  See `docs/AGENT-QUICKSTART.md`, `docs/AGENT-SECURITY.md` and `docs/EXTENDING-AGENTIC.md`.
* **Aurora.** A new default backend theme without a CSS framework, with a light and a dark scheme.
* **Manufacturing, rebuilt.** Production lines, fabrication orders, lot reservation, barcode capture,
  MRP with supplier lead times, product costing and a shop floor board.
* **Gradle build on Java 11, 17 and 21.** Ant is gone. Components under `hot-deploy/` and `addons/`
  are discovered automatically; `./gradlew createComponent` scaffolds an annotation-based component.

## What's included
* Business Applications & functions for
  * Accounting
  * Asset Maintenance
  * Catalog Management
  * Content Management
  * Customer Relationship Management (CRM)
  * Ecommerce
  * Human Resource Management
  * Manufacturing Management
  * Order Management
  * User Management
  * Warehouse Management
  * Work Effort (Time tracking)
* A templating toolkit (freemarker macros) to simplify UI creation
* A multi-language, multi-national, multi-store Ecommerce application
* A flexible datamodel
* Support of various third-party APIs (payment, shipping, apache camel, etc.)

## Installation
### System Requirements
* Operating System: Windows, Linux, OS X
* Core 2 Duo or Athlon X2 at 2.4 GHz or higher
* 4+GB RAM, 2+GB free hard disk space

### Software Requirements
* Java 11, 17, or 21 JDK (OpenJDK or Oracle)

### Recommended Development Tools
* Git client
* JetBrains IntelliJ IDEA with Scipio ERP Plugin

### Prerequisites
In order to install SCIPIO ERP, the following prerequisites must be installed:
* Java 11, 17, or 21 JDK (LTS versions recommended)
  * Download and Install
  * Set JAVA_HOME Path (Windows)

### Download
The standard way to get SCIPIO ERP is to checkout the master (main) branch, which
provides you with the latest stable version with important updates:

1. Open your command line and run:
  * git clone https://github.com/ilscipio/scipio-erp.git
  * cd scipio-erp
  * git checkout master

Note it is also possible to checkout specific version tags as well as stable
version series this way (e.g., git checkout 3.0.0), but main branch typically
contains the desirable setup for most demo, server and client usages.

### Installation Process
In order to install on a client system or start on a server, the following steps should be used:

1. Open your command line, go to the extracted folder and run:
  * Linux/OS X: `./gradlew build loadDemo`
  * Windows: `gradlew.bat build loadDemo`

2. From the same command line run:
  * Linux/OS X: `./gradlew start`
  * Windows: `gradlew.bat start`

3. To access the application visit the SCIPIO ERP Dashboard:
  https://localhost:8443/admin

4. To access the SCIPIO ERP applications from the Dashboard use:
  Username: admin
  Password: scipio

Note: These steps are typically too limited for developers; see section below.

                     **Congratulations, you have installed SCIPIO ERP!**

### Updates
1. Retrieve latest code updates from git:
a. git checkout master
b. git pull

2. Reload visual themes:
a. Restart SCIPIO server
b. Visit entity utility services page:
  https://localhost:8443/admin/control/EntityUtilityServices
c. Click "Visual Theme Resources - Reload All, Now"

### Optional Configuration
  https://www.scipioerp.com/community/developer/installation-configuration/configuration/

### Addons
Community and enterprise SCIPIO ERP addons can be added to your working
copy and updated using the 'git-addons' Bash script in the project root.
It requires a Bash 4-compatible terminal (Linux, Mac, Windows Git Bash, Cygwin).

Instructions can be found at:

  https://www.scipioerp.com/community/end-user/addons/

or for brief help and command list, type:

  ./git-addons help

### Docker
We also provide Docker images if you would like to try out Scipio with minimal effort. 
To create a fully functional SCIPIO ERP instance with some demo data already loaded, 
you can create a container with the following command:

  docker create -p 8080:8080 -p 8443:8443 ilscipio/scipio-erp:demo

### Development
For developers, the install/start commands above are typically too limited.
The JetBrains IntelliJ IDEA integrated development environment with Scipio ERP Plugin
(found in the integrated IDEA plugin store) is highly recommended.

#### Gradle Build System

SCIPIO ERP uses Gradle for building and dependency management.

1. Clear local database (Derby), clean out old JARs, build, load demo data and start with debug:
  * Linux/OS X: `./gradlew clean build loadDemo startDebug`
  * Windows: `gradlew.bat clean build loadDemo startDebug`

Commands can be listed using: `./gradlew tasks` (or `gradlew.bat tasks`)

Commonly used Gradle developer commands:
  * `clean` - Clean build artifacts
  * `build` - Compile and build all components
  * `compileJava` - Compile only (faster)
  * `loadDemo` - Load demo data to database
  * `loadSeed` - Load seed data only
  * `start` - Start the server
  * `startDebug` - Start with debug port 5005
  * `stop` - Stop the server

Key build files:
  * `settings.gradle.kts` - Multi-project configuration
  * `build.gradle.kts` - Root build configuration
  * `gradle/libs.versions.toml` - Centralized dependency versions

#### Creating New Components
Create a new component in hot-deploy:
```bash
./gradlew createComponent -PcomponentName=mycomponent -PresourceName=MyComponent
```

Create a shop component:
```bash
./gradlew createShopComponent -PcomponentName=myshop -PresourceName=MyShop
```

Optional parameters: `-PwebappName=name`, `-PbasePermission=PERMISSION`, `-PcomponentPackage=org.example`

#### Addon Compatibility
Existing Ant-based addons remain compatible with the Gradle-built framework.

This is a quick cheat sheet and further information for developers can be found on the website documentation.

## Support
For detailed information and changes about the SCIPIO ERP suite, visit the official website at:

  [https://www.scipioerp.com](https://www.scipioerp.com "Scipio ERP Website")

You can get in touch with the us at:

  [https://www.ilscipio.com](https://www.ilscipio.com "Ilscipio")

## OFBiz
Scipio ERP is a fork of the Apache OFBiz project.

For more details about OFBiz please visit the OFBiz Documentation page:

  http://ofbiz.apache.org/documentation.html

## License
The Scipio 4.0 core (Scipio Commerce) is licensed under the GNU Affero General
Public License, version 3 (AGPL-3.0). Ilscipio GmbH also offers a commercial
license. The file LICENSE holds the full AGPL-3.0 text and a note on the
commercial license.

Files that came from Apache OFBiz keep their Apache header and stay under the
Apache License, version 2.0. The Apache License text is in the file LICENSE
(part B). The file NOTICE holds the notices that the Apache License requires.

Libraries have their own licenses. The file THIRD-PARTY-NOTICES.md lists them
with their license texts and NOTICE texts. Run `gradlew generateThirdPartyNotices`
to write it again.

Each source file has a license header. The task `gradlew checkLicenseHeaders`
fails on a file without a header. The rules are in `buildSrc/license-headers/rules.txt`
and in `docs/wp/L-01.md`.

## Disclaimer
This software is provided as is and free of charge. There is no warranty
or support implied under the terms of the license included.

SCIPIO ERP and the SCIPIO logo are trademarks of Ilscipio GmbH.
© Copyright SCIPIO components 2016 Ilscipio GmbH.
Apache OFBiz, Apache, the Apache feather logo are trademarks
of The Apache Software Foundation.

### BIS Crypto TSU exception notice

   This distribution includes cryptographic software.  The country in
   which you currently reside may have restrictions on the import,
   possession, use, and/or re-export to another country, of
   encryption software.  BEFORE using any encryption software, please
   check your country's laws, regulations and policies concerning the
   import, possession, or use, and re-export of encryption software, to
   see if this is permitted.  See <http://www.wassenaar.org/> for more
   information.

   The U.S. Government Department of Commerce, Bureau of Industry and
   Security (BIS), has classified this software as Export Commodity
   Control Number (ECCN) 5D002.C.1, which includes information security
   software using or performing cryptographic functions with asymmetric
   algorithms.  The form and manner of this Apache Software Foundation
   distribution makes it eligible for export under the License Exception
   ENC Technology Software Unrestricted (TSU) exception (see the BIS
   Export Administration Regulations, Section 740.13) for both object
   code and source code.

   The following provides more details on the included cryptographic
   software:

    * Various classes in Scipio, including DesCrypt, HashCrypt, and
     BlowFishCrypt use libraries from the Sun Java JDK API including
     java.security.* and javax.crypto.* (the JCE, Java Cryptography
     Extensions API)
    * Other classes such as HttpClient and various related ones use
     the JSSE (Java Secure Sockets Extension) API
