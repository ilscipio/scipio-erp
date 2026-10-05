#!/usr/bin/env bash
# Scipio Commerce
# Copyright (C) Ilscipio GmbH
#
# This file is part of Scipio Commerce. Scipio Commerce is free software: you
# can redistribute it and modify it under the terms of the GNU Affero General
# Public License, version 3, as published by the Free Software Foundation.
# Scipio Commerce is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
# for more details. You should have received a copy of the license with this
# work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
# A commercial license is available from Ilscipio GmbH.
#
# SPDX-License-Identifier: AGPL-3.0-only
# SCIPIO: Scipio Commerce (W1-02): switch the component list of this source tree.
#   bin/use-profile.sh commerce   store pod: no manufacturing, no humanres (profile/commerce-component-load.xml)
#   bin/use-profile.sh full        all applications (git checkout of applications/component-load.xml)
# A pod image build runs "commerce" before the Gradle build. The change is a plain file copy.
set -euo pipefail
HERE="$(cd "$(dirname "$0")/.." && pwd)"
TARGET="$HERE/../component-load.xml"
case "${1:-}" in
  commerce) cp "$HERE/profile/commerce-component-load.xml" "$TARGET"; echo "component list: commerce profile" ;;
  full) git -C "$HERE" checkout -- "$TARGET"; echo "component list: full" ;;
  *) echo "usage: use-profile.sh commerce|full" >&2; exit 2 ;;
esac
