#!/bin/sh
##
##   Copyright 2017 Dmitry Kolesnikov, All Rights Reserved
##
##   Licensed under the Apache License, Version 2.0 (the "License");
##   you may not use this file except in compliance with the License.
##   You may obtain a copy of the License at
##
##       http://www.apache.org/licenses/LICENSE-2.0
##
##   Unless required by applicable law or agreed to in writing, software
##   distributed under the License is distributed on an "AS IS" BASIS,
##   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
##   See the License for the specific language governing permissions and
##   limitations under the License.
##
## @doc
##   read host identity of AWS node
##    * public-ipv4
##    * local-ipv4
##    * hostname
set -u
set -e

HOST=$(curl -s --connect-timeout 5 http://169.254.169.254/latest/meta-data/$1 || echo "127.0.0.1")
printf '%s' "$HOST"