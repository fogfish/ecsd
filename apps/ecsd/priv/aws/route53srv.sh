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
set -u
set -e

ACTN=$1
PORT=$2
FQDN=$3
ZONE=$4
HOST=$(curl -s --connect-timeout 5 http://169.254.169.254/latest/meta-data/hostname || echo "localhost")

##
##
REQUEST=$(mktemp /tmp/route53a.XXXXXXXX)
cat > ${REQUEST} << EOF
{
   "Comment":"Auto update @ `date`",
   "Changes":
   [
      {
         "Action":"${ACTN}",
         "ResourceRecordSet":
         {
            "ResourceRecords":
            [
               {
                  "Value": "1 1 ${PORT} ${HOST}"
               }
            ],
            "Weight": 100,
            "SetIdentifier": "${HOST}:${PORT}",
            "Name": "${FQDN}",
            "Type": "SRV",
            "TTL": 0
         }
      }
   ]
}
EOF

aws route53 change-resource-record-sets \
   --hosted-zone-id ${ZONE} \
   --change-batch file://"$REQUEST"

rm -f $REQUEST

