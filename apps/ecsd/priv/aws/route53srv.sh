#!/bin/sh
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

