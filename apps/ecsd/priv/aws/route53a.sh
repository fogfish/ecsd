#!/bin/sh
set -u
set -e

ACTN=$1
FQDN=$2
ZONE=$3
ADDR=$(curl -s --connect-timeout 5 http://169.254.169.254/latest/meta-data/public-ipv4 || echo "127.0.0.1")

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
                  "Value": "${ADDR}"
               }
            ],
            "Weight": 100,
            "SetIdentifier": "${ADDR}",
            "Name": "${FQDN}",
            "Type": "A",
            "TTL": 10
         }
      }
   ]
}
EOF

aws route53 change-resource-record-sets \
   --hosted-zone-id ${ZONE} \
   --change-batch file://"$REQUEST"

rm -f $REQUEST

