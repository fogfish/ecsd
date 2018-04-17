#!/bin/sh

set -x
until curl -s http://localhost:51678/v1/metadata
do
   sleep 1
done

instance_arn=$(curl -s http://localhost:51678/v1/metadata | jq -r '. | .ContainerInstanceArn' | awk -F/ '{print $NF}' )
cluster=$(curl -s http://localhost:51678/v1/metadata | jq -r '. | .Cluster' | awk -F/ '{print $NF}' )
region=$(curl -s http://localhost:51678/v1/metadata | jq -r '. | .ContainerInstanceArn' | awk -F: '{print $4}')

aws ecs start-task --cluster $cluster --task-definition $1 --container-instances $instance_arn --started-by $instance_arn --region $region
