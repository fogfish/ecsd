# Microservices supervisor with ECS

The solution implements a supervisor daemon for microservices deployable to Amazon ECS.  


## Key Features

**DNS load balancer**

**Service discovery**

**ECS Cluster configuration**


## Inspiration 


## Getting Started

The daemon supplies pre-built releases for Amazon Linux/x86_64. You can download them from GitHub release pages.

The easiest way to start with the daemon is a [Cloud Formation template](relecs.yaml). The template spawn a new ECS cluster into your AWS account and automatically provision the daemon to each cluster node. You need to define following mandatory parameters:
* cluster identity
* [Amazon ECS-Optimized AMI](http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html)
* [Route 53 Hosted Zone](http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/Welcome.html)  

```
export ECS_CLUSTER=ecsd
export ECS_AMI=ami-809f84e6
export ECS_HOSTED_ZONE=...

aws cloudformation create-stack \
   --stack-name ${ECS_CLUSTER} \
   --capabilities CAPABILITY_NAMED_IAM \
   --template-body file://./rel/ecs.yaml \ 
   --parameters \
      ParameterKey=EcsAmiId,ParameterValue=${ECS_AMI} \
      ParameterKey=EcsCluster,ParameterValue=${ECS_CLUSTER} \
      ParameterKey=HostedZoneId,ParameterValue=${ECS_HOSTED_ZONE}
```
