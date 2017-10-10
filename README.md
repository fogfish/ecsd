# Microservices supervisor with ECS

The solution implements a supervisor daemon for microservices deployable to Amazon ECS. It aims to solve service discovery, load balancing for microservices.  


## Key Features

**DNS load balancer** makes a modification to AWS Route53 service with public/private IP of your microservices. The feature is an alternative solution to AWS ELB if [the cost overhead](https://aws.amazon.com/elasticloadbalancing/classicloadbalancer/pricing/) is redundant. Especially, when you are building extra small service. 

**Service discovery with DNS** as defined by [RFC 2782](https://tools.ietf.org/html/rfc2782). The Domain Name System has Service record `SRV` to publish port and hostname of servers for specified services. This DNS feature helps microservices to discover a dynamic port allocation in container environments.

**ECS Cluster configuration** template to spawn ECS cluster using Infrastructure-as-a-Code.


## Inspiration 

The daemon targets fault-tolerance and simplicity for feature development and provisioning. Its development paradigm aims on shell scripts usage as code-block to executed cloud management transaction. The AWS SDK exists for many platforms, however the usage of `awscli` requires less LoC to implement and validate invocation of management transaction. The shell script is orchestrated by let-it-crash paradigm that ensures consistency of transaction invocation and rollback in case of error.   


## Getting Started

The daemon supplies pre-built releases for Amazon Linux/x86_64. You can download them from GitHub release pages.

Build the latest version of the daemon from the master branch. The build process requires [Erlang/OTP](http://www.erlang.org/downloads) version 19.0 or later. All development, including new features and bug fixes, take place on the `master` branch using forking and pull requests as described in contribution guidelines.


### Spawn ECS cluster with the supervisor daemon

The easiest way to start with the daemon is a [Cloud Formation template](rel/ecs.yaml). The template spawn a new ECS cluster into your AWS account and automatically provision the daemon to each cluster node. Use the following command and define mandatory parameters:
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

The cluster is ready for usage via [ECS command line utilities](http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ECS_CLI.html).


### Enable DNS load balancing.

Microservices uses `SERVICE_FQDN` and `SERVICE_FQDN_LOCAL` environment variable to define its fully qualified domain name. The daemon listens for changes of its containers runtime. It `UPSERT` or `DELETE` record set `A` when container starts or dies.     

Example of docker compose file
```
version: '2'
services:
   echo:
      image: fogfish/echo
      mem_limit: 1m
      ports:
         - "80:8888"
      environment:
         - "SERVICE_FQDN=echo.example.com"
```

Deploy the service to ECS cluster
```
ecs-cli compose --project-name echo --file echo.yaml service up
```

The service is available at specified domain name
```
curl http://echo.example.com/
```


### Enable Service discovery with DNS.

Microservices uses `SERVICE_PORT_port` environment variable to define a port mapping of its fully qualified domain name. The daemon listens for changes of its containers. It `UPSERT` or `DELETE` record set `SRV` when container starts or dies.

Example of docker compose file
```
version: '2'
services:
   echo:
      image: fogfish/echo
      mem_limit: 1m
      ports:
         - "80:8888"
      environment:
         - "SERVICE_PORT_80=echo.example.com"
```

Deploy the service to ECS cluster
```
ecs-cli compose --project-name echo --file echo.yaml service up
```

The service consumers uses the name `echo.example.com` to discover services and balance the requests between different containers. 

For example, service discovery with in bash:
```
dig echo.example.com SRV
```

For example, service discovery in Erlang
```erlang
inet_res:lookup("echo.example.com", any, srv).
```


### IAM Task Roles

Please refer to [IAM Task Roles](http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html#create_task_iam_policy_and_role) guideline from Amazon. The feature allow to specify an access roles for each microservices running within the cluster.

Define a IAM Role for your container and output its arn 
```
  DynamoDBAccessPolicy:
    Type: 'AWS::IAM::Role'
    Properties:
      RoleName: 
        Fn::Join:
          - "-"
          -
            - !Ref Env
            - "ddb"
            - "io"
      AssumeRolePolicyDocument:
        Version: 2012-10-17
        Statement:
          - Effect: Allow
            Principal:
              Service:
                - ecs-tasks.amazonaws.com
            Action:
              - 'sts:AssumeRole'
      Path: /
      ManagedPolicyArns:
        - "arn:aws:iam::aws:policy/AmazonDynamoDBFullAccess"
```

Spawn your service
```
ecs-cli compose --task-role-arn live-ddb-io ...
```


### Enable service discovery with ALB

The cluster template defines application load balancer and all other resources required to operate container. The ALB feature allow to use AWS provisioned SSL certificate and terminate SSL traffic.

You need to provision the cluster with two arguments
* `LoadBalancer` valid Load Balancer schema such as `internal` or `internet-facing`. The stack creates the ALB only if this parameter is defined.
* `SSLCertificate` ARN identify of SSL Certificate, as it is visible at your AWS console.

```
aws cloudformation create-stack \
   --stack-name ${ECS_CLUSTER} \
   --capabilities CAPABILITY_NAMED_IAM \
   --template-body file://./rel/ecs.yaml \
   --parameters \
      ...
      ParameterKey=LoadBalancer,ParameterValue=internet-facing \
      ParameterKey=SSLCertificate,ParameterValue=...
```

The stack create ALB, security group configurations, the listener and default target group. Use `EcsLoadBalancerListener` ARN to bind a microservice specific TargetGroups with this load balancer.

A typical micorservice deployment will require a ListenerRules and TargetGroup. You can manage them via Cloud Formation template

```
  LbListenerRule:
    Type: AWS::ElasticLoadBalancingV2::ListenerRule
    Properties:
      Actions:
        - Type: forward
          TargetGroupArn:
            Ref: LbTargetGroup
      Conditions:
        - Field: path-pattern
          Values:
            - "/service/*"
      ListenerArn: 
        Fn::ImportValue: !Sub ${NetworkStack}-SubnetID
      Priority: 1

  LbTargetGroup:
    Type: AWS::ElasticLoadBalancingV2::TargetGroup
    Properties:
      HealthCheckIntervalSeconds: 60
      UnhealthyThresholdCount: 10
      HealthCheckPath: /
      Name: !Sub ${Env}-void-${Vsn}-lb
      Port: 80
      Protocol: HTTP
      VpcId: 
        Fn::ImportValue: !Sub ${NetworkStack}-SubnetID
```

Deploy the service with following command
```
ecs-cli compose ... service up --target-group-arn ... --container-name ... --container-port ... --role ...
```


## Next Steps

* Get in touch with [Elastic Container Service](https://aws.amazon.com/ecs/)
* Learn Infrastructure-as-a-Code as it is defined by [Cloud Formation](https://aws.amazon.com/cloudformation/)


## Contribution

The daemon is Apache 2.0 licensed and accepts contributions via GitHub pull requests:

* Fork the repository on GitHub
* Read build instructions
* Make a pull request

The build process requires [Erlang/OTP](http://www.erlang.org/downloads) version 19.0 or later and essential build tools.

**Build** and **run** service in your development console. The following command boots Erlang virtual machine and opens Erlang shell.

```
git clone https://github.com/fogfish/ecsd
cd ecsd
make
make run
```

The development of ecsd daemon requires
* a valid AWS access credentials or access token 
* pre-configured Route 53 hosted zone and export its identity `export ECS_HOSTED_ZONE=...`. 

Now you are able to start ecsd is **debug** mode, it start to monitor your local docker environment and apply management actions accordingly. 

```erlang
ecsd:start().
```

**Package** the application into bundle tar-ball archive, which is as-is deployable to any host. 

```
make clean
make pkg PLAT=Linux
```
The archive `ecsd-{vsn}.{arch}.{plat}.bundle` contains both a Erlang VM, all required dependencies and the daemon.

### commit message

The commit message helps us to write a good release note, speed-up review process. The message should address two question what changed and why. The project follows the template defined by chapter [Contributing to a Project](http://git-scm.com/book/ch5-2.html) of Git book.

>
> Short (50 chars or less) summary of changes
>
> More detailed explanatory text, if necessary. Wrap it to about 72 characters or so. In some contexts, the first line is treated as the subject of an email and the rest of the text as the body. The blank line separating the summary from the body is critical (unless you omit the body entirely); tools like rebase can get confused if you run the two together.
> 
> Further paragraphs come after blank lines.
> 
> Bullet points are okay, too
> 
> Typically a hyphen or asterisk is used for the bullet, preceded by a single space, with blank lines in between, but conventions vary here
>
>

### bugs

If you experience any issues with OAuth 2.0 appliance, please let us know via [GitHub issues](https://github.com/fogfish/oauth2/issue). We appreciate detailed and accurate reports that help us to identity and replicate the issue. 

* **Specify** the configuration of your environment. Include which operating system you use and the versions of runtime environments. 

* **Attach** logs, screenshots and exceptions, in possible.

* **Reveal** the steps you took to reproduce the problem.


## Changelog

The appliance uses [semantic versions](http://semver.org) to identify stable releases. 

* 0.0.0 - initial release 


## License

Copyright 2017 Dmitry Kolesnikov

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.


