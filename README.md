
# Scientific-Search

This is the repository used for scientific-search project. Code based on Odinson ("https://gh.lum.ai/odinson/") engine.

# Run

## Step 1: Setup
Clone the repo on the cluster and setup the java/sbt version.
Once cloned, run from the main repo:

```sh
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
export PATH=$JAVA_HOME/bin/:$PATH
```

Now you can run the backend by runing this command(from the main repo)
```
sbt "frontend/run 8080"
```

## Step 2: Run frontend

From the local machine set up an ssh tunnel to the cluster:
```sh
ssh -L 8080:compsci-login-01.cs.duke.edu:8080 netid@compsci-login-01.cs.duke.edu
```

Where netid is your netid.

## See the frontend

On your local machine, go to localhost:8080 and you should be able to see it.
# Note

Make sure to set the doc, text, index paths in extra/src/main/resources/application.conf to FULL paths in your machine (i.e. /usr/....../scientificsearch/extra/data/PubMed/docs, etc) to avoid empty corpus in the engine. 
Demo Data save in in extra/data/demo_data. 


