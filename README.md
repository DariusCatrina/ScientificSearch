
# Scientific-Search

This is the repository used for scientific-search project. Code based on Odinson ("https://gh.lum.ai/odinson/") engine.

# Run

From the main repo:

sbt "extra/runMain ai.lum.odinson.extra.ScientificSearcher"

to start the syntactic search interface CLI.

To start the frontend interface, from the main repo:

sbt "frontend/run". Note that you need to copy and setup the article title database to your machine or error will occur.

# Note

Make sure to set the doc, text, index paths in extra/src/main/resources/application.conf to FULL paths in your machine (i.e. /usr/....../scientificsearch/extra/data/PubMed/docs, etc) to avoid empty corpus in the engine. Existing indexed PubMed data available in extra/data/PubMed. 


